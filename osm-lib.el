;; osm-lib.el --- Library for accessing OpenStreetMap data in Emacs

;; Copyright (C) 2018 
;; Author: Joel Svensson <svenssonjoel@yahoo.se> 

;; This file is part of Emacs-OSM.

;; Emacs-OSM is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Emacs-OSM is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs-OSM.  If not, see <https://www.gnu.org/licenses/>.


;; Lat/long to tile x,y transformation from https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
;; n = 2 ^ zoom
;; xtile = n * ((lon_deg + 180) / 360)
;; ytile = n * (1 - (log(tan(lat_rad) + sec(lat_rad)) / pi)) / 2

;; Example of url for accessing a map tile from the web 
;; https://maps.wikimedia.org/osm-intl/8/137/77.png

(require 'cl)

(defconst osm-lib-center-of-the-universe (cons 57.72101 12.9401))

(defvar osm-lib-map-tiles-url "https://tile.openstreetmap.org/")

(defvar osm-lib-cache-tiles 't
  "Flag indicating whether OSM tiles should be cached or not.")

;; TODO: Or set a number of MB per hierarchy level as the max.. 
(defvar osm-lib-cached-tiles 1024
  "Number of tiles to cache (at most) per zoom level.")

(defvar osm-lib-root-dir "~/emacs_osm/")
(defconst osm-lib-tiles-cache-dir "tile_cache/")
(defconst osm-lib-scratch-work-area "scratch/")

(defun osm-lib-deg-to-rad (degrees)
  "Convert from degrees to radians"
  (* degrees (/ float-pi 180.0)))

(defun osm-lib-rad-to-deg (radians)
  "Convert from radians to degrees"
  (* radians (/ 180.0 float-pi)))

(defun osm-lib-sec (alpha)
  (/ 1 (cos alpha)))

(defun osm-lib-x-tile-index (longitude zoom)
  "Calculates OSM tile x coordinate given longitude (in degrees) and zoom level"
  (let ((n (expt 2.0 zoom)))
    (floor (* n (/ (+ longitude 180.0) 360.0)))))

(defun osm-lib-y-tile-index (latitude zoom)
  "Calculates the OSM tile y coordinate given latitude (in degrees) and zoom level"
  (let ((n (expt 2 zoom))
	(lat-rad (osm-lib-deg-to-rad latitude)))
    (floor (* n (/ (- 1.0 (/ (log (+ (tan lat-rad) (osm-lib-sec lat-rad))) float-pi)) 2)))))

(defun osm-lib-x-y-tile-index (latlong zoom)
  "Calculates the OSM x, y coordinates given a lat & long dotted pair(in degrees) and zoom level"
  (let* ((lng (cdr latlong))
	 (lat (car latlong))
	 (x (osm-lib-x-tile-index lng zoom))
	 (y (osm-lib-y-tile-index lat zoom)))
    (osm-lib-xyzoom x y zoom)))
    ;;(cons x y)))

(defun osm-lib-num-tiles (zoom)
  "Number of tiles in X and Y direction at given zoom level"
  (expt 2 zoom))


(defmacro osm-lib-xyzoom (x y zoom)
  `(cons (cons ,x ,y) ,zoom))

(defmacro osm-lib-get-x (xyzoom)
  `(car (car ,xyzoom)))

(defmacro osm-lib-get-y (xyzoom)
  `(cdr (car ,xyzoom)))

(defmacro osm-lib-get-zoom (xyzoom)
  `(cdr ,xyzoom))

(defun osm-lib-gen-tile-url (xyzoom)
  "Returns string representing download address for tile"
  (let ((tile-z-x-y (format "%d/%d/%d.png"
			    (osm-lib-get-zoom xyzoom)
			    (osm-lib-get-x xyzoom)
			    (osm-lib-get-y xyzoom))))
    (concat osm-lib-map-tiles-url tile-z-x-y)))

(defun osm-lib-gen-tile-cache-filepath (xyzoom)
  (format "%s%s%d/tile%d_%d.png"
	  (expand-file-name osm-lib-root-dir)
	  osm-lib-tiles-cache-dir
	  (osm-lib-get-zoom xyzoom)
	  (osm-lib-get-x xyzoom)
	  (osm-lib-get-y xyzoom)))

(defun osm-lib-tile-grid-coords (m n xyzoom)
  "Returns list of coordinates for a grid of tiles centered on x,y at given zoom level"
  (let ((x_tile (osm-lib-get-x xyzoom))
	(y_tile (osm-lib-get-y xyzoom))
	(tile_max_x (- (osm-lib-num-tiles (osm-lib-get-zoom xyzoom)) 1))
	(tile_max_y (- (osm-lib-num-tiles (osm-lib-get-zoom xyzoom)) 1)))
    (let* ((mhalf (/ m 2))
	   (nhalf (/ n 2))
	   (mrest (- (- m mhalf) 1))
	   (nrest (- (- n nhalf) 1))
	   (r_x1 (if (<= (- x_tile mhalf) 0)
		     0 (- x_tile mhalf)))
	   (r_x2 (if (>= (+ x_tile mrest) tile_max_x)
		     tile_max_x (+ x_tile mrest)))
	   (r_y1 (if (<= (- y_tile nhalf) 0)
		     0 (- y_tile nhalf)))
	   (r_y2 (if (>= (+ y_tile nrest) tile_max_y)
		     tile_max_y (+ y_tile nrest)))
	   (xs (number-sequence r_x1 r_x2))
	   (ys (number-sequence r_y1 r_y2))
	   (m_new (length xs))
	   (n_new (length ys)))
      (cons (cons m_new n_new)
	    (apply #'append 
		   (mapcar
		    (lambda (y)
		      (mapcar
		       (lambda (x) (cons (cons x y) 
					 (osm-lib-get-zoom xyzoom)))
		       (number-sequence r_x1 r_x2)))
		    (number-sequence r_y1 r_y2)))))))

(defun osm-lib-create-directories ()
  "Create directories for the tiles cache and scratch work area if not already there" 
  (let* ((root-path (expand-file-name osm-lib-root-dir))
	 (cache-path (concat root-path osm-lib-tiles-cache-dir))
	 (scratch-path (concat root-path osm-lib-scratch-work-area)))
    (progn (when (not (file-directory-p root-path))
	     (make-directory root-path))
	     
	   (when (not (file-directory-p cache-path))
	     (make-directory cache-path))

	   (when (not (file-directory-p scratch-path))
	     (make-directory scratch-path)))))

(defun osm-lib-load-tile (xyzoom)
  "Load a tile from web if not present in cache. If tile is not in cache, add it"
  (let* (
	 (cache-path (concat (expand-file-name osm-lib-root-dir) osm-lib-tiles-cache-dir))
	 (cache-level-path (concat cache-path (format "%s/"
	 					      (osm-lib-get-zoom xyzoom))))
	 (tile-url (osm-lib-gen-tile-url xyzoom))
	 ;;(ofile (format "tile%d_%d.png"
	 ;;		(osm-lib-get-x xyzoom)
	 ;;		(osm-lib-get-y xyzoom)))
	 ;;(ofile-path (concat cache-level-path ofile))
	 (ofile-path (osm-lib-gen-tile-cache-filepath xyzoom)))
    (progn (when (not (file-directory-p cache-level-path))
	     (make-directory cache-level-path))
	   (if (file-exists-p ofile-path)
	       (progn
		 (message (format "OSM: %s exists in cache" ofile-path))
		 't)
	     (make-process :name "wget-tile"
			   :command (list "wget" tile-url "-O" ofile-path))))))
;; TODO: Check for success or failure of trying to load these
	   
(defun osm-lib-load-tiles (tiles)
  "Load many tiles based on a list of xyzooms"
  (dolist (elt tiles ())
    (osm-lib-load-tile elt)))


(defun osm-lib-gen-tile-montage (mntiles &optional do-on-success)
  "Compose tiles into a larger map"
  (let* ((mn (car mntiles))
	 (tiles (cdr mntiles))
	 (m (car mn))
	 (n (cdr mn))
	 (width (* m 256))
	 (height (* n 256))
	 (scratch-path (concat (expand-file-name osm-lib-root-dir) osm-lib-scratch-work-area))
	 (ofile (format "m%s.png" (sxhash tiles)))
	 (ofile-path (concat scratch-path ofile)))
    (let ((files-acc '()))
      (progn (osm-lib-load-tiles tiles) ;; Load all tiles (if not in cache) 
	     (dolist (elt (reverse tiles) '())
	       (setq files-acc (cons (format "%s" (osm-lib-gen-tile-cache-filepath elt)) files-acc)))
	     (lexical-let ((do-on-success do-on-success)
			   (ofile-path ofile-path))
			  (lexical-let 
			   ((sentinel (lambda (process signal)
				       (cond ((equal signal "finished\n")
					      (if do-on-success
						  (funcall do-on-success ofile-path)
						(message "do-on-success function is nil")))
					     (t (message "Montage failed!"))))))
			   (make-process :name "montage-proc"
					 :command (append (list "montage") files-acc  (list "-tile" (format "%sx%s" m n) "-geometry" "+0+0" ofile-path))
					 :buffer "montage-buffer"
					 :sentinel sentinel)))))))

(provide 'osm-lib)

;; (osm-lib-create-directories)

;; (osm-lib-x-y-tile-index osm-lib-center-of-the-universe 8)

;; (osm-lib-gen-tile-url (osm-lib-x-y-tile-index osm-lib-center-of-the-universe 8))
     
;; (osm-lib-load-tile (osm-lib-x-y-tile-index osm-lib-center-of-the-universe 8))

;; (osm-lib-tile-grid-coords 5 5 (cons (cons 2 2) 2))

;; (osm-lib-num-tiles 2)

;; (osm-lib-load-tiles (osm-lib-tile-grid-coords 5 5 (cons (cons 137 77) 8)))

;; (osm-lib-gen-tile-montage 5 5 (osm-lib-tile-grid-coords 5 5 (cons (cons 137 77) 8)))

;; (expand-file-name osm-lib-root-dir)

;; (osm-lib-gen-tile-cache-filepath (osm-lib-x-y-tile-index osm-lib-center-of-the-universe 8))
