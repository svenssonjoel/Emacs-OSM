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

(defconst osm-lib-center-of-the-universe (cons 57.72101 12.9401))

(defvar osm-lib-map-tiles-url "https://maps.wikimedia.org/osm-intl/")

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
  (let ((mhalf (/ m 2))
	(nhalf (/ n 2)))
    (apply #'append 
     (mapcar
      (lambda (y)
	(mapcar
	 (lambda (x) (cons (cons (+ (osm-lib-get-x xyzoom) x)
				 (+ (osm-lib-get-y xyzoom) y)) (osm-lib-get-zoom xyzoom)))
	 (number-sequence (- 0 mhalf) mhalf)))
      (number-sequence (- 0 nhalf) nhalf)))))
  
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
	   
(defun osm-lib-load-tiles (tiles)
  "Load many tiles based on a list of xyzooms"
  (dolist (elt tiles ())
    (osm-lib-load-tile elt)))


(defun osm-lib-gen-tile-montage (m n tiles)
  "Compose tiles into a larger map"
  (let ((width (* m 256))
	(height (* n 256)))
    
    ()))



;; (osm-lib-x-y-tile-index osm-lib-center-of-the-universe 8)

;; (osm-lib-gen-tile-url (osm-lib-x-y-tile-index osm-lib-center-of-the-universe 8))
     
;; (osm-lib-load-tile (osm-lib-x-y-tile-index osm-lib-center-of-the-universe 8))

;; (osm-lib-load-tiles (osm-lib-tile-grid-coords 5 5 (cons (cons 137 77) 8)))

;; (expand-file-name osm-lib-root-dir)

;; (osm-lib-gen-tile-cache-filepath (osm-lib-x-y-tile-index osm-lib-center-of-the-universe 8))
