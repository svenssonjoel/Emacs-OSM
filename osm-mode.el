;; osm-mode.el --- Minor mode for interaction with OpenStreetMap

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

;; TODOs
;; - Get window size of window displaying map and use that to decide with and height in number of tiles for display.
;;    - get-buffer-window, window-pixel-height and window-pixel-width
;; - See if it is possible to track resizing of the window


;; ------------------------------------------------------------
;; Requirements 
(require 'osm-lib)
(require 'image-mode)

;; ------------------------------------------------------------
;; Constants
(defconst osm-zoom-level-max 19)
(defconst osm-zoom-level-min 0)

;; ------------------------------------------------------------
;; Buffer local Vars (or will be made buffer local upon change) 
(defvar osm-image-scale 1.0)
(make-variable-buffer-local 'osm-image-scale)

(defvar osm-image-scale-delta 0.1)
(make-variable-buffer-local 'osm-image-scale-delta)

(defvar osm-zoom-level 8)
(make-variable-buffer-local 'osm-zoom-level)

(defvar osm-view-lat-long osm-lib-center-of-the-universe)
(make-variable-buffer-local 'osm-view-lat-long)

;; ------------------------------------------------------------
;; CODE
(defun osm-start ()
  "Start OpenStreetMap viewing. Creates a new buffer and sets image-mode and osm-mode"
  (interactive)
  (let* ((tile-index (osm-lib-x-y-tile-index osm-view-lat-long osm-zoom-level))
	 (tiles (osm-lib-tile-grid-coords 5 5 tile-index))
	 ;; (osm-lib-load-tiles tiles) ;; loads automatically at gen-tile-montage
	 (montage-file
	  (osm-lib-gen-tile-montage 5 5
				    (osm-lib-tile-grid-coords 5 5 tile-index))))
    (let ((buffer (generate-new-buffer "osm")))
      (progn 
	(with-current-buffer buffer
	  (setq major-mode 'image-mode)
	  (insert-file-contents montage-file)
	  (image-mode)
	  (osm-mode))
	(switch-to-buffer buffer)))))
    
(defun osm-image-scale-incr ()
  (interactive)
  (setq osm-image-scale (+ osm-image-scale osm-image-scale-delta))
  (image-transform-set-scale osm-image-scale))

(defun osm-image-scale-decr ()
  (interactive)
  (setq osm-image-scale (- osm-image-scale osm-image-scale-delta))
  (image-transform-set-scale osm-image-scale))

(defun osm-redraw-map ()
  (let* ((tile-index (osm-lib-x-y-tile-index osm-view-lat-long osm-zoom-level))
	 (tiles (osm-lib-tile-grid-coords 5 5 tile-index))
	 (montage-file
	  (osm-lib-gen-tile-montage 5 5
				    (osm-lib-tile-grid-coords 5 5 tile-index))))
    (image-transform-set-scale osm-image-scale)
    (image-toggle-display-text)
    (setf (buffer-string) "")
    (insert-file-contents montage-file)
    (image-toggle-display-image)))

(defun osm-zoom-level-incr ()
  (interactive)
  (when ( < osm-zoom-level osm-zoom-level-max)
    (progn 
      (message "zooming in")
      (setq osm-zoom-level (+ osm-zoom-level 1))
      (setq osm-image-scale 1.0)
      (osm-redraw-map)))) ;; Reset image scaling
;; TODO: create a new montage for the new zoom level and redisplay

(defun osm-zoom-level-decr ()
  (interactive)
  (when (> osm-zoom-level osm-zoom-level-min)
    (progn 
      (message "zooming out" ) 
      (setq osm-zoom-level (- osm-zoom-level 1))
      (setq osm-image-scale 1.0)
      (osm-redraw-map)))) ;; reset image scaling 
;; TODO: Create a new montage for the new zoom level and redisplay 


(defun osm-gen-keymap ()
  "Keybindings for osm-mode"
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-<up>") 'osm-image-scale-incr)
    (define-key km (kbd "C-<down>") 'osm-image-scale-decr)
    (define-key km (kbd "S-<up>") 'osm-zoom-level-incr)
    (define-key km (kbd "S-<down>") 'osm-zoom-level-decr)
    km))
  
(define-minor-mode osm-mode
  "OpenStreetMap minor mode for use in conjunction with image-mode"
  :lighter " osm"
  :keymap (osm-gen-keymap))

(provide 'osm-mode)

 
