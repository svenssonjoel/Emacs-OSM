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

(require 'osm-lib)


(defun osm-start ()
  "Start OpenStreetMap viewing. Creates a new buffer and sets image-mode and osm-mode"
  ()) ;; Todo implement


(defun osm-test ()
    (interactive)
    (message "testing testing"))

(defun osm-gen-keymap ()
  "Keybindings for osm-mode"
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-i") 'osm-test)
    km))
  
(define-minor-mode osm-mode
  "OpenStreetMap minor mode for use in conjunction with image-mode"
  :lighter " osm"
  :keymap (osm-gen-keymap))

(provide 'osm-mode)

 
