;;; posframe.el --- Pop a (child) frame at point

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Feng Shu <tumashu@163.com>
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/company-mode/company-mode
;; Version: 0.1.0
;; Keywords: abbrev, convenience, matching
;; Package-Requires: ((emacs "26.0"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; * Posframe README                                :README:
;; ** What is posframe
;; Posframe can pop a child-frame at the point

;; ** Installation

;; #+BEGIN_EXAMPLE
;; (require 'posframe)
;; #+END_EXAMPLE

;; ** Usage

;; #+BEGIN_EXAMPLE
;; (posframe-show "my-posframe"
;;                "This is a test"
;;                :position (point))
;; #+END_EXAMPLE

;;; Code:
;; * posframe's code                         :CODE:
(require 'cl-lib)

(defconst posframe-version "0.1.0")

(defgroup posframe nil
  "Pop a (child) frame at point"
  :group 'lisp
  :prefix "posframe-")

(defvar posframe-mouse-banish t
  "Mouse will be moved to (0 , 0) when it is non-nil.")

(defvar posframe--frame nil
  "Child-frame created by posframe.")

(defvar posframe--buffer nil
  "Buffer attached to the the frame of posframe.")

(defvar posframe--last-position nil
  "Record the last pixel position of posframe.")

(dolist (var '(posframe--frame
               posframe--buffer
               posframe--last-position))
  (make-variable-buffer-local var)
  (put var 'permanent-local t))

(defun posframe--return-buffer (posframe-name)
  "Return posframe-instance named POSFRAME-NAME's buffer."
  (if (and posframe-name (stringp posframe-name))
      (let ((buffer-name (format " *posframe-buffer-%s*" posframe-name)))
        (get-buffer-create buffer-name))
    (error "Posframe: posframe's name is invaild")))

(defun posframe--compute-pixel-position (position tooltip-width tooltip-height)
  "Return bottom-left-corner pixel POSITION in WINDOW.
its returned value is like (X . Y)

If TOOLTIP-WIDTH and TOOLTIP-HEIGHT are given,
this function will use two values to adjust its
output position, make sure the *tooltip* at position
not disappear by sticking out of the display."
  (let* ((window (selected-window))
         (frame (window-frame window))
         (xmax (frame-pixel-width frame))
         (ymax (frame-pixel-height frame))
         (header-line-height (window-header-line-height window))
         (posn-top-left (posn-at-point position window))
         (x (+ (car (window-inside-pixel-edges window))
               (- (or (car (posn-x-y posn-top-left)) 0)
                  (or (car (posn-object-x-y posn-top-left)) 0))))
         (y-top (+ (cadr (window-pixel-edges window))
                   header-line-height
                   (- (or (cdr (posn-x-y posn-top-left)) 0)
                      ;; Fix the conflict with flycheck
                      ;; http://lists.gnu.org/archive/html/emacs-devel/2018-01/msg00537.html
                      (or (cdr (posn-object-x-y posn-top-left)) 0))))
         (font-height
          (if (= position 1)
              (default-line-height)
            (aref (font-info
                   (font-at
                    (if (and (= position (point-max))) (- position 1) position)))
                  3)))
         (y-buttom (+ y-top font-height)))
    (cons (max 0 (min x (- xmax (or tooltip-width 0))))
          (max 0 (if (> (+ y-buttom (or tooltip-height 0)) ymax)
                     (- y-top (or tooltip-height 0))
                   y-buttom)))))

(cl-defun posframe--create-frame (posframe-name
                                  &key
                                  parent-frame
                                  face
                                  margin-left
                                  margin-right
                                  extra-parameters)
  "Create a child-frame for posframe named POSFRAME-NAME.

Arguments: PARENT-FRAME BACKGROUND EXTRA-PARAMETERS."
  (let ((buffer (posframe--return-buffer posframe-name))
        (after-make-frame-functions nil))
    (with-current-buffer buffer
      ;; Many variables take effect after call `set-window-buffer'
      (setq-local left-fringe-width (or margin-left 0))
      (setq-local right-fringe-width (or margin-right 0))
      (setq-local fringes-outside-margins 0)
      (setq-local truncate-lines t)
      (setq-local mode-line-format nil)
      (setq-local header-line-format nil)
      (setq-local cursor-type nil)
      (setq-local cursor-in-non-selected-windows nil)
      (setq-local show-trailing-whitespace nil)

      ;; Create child-frame
      (unless (frame-live-p posframe--frame)
        (posframe--delete-frame posframe-name)
        (setq-local posframe--frame
                    (make-frame
                     `(,@extra-parameters
                       (name . ,posframe-name)
                       (background-color . ,face)
                       (parent-frame . ,(or parent-frame (window-frame)))
                       (posframe-name . ,posframe-name)
                       (no-accept-focus . t)
                       (min-width  . 0)
                       (min-height . 0)
                       (border-width . 0)
                       (internal-border-width . 0)
                       (vertical-scroll-bars . nil)
                       (horizontal-scroll-bars . nil)
                       (left-fringe . ,margin-left)
                       (right-fringe . ,margin-right)
                       (menu-bar-lines . 0)
                       (tool-bar-lines . 0)
                       (line-spacing . 0)
                       (unsplittable . t)
                       (no-other-frame . t)
                       (undecorated . t)
                       (visibility . nil)
                       (cursor-type . nil)
                       (minibuffer . nil)
                       (width . 1)
                       (height . 1)
                       (no-special-glyphs . t)
                       (inhibit-double-buffering . nil)
                       ;; Do not save child-frame when use desktop.el
                       (desktop-dont-save . t))))
        (let ((window (frame-root-window posframe--frame)))
          ;; This method is more stable than 'setq mode/header-line-format nil'
          (set-window-parameter window 'mode-line-format 'none)
          (set-window-parameter window 'header-line-format 'none)
          (set-window-buffer window buffer))))))

(cl-defun posframe-show (posframe-name string
                                       &key
                                       position
                                       width
                                       height
                                       min-width
                                       min-height
                                       margin-left
                                       margin-right
                                       face)
  "Pop a frame and show STRING at point."
  (let* ((position (or position (point)))
         (buffer (posframe--return-buffer posframe-name))
         (frame-resize-pixelwise t)
         (frame (window-frame))
         x-and-y)

    (posframe--create-frame
     posframe-name
     :margin-left margin-left
     :margin-right margin-right
     :face face
     :parent-frame frame)

    ;; FIXME: This is a hacky fix for the mouse focus problem for child-frame
    ;; https://github.com/tumashu/posframe/issues/4#issuecomment-357514918
    (when (and posframe-mouse-banish
               (not (equal (cdr (mouse-position)) '(0 . 0))))
      (set-mouse-position frame 0 0))

    (when (and string (stringp string))
      (with-current-buffer buffer
        (erase-buffer)
        (insert string)))

    (let ((child-frame (buffer-local-value 'posframe--frame buffer)))
      (set-frame-parameter child-frame 'parent-frame (window-frame))
      (setq x-and-y (posframe--compute-pixel-position
                     position
                     (frame-pixel-width child-frame)
                     (frame-pixel-height child-frame)))
      (unless (equal x-and-y posframe--last-position)
        (set-frame-position child-frame (car x-and-y) (+ (cdr x-and-y) 1))
        (with-current-buffer buffer
          (setq-local posframe--last-position x-and-y)))
      (if (and width height)
          (set-frame-size child-frame width height)
        (fit-frame-to-buffer child-frame nil min-width nil min-height))
      (unless (frame-visible-p child-frame)
        (make-frame-visible child-frame)))))

(defun posframe-hide (posframe-name)
  "Hide posframe named POSFRAME-NAME."
  (with-current-buffer (posframe--return-buffer posframe-name)
    (when (frame-live-p posframe--frame)
      (make-frame-invisible posframe--frame))))

(defun posframe-delete (posframe-name)
  "Delete posframe named POSFRAME-NAME."
  (posframe--delete-frame posframe-name)
  (posframe--kill-buffer posframe-name))

(defun posframe--delete-frame (posframe-name)
  "Kill child-frame of posframe named POSFRAME-NAME."
  (dolist (frame (frame-list))
    (let ((value (frame-parameter frame 'posframe-name)))
      (when (equal posframe-name value)
        (delete-frame frame)))))

(defun posframe--kill-buffer (posframe-name)
  "Kill buffer of posframe named POSFRAME-NAME."
  (let ((buffer (posframe--return-buffer posframe-name)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(provide 'posframe)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; posframe.el ends here
