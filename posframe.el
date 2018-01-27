;;; posframe.el --- Pop a posframe (just a frame) at point

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Feng Shu <tumashu@163.com>
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/posframe
;; Version: 0.1.0
;; Keywords: tooltip
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
;; Posframe can pop a posframe at point, this *posframe* is a
;; child-frame with its root window's buffer.

;; The main advantages are:
;; 1. It is very fast, `posframe-show' is faster than `popup-tip'
;;    of popup.el.
;; 2. It works well with CJK language.

;; NOTE: posframe requires emacs (version >= 26.0.91).

;; [[./snapshots/posframe-1.png]]

;; ** Installation

;; #+BEGIN_EXAMPLE
;; (require 'posframe)
;; #+END_EXAMPLE

;; ** Usage

;; *** Create a posframe

;; #+BEGIN_EXAMPLE
;; (posframe-show " *my-posframe-buffer*"
;;                "This is a test"
;;                :position (point))
;; #+END_EXAMPLE

;; Addition arguments:
;; 1. :position, set the position when posframe is poped up.
;; 2. :background-color, set posframe's background color.
;; 3. :foreground-color, set posframe's foreground color.
;; 4. :margin-left, set posframe's left margin width.
;; 5. :margin-right, set posframe's right margin width.
;; 6. :override-parameters, User can use it to override
;;    *all* the frame parameters of posframe's child-frame.

;; *** Hide a posframe

;; #+BEGIN_EXAMPLE
;; (posframe-hide " *my-posframe-buffer*")
;; #+END_EXAMPLE

;; *** Delete a posframe
;; #+BEGIN_EXAMPLE
;; (posframe-delete " *my-posframe-buffer*")
;; #+END_EXAMPLE

;;; Code:
;; * posframe's code                         :CODE:
(require 'cl-lib)

(defconst posframe-version "0.1.0")

(defgroup posframe nil
  "Pop a posframe (just a frame) at point"
  :group 'lisp
  :prefix "posframe-")

(defcustom posframe-mouse-banish t
  "Mouse will be moved to (0 , 0) when it is non-nil."
  :group 'posframe
  :type 'boolean)

(defcustom posframe-inhibit-double-buffering nil
  "Set the posframe's frame-parameter: inhibit-double-buffering."
  :group 'posframe
  :type 'boolean)

(defvar posframe--frame nil
  "Record posframe's frame.")

(defvar posframe--last-position nil
  "Record the last pixel position of posframe's frame.")

(defvar posframe--last-size nil
  "Record the last size of posframe's frame.")

(dolist (var '(posframe--frame
               posframe--last-position
               posframe--last-size))
  (make-variable-buffer-local var)
  (put var 'permanent-local t))

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

(cl-defun posframe--create-frame (posframe-buffer
                                  &key
                                  parent-frame
                                  foreground-color
                                  background-color
                                  margin-left
                                  margin-right
                                  override-parameters)
  "Create a child-frame for posframe.
This posframe's buffer is POSFRAME-BUFFER."
  (let ((buffer (get-buffer-create posframe-buffer))
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
        (posframe--delete-frame posframe-buffer)
        (setq-local posframe--frame
                    (make-frame
                     `(,@override-parameters
                       ,(when foreground-color
                          (cons 'foreground-color foreground-color))
                       ,(when background-color
                          (cons 'background-color background-color))
                       (parent-frame . ,(or parent-frame (window-frame)))
                       (posframe-buffer . ,posframe-buffer)
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
                       (inhibit-double-buffering . ,posframe-inhibit-double-buffering)
                       ;; Do not save child-frame when use desktop.el
                       (desktop-dont-save . t))))
        (let ((window (frame-root-window posframe--frame)))
          ;; This method is more stable than 'setq mode/header-line-format nil'
          (set-window-parameter window 'mode-line-format 'none)
          (set-window-parameter window 'header-line-format 'none)
          (set-window-buffer window buffer))))))

(cl-defun posframe-show (posframe-buffer string
                                         &key
                                         position
                                         width
                                         height
                                         min-width
                                         min-height
                                         margin-left
                                         margin-right
                                         foreground-color
                                         background-color
                                         no-properties
                                         override-parameters)
  "Pop a posframe at point and show STRING.
This posframe's buffer is POSFRAME-BUFFER."
  (let* ((position (or position (point)))
         (indirected-buffer (buffer-live-p string))
         (buffer (get-buffer-create posframe-buffer))
         (frame-resize-pixelwise t)
         (frame (window-frame))
         x-and-y)

    (posframe--create-frame
     posframe-buffer
     :parent-frame frame
     :margin-left margin-left
     :margin-right margin-right
     :foreground-color foreground-color
     :background-color background-color
     :override-parameters override-parameters)

    ;; FIXME: This is a hacky fix for the mouse focus problem for child-frame
    ;; https://github.com/tumashu/posframe/issues/4#issuecomment-357514918
    (when (and posframe-mouse-banish
               (not (equal (cdr (mouse-position)) '(0 . 0))))
      (set-mouse-position frame 0 0))

    (when (and string (stringp string))
      (with-current-buffer buffer
        (erase-buffer)
        (if no-properties
            (insert (substring-no-properties string))
          (insert string))))

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
          (unless (equal (cons width height) posframe--last-size)
            (set-frame-size child-frame width height)
            (with-current-buffer buffer
              (setq-local posframe--last-size (cons width height))))
        (fit-frame-to-buffer
         child-frame nil (or min-width 1) nil (or min-height 1)))
      (unless (frame-visible-p child-frame)
        (make-frame-visible child-frame)))))

(defun posframe-hide (posframe-buffer)
  "Hide posframe which buffer is POSFRAME-BUFFER."
  (with-current-buffer (get-buffer-create posframe-buffer)
    (when (frame-live-p posframe--frame)
      (make-frame-invisible posframe--frame))))

(defun posframe-delete (posframe-buffer)
  "Delete posframe which buffer POSFRAME-BUFFER."
  (posframe--delete-frame posframe-buffer)
  (posframe--kill-buffer posframe-buffer))

(defun posframe--delete-frame (posframe-buffer)
  "Kill child-frame of posframe.
This posframe's buffer is POSFRAME-BUFFER."
  (dolist (frame (frame-list))
    (let ((buffer (frame-parameter frame 'posframe-buffer)))
      (when (equal posframe-buffer buffer)
        (delete-frame frame)))))

(defun posframe--kill-buffer (posframe-buffer)
  "Kill posframe's buffer: POSFRAME-BUFFER."
  (when (buffer-live-p posframe-buffer)
    (kill-buffer buffer)))

(provide 'posframe)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; posframe.el ends here
