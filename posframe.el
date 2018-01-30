;;; posframe.el --- Pop a posframe (just a frame) at point    -*- lexical-binding:t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Feng Shu <tumashu@163.com>
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/posframe
;; Version: 0.1.0
;; Keywords: tooltip
;; Package-Requires: ((emacs "24.4"))

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

;; NOTE: posframe requires emacs (version >= 26.0.91), but for
;; compatibility reasons, it does not require emacs26 at package
;; level, user should test emacs version before run `posframe-show'.

;; [[./snapshots/posframe-1.png]]

;; ** Installation

;; #+BEGIN_EXAMPLE
;; (require 'posframe)
;; #+END_EXAMPLE

;; ** Usage

;; *** Create a posframe

;; #+BEGIN_EXAMPLE
;; (when (>= emacs-major-version 26)
;;   (posframe-show " *my-posframe-buffer*"
;;                  "This is a test"
;;                  :position (point)))
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

;; *** Hide all posframes
;; #+BEGIN_EXAMPLE
;; M-x posframe-hide-all
;; #+END_EXAMPLE

;; *** Delete a posframe
;; #+BEGIN_EXAMPLE
;; (posframe-delete " *my-posframe-buffer*")
;; #+END_EXAMPLE

;; *** Delete all posframes
;; #+BEGIN_EXAMPLE
;; M-x posframe-delete-all
;; #+END_EXAMPLE

;; Note: this command will delete all posframe buffers,
;; suggest not run this command if you are sharing a buffer
;; between posframe and other packages.

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

(defvar posframe--timer nil
  "Record posframe's hide timer.")

(defvar posframe--last-position nil
  "Record the last pixel position of posframe's frame.")

(defvar posframe--last-size nil
  "Record the last size of posframe's frame.")

(dolist (var '(posframe--frame
               posframe--timer
               posframe--last-position
               posframe--last-size))
  (make-variable-buffer-local var)
  (put var 'permanent-local t))

(cl-defun posframe--compute-pixel-position (position
                                            &key
                                            posframe-width
                                            posframe-height
                                            (posframe-adjust t)
                                            (x-offset 0)
                                            (y-offset 0))
  "Return bottom-left-corner pixel POSITION in WINDOW.
its returned value is like (X . Y)

If POSFRAME-WIDTH and POSFRAME-HEIGHT are given
and POSFRAME-ADJUST is non-nil, this function will
use two values to adjust its output position,
make sure the *tooltip* at position not disappear
by sticking out of the display."
  (let* ((window (selected-window))
         (frame (window-frame window))
         (xmax (frame-pixel-width frame))
         (ymax (frame-pixel-height frame))
         (header-line-height (window-header-line-height window))
         (posn-top-left (posn-at-point position window))
         (x (+ (car (window-inside-pixel-edges window))
               (- (or (car (posn-x-y posn-top-left)) 0)
                  (or (car (posn-object-x-y posn-top-left)) 0))
               x-offset))
         (y-top (+ (cadr (window-pixel-edges window))
                   header-line-height
                   (- (or (cdr (posn-x-y posn-top-left)) 0)
                      ;; Fix the conflict with flycheck
                      ;; http://lists.gnu.org/archive/html/emacs-devel/2018-01/msg00537.html
                      (or (cdr (posn-object-x-y posn-top-left)) 0))
                   y-offset))
         (font-height
          (if (= position 1)
              (default-line-height)
            (aref (font-info
                   (font-at
                    (if (and (= position (point-max)))
                        (- position 1)
                      position)))
                  3)))
         (y-buttom (+ y-top font-height)))
    (if posframe-adjust
        (cons (max 0 (min x (- xmax (or posframe-width 0))))
              (max 0 (if (> (+ y-buttom (or posframe-height 0)) ymax)
                         (- y-top (or posframe-height 0))
                       y-buttom)))
      (cons x y-buttom))))

(cl-defun posframe--create-frame (posframe-buffer
                                  &key
                                  parent-frame
                                  foreground-color
                                  background-color
                                  margin-left
                                  margin-right
                                  font
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
                       ,(when font
                          (cons 'font font))
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
                                         pixelwise
                                         (min-width 1)
                                         (min-height 1)
                                         (x-offset 0)
                                         (y-offset 0)
                                         margin-left
                                         margin-right
                                         font
                                         foreground-color
                                         background-color
                                         no-properties
                                         override-parameters
                                         timeout)
  "Pop a posframe at point and show STRING.
This posframe's buffer is POSFRAME-BUFFER.

If NO-PROPERTIES is non-nil, The STRING's properties will
be removed before showed in posframe.

posframe's frame-size can be set by WIDTH and HEIGHT,
If one of them is nil, posframe's frame-size will fit the
content of buffer, if you don't want to posframe's
size too small, MIN-WIDTH and MIN-HEIGTH will be useful
When PIXELWISE is non-nil, the above WIDTH and HEIGHT
are regard as pixel width and pixel height.

If MARGIN-LEFT or MARGIN-RIGHT is a number, Left fringe or
right fringe with be showed with number width.

By default, posframe's font is deriverd from current frame
user can set posframe's font with FONT argument.

By default, posframe's foreground and background color are
deriverd from current frame, user can set them with the help
of FOREGROUND-COLOR and BACKGROUND-COLOR.

OVERRIDE-PARAMETERS is very powful, *all* the frame parameters
used by posframe's frame can be overrided by it.

If TIMEOUT is a number, a delay of number seconds, the posframe
will auto hide.

NOTE: posframe will reuse the existing frame, for speed
reason, deleting the existing frame with `posframe-delete'
is required if you want to change the below existing arguments:
1. MARGIN-*,
2. *-COLOR
3. OVERRIDE-PARAMETERS.

you can use `posframe-delete-all' to delete all posframes."
  (let* ((position (or position (point)))
         (buffer (get-buffer-create posframe-buffer))
         (frame-resize-pixelwise t)
         (frame (window-frame))
         x-and-y)

    (posframe--create-frame
     posframe-buffer
     :parent-frame frame
     :margin-left margin-left
     :margin-right margin-right
     :font font
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
                     :posframe-width (frame-pixel-width child-frame)
                     :posframe-height (frame-pixel-height child-frame)
                     :x-offset x-offset
                     :y-offset y-offset))
      (unless (equal x-and-y posframe--last-position)
        (set-frame-position child-frame (car x-and-y) (+ (cdr x-and-y) 1))
        (with-current-buffer buffer
          (setq-local posframe--last-position x-and-y)))
      (if (and width height)
          (unless (equal (cons width height) posframe--last-size)
            (set-frame-size child-frame width height pixelwise)
            (with-current-buffer buffer
              (setq-local posframe--last-size (cons width height))))
        (fit-frame-to-buffer child-frame nil min-height nil min-width)
        (with-current-buffer buffer
          (setq-local posframe--last-size
                      (if pixelwise
                          (cons (frame-pixel-width child-frame)
                                (frame-pixel-height child-frame))
                        (cons (frame-width child-frame)
                              (frame-height child-frame))))))
      (unless (frame-visible-p child-frame)
        (make-frame-visible child-frame))
      (posframe--run-hide-timer posframe-buffer timeout)
      nil)))

(defun posframe--run-hide-timer (posframe-buffer secs)
  "After a delay of SECS seconds, hide posframe which buffer is
POSFRAME-BUFFER."
  (when (and (numberp secs) (> secs 0))
    (with-current-buffer posframe-buffer
      (when (timerp posframe--timer)
        (cancel-timer posframe--timer))
      (setq-local posframe--timer
                  (run-with-timer
                   secs nil #'posframe-hide posframe-buffer)))))

(defun posframe-get-last-size (posframe-buffer)
  "Return the posframe's last framesize.
This posframe's buffer is POSFRAME-BUFFER."
  (with-current-buffer (get-buffer-create posframe-buffer)
    (when (local-variable-p 'posframe--last-size)
      posframe--last-size)))

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
    (kill-buffer posframe-buffer)))

;;;autoload
(defun posframe-hide-all ()
  "Hide all posframe's frames."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (frame-live-p posframe--frame)
        (make-frame-invisible posframe--frame)))))

;;;autoload
(defun posframe-delete-all ()
  "Delete all posframe's frames and buffers"
  (interactive)
  (dolist (frame (frame-list))
    (let ((buffer (frame-parameter frame 'posframe-buffer)))
      (when buffer (delete-frame frame))))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when posframe--frame
        (posframe--kill-buffer buffer)))))

(provide 'posframe)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; posframe.el ends here
