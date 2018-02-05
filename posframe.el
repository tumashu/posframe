;;; posframe.el --- Pop a posframe (just a frame) at point    -*- lexical-binding:t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Feng Shu <tumashu@163.com>
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/posframe
;; Version: 0.1.0
;; Keywords: tooltip
;; Package-Requires: ((emacs "26"))

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

;; NOTE: For MacOS users, posframe need emacs (version >= 26.0.91)

;; [[./snapshots/posframe-1.png]]

;; ** Installation

;; #+BEGIN_EXAMPLE
;; (require 'posframe)
;; #+END_EXAMPLE

;; ** Usage

;; *** Create a posframe

;; #+BEGIN_EXAMPLE
;; (posframe-show " *my-posframe-buffer*"
;;                :string "This is a test"
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

(defvar posframe--last-posframe-size nil
  "Record the last size of posframe's frame.")

(defvar posframe--last-parent-frame-size nil
  "Record the last size of posframe's parent-frame.")

(defvar posframe--last-args nil
  "Record the last arguments of `posframe--create-frame'.

If these args have changed, posframe will recreate its
frame.")

(defvar posframe--timeout-timer nil
  "Record the timer to deal with timeout argument
of `posframe-show'.")

(defvar posframe--refresh-timer nil
  "Record the timer to deal with refresh argument
of `posframe-show'.")

(dolist (var '(posframe--frame
               posframe--timer
               posframe--last-position
               posframe--last-posframe-size
               posframe--last-args
               posframe--timeout-timer
               posframe--refresh-timer))
  (make-variable-buffer-local var)
  (put var 'permanent-local t))

(cl-defun posframe--get-pixel-position (position
                                        &key
                                        posframe-width
                                        posframe-height
                                        (posframe-adjust t)
                                        (x-pixel-offset 0)
                                        (y-pixel-offset 0))
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
               x-pixel-offset))
         (y-top (+ (cadr (window-pixel-edges window))
                   header-line-height
                   (- (or (cdr (posn-x-y posn-top-left)) 0)
                      ;; Fix the conflict with flycheck
                      ;; http://lists.gnu.org/archive/html/emacs-devel/2018-01/msg00537.html
                      (or (cdr (posn-object-x-y posn-top-left)) 0))
                   y-pixel-offset))
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
      (cons (max 0 x) (max 0 y-buttom)))))

(cl-defun posframe--create-frame (posframe-buffer
                                  &key
                                  parent-frame
                                  foreground-color
                                  background-color
                                  margin-left
                                  margin-right
                                  font
                                  keep-ratio
                                  override-parameters)
  "Create a child-frame for posframe.
This posframe's buffer is POSFRAME-BUFFER."
  (let ((buffer (get-buffer-create posframe-buffer))
        (after-make-frame-functions nil)
        (args (list parent-frame
                    foreground-color
                    background-color
                    margin-right
                    margin-left
                    font
                    keep-ratio
                    override-parameters)))
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
      (unless (and (frame-live-p posframe--frame)
                   ;; For speed reason, posframe will reuse
                   ;; existing frame at possible, but when
                   ;; user change args, recreating frame
                   ;; is needed.
                   (equal posframe--last-args args))
        (posframe--delete-frame posframe-buffer)
        (setq-local posframe--last-args args)
        (setq-local posframe--last-position nil)
        (setq-local posframe--last-posframe-size nil)
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
                       (keep-ratio ,keep-ratio)
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

(cl-defun posframe-show (posframe-buffer
                         &key
                         string
                         position
                         width
                         height
                         (min-width 1)
                         (min-height 1)
                         (x-pixel-offset 0)
                         (y-pixel-offset 0)
                         margin-left
                         margin-right
                         font
                         foreground-color
                         background-color
                         no-properties
                         keep-ratio
                         override-parameters
                         timeout
                         refresh)
  "Pop posframe and show STRING at POSITION.
The POSITION can be a point or a cons of pixel numbers,
for example: (X . Y).

This posframe's buffer is POSFRAME-BUFFER.

If NO-PROPERTIES is non-nil, The STRING's properties will
be removed before showed in posframe.

posframe's frame-size can be set by WIDTH and HEIGHT,
If one of them is nil, posframe's frame-size will fit the
content of buffer, if you don't want to posframe's
size too small, MIN-WIDTH and MIN-HEIGTH will be useful

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

If REFRESH is a number, posframe's frame-size will be re-adjust
every mumber seconds.

you can use `posframe-delete-all' to delete all posframes."
  (let* ((position (or position (point)))
         (buffer (get-buffer-create posframe-buffer))
         (frame-resize-pixelwise t)
         (parent-frame (window-frame))
         (parent-frame-xmax (frame-pixel-width parent-frame))
         (parent-frame-ymax (frame-pixel-height parent-frame))
         child-frame x-and-y)

    (posframe--create-frame
     posframe-buffer
     :parent-frame parent-frame
     :margin-left margin-left
     :margin-right margin-right
     :font font
     :foreground-color foreground-color
     :background-color background-color
     :keep-ratio keep-ratio
     :override-parameters override-parameters)

    ;; FIXME: This is a hacky fix for the mouse focus problem for child-frame
    ;; https://github.com/tumashu/posframe/issues/4#issuecomment-357514918
    (when (and posframe-mouse-banish
               (not (equal (cdr (mouse-position)) '(0 . 0))))
      (set-mouse-position parent-frame 0 0))

    (with-current-buffer buffer
      ;; posframe--frame is a buffer variable which
      ;; used to store posframe's frame.
      (setq child-frame posframe--frame)

      ;; Insert string to posframe's buffer.
      (when (and string (stringp string))
        ;; Does inserting string then deleting the before
        ;; contents reduce flicking? Maybe :-)
        (goto-char (point-min))
        (if no-properties
            (insert (substring-no-properties string))
          (insert string))
        (delete-region (point) (point-max)))

      ;; Set posframe's size
      (if (and width height)
          (unless (equal posframe--last-posframe-size (cons width height))
            (set-frame-size child-frame width height)
            (setq-local posframe--last-posframe-size (cons width height)))
        (fit-frame-to-buffer child-frame height min-height width min-width)))

    ;; Get the posframe's position, this must run in user's working
    ;; buffer instead of posframe's buffer.
    (setq x-and-y
          (if (consp position)
              (posframe--respect-modeline-and-minibuffer
               (cons (+ (car position) x-pixel-offset)
                     (+ (cdr position) y-pixel-offset)))
            (posframe--get-pixel-position
             position
             :posframe-width (frame-pixel-width child-frame)
             :posframe-height (frame-pixel-height child-frame)
             :x-pixel-offset x-pixel-offset
             :y-pixel-offset y-pixel-offset)))

    (with-current-buffer buffer
      ;; Move posframe's child-frame.
      (unless (and (equal x-and-y posframe--last-position)
                   ;; When working frame's size change, re-posit
                   ;; the posframe.
                   (equal posframe--last-parent-frame-size
                          (cons parent-frame-xmax
                                parent-frame-ymax)))
        (set-frame-position child-frame (car x-and-y) (cdr x-and-y))
        (setq-local posframe--last-position x-and-y)
        (setq-local posframe--last-parent-frame-size
                    (cons parent-frame-xmax
                          parent-frame-ymax)))
      ;; Make posframe's child-frame visible
      (unless (frame-visible-p child-frame)
        (make-frame-visible child-frame))
      ;; Delay hide posframe when timeout is a number.
      (when (and (numberp timeout) (> timeout 0))
        (when (timerp posframe--timeout-timer)
          (cancel-timer posframe--timeout-timer))
        (setq-local posframe--timeout-timer
                    (run-with-timer
                     timeout nil #'posframe-hide posframe-buffer)))
      ;; Re-adjust posframe's size when buffer's content has changed.
      (when (and (numberp refresh) (> refresh 0))
        (unless (and width height)
          (when (timerp posframe--refresh-timer)
            (cancel-timer posframe--refresh-timer))
          (setq-local posframe--refresh-timer
                      (run-with-timer
                       nil refresh
                       #'(lambda (child-frame height min-height width min-width)
                           (when (and child-frame (frame-live-p child-frame))
                             (fit-frame-to-buffer
                              child-frame height min-height width min-width)))
                       child-frame height min-height width min-width))))
      nil)))

(defun posframe--respect-modeline-and-minibuffer (position)
  "Get adjusted position based of POSITION which like (0 . -1).
the new position respect modeline and minibuffer."
  (if (>= (cdr position) 0)
      position
    (let ((modeline-height (window-mode-line-height))
          (minibuffer-height
           (window-pixel-height (minibuffer-window))))
      (cons (car position)
            (min (cdr position)
                 (- 0 (+ modeline-height
                         minibuffer-height)))))))

(defun posframe-get-frame-size (posframe-buffer &optional pixelwise)
  "Return the posframe's current frame text or PIXELWISE size.
This posframe's buffer is POSFRAME-BUFFER."
  (with-current-buffer (get-buffer-create posframe-buffer)
    (when (frame-live-p posframe--frame)
      (if pixelwise
          (cons (frame-pixel-width posframe--frame)
                (frame-pixel-height posframe--frame))
        (cons (frame-width posframe--frame)
              (frame-height posframe--frame))))))

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
        (with-current-buffer posframe-buffer
          (dolist (timer '(posframe--refresh-timer
                           posframe--timeout-timer))
            (when (timerp timer)
              (cancel-timer timer))))
        (delete-frame frame)))))

(defun posframe--kill-buffer (posframe-buffer)
  "Kill posframe's buffer: POSFRAME-BUFFER."
  (when (buffer-live-p posframe-buffer)
    (kill-buffer posframe-buffer)))

;;;###autoload
(defun posframe-hide-all ()
  "Hide all posframe's frames."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (frame-live-p posframe--frame)
        (make-frame-invisible posframe--frame)))))

;;;###autoload
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
