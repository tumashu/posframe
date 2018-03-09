;;; posframe.el --- Pop a posframe (just a frame) at point    -*- lexical-binding:t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Feng Shu <tumashu@163.com>
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/posframe
;; Version: 0.3.0
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
;; 1. It is fast enough for daily usage :-)
;; 2. It works well with CJK language.

;; NOTE: For MacOS users, posframe need Emacs (version >= 26.0.91)

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

;; Arguments documents:
;; #+BEGIN_EXAMPLE
;; C-h f posframe-show
;; #+END_EXAMPLE

;; *** Hide a posframe
;; #+BEGIN_EXAMPLE
;; (posframe-hide " *my-posframe-buffer*")
;; #+END_EXAMPLE

;; *** Hide all posframes
;; #+BEGIN_EXAMPLE
;; M-x posframe-hide-all
;; #+END_EXAMPLE

;; *** Delete a posframe
;; 1. Delete posframe and its buffer
;;    #+BEGIN_EXAMPLE
;;    (posframe-delete " *my-posframe-buffer*")
;;    #+END_EXAMPLE
;; 2. Only delete posframe's frame
;;    #+BEGIN_EXAMPLE
;;    (posframe-delete-frame " *my-posframe-buffer*")
;;    #+END_EXAMPLE
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

(defvar-local posframe--frame nil
  "Record posframe's frame.")

(defvar-local posframe--last-position nil
  "Record the last pixel position of posframe's frame.")

(defvar-local posframe--last-posframe-size nil
  "Record the last size of posframe's frame.")

(defvar-local posframe--last-parent-frame-size nil
  "Record the last size of posframe's parent-frame.")

(defvar-local posframe--last-args nil
  "Record the last arguments of `posframe--create-posframe'.

If these args have changed, posframe will recreate its
frame.")

(defvar-local posframe--timeout-timer nil
  "Record the timer to deal with timeout argument of `posframe-show'.")

(defvar-local posframe--refresh-timer nil
  "Record the timer to deal with refresh argument of `posframe-show'.")


(cl-defun posframe--create-posframe (posframe-buffer
                                     &key
                                     parent-frame
                                     foreground-color
                                     background-color
                                     left-fringe
                                     right-fringe
                                     font
                                     keep-ratio
                                     override-parameters
                                     respect-header-line
                                     respect-mode-line)
  "Create a child-frame for posframe.
This posframe's buffer is POSFRAME-BUFFER."
  (let ((left-fringe (or left-fringe 0))
        (right-fringe (or right-fringe 0))
        (posframe-buffer (get-buffer-create posframe-buffer))
        (after-make-frame-functions nil)
        (args (list parent-frame
                    foreground-color
                    background-color
                    right-fringe
                    left-fringe
                    font
                    keep-ratio
                    override-parameters
                    respect-header-line
                    respect-mode-line)))
    (with-current-buffer posframe-buffer
      ;; Many variables take effect after call `set-window-buffer'
      (setq-local left-fringe-width nil)
      (setq-local right-fringe-width nil)
      (setq-local fringes-outside-margins 0)
      (setq-local truncate-lines t)
      (setq-local cursor-type nil)
      (setq-local cursor-in-non-selected-windows nil)
      (setq-local show-trailing-whitespace nil)
      (unless respect-mode-line
        (setq-local mode-line-format nil))
      (unless respect-header-line
        (setq-local header-line-format nil))

      ;; Create child-frame
      (unless (and (frame-live-p posframe--frame)
                   ;; For speed reason, posframe will reuse
                   ;; existing frame at possible, but when
                   ;; user change args, recreating frame
                   ;; is needed.
                   (equal posframe--last-args args))
        (posframe-delete-frame posframe-buffer)
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
                       (posframe-buffer . ,(cons (buffer-name posframe-buffer)
                                                 posframe-buffer))
                       (no-accept-focus . t)
                       (min-width  . 0)
                       (min-height . 0)
                       (border-width . 0)
                       (internal-border-width . 0)
                       (vertical-scroll-bars . nil)
                       (horizontal-scroll-bars . nil)
                       (left-fringe . ,left-fringe)
                       (right-fringe . ,right-fringe)
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
        (let ((posframe-window (frame-root-window posframe--frame)))
          ;; This method is more stable than 'setq mode/header-line-format nil'
          (unless respect-mode-line
            (set-window-parameter posframe-window 'mode-line-format 'none))
          (unless respect-header-line
            (set-window-parameter posframe-window 'header-line-format 'none))
          (set-window-buffer posframe-window posframe-buffer)))
      posframe--frame)))

(cl-defun posframe-show (posframe-buffer
                         &key
                         string
                         position
                         poshandler
                         width
                         height
                         min-width
                         min-height
                         x-pixel-offset
                         y-pixel-offset
                         left-fringe
                         right-fringe
                         font
                         foreground-color
                         background-color
                         respect-header-line
                         respect-mode-line
                         no-properties
                         keep-ratio
                         override-parameters
                         timeout
                         refresh)
  "Pop posframe and show STRING at POSITION.

POSITION can be:
1. A integer number, which regard as a point.
2. A cons of integer, which regard as absolute X and Y.
3. Other types, User should set POSHANDLER manual to deal
   with them.

POSHANDLER is a function with one argument, and return
a real position. its argument is a plist, which like

  (:position xxx :poshandler xxx
   :font-height xxx :font-width xxx
   :posframe xxx :posframe-buffer xxx
   :parent-frame xxx :parent-window xxx
   :x-pixel-offset xxx :y-pixel-offset xxx)

by default, poshandler is auto selected based on
POSITION's type, but user can *force* set one with
the help of POSHANDLER argument. the below are buildin
poshandler functions:
1. `posframe-poshandler-frame-center'
2. `posframe-poshandler-frame-bottom-left-corner'
3. `posframe-poshandler-frame-bottom-right-corner'
4. `posframe-poshandler-window-center'
5. `posframe-poshandler-window-top-left-corner'
6. `posframe-poshandler-window-top-right-corner'
7. `posframe-poshandler-window-bottom-left-corner'
8. `posframe-poshandler-window-bottom-right-corner'
9. `posframe-poshandler-point-top-left-corner'
9. `posframe-poshandler-point-bottom-left-corner'

This posframe's buffer is POSFRAME-BUFFER.

If NO-PROPERTIES is non-nil, The STRING's properties will
be removed before showed in posframe.

posframe's frame-size can be set by WIDTH and HEIGHT,
If one of them is nil, posframe's frame-size will fit the
content of buffer, if you don't want to posframe's
size too small, MIN-WIDTH and MIN-HEIGTH will be useful

If LEFT-FRINGE or RIGHT-FRINGE is a number, Left fringe or
right fringe with be showed with number width.

By default, posframe's font is deriverd from current frame
user can set posframe's font with FONT argument.

By default, posframe's foreground and background color are
deriverd from current frame, user can set them with the help
of FOREGROUND-COLOR and BACKGROUND-COLOR.

By default, posframe will force hide header-line and mode-line
If user want to show header-line or mode-line in posframe,
set RESPECT-HEADER-LINE or RESPECT-MODE-LINE to t.

OVERRIDE-PARAMETERS is very powful, *all* the frame parameters
used by posframe's frame can be overrided by it.

If TIMEOUT is a number, a delay of number seconds, the posframe
will auto hide.

If REFRESH is a number, posframe's frame-size will be re-adjust
every mumber seconds.

you can use `posframe-delete-all' to delete all posframes."
  (let* ((position (or position (point)))
         (posframe-buffer (get-buffer-create posframe-buffer))
         (x-pixel-offset (or x-pixel-offset 0))
         (y-pixel-offset (or y-pixel-offset 0))
         (min-width (or min-width 1))
         (min-height (or min-height 1))
         (parent-window (selected-window))
         (parent-frame (window-frame parent-window))
         (parent-frame-width (frame-pixel-width parent-frame))
         (parent-frame-height (frame-pixel-height parent-frame))
         (font-width (default-font-width))
         (font-height (posframe--get-font-height position))
         (frame-resize-pixelwise t)
         posframe)

    (with-current-buffer posframe-buffer
      ;; Move mouse to (0 . 0)
      (posframe--mouse-banish parent-frame)

      ;; Create posframe
      (setq posframe
            (posframe--create-posframe
             posframe-buffer
             :font font
             :parent-frame parent-frame
             :left-fringe left-fringe
             :right-fringe right-fringe
             :foreground-color foreground-color
             :background-color background-color
             :keep-ratio keep-ratio
             :respect-header-line respect-header-line
             :respect-mode-line respect-mode-line
             :override-parameters override-parameters))

      ;; Insert string to posframe-buffer.
      (posframe--insert-string string no-properties)

      ;; Set posframe's size
      (posframe--set-frame-size
       posframe height min-height width min-width)

      ;; Move posframe
      (posframe--set-frame-position
       posframe
       (posframe-run-poshandler
        `(;All poshandlers will get info from this plist.
          :position ,position
          :poshandler ,poshandler
          :font-height ,font-height
          :font-width ,font-width
          :posframe ,posframe
          :posframe-buffer ,posframe-buffer
          :parent-frame ,parent-frame
          :parent-window ,parent-window
          :x-pixel-offset ,x-pixel-offset
          :y-pixel-offset ,y-pixel-offset))
       parent-frame-width parent-frame-height)

      ;; Delay hide posframe when timeout is a number.
      (posframe--run-timeout-timer posframe timeout)

      ;; Re-adjust posframe's size when buffer's content has changed.
      (posframe--run-refresh-timer
       posframe refresh height min-height width min-width)

      ;; Do not return anything.
      nil)))

(defun posframe--get-font-height (position)
  "Get the font's height at POSITION."
  (when (integerp position)
    (if (= position 1)
        (default-line-height)
      (aref (font-info
             (font-at
              (if (and (= position (point-max)))
                  (- position 1)
                position)))
            3))))

(defun posframe--mouse-banish (frame)
  "Banish mouse to the (0 . 0) of FRAME.
FIXME: This is a hacky fix for the mouse focus problem, which like:
https://github.com/tumashu/posframe/issues/4#issuecomment-357514918"
  (when (and posframe-mouse-banish
             (not (equal (cdr (mouse-position)) '(0 . 0))))
    (set-mouse-position frame 0 0)))

(defun posframe--insert-string (string no-properties)
  "Insert STRING to current buffer.
If NO-PROPERTIES is non-nil, all properties of STRING
will be removed."
  (when (and string (stringp string))
    (remove-text-properties
     0 (length string) '(read-only t) string)
    ;; Does inserting string then deleting the before
    ;; contents reduce flicking? Maybe :-)
    (goto-char (point-min))
    (if no-properties
        (insert (substring-no-properties string))
      (insert string))
    (delete-region (point) (point-max))))

(defun posframe--set-frame-size (posframe height min-height width min-width)
  "Set POSFRAME's size.
It will set the size by the POSFRAME's HEIGHT, MIN-HEIGHT
WIDTH and MIN-WIDTH."
  (if (and width height)
      (unless (equal posframe--last-posframe-size
                     (cons width height))
        (set-frame-size posframe width height)
        (setq-local posframe--last-posframe-size
                    (cons width height)))
    (fit-frame-to-buffer
     posframe height min-height width min-width)))

(defun posframe--set-frame-position (posframe position
                                              parent-frame-width
                                              parent-frame-height)
  "Move POSFRAME to POSITION.
This need PARENT-FRAME-WIDTH and PARENT-FRAME-HEIGHT"
  (unless (and (equal position posframe--last-position)
               ;; When working frame's size change, re-posit
               ;; the posframe.
               (equal posframe--last-parent-frame-size
                      (cons parent-frame-width parent-frame-height)))
    (set-frame-position posframe (car position) (cdr position))
    (setq-local posframe--last-position position)
    (setq-local posframe--last-parent-frame-size
                (cons parent-frame-width parent-frame-height)))
  ;; Make posframe's posframe--frame visible
  (unless (frame-visible-p posframe)
    (make-frame-visible posframe)))

(defun posframe--run-timeout-timer (posframe secs)
  "Hide POSFRAME after a delay of SECS seconds."
  (when (and (numberp secs) (> secs 0))
    (when (timerp posframe--timeout-timer)
      (cancel-timer posframe--timeout-timer))
    (setq-local posframe--timeout-timer
                (run-with-timer
                 secs nil #'make-frame-invisible posframe))))

(defun posframe--run-refresh-timer (posframe repeat
                                             height min-height
                                             width min-width)
  "Refresh POSFRAME every REPEAT seconds.

It will set POSFRAME's size by the posframe's HEIGHT, MIN-HEIGHT,
WIDTH and MIN-WIDTH."
  (when (and (numberp repeat) (> repeat 0))
    (unless (and width height)
      (when (timerp posframe--refresh-timer)
        (cancel-timer posframe--refresh-timer))
      (setq-local posframe--refresh-timer
                  (run-with-timer
                   nil repeat
                   #'(lambda (frame height min-height width min-width)
                       (when (and frame (frame-live-p frame))
                         (fit-frame-to-buffer
                          frame height min-height width min-width)))
                   posframe height min-height width min-width)))))

(defun posframe-hide (posframe-buffer)
  "Hide posframe which buffer is POSFRAME-BUFFER."
  (dolist (frame (frame-list))
    (let ((buffer-info (frame-parameter frame 'posframe-buffer)))
      (when (or (equal posframe-buffer (car buffer-info))
                (equal posframe-buffer (cdr buffer-info)))
        (make-frame-invisible frame)))))

(defun posframe-delete (posframe-buffer)
  "Delete posframe which buffer POSFRAME-BUFFER."
  (posframe-delete-frame posframe-buffer)
  (posframe--kill-buffer posframe-buffer))

(defun posframe-delete-frame (posframe-buffer)
  "Kill child-frame of posframe.
This posframe's buffer is POSFRAME-BUFFER."
  (dolist (frame (frame-list))
    (let ((buffer-info (frame-parameter frame 'posframe-buffer))
          (buffer (get-buffer posframe-buffer)))
      (when (or (equal posframe-buffer (car buffer-info))
                (equal posframe-buffer (cdr buffer-info)))
        (when buffer
          (with-current-buffer buffer
            (dolist (timer '(posframe--refresh-timer
                             posframe--timeout-timer))
              (when (timerp timer)
                (cancel-timer timer)))))
        (delete-frame frame)))))

(defun posframe--kill-buffer (posframe-buffer)
  "Kill posframe's buffer: POSFRAME-BUFFER."
  (when (buffer-live-p posframe-buffer)
    (kill-buffer posframe-buffer)))

;;;###autoload
(defun posframe-hide-all ()
  "Hide all posframe's frames."
  (interactive)
  (dolist (frame (frame-list))
    (let ((buffer-info (frame-parameter frame 'posframe-buffer)))
      (when buffer-info (make-frame-invisible frame)))))

;;;###autoload
(defun posframe-delete-all ()
  "Delete all posframe's frames and buffers."
  (interactive)
  (dolist (frame (frame-list))
    (let ((buffer-info (frame-parameter frame 'posframe-buffer)))
      (when buffer-info (delete-frame frame))))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when posframe--frame
        (posframe--kill-buffer buffer)))))

(defun posframe-auto-delete ()
  "Auto delete posframe when its buffer is killed.

This function is used by `kill-buffer-hook'."
  (posframe-delete-frame (current-buffer)))

(add-hook 'kill-buffer-hook #'posframe-auto-delete)

;; Posframe's position handler
(defun posframe-run-poshandler (info)
  "Run posframe's position handler.

the structure of INFO can be found in docstring
of `posframe-show'."
  (funcall
   (or (plist-get info :poshandler)
       (let ((position (plist-get info :position)))
         (cond ((integerp position)
                #'posframe-poshandler-point-bottom-left-corner)
               ((and (consp position)
                     (integerp (car position))
                     (integerp (cdr position)))
                #'posframe-poshandler-absolute-x-y)
               (t (error "Posframe: have no valid poshandler")))))
   info))

(defun posframe-poshandler-absolute-x-y (info)
  "Posframe's position hanlder.

Deal with (integer . integer) style position,
the structure of INFO can be found in docstring
of `posframe-show'."
  (let ((position (plist-get info :position))
        (x-pixel-offset (plist-get info :x-pixel-offset))
        (y-pixel-offset (plist-get info :y-pixel-offset)))
    (cons (+ (car position) x-pixel-offset)
          (+ (cdr position) y-pixel-offset))))

(defun posframe-poshandler-point-bottom-left-corner (info &optional font-height)
  "Posframe's position hanlder.

Get bottom-left-corner pixel position of a point,
the structure of INFO can be found in docstring
of `posframe-show'.

Optional argument FONT-HEIGHT ."
  (let* ((position (plist-get info :position))
         (x-pixel-offset (plist-get info :x-pixel-offset))
         (y-pixel-offset (plist-get info :y-pixel-offset))
         (posframe-width (frame-pixel-width
                          (plist-get info :posframe)))
         (posframe-height (frame-pixel-height
                           (plist-get info :posframe)))
         (window (plist-get info :parent-window))
         (frame (plist-get info :parent-frame))
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
         (font-height (or font-height (plist-get info :font-height)))
         (y-bottom (+ y-top font-height)))
    (cons (max 0 (min x (- xmax (or posframe-width 0))))
          (max 0 (if (> (+ y-bottom (or posframe-height 0)) ymax)
                     (- y-top (or posframe-height 0))
                   y-bottom)))))

(defun posframe-poshandler-point-top-left-corner (info)
  "Posframe's position hanlder.

Get top-left-corner pixel position of a point,
the structure of INFO can be found in docstring
of `posframe-show'."
  (let ((font-height 0))
    (posframe-poshandler-point-bottom-left-corner info font-height)))

(defun posframe-poshandler-frame-center (info)
  "Posframe's position handler.

Get a position which let posframe stay onto its
parent-frame's center.  The structure of INFO can
be found in docstring of `posframe-show'."
  (let* ((posframe (plist-get info :posframe))
         (parent-frame (plist-get info :parent-frame)))
    (cons (/ (- (frame-pixel-width parent-frame)
                (frame-pixel-width posframe))
             2)
          (/ (- (frame-pixel-height parent-frame)
                (frame-pixel-height posframe))
             2))))

(defun posframe-poshandler-frame-bottom-left-corner (_info)
  "Posframe's position handler.

Get a position which let posframe stay onto its parent-frame's
bottom left corner.  The structure of INFO can be found
in docstring of `posframe-show'."
  (cons 0 (- 0
             (window-mode-line-height)
             (window-pixel-height (minibuffer-window)))))

(defun posframe-poshandler-frame-bottom-right-corner (_info)
  "Posframe's position handler.

Get a position which let posframe stay onto its parent-frame's
bottom right corner.  The structure of INFO can be found
in docstring of `posframe-show'."
  (cons -1 (- 0
              (window-mode-line-height)
              (window-pixel-height (minibuffer-window)))))

(defun posframe-poshandler-window-center (info)
  "Posframe's position handler.

Get a position which let posframe stay onto current window's
center.  The structure of INFO can be found in docstring
of `posframe-show'."
  (let* ((posframe (plist-get info :posframe))
         (parent-window (plist-get info :parent-window))
         (window-left (window-pixel-left parent-window))
         (window-top (window-pixel-top parent-window))
         (window-width (window-pixel-width parent-window))
         (window-height (window-pixel-height parent-window))
         (posframe-width (frame-pixel-width posframe))
         (posframe-height (frame-pixel-height posframe)))
    (cons (+ window-left (/ (- window-width posframe-width) 2))
          (+ window-top (/ (- window-height posframe-height) 2)))))

(defun posframe-poshandler-window-top-left-corner (info)
  "Posframe's position handler.

Get a position which let posframe stay onto current window's
top left corner.  The structure of INFO can be found in
docstring of `posframe-show'."
  (let* ((parent-window (plist-get info :parent-window))
         (window-left (window-pixel-left parent-window))
         (window-top (window-pixel-top parent-window)))
    (cons window-left
          window-top)))

(defun posframe-poshandler-window-top-right-corner (info)
  "Posframe's position handler.

Get a position which let posframe stay onto current window's
top right corner.  The structure of INFO can be found in
docstring of `posframe-show'."
  (let* ((posframe (plist-get info :posframe))
         (window (plist-get info :parent-window))
         (window-left (window-pixel-left window))
         (window-top (window-pixel-top window))
         (window-width (window-pixel-width window))
         (posframe-width (frame-pixel-width posframe)))
    (cons (+ window-left window-width
             (- 0 posframe-width))
          window-top)))

(defun posframe-poshandler-window-bottom-left-corner (info)
  "Posframe's position handler.

Get a position which let posframe stay onto current window's
bottom left corner.  The structure of INFO can be found in
docstring of `posframe-show'."
  (let* ((posframe (plist-get info :posframe))
         (window (plist-get info :parent-window))
         (window-left (window-pixel-left window))
         (window-top (window-pixel-top window))
         (window-height (window-pixel-height window))
         (posframe-height (frame-pixel-height posframe))
         (modeline-height (window-mode-line-height)))
    (cons window-left
          (+ window-top window-height
             (- 0 modeline-height posframe-height)))))

(defun posframe-poshandler-window-bottom-right-corner (info)
  "Posframe's position handler.

Get a position which let posframe stay onto current window's
bottom right corner.  The structure of INFO can be found in
docstring of `posframe-show'."
  (let* ((posframe (plist-get info :posframe))
         (window (plist-get info :parent-window))
         (window-left (window-pixel-left window))
         (window-top (window-pixel-top window))
         (window-width (window-pixel-width window))
         (window-height (window-pixel-height window))
         (posframe-width (frame-pixel-width posframe))
         (posframe-height (frame-pixel-height posframe))
         (modeline-height (window-mode-line-height)))
    (cons (+ window-left window-width
             (- 0 posframe-width))
          (+ window-top window-height
             (- 0 modeline-height posframe-height)))))

(provide 'posframe)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; posframe.el ends here
