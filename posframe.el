;;; posframe.el --- Pop a posframe (just a frame) at point    -*- lexical-binding:t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Feng Shu <tumashu@163.com>
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/posframe
;; Version: 0.5.0
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

;; **** Simple way
;; #+BEGIN_EXAMPLE
;; (when (posframe-workable-p)
;;   (posframe-show " *my-posframe-buffer*"
;;                  :string "This is a test"
;;                  :position (point)))
;; #+END_EXAMPLE

;; **** Advanced way
;; #+BEGIN_EXAMPLE
;; (defvar my-posframe-buffer " *my-posframe-buffer*")

;; (with-current-buffer (get-buffer-create my-posframe-buffer)
;;   (erase-buffer)
;;   (insert "Hello world"))

;; (when (posframe-workable-p)
;;   (posframe-show my-posframe-buffer
;;                  :position (point)))
;; #+END_EXAMPLE

;; **** Arguments

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

;; *** Customizing pointer control

;; By default, posframe moves the pointer to point (0,0) in
;; the frame, as a way to address an issue with mouse focus.
;; To disable this feature, add this to your init.el:
;; #+BEGIN_EXAMPLE
;; (setq posframe-mouse-banish nil)
;; #+END_EXAMPLE

;; *** Set fallback argument of posframe-show

;; user can set fallback values of posframe-show's arguments with the
;; help of `posframe-arghandler'. the below example set fallback
;; border-width to 10 and fallback background color to green.

;; #+BEGIN_EXAMPLE
;; (setq posframe-arghandler #'my-posframe-arghandler)
;; (defun my-posframe-arghandler (posframe-buffer arg-name value)
;;   (let ((info '(:internal-border-width 10 :background-color "green")))
;;     (or (plist-get info arg-name) value)))
;; #+END_EXAMPLE

;;; Code:
;; * posframe's code                         :CODE:
(require 'cl-lib)

(defgroup posframe nil
  "Pop a posframe (just a frame) at point"
  :group 'lisp
  :prefix "posframe-")

(defcustom posframe-mouse-banish (not (eq system-type 'darwin))
  "Mouse will be moved to (0 , 0) when it is non-nil."
  :group 'posframe
  :type 'boolean)

(defcustom posframe-inhibit-double-buffering nil
  "Set the posframe's frame-parameter: inhibit-double-buffering."
  :group 'posframe
  :type 'boolean)

(defcustom posframe-arghandler #'posframe-arghandler-default
  "A function used to handle posframe-show's argument.

User can use this feature to set the default value of
posframe-show's argument."
  :group 'posframe
  :type 'function)

(defvar-local posframe--frame nil
  "Record posframe's frame.")

(defvar-local posframe--last-posframe-pixel-position nil
  "Record the last pixel position of posframe's frame.")

(defvar-local posframe--last-posframe-size nil
  "Record the last size of posframe's frame.")

(defvar-local posframe--last-parent-frame-size nil
  "Record the last size of posframe's parent-frame.")

(defvar-local posframe--last-poshandler-info nil
  "Record the last poshandler info.")

(defvar-local posframe--last-font-height-info nil
  "Record the last font height info.")

(defvar-local posframe--last-args nil
  "Record the last arguments of `posframe--create-posframe'.

If these args have changed, posframe will recreate its
frame.")

(defvar-local posframe--timeout-timer nil
  "Record the timer to deal with timeout argument of `posframe-show'.")

(defvar-local posframe--refresh-timer nil
  "Record the timer to deal with refresh argument of `posframe-show'.")

(defvar-local posframe--initialized-p nil
  "Record initialize status of `posframe-show'.")

(defun posframe-workable-p ()
  "Test posframe workable status."
  (and (>= emacs-major-version 26)
       (not (or noninteractive
                emacs-basic-display
                (not (display-graphic-p))))))

(cl-defun posframe--create-posframe (posframe-buffer
                                     &key
                                     parent-frame
                                     foreground-color
                                     background-color
                                     left-fringe
                                     right-fringe
                                     internal-border-width
                                     internal-border-color
                                     font
                                     keep-ratio
                                     override-parameters
                                     respect-header-line
                                     respect-mode-line)
  "Create a child-frame for posframe.
This posframe's buffer is POSFRAME-BUFFER."
  (let ((left-fringe (or left-fringe 0))
        (right-fringe (or right-fringe 0))
        (internal-border-width (or internal-border-width 0))
        (posframe-buffer (get-buffer-create posframe-buffer))
        (after-make-frame-functions nil)
        (args (list parent-frame
                    foreground-color
                    background-color
                    right-fringe
                    left-fringe
                    internal-border-width
                    font
                    keep-ratio
                    override-parameters
                    respect-header-line
                    respect-mode-line)))
    (with-current-buffer posframe-buffer
      ;; Many variables take effect after call `set-window-buffer'
      (setq-local display-line-numbers nil)
      (setq-local frame-title-format "")
      (setq-local left-margin-width nil)
      (setq-local right-margin-width nil)
      (setq-local left-fringe-width nil)
      (setq-local right-fringe-width nil)
      (setq-local fringes-outside-margins 0)
      (setq-local truncate-lines nil)
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
        (setq-local posframe--last-posframe-pixel-position nil)
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
                       (fullscreen . nil)
                       (no-accept-focus . t)
                       (min-width  . 0)
                       (min-height . 0)
                       (border-width . 0)
                       (internal-border-width . ,internal-border-width)
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
        (when internal-border-color
          (set-face-background 'internal-border
                               internal-border-color posframe--frame))
        (let ((posframe-window (frame-root-window posframe--frame)))
          ;; This method is more stable than 'setq mode/header-line-format nil'
          (unless respect-mode-line
            (set-window-parameter posframe-window 'mode-line-format 'none))
          (unless respect-header-line
            (set-window-parameter posframe-window 'header-line-format 'none))
          (set-window-buffer posframe-window posframe-buffer)))
      posframe--frame)))

(defun posframe-arghandler-default (posframe-buffer arg-name value)
  "The default value of `posframe-arghandler'"
  value)

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
                         internal-border-width
                         internal-border-color
                         font
                         foreground-color
                         background-color
                         respect-header-line
                         respect-mode-line
                         initialize
                         no-properties
                         keep-ratio
                         override-parameters
                         timeout
                         refresh
                         &allow-other-keys)
  "Pop posframe and show STRING at POSITION.

POSITION can be:
1. A integer number, which regard as a point.
2. A cons of integer, which regard as absolute X and Y.
3. Other types, User should set POSHANDLER manual to deal
   with them.

POSHANDLER is a function with one argument, and return
a real position. its argument is a plist, which like

  (:position xxx
   :position-info xxx
   :poshandler xxx
   :font-height xxx
   :font-width xxx
   :posframe xxx
   :posframe-width xxx
   :posframe-height xxx
   :posframe-buffer xxx
   :parent-frame xxx
   :parent-window-left xxx
   :parent-window-top xxx
   :parent-frame-width xxx
   :parent-frame-height xxx
   :parent-window xxx
   :parent-window-width  xxx
   :parent-window-height xxx
   :minibuffer-height
   :mode-line-height
   :header-line-height
   :x-pixel-offset xxx
   :y-pixel-offset xxx)

by default, poshandler is auto selected based on
POSITION's type, but user can *force* set one with
the help of POSHANDLER argument. the below are buildin
poshandler functions:
1.  `posframe-poshandler-frame-center'
2.  `posframe-poshandler-frame-top-center'
3.  `posframe-poshandler-frame-top-left-corner'
4.  `posframe-poshandler-frame-top-right-corner'
5.  `posframe-poshandler-frame-bottom-left-corner'
6.  `posframe-poshandler-frame-bottom-right-corner'
7.  `posframe-poshandler-window-center'
8.  `posframe-poshandler-window-top-left-corner'
9.  `posframe-poshandler-window-top-right-corner'
10. `posframe-poshandler-window-bottom-left-corner'
11. `posframe-poshandler-window-bottom-right-corner'
12. `posframe-poshandler-point-top-left-corner'
13. `posframe-poshandler-point-bottom-left-corner'

This posframe's buffer is POSFRAME-BUFFER.

If NO-PROPERTIES is non-nil, The STRING's properties will
be removed before showed in posframe.

posframe's frame-size can be set by WIDTH and HEIGHT,
If one of them is nil, posframe's frame-size will fit the
content of buffer, if you don't want to posframe's
size too small, MIN-WIDTH and MIN-HEIGTH will be useful

If LEFT-FRINGE or RIGHT-FRINGE is a number, Left fringe or
right fringe with be showed with number width.

By default, posframe shows no border, user can let border
showed by setting INTERNAL-BORDER-WIDTH to a postive number,
by the way, border's color can be specified by INTERNAL-BORDER-COLOR
or ‘internal-border’ face.

By default, posframe's font is deriverd from current frame
user can set posframe's font with FONT argument.

By default, posframe's foreground and background color are
deriverd from current frame, user can set them with the help
of FOREGROUND-COLOR and BACKGROUND-COLOR.

By default, posframe will force hide header-line and mode-line
If user want to show header-line or mode-line in posframe,
set RESPECT-HEADER-LINE or RESPECT-MODE-LINE to t.

INITIALIZE is a function with no argument, it will run when
posframe buffer is first selected with `with-current-buffer'
in posframe-show, and only run once for speed reason, If INITIALIZE
is nil, `posframe-default-initialize-function' will be used as
fallback, user can use this variable to global set posframe buffer.

OVERRIDE-PARAMETERS is very powful, *all* the frame parameters
used by posframe's frame can be overrided by it.

If TIMEOUT is a number, a delay of number seconds, the posframe
will auto hide.

If REFRESH is a number, posframe's frame-size will be re-adjust
every mumber seconds.

you can use `posframe-delete-all' to delete all posframes."
  (let* ((position (or (funcall posframe-arghandler posframe-buffer :position position) (point)))
         (poshandler (funcall posframe-arghandler posframe-buffer :poshandler poshandler))
         (width (funcall posframe-arghandler posframe-buffer :width width))
         (height (funcall posframe-arghandler posframe-buffer :height height))
         (min-width (or (funcall posframe-arghandler posframe-buffer :min-width min-width) 1))
         (min-height (or (funcall posframe-arghandler posframe-buffer :min-height min-height) 1))
         (x-pixel-offset (or (funcall posframe-arghandler posframe-buffer :x-pixel-offset x-pixel-offset) 0))
         (y-pixel-offset (or (funcall posframe-arghandler posframe-buffer :y-pixel-offset y-pixel-offset) 0))
         (left-fringe (funcall posframe-arghandler posframe-buffer :left-fringe left-fringe))
         (right-fringe (funcall posframe-arghandler posframe-buffer :right-fringe right-fringe))
         (internal-border-width (funcall posframe-arghandler posframe-buffer :internal-border-width internal-border-width))
         (internal-border-color (funcall posframe-arghandler posframe-buffer :internal-border-color internal-border-color))
         (font (funcall posframe-arghandler posframe-buffer :font font))
         (foreground-color (funcall posframe-arghandler posframe-buffer :foreground-color foreground-color))
         (background-color (funcall posframe-arghandler posframe-buffer :background-color background-color))
         (respect-header-line (funcall posframe-arghandler posframe-buffer :respect-header-line respect-header-line))
         (respect-mode-line (funcall posframe-arghandler posframe-buffer :respect-mode-line respect-mode-line))
         (initialize (funcall posframe-arghandler posframe-buffer :initialize initialize))
         (no-properties (funcall posframe-arghandler posframe-buffer :no-properties no-properties))
         (keep-ratio (funcall posframe-arghandler posframe-buffer :keep-ratio keep-ratio))
         (override-parameters (funcall posframe-arghandler posframe-buffer :override-parameters override-parameters))
         (timeout (funcall posframe-arghandler posframe-buffer :timeout timeout))
         (refresh (funcall posframe-arghandler posframe-buffer :refresh refresh))
         ;;-----------------------------------------------------
         (posframe-buffer (get-buffer-create posframe-buffer))
         (parent-window (selected-window))
         (parent-window-top (window-pixel-top parent-window))
         (parent-window-left (window-pixel-left parent-window))
         (parent-window-width (window-pixel-width parent-window))
         (parent-window-height (window-pixel-height parent-window))
         (position-info
          (if (integerp position)
              (posn-at-point position parent-window)
            position))
         (parent-frame (window-frame parent-window))
         (parent-frame-width (frame-pixel-width parent-frame))
         (parent-frame-height (frame-pixel-height parent-frame))
         (font-width (default-font-width))
         (font-height (posframe--get-font-height position))
         (mode-line-height (window-mode-line-height))
         (minibuffer-height (window-pixel-height (minibuffer-window)))
         (header-line-height (window-header-line-height parent-window))
         (frame-resize-pixelwise t)
         posframe)

    (with-current-buffer posframe-buffer

      ;; Initialize
      (unless posframe--initialized-p
        (let ((func initialize))
          (when (functionp func)
            (funcall func)
            (setq posframe--initialized-p t))))

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
             :internal-border-width internal-border-width
             :internal-border-color internal-border-color
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
          :position-info ,position-info
          :poshandler ,poshandler
          :font-height ,font-height
          :font-width ,font-width
          :posframe ,posframe
          :posframe-width ,(frame-pixel-width posframe)
          :posframe-height ,(frame-pixel-height posframe)
          :posframe-buffer ,posframe-buffer
          :parent-frame ,parent-frame
          :parent-frame-width ,parent-frame-width
          :parent-frame-height ,parent-frame-height
          :parent-window ,parent-window
          :parent-window-top ,parent-window-top
          :parent-window-left ,parent-window-left
          :parent-window-width ,parent-window-width
          :parent-window-height ,parent-window-height
          :mode-line-height ,mode-line-height
          :minibuffer-height ,minibuffer-height
          :header-line-height ,header-line-height
          :x-pixel-offset ,x-pixel-offset
          :y-pixel-offset ,y-pixel-offset))
       parent-frame-width parent-frame-height)

      ;; Delay hide posframe when timeout is a number.
      (posframe--run-timeout-timer posframe timeout)

      ;; Re-adjust posframe's size when buffer's content has changed.
      (posframe--run-refresh-timer
       posframe refresh height min-height width min-width)

      ;; Make sure not hide buffer's content for scroll down.
      (set-window-point (frame-root-window posframe--frame) 0)

      ;; Do not return anything.
      nil)))

(defun posframe--get-font-height (position)
  "Get the font's height at POSITION."
  (if (eq position (car posframe--last-font-height-info))
      (cdr posframe--last-font-height-info)
    (let* ((font (when (and (integerp position)
                            (not (= position 1)))
                   (font-at (if (and (= position (point-max)))
                                (- position 1)
                              position))))
           (height (when (integerp position)
                     (if (or (= position 1) (not (fontp font)))
                         (default-line-height)
                       (aref (font-info font) 3)))))
      (setq posframe--last-font-height-info
            (cons position height))
      height)))

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
    (let ((str (if no-properties
                   (substring-no-properties string)
                 string)))
      (erase-buffer)
      (insert str))))

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
  (unless (and (equal position posframe--last-posframe-pixel-position)
               ;; When working frame's size change, re-posit
               ;; the posframe.
               (equal posframe--last-parent-frame-size
                      (cons parent-frame-width parent-frame-height)))
    (set-frame-position posframe (car position) (cdr position))
    (setq-local posframe--last-posframe-pixel-position position)
    (setq-local posframe--last-parent-frame-size
                (cons parent-frame-width parent-frame-height)))
  ;; Make posframe's posframe--frame visible
  (unless (frame-visible-p posframe)
    (make-frame-visible posframe)
    ;; Fix issue: https://github.com/tumashu/ivy-posframe/pull/30
    (redraw-frame posframe)))

(defun posframe--run-timeout-timer (posframe secs)
  "Hide POSFRAME after a delay of SECS seconds."
  (when (and (numberp secs) (> secs 0))
    (when (timerp posframe--timeout-timer)
      (cancel-timer posframe--timeout-timer))
    (setq-local posframe--timeout-timer
                (run-with-timer
                 secs nil #'posframe--make-frame-invisible posframe))))

(defun posframe--make-frame-invisible (frame)
  "This function used to instead `make-frame-invisible' to make hide frame safely."
  (when (frame-live-p frame)
    (make-frame-invisible frame)))

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
        (posframe--make-frame-invisible frame)))))

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

(defun posframe-funcall (posframe-buffer function &rest arguments)
  "Select posframe of POSFRAME-BUFFER's, and call FUNCTION.
passing remaining arguments to it."
  (when (get-buffer posframe-buffer)
    (with-current-buffer posframe-buffer
      (when (framep posframe--frame)
        (with-selected-frame posframe--frame
          (when (functionp function)
            (apply function arguments)))))))

;;;###autoload
(defun posframe-hide-all ()
  "Hide all posframe's frames."
  (interactive)
  (dolist (frame (frame-list))
    (let ((buffer-info (frame-parameter frame 'posframe-buffer)))
      (when buffer-info (posframe--make-frame-invisible frame)))))

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
  (if (equal info posframe--last-poshandler-info)
      posframe--last-posframe-pixel-position
    (setq posframe--last-poshandler-info info)
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
     info)))

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
  (let* ((x-pixel-offset (plist-get info :x-pixel-offset))
         (y-pixel-offset (plist-get info :y-pixel-offset))
         (posframe-width (plist-get info :posframe-width))
         (posframe-height (plist-get info :posframe-height))
         (window (plist-get info :parent-window))
         (xmax (plist-get info :parent-frame-width))
         (ymax (plist-get info :parent-frame-height))
         (position-info (plist-get info :position-info))
         (header-line-height (plist-get info :header-line-height))
         (x (+ (car (window-inside-pixel-edges window))
               (- (or (car (posn-x-y position-info)) 0)
                  (or (car (posn-object-x-y position-info)) 0))
               x-pixel-offset))
         (y-top (+ (cadr (window-pixel-edges window))
                   header-line-height
                   (- (or (cdr (posn-x-y position-info)) 0)
                      ;; Fix the conflict with flycheck
                      ;; http://lists.gnu.org/archive/html/emacs-devel/2018-01/msg00537.html
                      (or (cdr (posn-object-x-y position-info)) 0))
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
  (cons (/ (- (plist-get info :parent-frame-width)
              (plist-get info :posframe-width))
           2)
        (/ (- (plist-get info :parent-frame-height)
              (plist-get info :posframe-height))
           2)))


(defun posframe-poshandler-frame-top-center (info)
  "Posframe's position handler.

Get a position which let posframe stay onto its
parent-frame's top center.  The structure of INFO can
be found in docstring of `posframe-show'."
  (cons (/ (- (plist-get info :parent-frame-width)
              (plist-get info :posframe-width))
           2)
        0))

(defun posframe-poshandler-frame-top-left-corner (_info)
  "Posframe's position handler.

Get a position which let posframe stay onto its parent-frame's
top left corner.  The structure of INFO can be found
in docstring of `posframe-show'."
  '(0 . 0))

(defun posframe-poshandler-frame-top-right-corner (_info)
  "Posframe's position handler.

Get a position which let posframe stay onto its parent-frame's
top right corner.  The structure of INFO can be found
in docstring of `posframe-show'."
  '(-1 . 0))


(defun posframe-poshandler-frame-bottom-left-corner (info)
  "Posframe's position handler.

Get a position which let posframe stay onto its parent-frame's
bottom left corner.  The structure of INFO can be found
in docstring of `posframe-show'."
  (cons 0 (- 0
             (plist-get info :mode-line-height)
             (plist-get info :minibuffer-height))))

(defun posframe-poshandler-frame-bottom-right-corner (info)
  "Posframe's position handler.

Get a position which let posframe stay onto its parent-frame's
bottom right corner.  The structure of INFO can be found
in docstring of `posframe-show'."
  (cons -1 (- 0
              (plist-get info :mode-line-height)
              (plist-get info :minibuffer-height))))

(defun posframe-poshandler-window-center (info)
  "Posframe's position handler.

Get a position which let posframe stay onto current window's
center.  The structure of INFO can be found in docstring
of `posframe-show'."
  (let* ((window-left (plist-get info :parent-window-left))
         (window-top (plist-get info :parent-window-top))
         (window-width (plist-get info :parent-window-width))
         (window-height (plist-get info :parent-window-height))
         (posframe-width (plist-get info :posframe-width))
         (posframe-height (plist-get info :posframe-height)))
    (cons (+ window-left (/ (- window-width posframe-width) 2))
          (+ window-top (/ (- window-height posframe-height) 2)))))

(defun posframe-poshandler-window-top-left-corner (info)
  "Posframe's position handler.

Get a position which let posframe stay onto current window's
top left corner.  The structure of INFO can be found in
docstring of `posframe-show'."
  (let* ((window-left (plist-get info :parent-window-left))
         (window-top (plist-get info :parent-window-top)))
    (cons window-left
          window-top)))

(defun posframe-poshandler-window-top-right-corner (info)
  "Posframe's position handler.

Get a position which let posframe stay onto current window's
top right corner.  The structure of INFO can be found in
docstring of `posframe-show'."
  (let* ((window-left (plist-get info :parent-window-left))
         (window-top (plist-get info :parent-window-top))
         (window-width (plist-get info :parent-window-width))
         (posframe-width (plist-get info :posframe-width)))
    (cons (+ window-left window-width
             (- 0 posframe-width))
          window-top)))

(defun posframe-poshandler-window-bottom-left-corner (info)
  "Posframe's position handler.

Get a position which let posframe stay onto current window's
bottom left corner.  The structure of INFO can be found in
docstring of `posframe-show'."
  (let* ((window-left (plist-get info :parent-window-left))
         (window-top (plist-get info :parent-window-top))
         (window-height (plist-get info :parent-window-height))
         (posframe-height (plist-get info :posframe-height))
         (mode-line-height (plist-get info :mode-line-height)))
    (cons window-left
          (+ window-top window-height
             (- 0 mode-line-height posframe-height)))))

(defun posframe-poshandler-window-bottom-right-corner (info)
  "Posframe's position handler.

Get a position which let posframe stay onto current window's
bottom right corner.  The structure of INFO can be found in
docstring of `posframe-show'."
  (let* ((window-left (plist-get info :parent-window-left))
         (window-top (plist-get info :parent-window-top))
         (window-width (plist-get info :parent-window-width))
         (window-height (plist-get info :parent-window-height))
         (posframe-width (plist-get info :posframe-width))
         (posframe-height (plist-get info :posframe-height))
         (mode-line-height (plist-get info :mode-line-height)))
    (cons (+ window-left window-width
             (- 0 posframe-width))
          (+ window-top window-height
             (- 0 mode-line-height posframe-height)))))

(provide 'posframe)

;;; posframe.el ends here
