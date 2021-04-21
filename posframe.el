;;; posframe.el --- Pop a posframe (just a frame) at point    -*- lexical-binding:t -*-

;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

;; Author: Feng Shu <tumashu@163.com>
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/posframe
;; Version: 1.0.1
;; Keywords: convenience, tooltip
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
;; ** What is posframe?
;; Posframe can pop up a frame at point, this *posframe* is a
;; child-frame connected to its root window's buffer.

;; The main advantages are:
;; 1. It is fast enough for daily usage :-)
;; 2. It works well with CJK languages.

;; NOTE:
;; 1. For MacOS users, posframe needs Emacs version >= 26.0.91
;; 2. GNOME users with GTK3 builds need Emacs 27 or later.
;;    See variable `posframe-gtk-resize-child-frames'
;;    which auto-detects this configuration.

;;    More details:
;;    1. [[https://git.savannah.gnu.org/cgit/emacs.git/commit/?h=emacs-27&id=c49d379f17bcb0ce82604def2eaa04bda00bd5ec][Fix some problems with moving and resizing child frames]]
;;    2. [[https://lists.gnu.org/archive/html/emacs-devel/2020-01/msg00343.html][Emacs's set-frame-size can not work well with gnome-shell?]]

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
;; 2. Only delete the frame
;;    #+BEGIN_EXAMPLE
;;    (posframe-delete-frame " *my-posframe-buffer*")
;;    #+END_EXAMPLE
;; *** Delete all posframes
;; #+BEGIN_EXAMPLE
;; M-x posframe-delete-all
;; #+END_EXAMPLE

;; Note: this command will delete all posframe buffers.
;; You probably shouldn't use it if you are sharing a buffer
;; between posframe and other packages.

;; *** Customizing mouse pointer control

;; By default, posframe moves the pointer to point (0,0) in
;; the frame, as a way to address an issue with mouse focus.
;; To disable this feature, add this to your init.el:
;; #+BEGIN_EXAMPLE
;; (setq posframe-mouse-banish nil)
;; #+END_EXAMPLE

;; *** Set fallback arguments of posframe-show

;; Users can set fallback values of posframe-show's arguments with the
;; help of `posframe-arghandler'.  The example below sets fallback
;; border-width to 10 and fallback background color to green.

;; #+BEGIN_EXAMPLE
;; (setq posframe-arghandler #'my-posframe-arghandler)
;; (defun my-posframe-arghandler (buffer-or-name arg-name value)
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
  "Mouse will be moved to `posframe--mouse-banish-coordinate` when it is non-nil.

This option is used to solve the problem of child frame getting
focus, with the help of `posframe--redirect-posframe-focus',
setting this option to `nil' will work well in *most* cases."
  :group 'posframe
  :type 'boolean)

(defcustom posframe--mouse-banish-coordinate '(0 . 0)
  "Set the position to which to banish the mouse when `posframe-mouse-banish` is not nil. "
  :group 'posframe
  :type 'list)

(defcustom posframe-inhibit-double-buffering nil
  "Set the posframe's frame-parameter: inhibit-double-buffering."
  :group 'posframe
  :type 'boolean)

(defcustom posframe-arghandler #'posframe-arghandler-default
  "A function used to handle posframe-show's argument.

Users can use this feature to set the default value of
posframe-show's arguments."
  :group 'posframe
  :type 'function)

(defvar-local posframe--frame nil
  "Record posframe's frame.")

(defvar-local posframe--last-posframe-pixel-position nil
  "Record the last pixel position of posframe's frame.")

(defvar-local posframe--last-posframe-size nil
  "Record the last size of posframe's frame.")

(defvar-local posframe--last-posframe-displayed-size nil
  "Record the last displayed size of posframe's frame.")

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

;; Avoid compilation warnings on Emacs < 27.
(defvar x-gtk-resize-child-frames)

(defvar posframe-gtk-resize-child-frames
  (when (and
         (> emacs-major-version 26)
         (string-match-p "GTK3" system-configuration-features)
         (let ((value (or (getenv "XDG_CURRENT_DESKTOP") (getenv "DESKTOP_SESSION"))))
           (and (stringp value)
                ;; It can be "ubuntu:GNOME".
                (string-match-p "GNOME" value))))
    ;; Not future-proof, but we can use it now.
    'resize-mode)
  "Value to bind `x-gtk-resize-child-frames' to.

The value `resize-mode' only has effect on new child frames, so
if you change it, call `posframe-delete-all' for it to take
effect.")

;;;###autoload
(defun posframe-workable-p ()
  "Test posframe workable status."
  (and (>= emacs-major-version 26)
       (not (or noninteractive
                emacs-basic-display
                (not (display-graphic-p))))))

(cl-defun posframe--create-posframe (buffer-or-name
                                     &key
                                     parent-frame
                                     foreground-color
                                     background-color
                                     left-fringe
                                     right-fringe
                                     border-width
                                     border-color
                                     internal-border-width
                                     internal-border-color
                                     font
                                     keep-ratio
                                     lines-truncate
                                     override-parameters
                                     respect-header-line
                                     respect-mode-line
                                     accept-focus)
  "Create and return a posframe child frame.
This posframe's buffer is BUFFER-OR-NAME."
  (let ((left-fringe (or left-fringe 0))
        (right-fringe (or right-fringe 0))
        ;; See emacs.git:  Add distinct controls for child frames' borders (Bug#45620)
        ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=ff7b1a133bfa7f2614650f8551824ffaef13fadc
        (border-width (or border-width internal-border-width 0))
        (border-color (or border-color internal-border-color))
        (buffer (get-buffer-create buffer-or-name))
        (after-make-frame-functions nil)
        (x-gtk-resize-child-frames posframe-gtk-resize-child-frames)
        (args (list foreground-color
                    background-color
                    right-fringe
                    left-fringe
                    border-width
                    internal-border-width
                    font
                    keep-ratio
                    override-parameters
                    respect-header-line
                    respect-mode-line
                    accept-focus)))
    (with-current-buffer buffer
      ;; Many variables take effect after call `set-window-buffer'
      (setq-local display-line-numbers nil)
      (setq-local frame-title-format "")
      (setq-local left-margin-width nil)
      (setq-local right-margin-width nil)
      (setq-local left-fringe-width nil)
      (setq-local right-fringe-width nil)
      (setq-local fringes-outside-margins 0)
      ;; Need to use `lines-truncate' as our keyword variable instead of
      ;; `truncate-lines' so we don't shadow the variable that we are trying to
      ;; set.
      (setq-local truncate-lines lines-truncate)
      (setq-local cursor-type nil)
      (setq-local cursor-in-non-selected-windows nil)
      (setq-local show-trailing-whitespace nil)
      (unless respect-mode-line
        (setq-local mode-line-format nil))
      (unless respect-header-line
        (setq-local header-line-format nil))

      (add-hook 'kill-buffer-hook #'posframe-auto-delete nil t)

      ;; Create child-frame
      (unless (and (frame-live-p posframe--frame)
                   ;; For speed reason, posframe will reuse
                   ;; existing frame at possible, but when
                   ;; user change args, recreating frame
                   ;; is needed.
                   (equal posframe--last-args args))
        (posframe-delete-frame buffer)
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
                       (title . "posframe")
                       (parent-frame . ,parent-frame)
                       (keep-ratio ,keep-ratio)
                       (posframe-buffer . ,(cons (buffer-name buffer)
                                                 buffer))
                       (fullscreen . nil)
                       (no-accept-focus . ,(not accept-focus))
                       (min-width  . 0)
                       (min-height . 0)
                       (border-width . 0)
                       (internal-border-width . ,border-width)
                       (child-frame-border-width . ,border-width)
                       (vertical-scroll-bars . nil)
                       (horizontal-scroll-bars . nil)
                       (left-fringe . ,left-fringe)
                       (right-fringe . ,right-fringe)
                       (menu-bar-lines . 0)
                       (tool-bar-lines . 0)
                       (tab-bar-lines . 0)
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
                       (skip-taskbar . t)
                       (inhibit-double-buffering . ,posframe-inhibit-double-buffering)
                       ;; Do not save child-frame when use desktop.el
                       (desktop-dont-save . t))))
        (when border-color
          (set-face-background 'internal-border border-color posframe--frame)
          (when (facep 'child-frame-border)
            (set-face-background 'child-frame-border border-color posframe--frame)))
        (let ((posframe-window (frame-root-window posframe--frame)))
          ;; This method is more stable than 'setq mode/header-line-format nil'
          (unless respect-mode-line
            (set-window-parameter posframe-window 'mode-line-format 'none))
          (unless respect-header-line
            (set-window-parameter posframe-window 'header-line-format 'none))
          (set-window-buffer posframe-window buffer)
          (set-window-dedicated-p posframe-window t)))

      ;; Remove tab-bar always.
      (set-frame-parameter posframe--frame 'tab-bar-lines 0)
      (when (version< "27.0" emacs-version)
        (setq-local tab-line-format nil))

      ;; If user set 'parent-frame to nil after run posframe-show.
      ;; for cache reason, next call to posframe-show will be affected.
      ;; so we should force set parent-frame again in this place.
      (set-frame-parameter posframe--frame 'parent-frame parent-frame)

      posframe--frame)))

(defun posframe-arghandler-default (_buffer-or-name _arg-name value)
  "The default value of `posframe-arghandler'.  Return VALUE."
  value)

;;;###autoload
(cl-defun posframe-show (buffer-or-name
                         &key
                         string
                         position
                         poshandler
                         poshandler-extra-info
                         width
                         height
                         min-width
                         min-height
                         x-pixel-offset
                         y-pixel-offset
                         left-fringe
                         right-fringe
                         border-width
                         border-color
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
                         lines-truncate
                         override-parameters
                         timeout
                         refresh
                         accept-focus
                         hidehandler
                         refposhandler
                         &allow-other-keys)
  "Pop up a posframe and show STRING at POSITION.

(1) POSITION

POSITION can be:
1. An integer, meaning point position.
2. A cons of two integers, meaning absolute X and Y coordinates.
3. Other type, in which case the corresponding POSHANDLER should be
   provided.

(2) POSHANDLER

POSHANDLER is a function of one argument returning an actual
position.  Its argument is a plist of the following form:

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
   :minibuffer-height xxx
   :mode-line-height  xxx
   :header-line-height xxx
   :tab-line-height xxx
   :x-pixel-offset xxx
   :y-pixel-offset xxx)

By default, poshandler is auto-selected based on the type of POSITION,
but the selection can be overridden using the POSHANDLER argument.
The builtin poshandler functions are listed below:

1.  `posframe-poshandler-frame-center'
2.  `posframe-poshandler-frame-top-center'
3.  `posframe-poshandler-frame-top-left-corner'
4.  `posframe-poshandler-frame-top-right-corner'
5.  `posframe-poshandler-frame-bottom-center'
6.  `posframe-poshandler-frame-bottom-left-corner'
7.  `posframe-poshandler-frame-bottom-right-corner'
8.  `posframe-poshandler-window-center'
9.  `posframe-poshandler-window-top-center'
10. `posframe-poshandler-window-top-left-corner'
11. `posframe-poshandler-window-top-right-corner'
12. `posframe-poshandler-window-bottom-center'
13. `posframe-poshandler-window-bottom-left-corner'
14. `posframe-poshandler-window-bottom-right-corner'
15. `posframe-poshandler-point-top-left-corner'
16. `posframe-poshandler-point-bottom-left-corner'
17. `posframe-poshandler-point-bottom-left-corner-upward'
18. `posframe-poshandler-point-window-center'

by the way, poshandler can be used by other packages easily
(for example: mini-frame) with the help of function
`posframe-poshandler-argbuilder'. like:

   (let* ((info (posframe-poshandler-argbuilder child-frame))
          (posn (posframe-poshandler-window-center info)))
     `((left . ,(car posn))
       (top . ,(cdr posn))))

(3) POSHANDLER-EXTRA-INFO

POSHANDLER-EXTRA-INFO is a plist, which will prepend to the
argument of poshandler function: 'info', it will *OVERRIDE* the
exist key in 'info'.

(4) BUFFER-OR-NAME

This posframe's buffer is BUFFER-OR-NAME, which can be a buffer
or a name of a (possibly nonexistent) buffer.

(5) NO-PROPERTIES

If NO-PROPERTIES is non-nil, The STRING's properties will
be removed before being shown in posframe.

(6) WIDTH, MIN-WIDTH, HEIGHT and MIN-HEIGHT

WIDTH, MIN-WIDTH, HEIGHT and MIN-HEIGHT, specify bounds on the
new total size of posframe.  MIN-HEIGHT and MIN-WIDTH default to
the values of ‘window-min-height’ and ‘window-min-width’
respectively.  These arguments are specified in the canonical
character width and height of posframe.

(7) LEFT-FRINGE and RIGHT-FRINGE

If LEFT-FRINGE or RIGHT-FRINGE is a number, left fringe or
right fringe with be shown with the specified width.

(8) BORDER-WIDTH, BORDER-COLOR, INTERNAL-BORDER-WIDTH and INTERNAL-BORDER-COLOR

By default, posframe shows no borders, but users can specify
borders by setting BORDER-WIDTH to a positive number.  Border
color can be specified by BORDER-COLOR.

INTERNAL-BORDER-WIDTH and INTERNAL-BORDER-COLOR are same as
BORDER-WIDTH and BORDER-COLOR, but do not suggest to use for the
reason:

   Add distinct controls for child frames' borders (Bug#45620)
   http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=ff7b1a133bfa7f2614650f8551824ffaef13fadc

(9) FONT, FOREGROUND-COLOR and BACKGROUND-COLOR

Posframe's font as well as foreground and background colors are
derived from the current frame by default, but can be overridden
using the FONT, FOREGROUND-COLOR and BACKGROUND-COLOR arguments,
respectively.

(10) RESPECT-HEADER-LINE and RESPECT-MODE-LINE

By default, posframe will display no header-line, mode-line and
tab-line.  In case a header-line, mode-line or tab-line is
desired, users can set RESPECT-HEADER-LINE and RESPECT-MODE-LINE
to t.

(11) INITIALIZE

INITIALIZE is a function with no argument.  It will run when
posframe buffer is first selected with `with-current-buffer'
in `posframe-show', and only run once (for performance reasons).

(12) LINES-TRUNCATE

If LINES-TRUNCATE is non-nil, then lines will truncate in the
posframe instead of wrap.

(13) OVERRIDE-PARAMETERS

OVERRIDE-PARAMETERS is very powful, *all* the frame parameters
used by posframe's frame can be overridden by it.

(14) TIMEOUT

TIMEOUT can specify the number of seconds after which the posframe
will auto-hide.

(15) REFRESH

If REFRESH is a number, posframe's frame-size will be re-adjusted
every REFRESH seconds.

(16) ACCEPT-FOCUS

When ACCEPT-FOCUS is non-nil, posframe will accept focus.
be careful, you may face some bugs when set it to non-nil.

(17) HIDEHANDLER

HIDEHANDLER is a function, when it return t, posframe will be
hide when `post-command-hook' is executed, this function has a
plist argument:

  (:posframe-buffer xxx
   :posframe-parent-buffer xxx)

The builtin hidehandler functions are listed below:

1. `posframe-hidehandler-when-buffer-switch'

(18) REFPOSHANDLER

REFPOSHANDLER is a function, a reference position (most is
top-left of current frame) will be returned when call this
function.

when it is nil or it return nil, child-frame feature will be used
and reference position will be deal with in emacs.

The user case I know at the moment is let ivy-posframe work well
in EXWM environment (let posframe show on the other appliction
window).

         DO NOT USE UNLESS NECESSARY!!!

An example parent frame poshandler function is:

1. `posframe-refposhandler-xwininfo'

(19) Others

You can use `posframe-delete-all' to delete all posframes."
  (let* ((position (or (funcall posframe-arghandler buffer-or-name :position position) (point)))
         (poshandler (funcall posframe-arghandler buffer-or-name :poshandler poshandler))
         (poshandler-extra-info (funcall posframe-arghandler buffer-or-name :poshandler-extra-info poshandler-extra-info))
         (width (funcall posframe-arghandler buffer-or-name :width width))
         (height (funcall posframe-arghandler buffer-or-name :height height))
         (min-width (or (funcall posframe-arghandler buffer-or-name :min-width min-width) 1))
         (min-height (or (funcall posframe-arghandler buffer-or-name :min-height min-height) 1))
         (x-pixel-offset (or (funcall posframe-arghandler buffer-or-name :x-pixel-offset x-pixel-offset) 0))
         (y-pixel-offset (or (funcall posframe-arghandler buffer-or-name :y-pixel-offset y-pixel-offset) 0))
         (left-fringe (funcall posframe-arghandler buffer-or-name :left-fringe left-fringe))
         (right-fringe (funcall posframe-arghandler buffer-or-name :right-fringe right-fringe))
         (border-width (funcall posframe-arghandler buffer-or-name :border-width border-width))
         (border-color (funcall posframe-arghandler buffer-or-name :border-color border-color))
         (internal-border-width (funcall posframe-arghandler buffer-or-name :internal-border-width internal-border-width))
         (internal-border-color (funcall posframe-arghandler buffer-or-name :internal-border-color internal-border-color))
         (font (funcall posframe-arghandler buffer-or-name :font font))
         (foreground-color (funcall posframe-arghandler buffer-or-name :foreground-color foreground-color))
         (background-color (funcall posframe-arghandler buffer-or-name :background-color background-color))
         (respect-header-line (funcall posframe-arghandler buffer-or-name :respect-header-line respect-header-line))
         (respect-mode-line (funcall posframe-arghandler buffer-or-name :respect-mode-line respect-mode-line))
         (initialize (funcall posframe-arghandler buffer-or-name :initialize initialize))
         (no-properties (funcall posframe-arghandler buffer-or-name :no-properties no-properties))
         (keep-ratio (funcall posframe-arghandler buffer-or-name :keep-ratio keep-ratio))
         (lines-truncate (funcall posframe-arghandler buffer-or-name :lines-truncate lines-truncate))
         (override-parameters (funcall posframe-arghandler buffer-or-name :override-parameters override-parameters))
         (timeout (funcall posframe-arghandler buffer-or-name :timeout timeout))
         (refresh (funcall posframe-arghandler buffer-or-name :refresh refresh))
         (accept-focus (funcall posframe-arghandler buffer-or-name :accept-focus accept-focus))
         (hidehandler (funcall posframe-arghandler buffer-or-name :hidehandler hidehandler))
         (refposhandler (funcall posframe-arghandler buffer-or-name :refposhandler refposhandler))
         ;;-----------------------------------------------------
         (buffer (get-buffer-create buffer-or-name))
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
         (ref-position
          (when (functionp refposhandler)
            (ignore-errors
              (funcall refposhandler parent-frame))))
         (font-width (default-font-width))
         (font-height (with-current-buffer (window-buffer parent-window)
                        (posframe--get-font-height position)))
         (mode-line-height (window-mode-line-height))
         (minibuffer-height (window-pixel-height (minibuffer-window)))
         (header-line-height (window-header-line-height parent-window))
         (tab-line-height (if (functionp 'window-tab-line-height)
                              (window-tab-line-height)
                            0))
         (frame-resize-pixelwise t)
         posframe)

    (with-current-buffer buffer

      ;; Initialize
      (unless posframe--initialized-p
        (let ((func initialize))
          (when (functionp func)
            (funcall func)
            (setq posframe--initialized-p t))))

      ;; Create posframe
      (setq posframe
            (posframe--create-posframe
             buffer
             :font font
             :parent-frame
             (unless ref-position
               parent-frame)
             :left-fringe left-fringe
             :right-fringe right-fringe
             :border-width border-width
             :border-color border-color
             :internal-border-width internal-border-width
             :internal-border-color internal-border-color
             :foreground-color foreground-color
             :background-color background-color
             :keep-ratio keep-ratio
             :lines-truncate lines-truncate
             :respect-header-line respect-header-line
             :respect-mode-line respect-mode-line
             :override-parameters override-parameters
             :accept-focus accept-focus))

      ;; Move mouse to (0 . 0)
      (posframe--mouse-banish parent-frame posframe)

      ;; Insert string into the posframe buffer
      (posframe--insert-string string no-properties)

      ;; Set posframe's size
      (posframe--set-frame-size
       posframe height min-height width min-width)

      ;; Move posframe
      (posframe--set-frame-position
       posframe
       (posframe-run-poshandler
        ;; All poshandlers will get info from this plist.
        `(,@poshandler-extra-info
          ,@(list :position position
                  :position-info position-info
                  :poshandler poshandler
                  :font-height font-height
                  :font-width font-width
                  :posframe posframe
                  :posframe-width (frame-pixel-width posframe)
                  :posframe-height (frame-pixel-height posframe)
                  :posframe-buffer buffer
                  :parent-frame parent-frame
                  :parent-frame-width parent-frame-width
                  :parent-frame-height parent-frame-height
                  :ref-position ref-position
                  :parent-window parent-window
                  :parent-window-top parent-window-top
                  :parent-window-left parent-window-left
                  :parent-window-width parent-window-width
                  :parent-window-height parent-window-height
                  :mode-line-height mode-line-height
                  :minibuffer-height minibuffer-height
                  :header-line-height header-line-height
                  :tab-line-height tab-line-height
                  :x-pixel-offset x-pixel-offset
                  :y-pixel-offset y-pixel-offset)))
       parent-frame-width parent-frame-height)

      ;; Delay hide posframe when timeout is a number.
      (posframe--run-timeout-timer posframe timeout)

      ;; Re-adjust posframe's size when buffer's content has changed.
      (posframe--run-refresh-timer
       posframe refresh height min-height width min-width)

      ;; Make sure not hide buffer's content for scroll down.
      (let ((window (frame-root-window posframe--frame)))
        (when (window-live-p window)
          (set-window-point window 0)))

      ;; Force raise the current posframe.
      (raise-frame posframe--frame)

      ;; Hide posframe when switch buffer
      (let* ((parent-buffer (window-buffer parent-window))
             (parent-buffer-name (buffer-name parent-buffer)))
        (set-frame-parameter posframe--frame 'posframe-hidehandler hidehandler)
        (set-frame-parameter posframe--frame 'posframe-parent-buffer
                             (cons parent-buffer-name parent-buffer)))

      ;; Return posframe
      posframe)))

(defun posframe--get-font-height (position)
  "Get the font's height at POSITION."
  (if (eq position (car posframe--last-font-height-info))
      (cdr posframe--last-font-height-info)
    (let* ((font (when (and (integerp position)
                            (not (= position 1)))
                   (font-at (if (>= position (point-max))
                                (- (point-max) 1)
                              position))))
           (height (when (integerp position)
                     (if (or (= position 1) (not (fontp font)))
                         (default-line-height)
                       (aref (font-info font) 3)))))
      (setq posframe--last-font-height-info
            (cons position height))
      height)))

(defun posframe--redirect-posframe-focus ()
  "Redirect focus from the posframe to the parent frame. This prevents the
posframe from catching keyboard input if the window manager selects it."
  (when (and (eq (selected-frame) posframe--frame)
             ;; Do not redirect focus when posframe can accept focus.
             ;; See posframe-show's accept-focus argument.
             (frame-parameter (selected-frame) 'no-accept-focus))
    (redirect-frame-focus posframe--frame (frame-parent))))

(if (version< emacs-version "27.1")
    (add-hook 'focus-in-hook #'posframe--redirect-posframe-focus)
  (add-function :after after-focus-change-function #'posframe--redirect-posframe-focus))

(defun posframe--mouse-banish (parent-frame &optional posframe)
  "Banish mouse to the (0 . 0) of PARENT-FRAME.
Do not banish mouse when no-accept-focus frame parameter of POSFRAME
is non-nil.

FIXME: This is a hacky fix for the mouse focus problem, which like:
https://github.com/tumashu/posframe/issues/4#issuecomment-357514918"
  (when (and posframe-mouse-banish
             ;; Do not banish mouse when posframe can accept focus.
             ;; See posframe-show's accept-focus argument.
             (frame-parameter posframe 'no-accept-focus)
             (not (equal (cdr (mouse-position)) '(0 . 0))))
    (set-mouse-position parent-frame
                        (car posframe--mouse-banish-coordinate)
                        (cdr posframe--mouse-banish-coordinate))))

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

(defun posframe--fit-frame-to-buffer (posframe height min-height width min-width)
  ;; This only has effect if the user set the latter var to `hide'.
  (let ((x-gtk-resize-child-frames posframe-gtk-resize-child-frames))
    ;; More info: Don't skip empty lines when fitting mini frame to buffer (Bug#44080)
    ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=e0de9f3295b4c46cb7198ec0b9634809d7b7a36d
    (if (functionp 'fit-frame-to-buffer-1)
        (fit-frame-to-buffer-1
         posframe height min-height width min-width nil nil nil)
      (fit-frame-to-buffer
       posframe height min-height width min-width))))

(defun posframe--set-frame-size (posframe height min-height width min-width)
  "Set POSFRAME's size.
It will set the size by the POSFRAME's HEIGHT, MIN-HEIGHT
WIDTH and MIN-WIDTH."
  (posframe--fit-frame-to-buffer
   posframe height min-height width min-width)
  (setq-local posframe--last-posframe-size
              (list height min-height width min-width)))

(defun posframe--set-frame-position (posframe position
                                              parent-frame-width
                                              parent-frame-height)
  "Move POSFRAME to POSITION.
This need PARENT-FRAME-WIDTH and PARENT-FRAME-HEIGHT"
  (unless (and (equal position posframe--last-posframe-pixel-position)
               ;; When working frame's size change, re-posit
               ;; the posframe.
               (equal posframe--last-parent-frame-size
                      (cons parent-frame-width parent-frame-height))
               (equal posframe--last-posframe-displayed-size
                      (cons (frame-pixel-width posframe)
                            (frame-pixel-height posframe))))
    (set-frame-position posframe (car position) (cdr position))
    (setq-local posframe--last-posframe-pixel-position position)
    (setq-local posframe--last-parent-frame-size
                (cons parent-frame-width parent-frame-height))
    (setq-local posframe--last-posframe-displayed-size
                (cons (frame-pixel-width posframe)
                      (frame-pixel-height posframe))))
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
  "`make-frame-invisible' replacement to hide FRAME safely."
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
                       (let ((frame-resize-pixelwise t))
                         (when (and frame (frame-live-p frame))
                           (posframe--fit-frame-to-buffer
                            frame height min-height width min-width))))
                   posframe height min-height width min-width)))))

(defun posframe-refresh (buffer-or-name)
  "Refresh posframe pertaining to BUFFER-OR-NAME.

For example:

   (defvar buf \" *test*\")
   (posframe-show buf)

   (with-current-buffer buf
     (erase-buffer)
     (insert \"ffffffffffffff\")
     (posframe-refresh buf))

User can use posframe-show's :refresh argument,
to do similar job:

   (defvar buf \" *test*\")
   (posframe-show buf :refresh 0.25)

   (with-current-buffer buf
     (erase-buffer)
     (insert \"ffffffffffffff\"))"
  (dolist (frame (frame-list))
    (let ((buffer-info (frame-parameter frame 'posframe-buffer))
          (frame-resize-pixelwise t))
      (when (or (equal buffer-or-name (car buffer-info))
                (equal buffer-or-name (cdr buffer-info)))
        (with-current-buffer buffer-or-name
          (apply #'posframe--fit-frame-to-buffer
                 frame posframe--last-posframe-size))))))

(defun posframe-hide (buffer-or-name)
  "Hide posframe pertaining to BUFFER-OR-NAME.
BUFFER-OR-NAME can be a buffer or a buffer name."
  ;; Make sure buffer-list-update-hook is nil when posframe-hide is
  ;; called, otherwise:
  ;;   (add-hook 'buffer-list-update-hook  #'posframe-hide)
  ;; will lead to infinite recursion.
  (let ((buffer-list-update-hook nil))
    (dolist (frame (frame-list))
      (let ((buffer-info (frame-parameter frame 'posframe-buffer)))
        (when (or (equal buffer-or-name (car buffer-info))
                  (equal buffer-or-name (cdr buffer-info)))
          (posframe--make-frame-invisible frame))))))

(defun posframe-run-hidehandler ()
  "Run posframe hidehandler. this function is used in `post-command-hook'."
  (ignore-errors
    (dolist (frame (frame-list))
      (let ((hidehandler (frame-parameter frame 'posframe-hidehandler))
            (buffer (frame-parameter frame 'posframe-buffer))
            (parent-buffer (frame-parameter frame 'posframe-parent-buffer)))
        (when (and hidehandler
                   (funcall hidehandler
                            (list
                             :posframe-buffer buffer
                             :posframe-parent-buffer parent-buffer)))
          (posframe--make-frame-invisible frame))))))

(add-hook 'post-command-hook #'posframe-run-hidehandler)

(defun posframe-hidehandler-when-buffer-switch (info)
  "Posframe hidehandler function.

This function let posframe hide when user switch buffer.
Note: This function is called in `post-command-hook'."
  (let ((parent-buffer (cdr (plist-get info :posframe-parent-buffer))))
    (and (buffer-live-p parent-buffer)
         (not (equal parent-buffer (current-buffer))))))

(defun posframe-delete (buffer-or-name)
  "Delete posframe pertaining to BUFFER-OR-NAME and kill the buffer.
BUFFER-OR-NAME can be a buffer or a buffer name."
  (posframe-delete-frame buffer-or-name)
  (posframe--kill-buffer buffer-or-name))

(defun posframe-delete-frame (buffer-or-name)
  "Delete posframe pertaining to BUFFER-OR-NAME.
BUFFER-OR-NAME can be a buffer or a buffer name."
  (dolist (frame (frame-list))
    (let ((buffer-info (frame-parameter frame 'posframe-buffer))
          (buffer (get-buffer buffer-or-name)))
      (when (or (equal buffer-or-name (car buffer-info))
                (equal buffer-or-name (cdr buffer-info)))
        (when buffer
          (with-current-buffer buffer
            (dolist (timer '(posframe--refresh-timer
                             posframe--timeout-timer))
              (when (timerp timer)
                (cancel-timer timer)))))
        (delete-frame frame)))))

(defun posframe--kill-buffer (buffer-or-name)
  "Kill posframe's buffer: BUFFER-OR-NAME.
BUFFER-OR-NAME can be a buffer or a buffer name."
  (when (buffer-live-p (get-buffer buffer-or-name))
    (kill-buffer buffer-or-name)))

(defun posframe-funcall (buffer-or-name function &rest arguments)
  "Select posframe of BUFFER-OR-NAME and call FUNCTION with ARGUMENTS.
BUFFER-OR-NAME can be a buffer or a buffer name."
  (when (functionp function)
    (when (get-buffer buffer-or-name)
      (with-current-buffer buffer-or-name
        (when (framep posframe--frame)
          (with-selected-frame posframe--frame
            (apply function arguments)))))))

;;;###autoload
(defun posframe-hide-all ()
  "Hide all posframe frames."
  (interactive)
  (dolist (frame (frame-list))
    (when (frame-parameter frame 'posframe-buffer)
      (posframe--make-frame-invisible frame))))

;;;###autoload
(defun posframe-delete-all ()
  "Delete all posframe frames and buffers."
  (interactive)
  (dolist (frame (frame-list))
    (when (frame-parameter frame 'posframe-buffer)
      (delete-frame frame)))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when posframe--frame
        (posframe--kill-buffer buffer)))))

(defun posframe-auto-delete ()
  "Auto delete posframe when its buffer is killed.

This function is used by `kill-buffer-hook'."
  (posframe-delete-frame (current-buffer)))

;; Posframe's position handler
(defun posframe-run-poshandler (info)
  "Run posframe's position handler.

the structure of INFO can be found in docstring
of `posframe-show'."
  (if (equal info posframe--last-poshandler-info)
      posframe--last-posframe-pixel-position
    (setq posframe--last-poshandler-info info)
    (let* ((ref-position (plist-get info :ref-position))
           (position (funcall
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
           (x (car position))
           (y (cdr position)))
      (if (not ref-position)
          position
        (let* ((parent-frame-width (plist-get info :parent-frame-width))
               (parent-frame-height (plist-get info :parent-frame-height))
               (posframe-width (plist-get info :posframe-width))
               (posframe--height (plist-get info :posframe-height))
               (ref-x (or (car ref-position) 0))
               (ref-y (or (cdr ref-position) 0)))
          (when (< x 0)
            (setq x (- (+ x parent-frame-width) posframe-width)))
          (when (< y 0)
            (setq y (- (+ y parent-frame-height) posframe--height)))
          (cons (+ ref-x x)
                (+ ref-y y)))))))

(cl-defun posframe-poshandler-argbuilder (&optional
                                          child-frame
                                          &key
                                          position
                                          poshandler
                                          x-pixel-offset
                                          y-pixel-offset)
  "Return a info list of CHILD-FRAME, which can be used as poshandler's info argument.

if CHILD-FRAME is nil, parent frame will use selected frame.  The
documents of POSITION, POSHANDLER, X-PIXEL-OFFSET and
Y-PIXEL-OFFSET can be found in dostring of `posframe-show'.

NOTE: this function is not used by posframe itself, it just let
poshandler easily used for other purposes."
  (let* ((position (or position (point)))
         (frame-width (or (and child-frame (frame-pixel-width child-frame)) 0))
         (frame-height (or (and child-frame (frame-pixel-height child-frame)) 0))
         (frame-buffer (and child-frame (window-buffer (frame-root-window child-frame))))
         (parent-frame (if child-frame
                           (frame-parent child-frame)
                         (selected-frame)))
         (parent-frame-width (frame-pixel-width parent-frame))
         (parent-frame-height (frame-pixel-height parent-frame))
         (parent-window
          (if child-frame
              (frame-root-window parent-frame)
            (selected-window)))
         (parent-window-top (window-pixel-top parent-window))
         (parent-window-left (window-pixel-left parent-window))
         (parent-window-width (window-pixel-width parent-window))
         (parent-window-height (window-pixel-height parent-window))
         (position-info
          (if (integerp position)
              (posn-at-point position parent-window)
            position))
         (font-width (default-font-width))
         (font-height (with-current-buffer (window-buffer parent-window)
                        (posframe--get-font-height position)))
         (mode-line-height (window-mode-line-height parent-window))
         (minibuffer-height (window-pixel-height (minibuffer-window)))
         (header-line-height (window-header-line-height parent-window))
         (tab-line-height (if (functionp 'window-tab-line-height)
                              (window-tab-line-height parent-window)
                            0)))
    (list :position position
          :position-info position-info
          :poshandler poshandler
          :font-height font-height
          :font-width font-width
          :posframe child-frame
          :posframe-width frame-width
          :posframe-height frame-height
          :posframe-buffer frame-buffer
          :parent-frame parent-frame
          :parent-frame-width parent-frame-width
          :parent-frame-height parent-frame-height
          :parent-window parent-window
          :parent-window-top parent-window-top
          :parent-window-left parent-window-left
          :parent-window-width parent-window-width
          :parent-window-height parent-window-height
          :mode-line-height mode-line-height
          :minibuffer-height minibuffer-height
          :header-line-height header-line-height
          :tab-line-height tab-line-height
          :x-pixel-offset (or x-pixel-offset 0)
          :y-pixel-offset (or y-pixel-offset 0))))

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

(defun posframe-poshandler-point-bottom-left-corner (info &optional font-height upward centering)
  "Posframe's position hanlder.

Get bottom-left-corner pixel position of a point,
the structure of INFO can be found in docstring
of `posframe-show'.

Optional argument FONT-HEIGHT, UPWARD, CENTERING ."
  (let* ((x-pixel-offset (plist-get info :x-pixel-offset))
         (y-pixel-offset (plist-get info :y-pixel-offset))
         (posframe-width (plist-get info :posframe-width))
         (posframe-height (plist-get info :posframe-height))
         (window (plist-get info :parent-window))
         (window-left (plist-get info :parent-window-left))
         (window-width (plist-get info :parent-window-width))
         (xmax (plist-get info :parent-frame-width))
         (ymax (plist-get info :parent-frame-height))
         (position-info (plist-get info :position-info))
         (header-line-height (plist-get info :header-line-height))
         (tab-line-height (plist-get info :tab-line-height))
         (x (+ (car (window-inside-pixel-edges window))
               (- (or (car (posn-x-y position-info)) 0)
                  (or (car (posn-object-x-y position-info)) 0))
               x-pixel-offset))
         (y-top (+ (cadr (window-pixel-edges window))
                   tab-line-height
                   header-line-height
                   (- (or (cdr (posn-x-y position-info)) 0)
                      ;; Fix the conflict with flycheck
                      ;; http://lists.gnu.org/archive/html/emacs-devel/2018-01/msg00537.html
                      (or (cdr (posn-object-x-y position-info)) 0))
                   y-pixel-offset))
         (font-height (or font-height (plist-get info :font-height)))
         (y-bottom (+ y-top font-height)))
    (cons (if centering
              (+ window-left (/ (- window-width posframe-width) 2))
            (max 0 (min x (- xmax (or posframe-width 0)))))
          (max 0 (if (if upward
                         (> (- y-bottom (or posframe-height 0)) 0)
                       (> (+ y-bottom (or posframe-height 0)) ymax))
                     (- y-top (or posframe-height 0))
                   y-bottom)))))

(defun posframe-poshandler-point-window-center (info)
  "Posframe's position hanlder.

Get a position of a point, by which a window-centered posframe
can be put below it, the structure of INFO can be found in
docstring of `posframe-show'. "

  (posframe-poshandler-point-bottom-left-corner info nil nil t))

(defun posframe-poshandler-point-bottom-left-corner-upward (info)
  "Posframe's position hanlder.

Get a position of a point, by which posframe can put above it,
the structure of INFO can be found in docstring
of `posframe-show'.

Optional argument FONT-HEIGHT ."
  (posframe-poshandler-point-bottom-left-corner info nil t))

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

(defun posframe-poshandler-frame-bottom-center (info)
  "Posframe's position handler.

Get a position which let posframe stay onto its parent-frame's
bottom center.  The structure of INFO can be found in docstring of
`posframe-show'."
  (cons (/ (- (plist-get info :parent-frame-width)
              (plist-get info :posframe-width))
           2)
        (- (plist-get info :parent-frame-height)
           (plist-get info :posframe-height)
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

(defun posframe-poshandler-window-top-center (info)
  "Posframe's position handler.

Get a position which let posframe stay onto current window's
top center.  The structure of INFO can be found in docstring of
`posframe-show'."
  (let* ((window-left (plist-get info :parent-window-left))
         (window-top (plist-get info :parent-window-top))
         (window-width (plist-get info :parent-window-width))
         (posframe-width (plist-get info :posframe-width)))
    (cons (+ window-left (/ (- window-width posframe-width) 2))
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

(defun posframe-poshandler-window-bottom-center (info)
  "Posframe's position handler.

Get a position which let posframe stay onto current window's
bottom center.  The structure of INFO can be found in docstring of
`posframe-show'."
  (let* ((window-left (plist-get info :parent-window-left))
         (window-top (plist-get info :parent-window-top))
         (window-width (plist-get info :parent-window-width))
         (window-height (plist-get info :parent-window-height))
         (posframe-width (plist-get info :posframe-width))
         (posframe-height (plist-get info :posframe-height))
         (mode-line-height (plist-get info :mode-line-height)))
    (cons (+ window-left (/ (- window-width posframe-width) 2))
          (+ window-top window-height
             (- 0 mode-line-height posframe-height)))))

(defun posframe-refposhandler-xwininfo (&optional frame)
  "Parent frame poshander function.
Get the position of parent frame (current frame) with the help of
xwininfo."
  (when (executable-find "xwininfo")
    (with-temp-buffer
      (let ((case-fold-search nil)
            (args (format "xwininfo -display %s -id %s"
		          (frame-parameter frame 'display)
		          (frame-parameter frame 'window-id))))
        (call-process shell-file-name nil t nil shell-command-switch args)
        (goto-char (point-min))
        (search-forward "Absolute upper-left")
        (let ((x (string-to-number
                  (buffer-substring-no-properties
	           (search-forward "X: ")
	           (line-end-position))))
              (y (string-to-number
                  (buffer-substring-no-properties
	           (search-forward "Y: ")
	           (line-end-position)))))
          (cons x y))))))


(provide 'posframe)

;;; posframe.el ends here
