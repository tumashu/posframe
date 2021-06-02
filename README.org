# Created 2021-06-01 Tue 10:41
#+TITLE: Pop a posframe (just a frame) at point
#+AUTHOR: Feng Shu

#+html: <a href="http://elpa.gnu.org/packages/posframe.html"><img alt="GNU ELPA" src="https://elpa.gnu.org/packages/posframe.svg"/></a>
#+html: <a href="http://elpa.gnu.org/devel/posframe.html"><img alt="GNU-devel ELPA" src="https://elpa.gnu.org/devel/posframe.svg"/></a>
#+html: <a href="https://melpa.org/#/posframe"><img alt="MELPA" src="https://melpa.org/packages/posframe-badge.svg"/></a>

* What is posframe?
Posframe can pop up a frame at point, this *posframe* is a
child-frame connected to its root window's buffer.

The main advantages are:
1. It is fast enough for daily usage :-)
2. It works well with CJK languages.

NOTE:
1. For MacOS users, posframe needs Emacs version >= 26.0.91
2. GNOME users with GTK3 builds need Emacs 27 or later.
   See variable `posframe-gtk-resize-child-frames'
   which auto-detects this configuration.

   More details:
   1. [[https://git.savannah.gnu.org/cgit/emacs.git/commit/?h=emacs-27&id=c49d379f17bcb0ce82604def2eaa04bda00bd5ec][Fix some problems with moving and resizing child frames]]
   2. [[https://lists.gnu.org/archive/html/emacs-devel/2020-01/msg00343.html][Emacs's set-frame-size can not work well with gnome-shell?]]

[[file:./snapshots/posframe-1.png]]

* Installation

#+begin_example
(require 'posframe)
#+end_example

* Usage

** Create a posframe

*** Simple way
#+begin_example
(when (posframe-workable-p)
  (posframe-show " *my-posframe-buffer*"
                 :string "This is a test"
                 :position (point)))
#+end_example

*** Advanced way
#+begin_example
(defvar my-posframe-buffer " *my-posframe-buffer*")

(with-current-buffer (get-buffer-create my-posframe-buffer)
  (erase-buffer)
  (insert "Hello world"))

(when (posframe-workable-p)
  (posframe-show my-posframe-buffer
                 :position (point)))
#+end_example

*** Arguments

#+begin_example
C-h f posframe-show
#+end_example

** Hide a posframe
#+begin_example
(posframe-hide " *my-posframe-buffer*")
#+end_example

** Hide all posframes
#+begin_example
M-x posframe-hide-all
#+end_example

** Delete a posframe
1. Delete posframe and its buffer
   #+begin_example
   (posframe-delete " *my-posframe-buffer*")
   #+end_example
2. Only delete the frame
   #+begin_example
   (posframe-delete-frame " *my-posframe-buffer*")
   #+end_example
** Delete all posframes
#+begin_example
M-x posframe-delete-all
#+end_example

Note: this command will delete all posframe buffers.
You probably shouldn't use it if you are sharing a buffer
between posframe and other packages.

** Customizing mouse pointer control

By default, posframe moves the pointer to point (0,0) in
the frame, as a way to address an issue with mouse focus.
To disable this feature, add this to your init.el:
#+begin_example
(setq posframe-mouse-banish nil)
#+end_example

** Set fallback arguments of posframe-show

Users can set fallback values of posframe-show's arguments with the
help of `posframe-arghandler'.  The example below sets fallback
border-width to 10 and fallback background color to green.

#+begin_example
(setq posframe-arghandler #'my-posframe-arghandler)
(defun my-posframe-arghandler (buffer-or-name arg-name value)
  (let ((info '(:internal-border-width 10 :background-color "green")))
    (or (plist-get info arg-name) value)))
#+end_example
