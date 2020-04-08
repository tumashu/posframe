Note: this file is auto converted from posframe.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [Posframe README](#orgb61be23)
    1.  [What is posframe?](#org5080ea1)
    2.  [Installation](#org13757b4)
    3.  [Usage](#orgb6f31c8)
        1.  [Create a posframe](#org0fed3aa)
        2.  [Hide a posframe](#org51f216e)
        3.  [Hide all posframes](#orgd19653c)
        4.  [Delete a posframe](#org665e97b)
        5.  [Delete all posframes](#org8302046)
        6.  [Customizing mouse pointer control](#orgb484be3)
        7.  [Set fallback arguments of posframe-show](#orgfcc0fc2)
        8.  [Some packages which use posframe](#org56e8b88)


<a id="orgb61be23"></a>

# Posframe README


<a id="org5080ea1"></a>

## What is posframe?

Posframe can pop up a frame at point, this **posframe** is a
child-frame connected to its root window's buffer.

The main advantages are:

1.  It is fast enough for daily usage :-)
2.  It works well with CJK languages.

NOTE:

1.  For MacOS users, posframe needs Emacs version >= 26.0.91
2.  GNOME users with GTK3 builds should set \`x-gtk-resize-child-frames'
    to 'resize-mode or 'hide, then run command \`posframe-hack'.
    
    1.  'resize-mode has better behavior but not future-compatible.
    2.  'hide is more future-proof but will blink the child frame every
        time it's resized.
    
    More details:
    
    1.  [Fix some problems with moving and resizing child frames](https://git.savannah.gnu.org/cgit/emacs.git/commit/?h=emacs-27&id=c49d379f17bcb0ce82604def2eaa04bda00bd5ec)
    2.  [Emacs's set-frame-size can not work well with gnome-shell?](https://lists.gnu.org/archive/html/emacs-devel/2020-01/msg00343.html)

![img](./snapshots/posframe-1.png)


<a id="org13757b4"></a>

## Installation

    (require 'posframe)


<a id="orgb6f31c8"></a>

## Usage


<a id="org0fed3aa"></a>

### Create a posframe

1.  Simple way

        (when (posframe-workable-p)
          (posframe-show " *my-posframe-buffer*"
                         :string "This is a test"
                         :position (point)))

2.  Advanced way

        (defvar my-posframe-buffer " *my-posframe-buffer*")
        
        (with-current-buffer (get-buffer-create my-posframe-buffer)
          (erase-buffer)
          (insert "Hello world"))
        
        (when (posframe-workable-p)
          (posframe-show my-posframe-buffer
                         :position (point)))

3.  Arguments

        C-h f posframe-show


<a id="org51f216e"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="orgd19653c"></a>

### Hide all posframes

    M-x posframe-hide-all


<a id="org665e97b"></a>

### Delete a posframe

1.  Delete posframe and its buffer
    
        (posframe-delete " *my-posframe-buffer*")
2.  Only delete the frame
    
        (posframe-delete-frame " *my-posframe-buffer*")


<a id="org8302046"></a>

### Delete all posframes

    M-x posframe-delete-all

Note: this command will delete all posframe buffers.
You probably shouldn't use it if you are sharing a buffer
between posframe and other packages.


<a id="orgb484be3"></a>

### Customizing mouse pointer control

By default, posframe moves the pointer to point (0,0) in
the frame, as a way to address an issue with mouse focus.
To disable this feature, add this to your init.el:

    (setq posframe-mouse-banish nil)


<a id="orgfcc0fc2"></a>

### Set fallback arguments of posframe-show

Users can set fallback values of posframe-show's arguments with the
help of \`posframe-arghandler'.  The example below sets fallback
border-width to 10 and fallback background color to green.

    (setq posframe-arghandler #'my-posframe-arghandler)
    (defun my-posframe-arghandler (buffer-or-name arg-name value)
      (let ((info '(:internal-border-width 10 :background-color "green")))
        (or (plist-get info arg-name) value)))


<a id="org56e8b88"></a>

### Some packages which use posframe

1.  [which-key-posframe](https://github.com/yanghaoxie/which-key-posframe)
2.  [ddskk-posframe](https://github.com/conao3/ddskk-posframe.el)
3.  [pyim](https://github.com/tumashu/pyim)
4.  [ivy-posframe](https://github.com/tumashu/ivy-posframe)
5.  [company-posframe](https://github.com/tumashu/company-posframe)
6.  &#x2026;

