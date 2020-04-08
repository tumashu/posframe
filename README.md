Note: this file is auto converted from posframe.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [Posframe README](#org9250ddd)
    1.  [What is posframe?](#orga3a4de5)
    2.  [Installation](#org54522b5)
    3.  [Usage](#org5a4c1a3)
        1.  [Create a posframe](#org15d439c)
        2.  [Hide a posframe](#orga0789fc)
        3.  [Hide all posframes](#orgfea11fc)
        4.  [Delete a posframe](#orgb945e53)
        5.  [Delete all posframes](#orgf34da80)
        6.  [Customizing mouse pointer control](#org3142fd3)
        7.  [Set fallback arguments of posframe-show](#org8d2c22f)
        8.  [Some packages which use posframe](#org6b1b801)


<a id="org9250ddd"></a>

# Posframe README


<a id="orga3a4de5"></a>

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


<a id="org54522b5"></a>

## Installation

    (require 'posframe)


<a id="org5a4c1a3"></a>

## Usage


<a id="org15d439c"></a>

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


<a id="orga0789fc"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="orgfea11fc"></a>

### Hide all posframes

    M-x posframe-hide-all


<a id="orgb945e53"></a>

### Delete a posframe

1.  Delete posframe and its buffer
    
        (posframe-delete " *my-posframe-buffer*")
2.  Only delete the frame
    
        (posframe-delete-frame " *my-posframe-buffer*")


<a id="orgf34da80"></a>

### Delete all posframes

    M-x posframe-delete-all

Note: this command will delete all posframe buffers.
You probably shouldn't use it if you are sharing a buffer
between posframe and other packages.


<a id="org3142fd3"></a>

### Customizing mouse pointer control

By default, posframe moves the pointer to point (0,0) in
the frame, as a way to address an issue with mouse focus.
To disable this feature, add this to your init.el:

    (setq posframe-mouse-banish nil)


<a id="org8d2c22f"></a>

### Set fallback arguments of posframe-show

Users can set fallback values of posframe-show's arguments with the
help of \`posframe-arghandler'.  The example below sets fallback
border-width to 10 and fallback background color to green.

    (setq posframe-arghandler #'my-posframe-arghandler)
    (defun my-posframe-arghandler (buffer-or-name arg-name value)
      (let ((info '(:internal-border-width 10 :background-color "green")))
        (or (plist-get info arg-name) value)))


<a id="org6b1b801"></a>

### Some packages which use posframe

1.  [which-key-posframe](https://github.com/yanghaoxie/which-key-posframe)
2.  [ddskk-posframe](https://github.com/conao3/ddskk-posframe.el)
3.  [pyim](https://github.com/tumashu/pyim)
4.  [ivy-posframe](https://github.com/tumashu/ivy-posframe)
5.  [company-posframe](https://github.com/tumashu/company-posframe)
6.  &#x2026;

