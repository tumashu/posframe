Note: this file is auto converted from posframe.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [Posframe README](#org39288b3)
    1.  [What is posframe?](#orgd6e2b14)
    2.  [Installation](#orgc1a54a8)
    3.  [Usage](#org489af32)
        1.  [Create a posframe](#orgaea7ff4)
        2.  [Hide a posframe](#org418597e)
        3.  [Hide all posframes](#org89f2772)
        4.  [Delete a posframe](#org6455737)
        5.  [Delete all posframes](#org085d523)
        6.  [Customizing mouse pointer control](#org3efdec6)
        7.  [Set fallback arguments of posframe-show](#org873b94e)


<a id="org39288b3"></a>

# Posframe README


<a id="orgd6e2b14"></a>

## What is posframe?

Posframe can pop up a frame at point, this **posframe** is a
child-frame connected to its root window's buffer.

The main advantages are:

1.  It is fast enough for daily usage :-)
2.  It works well with CJK languages.

NOTE: For MacOS users, posframe needs Emacs version >= 26.0.91

![img](./snapshots/posframe-1.png)


<a id="orgc1a54a8"></a>

## Installation

    (require 'posframe)


<a id="org489af32"></a>

## Usage


<a id="orgaea7ff4"></a>

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


<a id="org418597e"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="org89f2772"></a>

### Hide all posframes

    M-x posframe-hide-all


<a id="org6455737"></a>

### Delete a posframe

1.  Delete posframe and its buffer
    
        (posframe-delete " *my-posframe-buffer*")
2.  Only delete the frame
    
        (posframe-delete-frame " *my-posframe-buffer*")


<a id="org085d523"></a>

### Delete all posframes

    M-x posframe-delete-all

Note: this command will delete all posframe buffers.
You probably shouldn't use it if you are sharing a buffer
between posframe and other packages.


<a id="org3efdec6"></a>

### Customizing mouse pointer control

By default, posframe moves the pointer to point (0,0) in
the frame, as a way to address an issue with mouse focus.
To disable this feature, add this to your init.el:

    (setq posframe-mouse-banish nil)


<a id="org873b94e"></a>

### Set fallback arguments of posframe-show

Users can set fallback values of posframe-show's arguments with the
help of \`posframe-arghandler'.  The example below sets fallback
border-width to 10 and fallback background color to green.

    (setq posframe-arghandler #'my-posframe-arghandler)
    (defun my-posframe-arghandler (buffer-or-name arg-name value)
      (let ((info '(:internal-border-width 10 :background-color "green")))
        (or (plist-get info arg-name) value)))

