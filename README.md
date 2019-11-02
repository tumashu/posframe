Note: this file is auto converted from posframe.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [Posframe README](#orgf92394f)
    1.  [What is posframe?](#orge2bbb9c)
    2.  [Installation](#org0e452aa)
    3.  [Usage](#org5efb6f0)
        1.  [Create a posframe](#orgc8de1c8)
        2.  [Hide a posframe](#org7f00a73)
        3.  [Hide all posframes](#orgfec5a74)
        4.  [Delete a posframe](#org9796637)
        5.  [Delete all posframes](#org0b9ea7e)
        6.  [Customizing mouse pointer control](#orgb29298b)
        7.  [Set fallback arguments of posframe-show](#org96afba9)


<a id="orgf92394f"></a>

# Posframe README


<a id="orge2bbb9c"></a>

## What is posframe?

Posframe can pop up a frame at point, this **posframe** is a
child-frame connected to its root window's buffer.

The main advantages are:

1.  It is fast enough for daily usage :-)
2.  It works well with CJK languages.

NOTE:

1.  For MacOS users, posframe needs Emacs version >= 26.0.91
2.  Posframe will be very very slow when emacs is built with &#x2013;with-x-toolkit=athena.

![img](./snapshots/posframe-1.png)


<a id="org0e452aa"></a>

## Installation

    (require 'posframe)


<a id="org5efb6f0"></a>

## Usage


<a id="orgc8de1c8"></a>

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


<a id="org7f00a73"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="orgfec5a74"></a>

### Hide all posframes

    M-x posframe-hide-all


<a id="org9796637"></a>

### Delete a posframe

1.  Delete posframe and its buffer
    
        (posframe-delete " *my-posframe-buffer*")
2.  Only delete the frame
    
        (posframe-delete-frame " *my-posframe-buffer*")


<a id="org0b9ea7e"></a>

### Delete all posframes

    M-x posframe-delete-all

Note: this command will delete all posframe buffers.
You probably shouldn't use it if you are sharing a buffer
between posframe and other packages.


<a id="orgb29298b"></a>

### Customizing mouse pointer control

By default, posframe moves the pointer to point (0,0) in
the frame, as a way to address an issue with mouse focus.
To disable this feature, add this to your init.el:

    (setq posframe-mouse-banish nil)


<a id="org96afba9"></a>

### Set fallback arguments of posframe-show

Users can set fallback values of posframe-show's arguments with the
help of \`posframe-arghandler'.  The example below sets fallback
border-width to 10 and fallback background color to green.

    (setq posframe-arghandler #'my-posframe-arghandler)
    (defun my-posframe-arghandler (buffer-or-name arg-name value)
      (let ((info '(:internal-border-width 10 :background-color "green")))
        (or (plist-get info arg-name) value)))

