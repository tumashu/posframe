Note: this file is auto converted from posframe.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [Posframe README](#org39f1199)
    1.  [What is posframe](#org75ffb09)
    2.  [Installation](#org24e4d5a)
    3.  [Usage](#org13172eb)
        1.  [Create a posframe](#org71b175a)
        2.  [Hide a posframe](#orgd63c00f)
        3.  [Hide all posframes](#orge4e5227)
        4.  [Delete a posframe](#orga7363c0)
        5.  [Delete all posframes](#org58e322a)
        6.  [Customizing pointer control](#orge28a877)
        7.  [Set fallback argument of posframe-show](#org4610750)


<a id="org39f1199"></a>

# Posframe README


<a id="org75ffb09"></a>

## What is posframe

Posframe can pop a posframe at point, this **posframe** is a
child-frame with its root window's buffer.

The main advantages are:

1.  It is fast enough for daily usage :-)
2.  It works well with CJK language.

NOTE: For MacOS users, posframe need Emacs (version >= 26.0.91)

![img](./snapshots/posframe-1.png)


<a id="org24e4d5a"></a>

## Installation

    (require 'posframe)


<a id="org13172eb"></a>

## Usage


<a id="org71b175a"></a>

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


<a id="orgd63c00f"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="orge4e5227"></a>

### Hide all posframes

    M-x posframe-hide-all


<a id="orga7363c0"></a>

### Delete a posframe

1.  Delete posframe and its buffer
    
        (posframe-delete " *my-posframe-buffer*")
2.  Only delete posframe's frame
    
        (posframe-delete-frame " *my-posframe-buffer*")


<a id="org58e322a"></a>

### Delete all posframes

    M-x posframe-delete-all

Note: this command will delete all posframe buffers,
suggest not run this command if you are sharing a buffer
between posframe and other packages.


<a id="orge28a877"></a>

### Customizing pointer control

By default, posframe moves the pointer to point (0,0) in
the frame, as a way to address an issue with mouse focus.
To disable this feature, add this to your init.el:

    (setq posframe-mouse-banish nil)


<a id="org4610750"></a>

### Set fallback argument of posframe-show

user can set fallback values of posframe-show's arguments with the
help of \`posframe-arghandler'. the below example set fallback
border-width to 10 and fallback background color to green.

    (setq posframe-arghandler #'my-posframe-arghandler)
    (defun my-posframe-arghandler (posframe-buffer arg-name value)
      (let ((info '(:internal-border-width 10 :background-color "green")))
        (or (plist-get info arg-name) value)))

