Note: this file is auto converted from posframe.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# Table of Contents

1.  [Posframe README](#orgff02ab2)
    1.  [What is posframe](#org9798090)
    2.  [Installation](#org3adce4c)
    3.  [Usage](#org4ef3be4)
        1.  [Create a posframe](#org7623f9d)
        2.  [Hide a posframe](#orga8e2a61)
        3.  [Hide all posframes](#org7414b66)
        4.  [Delete a posframe](#org448902b)
        5.  [Delete all posframes](#org3c8f39b)
        6.  [Customizing pointer control](#orgf81a00a)


<a id="orgff02ab2"></a>

# Posframe README


<a id="org9798090"></a>

## What is posframe

Posframe can pop a posframe at point, this **posframe** is a
child-frame with its root window's buffer.

The main advantages are:

1.  It is fast enough for daily usage :-)
2.  It works well with CJK language.

NOTE: For MacOS users, posframe need Emacs (version >= 26.0.91)

![img](./snapshots/posframe-1.png)


<a id="org3adce4c"></a>

## Installation

    (require 'posframe)


<a id="org4ef3be4"></a>

## Usage


<a id="org7623f9d"></a>

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


<a id="orga8e2a61"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="org7414b66"></a>

### Hide all posframes

    M-x posframe-hide-all


<a id="org448902b"></a>

### Delete a posframe

1.  Delete posframe and its buffer
    
        (posframe-delete " *my-posframe-buffer*")
2.  Only delete posframe's frame
    
        (posframe-delete-frame " *my-posframe-buffer*")


<a id="org3c8f39b"></a>

### Delete all posframes

    M-x posframe-delete-all

Note: this command will delete all posframe buffers,
suggest not run this command if you are sharing a buffer
between posframe and other packages.


<a id="orgf81a00a"></a>

### Customizing pointer control

By default, posframe moves the pointer to point (0,0) in
the frame, as a way to address an issue with mouse focus.
To disable this feature, add this to your init.el:

    (setq posframe-mouse-banish nil)

