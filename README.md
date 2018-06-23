Note: this file is auto converted from posframe.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [Posframe README](#org83cbaa0)
    1.  [What is posframe](#org3ce725d)
    2.  [Installation](#org545827a)
    3.  [Usage](#orgdac766b)
        1.  [Create a posframe](#orgea6aecd)
        2.  [Hide a posframe](#orga5310b9)
        3.  [Hide all posframes](#orgbf09855)
        4.  [Delete a posframe](#org68db811)
        5.  [Delete all posframes](#org12f2467)


<a id="org83cbaa0"></a>

# Posframe README


<a id="org3ce725d"></a>

## What is posframe

Posframe can pop a posframe at point, this **posframe** is a
child-frame with its root window's buffer.

The main advantages are:

1.  It is fast enough for daily usage :-)
2.  It works well with CJK language.

NOTE: For MacOS users, posframe need Emacs (version >= 26.0.91)

![img](./snapshots/posframe-1.png)


<a id="org545827a"></a>

## Installation

    (require 'posframe)


<a id="orgdac766b"></a>

## Usage


<a id="orgea6aecd"></a>

### Create a posframe

1.  Simple way

        NOTE: buffers prefixed with space will be not showed in buffer-list.
        (posframe-show " *my-posframe-buffer*"
                       :string "This is a test"
                       :position (point))

2.  Advanced way

        (defvar my-posframe-buffer " *my-posframe-buffer*")

        (with-current-buffer (get-buffer-create my-posframe-buffer)
          (erase-buffer)
          (insert "Hello world"))

        (posframe-show my-posframe-buffer
                       :position (point))

3.  Arguments

        C-h f posframe-show


<a id="orga5310b9"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="orgbf09855"></a>

### Hide all posframes

    M-x posframe-hide-all


<a id="org68db811"></a>

### Delete a posframe

1.  Delete posframe and its buffer

        (posframe-delete " *my-posframe-buffer*")
2.  Only delete posframe's frame

        (posframe-delete-frame " *my-posframe-buffer*")


<a id="org12f2467"></a>

### Delete all posframes

    M-x posframe-delete-all

Note: this command will delete all posframe buffers,
suggest not run this command if you are sharing a buffer
between posframe and other packages.
