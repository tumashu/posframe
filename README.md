Note: this file is auto converted from posframe.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [Posframe README](#org0b9c3bd)
    1.  [What is posframe](#org0728352)
    2.  [Installation](#org515eb16)
    3.  [Usage](#orgf346c5e)
        1.  [Create a posframe](#orgfd5312d)
        2.  [Hide a posframe](#org530c1c6)
        3.  [Hide all posframes](#org5cf1eb8)
        4.  [Delete a posframe](#org5147f85)
        5.  [Delete all posframes](#org22b7dfe)


<a id="org0b9c3bd"></a>

# Posframe README


<a id="org0728352"></a>

## What is posframe

Posframe can pop a posframe at point, this **posframe** is a
child-frame with its root window's buffer.

The main advantages are:

1.  It is fast enough for daily usage :-)
2.  It works well with CJK language.

NOTE: For MacOS users, posframe need Emacs (version >= 26.0.91)

![img](./snapshots/posframe-1.png)


<a id="org515eb16"></a>

## Installation

    (require 'posframe)


<a id="orgf346c5e"></a>

## Usage


<a id="orgfd5312d"></a>

### Create a posframe

1.  Simple way

        (posframe-show " *my-posframe-buffer*"
                       :string "This is a test"
                       :position (point))

    Arguments documents:

        C-h f posframe-show

    Note: buffers prefixed with space will be not showed in buffer-list.

2.  Advanced way

        (defvar my-posframe-buffer " *my-posframe-buffer*")

        (with-current-buffer my-posframe-buffer
          (erase-buffer)
          (insert "Hello world"))

        (posframe-show my-posframe-buffer
                       :position (point))


<a id="org530c1c6"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="org5cf1eb8"></a>

### Hide all posframes

    M-x posframe-hide-all


<a id="org5147f85"></a>

### Delete a posframe

1.  Delete posframe and its buffer

        (posframe-delete " *my-posframe-buffer*")
2.  Only delete posframe's frame

        (posframe-delete-frame " *my-posframe-buffer*")


<a id="org22b7dfe"></a>

### Delete all posframes

    M-x posframe-delete-all

Note: this command will delete all posframe buffers,
suggest not run this command if you are sharing a buffer
between posframe and other packages.
