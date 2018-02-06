
# &#30446;&#24405;

1.  [Posframe README](#org53f6910)
    1.  [What is posframe](#org5cf1be8)
    2.  [Installation](#org5f830e9)
    3.  [Usage](#org40db1ee)
        1.  [Create a posframe](#org6f36699)
        2.  [Hide a posframe](#orgcd529fe)
        3.  [Hide all posframes](#org4e71a28)
        4.  [Delete a posframe](#org0f069f2)
        5.  [Delete all posframes](#org853d491)


<a id="org53f6910"></a>

# Posframe README


<a id="org5cf1be8"></a>

## What is posframe

Posframe can pop a posframe at point, this **posframe** is a
child-frame with its root window's buffer.

The main advantages are:

1.  It is fast enough for daily usage :-)
2.  It works well with CJK language.

NOTE: For MacOS users, posframe need emacs (version >= 26.0.91)

![img](./snapshots/posframe-1.png)


<a id="org5f830e9"></a>

## Installation

    (require 'posframe)


<a id="org40db1ee"></a>

## Usage


<a id="org6f36699"></a>

### Create a posframe

    (posframe-show " *my-posframe-buffer*"
                   :string "This is a test"
                   :position (point))

Arguments documents:

    C-h f posframe-show


<a id="orgcd529fe"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="org4e71a28"></a>

### Hide all posframes

    M-x posframe-hide-all


<a id="org0f069f2"></a>

### Delete a posframe

    (posframe-delete " *my-posframe-buffer*")


<a id="org853d491"></a>

### Delete all posframes

    M-x posframe-delete-all

Note: this command will delete all posframe buffers,
suggest not run this command if you are sharing a buffer
between posframe and other packages.



Converted from posframe.el by [el2org](https://github.com/tumashu/el2org) .