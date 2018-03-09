
# &#30446;&#24405;

1.  [Posframe README](#orgb0e618a)
    1.  [What is posframe](#orga5aca6b)
    2.  [Installation](#orgc054dab)
    3.  [Usage](#orga60ff65)
        1.  [Create a posframe](#org216b84e)
        2.  [Hide a posframe](#orgda0528f)
        3.  [Hide all posframes](#org7904f7f)
        4.  [Delete a posframe](#org9aed6aa)
        5.  [Delete all posframes](#orga1ab8fd)


<a id="orgb0e618a"></a>

# Posframe README


<a id="orga5aca6b"></a>

## What is posframe

Posframe can pop a posframe at point, this **posframe** is a
child-frame with its root window's buffer.

The main advantages are:

1.  It is fast enough for daily usage :-)
2.  It works well with CJK language.

NOTE: For MacOS users, posframe need Emacs (version >= 26.0.91)

![img](./snapshots/posframe-1.png)


<a id="orgc054dab"></a>

## Installation

    (require 'posframe)


<a id="orga60ff65"></a>

## Usage


<a id="org216b84e"></a>

### Create a posframe

    (posframe-show " *my-posframe-buffer*"
                   :string "This is a test"
                   :position (point))

Arguments documents:

    C-h f posframe-show


<a id="orgda0528f"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="org7904f7f"></a>

### Hide all posframes

    M-x posframe-hide-all


<a id="org9aed6aa"></a>

### Delete a posframe

1.  Delete posframe and its buffer

        (posframe-delete " *my-posframe-buffer*")
2.  Only delete posframe's frame

        (posframe-delete-frame " *my-posframe-buffer*")


<a id="orga1ab8fd"></a>

### Delete all posframes

    M-x posframe-delete-all

Note: this command will delete all posframe buffers,
suggest not run this command if you are sharing a buffer
between posframe and other packages.


Converted from posframe.el by [el2org](https://github.com/tumashu/el2org).
