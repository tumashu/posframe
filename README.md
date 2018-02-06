
# &#30446;&#24405;

1.  [Posframe README](#org3c9c1cd)
    1.  [What is posframe](#orgb4e9f42)
    2.  [Installation](#org7f774f0)
    3.  [Usage](#org91d2ede)
        1.  [Create a posframe](#org0326f21)
        2.  [Hide a posframe](#org6e6d25e)
        3.  [Hide all posframes](#org35f6819)
        4.  [Delete a posframe](#org76b96fc)
        5.  [Delete all posframes](#orge23219b)


<a id="org3c9c1cd"></a>

# Posframe README


<a id="orgb4e9f42"></a>

## What is posframe

Posframe can pop a posframe at point, this **posframe** is a
child-frame with its root window's buffer.

The main advantages are:

1.  It is very fast, \`posframe-show' is faster than \`popup-tip'
    of popup.el.
2.  It works well with CJK language.

NOTE: For MacOS users, posframe need emacs (version >= 26.0.91)

![img](./snapshots/posframe-1.png)


<a id="org7f774f0"></a>

## Installation

    (require 'posframe)


<a id="org91d2ede"></a>

## Usage


<a id="org0326f21"></a>

### Create a posframe

    (posframe-show " *my-posframe-buffer*"
                   :string "This is a test"
                   :position (point))

Arguments documents:

    C-h f posframe-show


<a id="org6e6d25e"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="org35f6819"></a>

### Hide all posframes

    M-x posframe-hide-all


<a id="org76b96fc"></a>

### Delete a posframe

    (posframe-delete " *my-posframe-buffer*")


<a id="orge23219b"></a>

### Delete all posframes

    M-x posframe-delete-all

Note: this command will delete all posframe buffers,
suggest not run this command if you are sharing a buffer
between posframe and other packages.



Converted from posframe.el by [el2org](https://github.com/tumashu/el2org) .