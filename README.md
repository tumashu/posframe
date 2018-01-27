
# &#30446;&#24405;

1.  [Posframe README](#orgb926c32)
    1.  [What is posframe](#orge831c4a)
    2.  [Installation](#orgc0b324d)
    3.  [Usage](#orgfabe960)
        1.  [Create a posframe](#org024e710)
        2.  [Hide a posframe](#org2764313)
        3.  [Delete a posframe](#org705bb4f)


<a id="orgb926c32"></a>

# Posframe README


<a id="orge831c4a"></a>

## What is posframe

Posframe can pop a posframe at point, this **posframe** is a
child-frame with its root window's buffer.

The main advantages are:

1.  It is very fast, \`posframe-show' is faster than \`popup-tip'
    of popup.el.
2.  It works well with CJK language.

NOTE: posframe requires emacs (version >= 26.0.91).

![img](./snapshots/posframe-1.png)


<a id="orgc0b324d"></a>

## Installation

    (require 'posframe)


<a id="orgfabe960"></a>

## Usage


<a id="org024e710"></a>

### Create a posframe

    (posframe-show " *my-posframe-buffer*"
                   "This is a test"
                   :position (point))

Addition arguments:

1.  :position, set the position when posframe is poped up.
2.  :background-color, set posframe's background color.
3.  :foreground-color, set posframe's foreground color.
4.  :margin-left, set posframe's left margin width.
5.  :margin-right, set posframe's right margin width.
6.  :override-parameters, User can use it to override
    **all** the frame parameters of posframe's child-frame.


<a id="org2764313"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="org705bb4f"></a>

### Delete a posframe

    (posframe-delete " *my-posframe-buffer*")



Converted from posframe.el by [el2org](https://github.com/tumashu/el2org) .