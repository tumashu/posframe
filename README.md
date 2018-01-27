
# &#30446;&#24405;

1.  [Posframe README](#orgbfa24f1)
    1.  [What is posframe](#orgd944174)
    2.  [Installation](#org433d3be)
    3.  [Usage](#org6dd34de)
        1.  [Create a posframe](#org984573c)
        2.  [Hide a posframe](#org67129fd)
        3.  [Delete a posframe](#org3c4f78b)


<a id="orgbfa24f1"></a>

# Posframe README


<a id="orgd944174"></a>

## What is posframe

Posframe can pop a posframe at point, this **posframe** is a
child-frame with its root window's buffer.

The main advantages are:

1.  It is very fast, \`posframe-show' is faster than \`popup-tip'
    of popup.el.
2.  It works well with CJK language.

![img](./snapshots/posframe-1.png)


<a id="org433d3be"></a>

## Installation

    (require 'posframe)


<a id="org6dd34de"></a>

## Usage


<a id="org984573c"></a>

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


<a id="org67129fd"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="org3c4f78b"></a>

### Delete a posframe

    (posframe-delete " *my-posframe-buffer*")



Converted from posframe.el by [el2org](https://github.com/tumashu/el2org) .