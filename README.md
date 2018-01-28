
# &#30446;&#24405;

1.  [Posframe README](#org36b5ef2)
    1.  [What is posframe](#org7146a1d)
    2.  [Installation](#orgf7207a7)
    3.  [Usage](#orge9f36ba)
        1.  [Create a posframe](#org398d423)
        2.  [Hide a posframe](#org9067e19)
        3.  [Hide all posframes](#org406784e)
        4.  [Delete a posframe](#org8f485df)
        5.  [Delete all posframes](#orge995c04)


<a id="org36b5ef2"></a>

# Posframe README


<a id="org7146a1d"></a>

## What is posframe

Posframe can pop a posframe at point, this **posframe** is a
child-frame with its root window's buffer.

The main advantages are:

1.  It is very fast, \`posframe-show' is faster than \`popup-tip'
    of popup.el.
2.  It works well with CJK language.

NOTE: posframe requires emacs (version >= 26.0.91).

![img](./snapshots/posframe-1.png)


<a id="orgf7207a7"></a>

## Installation

    (require 'posframe)


<a id="orge9f36ba"></a>

## Usage


<a id="org398d423"></a>

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


<a id="org9067e19"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="org406784e"></a>

### Hide all posframes

    M-x posframe-hide-all


<a id="org8f485df"></a>

### Delete a posframe

    (posframe-delete " *my-posframe-buffer*")


<a id="orge995c04"></a>

### Delete all posframes

    M-x posframe-delete-all

Note: this command will delete all posframe buffers,
suggest not run this command if you are sharing a buffer
between posframe and other packages.



Converted from posframe.el by [el2org](https://github.com/tumashu/el2org) .