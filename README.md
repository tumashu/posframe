
# &#30446;&#24405;

1.  [Posframe README](#orga0dbc56)
    1.  [What is posframe](#orga325819)
    2.  [Installation](#orgb9c1912)
    3.  [Usage](#org0428193)
        1.  [Create a posframe](#org581b74f)
        2.  [Hide a posframe](#org446ea4a)
        3.  [Delete a posframe](#org057e671)


<a id="orga0dbc56"></a>

# Posframe README


<a id="orga325819"></a>

## What is posframe

Posframe can pop a posframe at point, this **posframe** is a
child-frame with its root window's buffer.

![img](./snapshots/posframe-1.png)


<a id="orgb9c1912"></a>

## Installation

    (require 'posframe)


<a id="org0428193"></a>

## Usage


<a id="org581b74f"></a>

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


<a id="org446ea4a"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="org057e671"></a>

### Delete a posframe

    (posframe-delete " *my-posframe-buffer*")



Converted from posframe.el by [el2org](https://github.com/tumashu/el2org) .