
# &#30446;&#24405;

1.  [Posframe README](#orgf19b892)
    1.  [What is posframe](#orga17ac26)
    2.  [Installation](#org03e959d)
    3.  [Usage](#orgdf33141)
        1.  [Create a posframe](#org578f385)
        2.  [Hide a posframe](#orgf00d813)
        3.  [Delete a posframe](#org2ffbbcf)


<a id="orgf19b892"></a>

# Posframe README


<a id="orga17ac26"></a>

## What is posframe

Posframe can pop a posframe at point, this **posframe** is a
child-frame with its root window's buffer.

![img](./snapshots/posframe-1.png)


<a id="org03e959d"></a>

## Installation

    (require 'posframe)


<a id="orgdf33141"></a>

## Usage


<a id="org578f385"></a>

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
6.  :extra-parameters, User can use this argument to override
    default frame parameters used by posframe's child-frame,
    by the way, the above arguments can be override too.


<a id="orgf00d813"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="org2ffbbcf"></a>

### Delete a posframe

    (posframe-delete " *my-posframe-buffer*")



Converted from posframe.el by [el2org](https://github.com/tumashu/el2org) .