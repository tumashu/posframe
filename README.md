
# &#30446;&#24405;

1.  [Posframe README](#org1d72d21)
    1.  [What is posframe](#orgf708d12)
    2.  [Installation](#org2927886)
    3.  [Usage](#orgdd4adb1)
        1.  [Create a posframe named "my-posframe".](#orgc496587)
        2.  [Hide posframe named  "my-posframe".](#org4f73b3c)
        3.  [Delete posframe named "my-posframe".](#org450fd4b)


<a id="org1d72d21"></a>

# Posframe README


<a id="orgf708d12"></a>

## What is posframe

Posframe can pop a child-frame at the point.

![img](./snapshots/posframe-1.png)


<a id="org2927886"></a>

## Installation

    (require 'posframe)


<a id="orgdd4adb1"></a>

## Usage


<a id="orgc496587"></a>

### Create a posframe named "my-posframe".

    (posframe-show "my-posframe"
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


<a id="org4f73b3c"></a>

### Hide posframe named  "my-posframe".

    (posframe-hide "my-posframe")


<a id="org450fd4b"></a>

### Delete posframe named "my-posframe".

    (posframe-delete "my-posframe")



Converted from posframe.el by [el2org](https://github.com/tumashu/el2org) .