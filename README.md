
# &#30446;&#24405;

1.  [Posframe README](#org7441153)
    1.  [What is posframe](#orgdac4234)
    2.  [Installation](#orgd8e377e)
    3.  [Usage](#orgf3c3baa)


<a id="org7441153"></a>

# Posframe README


<a id="orgdac4234"></a>

## What is posframe

Posframe can pop a child-frame at the point.

![img](./snapshots/posframe-1.png)


<a id="orgd8e377e"></a>

## Installation

    (require 'posframe)


<a id="orgf3c3baa"></a>

## Usage

1.  Create a posframe named "my-posframe".

        (posframe-show "my-posframe"
                       "This is a test"
                       :position (point))
2.  Hide posframe named  "my-posframe".

        (posframe-hide "my-posframe")
3.  Delete posframe named "my-posframe".

        (posframe-delete "my-posframe")



Converted from posframe.el by [el2org](https://github.com/tumashu/el2org) .