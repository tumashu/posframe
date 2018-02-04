
# &#30446;&#24405;

1.  [Posframe README](#org8d00721)
    1.  [What is posframe](#org16493ca)
    2.  [Installation](#orgbedc0b4)
    3.  [Usage](#org396aaad)
        1.  [Create a posframe](#org28cfe91)
        2.  [Hide a posframe](#org9d35f14)
        3.  [Hide all posframes](#org261ad75)
        4.  [Delete a posframe](#orgf31caea)
        5.  [Delete all posframes](#orge5eb6c4)


<a id="org8d00721"></a>

# Posframe README


<a id="org16493ca"></a>

## What is posframe

Posframe can pop a posframe at point, this **posframe** is a
child-frame with its root window's buffer.

The main advantages are:

1.  It is very fast, \`posframe-show' is faster than \`popup-tip'
    of popup.el.
2.  It works well with CJK language.

NOTE: For MacOS users, posframe need emacs (version >= 26.0.91)

![img](./snapshots/posframe-1.png)


<a id="orgbedc0b4"></a>

## Installation

    (require 'posframe)


<a id="org396aaad"></a>

## Usage


<a id="org28cfe91"></a>

### Create a posframe

    (posframe-show " *my-posframe-buffer*"
                   :string "This is a test"
                   :position (point))

Addition arguments:

1.  :position, set the position when posframe is poped up.
2.  :background-color, set posframe's background color.
3.  :foreground-color, set posframe's foreground color.
4.  :margin-left, set posframe's left margin width.
5.  :margin-right, set posframe's right margin width.
6.  :override-parameters, User can use it to override
    **all** the frame parameters of posframe's child-frame.


<a id="org9d35f14"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="org261ad75"></a>

### Hide all posframes

    M-x posframe-hide-all


<a id="orgf31caea"></a>

### Delete a posframe

    (posframe-delete " *my-posframe-buffer*")


<a id="orge5eb6c4"></a>

### Delete all posframes

    M-x posframe-delete-all

Note: this command will delete all posframe buffers,
suggest not run this command if you are sharing a buffer
between posframe and other packages.



Converted from posframe.el by [el2org](https://github.com/tumashu/el2org) .