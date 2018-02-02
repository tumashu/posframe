
# &#30446;&#24405;

1.  [Posframe README](#org868836c)
    1.  [What is posframe](#org4059e00)
    2.  [Installation](#org793f684)
    3.  [Usage](#org4f80187)
        1.  [Create a posframe](#org8dc61d6)
        2.  [Hide a posframe](#org7d26675)
        3.  [Hide all posframes](#orgae3b75a)
        4.  [Delete a posframe](#org1ce137f)
        5.  [Delete all posframes](#org5e5ed57)


<a id="org868836c"></a>

# Posframe README


<a id="org4059e00"></a>

## What is posframe

Posframe can pop a posframe at point, this **posframe** is a
child-frame with its root window's buffer.

The main advantages are:

1.  It is very fast, \`posframe-show' is faster than \`popup-tip'
    of popup.el.
2.  It works well with CJK language.

NOTE: posframe requires emacs (version >= 26.0.91), but for
compatibility reasons, it does not require emacs26 at package
level, user should test emacs version before run \`posframe-show'.

![img](./snapshots/posframe-1.png)


<a id="org793f684"></a>

## Installation

    (require 'posframe)


<a id="org4f80187"></a>

## Usage


<a id="org8dc61d6"></a>

### Create a posframe

    (when (>= emacs-major-version 26)
      (posframe-show " *my-posframe-buffer*"
                     :string "This is a test"
                     :position (point)))

Addition arguments:

1.  :position, set the position when posframe is poped up.
2.  :background-color, set posframe's background color.
3.  :foreground-color, set posframe's foreground color.
4.  :margin-left, set posframe's left margin width.
5.  :margin-right, set posframe's right margin width.
6.  :override-parameters, User can use it to override
    **all** the frame parameters of posframe's child-frame.


<a id="org7d26675"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="orgae3b75a"></a>

### Hide all posframes

    M-x posframe-hide-all


<a id="org1ce137f"></a>

### Delete a posframe

    (posframe-delete " *my-posframe-buffer*")


<a id="org5e5ed57"></a>

### Delete all posframes

    M-x posframe-delete-all

Note: this command will delete all posframe buffers,
suggest not run this command if you are sharing a buffer
between posframe and other packages.



Converted from posframe.el by [el2org](https://github.com/tumashu/el2org) .