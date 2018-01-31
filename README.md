
# &#30446;&#24405;

1.  [Posframe README](#org035f222)
    1.  [What is posframe](#org4cfefc0)
    2.  [Installation](#org860fe5c)
    3.  [Usage](#orgd2cc6e4)
        1.  [Create a posframe](#org9536a5a)
        2.  [Hide a posframe](#org4441abd)
        3.  [Hide all posframes](#org08545c6)
        4.  [Delete a posframe](#orgf8b48eb)
        5.  [Delete all posframes](#org4ea3642)


<a id="org035f222"></a>

# Posframe README


<a id="org4cfefc0"></a>

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


<a id="org860fe5c"></a>

## Installation

    (require 'posframe)


<a id="orgd2cc6e4"></a>

## Usage


<a id="org9536a5a"></a>

### Create a posframe

    (when (>= emacs-major-version 26)
      (posframe-show " *my-posframe-buffer*"
                     "This is a test"
                     :position (point)))

Addition arguments:

1.  :position, set the position when posframe is poped up.
2.  :background-color, set posframe's background color.
3.  :foreground-color, set posframe's foreground color.
4.  :margin-left, set posframe's left margin width.
5.  :margin-right, set posframe's right margin width.
6.  :override-parameters, User can use it to override
    **all** the frame parameters of posframe's child-frame.


<a id="org4441abd"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="org08545c6"></a>

### Hide all posframes

    M-x posframe-hide-all


<a id="orgf8b48eb"></a>

### Delete a posframe

    (posframe-delete " *my-posframe-buffer*")


<a id="org4ea3642"></a>

### Delete all posframes

    M-x posframe-delete-all

Note: this command will delete all posframe buffers,
suggest not run this command if you are sharing a buffer
between posframe and other packages.



Converted from posframe.el by [el2org](https://github.com/tumashu/el2org) .