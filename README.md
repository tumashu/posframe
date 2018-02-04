
# &#30446;&#24405;

1.  [Posframe README](#org722523d)
    1.  [What is posframe](#orgba000d1)
    2.  [Installation](#org2f1ad23)
    3.  [Usage](#org402dda2)
        1.  [Create a posframe](#orgc58b1f3)
        2.  [Hide a posframe](#orga881d72)
        3.  [Hide all posframes](#org9b18b4b)
        4.  [Delete a posframe](#org47ca9d2)
        5.  [Delete all posframes](#org7d88d0f)


<a id="org722523d"></a>

# Posframe README


<a id="orgba000d1"></a>

## What is posframe

Posframe can pop a posframe at point, this **posframe** is a
child-frame with its root window's buffer.

The main advantages are:

1.  It is very fast, \`posframe-show' is faster than \`popup-tip'
    of popup.el.
2.  It works well with CJK language.

NOTE: For MacOS users, posframe need emacs (version >= 26.0.91)

![img](./snapshots/posframe-1.png)


<a id="org2f1ad23"></a>

## Installation

    (require 'posframe)


<a id="org402dda2"></a>

## Usage


<a id="orgc58b1f3"></a>

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


<a id="orga881d72"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="org9b18b4b"></a>

### Hide all posframes

    M-x posframe-hide-all


<a id="org47ca9d2"></a>

### Delete a posframe

    (posframe-delete " *my-posframe-buffer*")


<a id="org7d88d0f"></a>

### Delete all posframes

    M-x posframe-delete-all

Note: this command will delete all posframe buffers,
suggest not run this command if you are sharing a buffer
between posframe and other packages.



Converted from posframe.el by [el2org](https://github.com/tumashu/el2org) .