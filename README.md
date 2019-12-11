Note: this file is auto converted from posframe.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [Posframe README](#org4a5b8ab)
    1.  [What is posframe?](#org512c1a8)
    2.  [Installation](#org9ff09f5)
    3.  [Usage](#org9ab689b)
        1.  [Create a posframe](#org9f6f413)
        2.  [Hide a posframe](#orgf327314)
        3.  [Hide all posframes](#org20ba322)
        4.  [Delete a posframe](#org1069d7d)
        5.  [Delete all posframes](#orge48c6c3)
        6.  [Customizing mouse pointer control](#org143f992)
        7.  [Set fallback arguments of posframe-show](#orgdbf5b19)
        8.  [Some packages which use posframe](#org946d42b)


<a id="org4a5b8ab"></a>

# Posframe README


<a id="org512c1a8"></a>

## What is posframe?

Posframe can pop up a frame at point, this **posframe** is a
child-frame connected to its root window's buffer.

The main advantages are:

1.  It is fast enough for daily usage :-)
2.  It works well with CJK languages.

NOTE:

1.  For MacOS users, posframe needs Emacs version >= 26.0.91
2.  Posframe will be very very slow when emacs is built with &#x2013;with-x-toolkit=athena.

![img](./snapshots/posframe-1.png)


<a id="org9ff09f5"></a>

## Installation

    (require 'posframe)


<a id="org9ab689b"></a>

## Usage


<a id="org9f6f413"></a>

### Create a posframe

1.  Simple way

        (when (posframe-workable-p)
          (posframe-show " *my-posframe-buffer*"
                         :string "This is a test"
                         :position (point)))

2.  Advanced way

        (defvar my-posframe-buffer " *my-posframe-buffer*")
        
        (with-current-buffer (get-buffer-create my-posframe-buffer)
          (erase-buffer)
          (insert "Hello world"))
        
        (when (posframe-workable-p)
          (posframe-show my-posframe-buffer
                         :position (point)))

3.  Arguments

        C-h f posframe-show


<a id="orgf327314"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="org20ba322"></a>

### Hide all posframes

    M-x posframe-hide-all


<a id="org1069d7d"></a>

### Delete a posframe

1.  Delete posframe and its buffer
    
        (posframe-delete " *my-posframe-buffer*")
2.  Only delete the frame
    
        (posframe-delete-frame " *my-posframe-buffer*")


<a id="orge48c6c3"></a>

### Delete all posframes

    M-x posframe-delete-all

Note: this command will delete all posframe buffers.
You probably shouldn't use it if you are sharing a buffer
between posframe and other packages.


<a id="org143f992"></a>

### Customizing mouse pointer control

By default, posframe moves the pointer to point (0,0) in
the frame, as a way to address an issue with mouse focus.
To disable this feature, add this to your init.el:

    (setq posframe-mouse-banish nil)


<a id="orgdbf5b19"></a>

### Set fallback arguments of posframe-show

Users can set fallback values of posframe-show's arguments with the
help of \`posframe-arghandler'.  The example below sets fallback
border-width to 10 and fallback background color to green.

    (setq posframe-arghandler #'my-posframe-arghandler)
    (defun my-posframe-arghandler (buffer-or-name arg-name value)
      (let ((info '(:internal-border-width 10 :background-color "green")))
        (or (plist-get info arg-name) value)))


<a id="org946d42b"></a>

### Some packages which use posframe

1.  [which-key-posframe](https://github.com/yanghaoxie/which-key-posframe)
2.  [ddskk-posframe](https://github.com/conao3/ddskk-posframe.el)
3.  [pyim](https://github.com/tumashu/pyim)
4.  [ivy-posframe](https://github.com/tumashu/ivy-posframe)
5.  [company-posframe](https://github.com/tumashu/company-posframe)
6.  &#x2026;

