Note: this file is auto converted from posframe.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [Posframe README](#orgdd23017)
    1.  [What is posframe](#org60c68cd)
    2.  [Installation](#orge470bde)
    3.  [Usage](#orgd2e1d8c)
        1.  [Create a posframe](#orgab51022)
        2.  [Hide a posframe](#org7e469e5)
        3.  [Hide all posframes](#org8811267)
        4.  [Delete a posframe](#orge8a869d)
        5.  [Delete all posframes](#org1e3a0e6)
        6.  [Customizing pointer control](#org7d9507c)
        7.  [Set fallback argument of posframe-show](#orgc078001)


<a id="orgdd23017"></a>

# Posframe README


<a id="org60c68cd"></a>

## What is posframe

Posframe can pop a posframe at point, this **posframe** is a
child-frame with its root window's buffer.

The main advantages are:

1.  It is fast enough for daily usage :-)
2.  It works well with CJK language.

NOTE: For MacOS users, posframe need Emacs (version >= 26.0.91)

![img](./snapshots/posframe-1.png)


<a id="orge470bde"></a>

## Installation

    (require 'posframe)


<a id="orgd2e1d8c"></a>

## Usage


<a id="orgab51022"></a>

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


<a id="org7e469e5"></a>

### Hide a posframe

    (posframe-hide " *my-posframe-buffer*")


<a id="org8811267"></a>

### Hide all posframes

    M-x posframe-hide-all


<a id="orge8a869d"></a>

### Delete a posframe

1.  Delete posframe and its buffer
    
        (posframe-delete " *my-posframe-buffer*")
2.  Only delete posframe's frame
    
        (posframe-delete-frame " *my-posframe-buffer*")


<a id="org1e3a0e6"></a>

### Delete all posframes

    M-x posframe-delete-all

Note: this command will delete all posframe buffers,
suggest not run this command if you are sharing a buffer
between posframe and other packages.


<a id="org7d9507c"></a>

### Customizing pointer control

By default, posframe moves the pointer to point (0,0) in
the frame, as a way to address an issue with mouse focus.
To disable this feature, add this to your init.el:

    (setq posframe-mouse-banish nil)


<a id="orgc078001"></a>

### Set fallback argument of posframe-show

user can set fallback values of posframe-show's arguments with the
help of \`posframe-arghandler'. the below example set fallback
border-width to 10 and fallback background color to green.

(setq posframe-arghandler #'my-posframe-arghandler)
(defun my-posframe-arghandler (posframe-buffer arg-name value)
  (let ((info '(:internal-border-width 10 :background-color "green")))
    (or (plist-get info arg-name) value)))

