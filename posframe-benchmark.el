;;; posframe-benchmark.el --- Benchmark tool for posframe    -*- lexical-binding:t -*-

;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

;; Author: Feng Shu <tumashu@163.com>
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/posframe
;; Version: 1.0.3
;; Keywords: convenience, tooltip
;; Package-Requires: ((emacs "26"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'posframe)

(defun posframe-benchmark ()
  "Benchmark tool for posframe."
  (interactive)
  (let ((str (with-temp-buffer
               (insert-file-contents (locate-library "posframe.el"))
               (buffer-string)))
        (n 10000))

    (message "\n* Benchmark `font-at' %s times ..." n)
    (benchmark n '(font-at (point-min)))

    (message "\n* Benchmark `redraw-display' %s times ..." n)
    (benchmark n '(redraw-display))

    (message "\n* Benchmark `remove-text-properties' %s times ..." n)
    (benchmark n `(remove-text-properties
                   0 (length str) '(read-only t)
                   ,str))

    (message "\n* Benchmark `posframe--mouse-banish' %s times ..." n)
    (benchmark n `(posframe--mouse-banish (window-frame)))

    (message "\n* Benchmark `mouse-position' %s times ..." n)
    (benchmark n '(mouse-position))

    (message "\n* Benchmark `default-font-width' %s times ..." n)
    (benchmark n '(default-font-width))

    (message "\n Finished.")))


(provide 'posframe-benchmark)

;;; posframe.el ends here
