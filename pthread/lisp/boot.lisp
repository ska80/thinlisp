(in-package "TL-USER")

;;;; Module BOOT

;;; Copyright (c) 2000-2001 The ThinLisp Group
;;; All rights reserved.

;;; This file is part of ThinLisp.

;;; ThinLisp is open source; you can redistribute it and/or modify it
;;; under the terms of the ThinLisp License as published by the ThinLisp
;;; Group; either version 1 or (at your option) any later version.

;;; ThinLisp is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; For additional information see <http://www.thinlisp.org/>

;;; Author: Jim Allard






;;;; Pthread Support



(unless (find-package "PTHREAD")
  (make-package "PTHREAD"))

(declare-system (pthread :library t :used-systems (tl))
  boot
  threads)
