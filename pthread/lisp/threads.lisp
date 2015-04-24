(in-package "PTHREAD")

;;;; Module THREADS

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






;;;; Thread Library Implementation



;;; This file implements all of the functions used to interface with the Posic
;;; PTHREADs library.  Access is provided to all of the standard tools, and also
;;; to two additional forms, progn-parallel and progn-parallel-race.

;;; In ThinLisp, threads are represented as a structure that you created with make-pthread

(defstruct (pthread (:reclaimer reclaim-pthread))
  (thread-id 0 :type uint32))

