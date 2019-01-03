(defpackage #:progui-sys
  (:use #:common-lisp)
  (:export
   #:initialize
   #:virtual-screen-left
   #:virtual-screen-top
   #:virtual-screen-width
   #:virtual-screen-height
   #:cursor-position
   ))

(defpackage #:progui
  (:use #:common-lisp)
  (:export
   #:initialize
   #:rect
   #:rect-left
   #:rect-top
   #:rect-width
   #:rect-height
   #:virtual-screen-rect
   #:get-mouse-position
   ))
