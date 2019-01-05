(defpackage #:progui-sys
  (:use #:common-lisp)
  (:export
   #:virtual-screen-left
   #:virtual-screen-top
   #:virtual-screen-width
   #:virtual-screen-height
   #:get-cursor-position
   #:move-cursor
   #:press-mouse-button
   #:release-mouse-button
   #:get-double-click-time
   ))

(defpackage #:progui
  (:use #:common-lisp)
  (:export
   #:rect
   #:rect-left
   #:rect-top
   #:rect-width
   #:rect-height
   #:virtual-screen-rect
   #:get-cursor-position
   #:move-cursor
   #:press-mouse-button
   #:release-mouse-button
   #:click-mouse-button
   #:double-click-mouse-button
   ))
