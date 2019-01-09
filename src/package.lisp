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
   #:rotate-mouse-wheel
   #:rotate-mouse-wheel-horizontally
   #:click-mouse-button
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
   #:move-cursor-smoothly
   #:press-mouse-button
   #:release-mouse-button
   #:get-double-click-time
   #:click-mouse-button
   #:double-click-mouse-button
   #:rotate-mouse-wheel
   #:rotate-mouse-wheel-horizontally
   ))
