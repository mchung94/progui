(in-package #:progui)

(defclass rect ()
  ((left :accessor rect-left :initarg :left :type fixnum
         :documentation "The X coordinate of the left side of the rectangle.")
   (top :accessor rect-top :initarg :top :type fixnum
        :documentation "The Y coordinate of the top of the rectangle.")
   (width :accessor rect-width :initarg :width :type fixnum
          :documentation "The width of the rectangle in pixels.")
   (height :accessor rect-height :initarg :height :type fixnum
           :documentation "The height of the rectangle in pixels."))
  (:documentation "A rectangle represented by its upper left corner and its width and height."))

(defmethod print-object ((object rect) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "left: ~D top: ~D width: ~D height: ~D"
            (rect-left object) (rect-top object) (rect-width object) (rect-height object))))

(defun virtual-screen-rect ()
  "Return a rect showing the bounding rectangle around all the monitors."
  (make-instance 'rect
                 :left (progui-sys:virtual-screen-left)
                 :top (progui-sys:virtual-screen-top)
                 :width (progui-sys:virtual-screen-width)
                 :height (progui-sys:virtual-screen-height)))

(defun get-cursor-position ()
  "Return the X, Y virtual screen coordinates of the cursor in a cons."
  (progui-sys:get-cursor-position))

(defun move-cursor (x y)
  "Move the cursor to the given (X, Y) coordinate.  Return T if the event was successfully sent."
  (progui-sys:move-cursor x y))

(defun press-mouse-button (button)
  "Press the given mouse button, one of :LEFT :MIDDLE :RIGHT :XBUTTON1 :XBUTTON2 :PRIMARY :SECONDARY.
:LEFT, :MIDDLE, and :RIGHT press the left/middle/right buttons.  :XBUTTON1 and :XBUTTON2 are the buttons that may be
on the side of the mouse.  :PRIMARY and :SECONDARY are equal to :LEFT and :RIGHT unless the mouse buttons are swapped.
Return T if the event was successfully sent."
  (progui-sys:press-mouse-button button))

(defun release-mouse-button (button)
  "Release the given mouse button, one of :LEFT :MIDDLE :RIGHT :XBUTTON1 :XBUTTON2 :PRIMARY :SECONDARY.
:LEFT, :MIDDLE, and :RIGHT release the left/middle/right buttons.  :XBUTTON1 and :XBUTTON2 are the buttons that may be
on the side of the mouse.  :PRIMARY and :SECONDARY are equal to :LEFT and :RIGHT unless the mouse buttons are swapped.
Return T if the event was successfully sent."
  (progui-sys:release-mouse-button button))

(defun click-mouse-button (&optional (button :primary) (hold-down-seconds 0))
  "Click (press and release) the given mouse button, which defaults to the primary button.
The button can be one of :LEFT :MIDDLE :RIGHT :XBUTTON1 :XBUTTON2 :PRIMARY :SECONDARY.
hold-down-seconds is the length of time in seconds to sleep while holding the button down.
The time must be a valid argument to the sleep function (a non-negative real).
Return T if the events were successfully sent."
  (when (press-mouse-button button)
    (sleep hold-down-seconds)
    (release-mouse-button button)))

(defun double-click-mouse-button (&optional (button :primary) (hold-down-seconds 0) (delay-between-clicks 0))
  "Double click the given mouse button, which defaults to the primary button.
The button can be one of :LEFT :MIDDLE :RIGHT :XBUTTON1 :XBUTTON2 :PRIMARY :SECONDARY.
hold-down-seconds is the length of time in seconds to sleep while holding the button down each click.
delay-between-clicks is the length of time in seconds to sleep between clicks.  Both times must be valid arguments
to the sleep function (non-negative reals).  Return T if the events were successfully sent."
  (let ((max-delay (progui-sys:get-double-click-time)))
    (when (> delay-between-clicks max-delay)
      (error "The delay-between-clicks seconds must be less than the max double-click time delay (~A)." max-delay)))
  (when (click-mouse-button button hold-down-seconds)
    (sleep delay-between-clicks)
    (click-mouse-button button hold-down-seconds)))

(defun rotate-mouse-wheel (clicks)
  "Rotate the mouse wheel the given number of clicks.  Positive values rotate the wheel forward, away from the user,
and negative values rotate the wheel backward, toward the user.  Return T if the event was successfully sent."
  (progui-sys:rotate-mouse-wheel clicks))

(defun rotate-mouse-wheel-horizontally (clicks)
  "Rotate the mouse wheel horizontally the given numberr of clicks.  Positive values rotate the wheel to the right,
and negative values rotate the wheel to the left.  Return T if the event was successfully sent."
  (progui-sys:rotate-mouse-wheel-horizontally clicks))