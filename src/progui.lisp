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

(defun mouse-down (button)
  "Press the given mouse button, one of :LEFT :MIDDLE :RIGHT :XBUTTON1 :XBUTTON2 :PRIMARY :SECONDARY.
:LEFT, :MIDDLE, and :RIGHT press the left/middle/right buttons.  :XBUTTON1 and :XBUTTON2 are the buttons that may be
on the side of the mouse.  :PRIMARY and :SECONDARY are equal to :LEFT and :RIGHT unless the mouse buttons are swapped.
Return T if the event was successfully sent."
  (progui-sys:mouse-down button))

(defun mouse-up (button)
  "Release the given mouse button, one of :LEFT :MIDDLE :RIGHT :XBUTTON1 :XBUTTON2 :PRIMARY :SECONDARY.
:LEFT, :MIDDLE, and :RIGHT release the left/middle/right buttons.  :XBUTTON1 and :XBUTTON2 are the buttons that may be
on the side of the mouse.  :PRIMARY and :SECONDARY are equal to :LEFT and :RIGHT unless the mouse buttons are swapped.
Return T if the event was successfully sent."
  (progui-sys:mouse-up button))

(defun click (&optional (button :primary) (hold-down-seconds 0))
  "Click (press and release) the given button, which defaults to the primary button.
The button can be one of :LEFT :MIDDLE :RIGHT :XBUTTON1 :XBUTTON2 :PRIMARY :SECONDARY.
hold-down-seconds is the length of time in seconds to sleep while holding the button down (can be floating point).
Return T if the events were successfully sent."
  (when (mouse-down button)
    (unless (zerop hold-down-seconds)
      (sleep hold-down-seconds))
    (mouse-up button)))
