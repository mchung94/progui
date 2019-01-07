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
Return T if the event was successfully sent."
  (progui-sys:press-mouse-button button))

(defun release-mouse-button (button)
  "Release the given mouse button, one of :LEFT :MIDDLE :RIGHT :XBUTTON1 :XBUTTON2 :PRIMARY :SECONDARY.
Return T if the event was successfully sent."
  (progui-sys:release-mouse-button button))

(defun get-double-click-time ()
  "Return the maximum number of seconds (a real number) delay between two clicks to count as a double-click."
  (progui-sys:get-double-click-time))

(defun click-mouse-button (&optional (button :primary) (num-times 1))
  "Click (press and release) the given mouse button, which defaults to the primary button.
The button can be one of :LEFT :MIDDLE :RIGHT :XBUTTON1 :XBUTTON2 :PRIMARY :SECONDARY.
NUM-TIMES can be used to click multiple times, like double or triple clicking.
Return T if all the events were successfully sent."
  (progui-sys:click-mouse-button button num-times))

(defun double-click-mouse-button (&optional (button :primary) (seconds-between-clicks 0))
  "Double click the given mouse button, which defaults to the primary button.
The button can be one of :LEFT :MIDDLE :RIGHT :XBUTTON1 :XBUTTON2 :PRIMARY :SECONDARY.
SECONDS-BETWEEN-CLICKS adds a wait between each click.  Return T if all the events were successfully sent."
  (if (zerop seconds-between-clicks)
      (click-mouse-button button 2)
    (and (click-mouse-button button)
         (progn (sleep seconds-between-clicks)
           (click-mouse-button button)))))

(defun rotate-mouse-wheel (clicks)
  "Rotate the mouse wheel the given number of clicks.  Positive values rotate the wheel forward, away from the user,
and negative values rotate the wheel backward, toward the user.  Return T if the event was successfully sent."
  (progui-sys:rotate-mouse-wheel clicks))

(defun rotate-mouse-wheel-horizontally (clicks)
  "Rotate the mouse wheel horizontally the given numberr of clicks.  Positive values rotate the wheel to the right,
and negative values rotate the wheel to the left.  Return T if the event was successfully sent."
  (progui-sys:rotate-mouse-wheel-horizontally clicks))