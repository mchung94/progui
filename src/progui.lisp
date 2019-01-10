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

(defun quartic-easing-out (start end current-time total-time)
  "Calculate a value moving from START to END over TOTAL-TIME, gradually decelerating to zero velocity.
CURRENT-TIME determines the value at the given point in time.
Both times are arbitrary units as long as they're consistent."
  (let ((total-distance (- end start))
        (time-fraction (1- (/ current-time total-time))))
    (+ start (* (- total-distance) (1- (expt time-fraction 4))))))

(defun move-cursor-smoothly (x y &optional (duration-seconds 1/2))
  "Move the cursor from its current position to (X, Y) over a duration given in seconds.
Return T if the cursor ends at the given position."
  (loop with (start-x . start-y) = (get-cursor-position)
        with start-time = (get-internal-real-time)
        with total-time = (* duration-seconds internal-time-units-per-second)
        for current-time = (- (get-internal-real-time) start-time)
        for current-x = (quartic-easing-out start-x x current-time total-time)
        for current-y = (quartic-easing-out start-y y current-time total-time)
        while (<= current-time total-time)
        do (move-cursor current-x current-y)
        (sleep 0.001)
        finally (progn
                  (move-cursor x y)
                  (destructuring-bind (final-x . final-y) (get-cursor-position)
                    (return (and (= x final-x) (= y final-y)))))))

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

(defvar *keys* (loop for k being the hash-keys in progui-sys:*keys* collect k)
  "A list of all valid keys (as keyword symbols) to be used in keyboard functions.")

(defun press-key (key)
  "Press the given key on the keyboard."
  (progui-sys:press-key key))

(defun release-key (key)
  "Release the given key on the keyboard."
  (progui-sys:release-key key))
