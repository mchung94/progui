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

(defun initialize ()
  "Load libraries during application initialization time if needed."
  (progui-sys:initialize))

(defun virtual-screen-rect ()
  "Return a rect showing the bounding rectangle around all the monitors."
  (make-instance 'rect
                 :left (progui-sys:virtual-screen-left)
                 :top (progui-sys:virtual-screen-top)
                 :width (progui-sys:virtual-screen-width)
                 :height (progui-sys:virtual-screen-height)))

(defun get-mouse-position ()
  "Return the X, Y virtual screen coordinates of the mouse cursor in a cons."
  (progui-sys:cursor-position))
