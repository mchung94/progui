(in-package #:progui-sys)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library user32
    (t (:default "user32")))
  (cffi:use-foreign-library user32))

(defconstant +sm-xvirtualscreen+ 76 "The coordinate for the left side of the virtual screen.")
(defconstant +sm-yvirtualscreen+ 77 "The coordinate for the top of the virtual screen.")
(defconstant +sm-cxvirtualscreen+ 78 "The width of the virtual screen in pixels.")
(defconstant +sm-cyvirtualscreen+ 79 "The height of the virtual screen in pixels.")

(cffi:defcfun (get-system-metrics "GetSystemMetrics" :library user32) :int
  "Retrieve the specified system metric or system configuration setting."
  (n-index :int))

(defun virtual-screen-left ()
  "Return the coordinate for the left side of the virtual screen."
  (get-system-metrics +sm-xvirtualscreen+))

(defun virtual-screen-top ()
  "Return the coordinate for the top of the virtual screen."
  (get-system-metrics +sm-yvirtualscreen+))

(defun virtual-screen-width ()
  "Return the width of the virtual screen in pixels."
  (get-system-metrics +sm-cxvirtualscreen+))

(defun virtual-screen-height ()
  "Return the height of the virtual screen in pixels."
  (get-system-metrics +sm-cyvirtualscreen+))

;;; See https://docs.microsoft.com/en-us/windows/desktop/hidpi/dpi-awareness-context
;;; Basically we want to treat the monitors as if they are at their current resolution.
;;; But if the process is DPI unaware with some scaling factor, Windows might scale all the coordinates before we get
;;; to see what the actual coordinates are.  For example, my 3200x1800 laptop screen at native resolution but with
;;; 250% scaling, coordinates are reported as if the screen were 1280x720 when DPI unaware.
(defvar *dpi-awareness-context-unaware* (cffi:make-pointer -1))
(defvar *dpi-awareness-context-system-aware* (cffi:make-pointer -2))
(defvar *dpi-awareness-context-per-monitor-aware* (cffi:make-pointer -3))
(defvar *dpi-awareness-context-per-monitor-aware-v2* (cffi:make-pointer -4))
(defvar *dpi-awareness-context-unaware-gdiscaled* (cffi:make-pointer -5))

(cffi:defcfun (set-thread-dpi-awareness-context "SetThreadDpiAwarenessContext" :library user32) :pointer
  "Set the DPI awareness for the current thread to the given awareness context."
  (dpi-context :pointer))

(defmacro with-dpi-awareness (dpi-awareness-context &body body)
  "Set the thread DPI awareness to a given value, do some work, then revert it afterwards."
  (let ((old-context (gensym)))
    `(let ((,old-context (set-thread-dpi-awareness-context ,dpi-awareness-context)))
       (unwind-protect (progn ,@body)
         (set-thread-dpi-awareness-context ,old-context)))))

(cffi:defcstruct point
  (x :long)
  (y :long))

(cffi:defcfun (get-cursor-pos "GetCursorPos" :library user32) :boolean
  (lp-point :pointer))

(defun cursor-position ()
  "Return the cursor's X and Y virtual screen coordinates in a cons."
  (with-dpi-awareness *dpi-awareness-context-per-monitor-aware-v2*
    (cffi:with-foreign-object (p '(:struct point))
      (get-cursor-pos p)
      (cons (cffi:foreign-slot-value p '(:struct point) 'x)
            (cffi:foreign-slot-value p '(:struct point) 'y)))))
