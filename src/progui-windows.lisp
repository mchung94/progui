(in-package #:progui-sys)

(cffi:define-foreign-library user32
  (t (:default "user32")))
(cffi:use-foreign-library user32)

;;; Define the Windows API GetSystemMetrics function
(defconstant +sm-swapbutton+ 23)
(defconstant +sm-xvirtualscreen+ 76)
(defconstant +sm-yvirtualscreen+ 77)
(defconstant +sm-cxvirtualscreen+ 78)
(defconstant +sm-cyvirtualscreen+ 79)

(cffi:defcfun (%get-system-metrics "GetSystemMetrics" :convention :stdcall) :int
  "Retrieve the specified system metric or system configuration setting."
  (n-index :int))

;;; Define the Windows API SetThreadDpiAwarenessContext function
(defvar *dpi-awareness-context-unaware* (cffi:make-pointer -1))
(defvar *dpi-awareness-context-system-aware* (cffi:make-pointer -2))
(defvar *dpi-awareness-context-per-monitor-aware* (cffi:make-pointer -3))
(defvar *dpi-awareness-context-per-monitor-aware-v2* (cffi:make-pointer -4))
(defvar *dpi-awareness-context-unaware-gdiscaled* (cffi:make-pointer -5))

(cffi:defcfun (%set-thread-dpi-awareness-context "SetThreadDpiAwarenessContext" :convention :stdcall) :pointer
  "Set the DPI awareness for the current thread to the given awareness context.
Return the old DPI awareness context."
  (dpi-context :pointer))

;;; Define the Windows API GetCursorPos function
(cffi:defctype bool :int)

(cffi:defcstruct point
  (x :long)
  (y :long))

(cffi:defcfun (%get-cursor-pos "GetCursorPos" :convention :stdcall) bool
  (lp-point :pointer))

;;; Define the Windows API SendInput function
(cffi:defctype dword :unsigned-long)
(cffi:defctype word :unsigned-short)
(cffi:defctype ulong-ptr #+:x86-64 :uint64 #-:x86-64 :unsigned-long)

(cffi:defcstruct tag-mouse-input
  (dx :long)
  (dy :long)
  (mouse-data dword)
  (dw-flags dword)
  (time dword)
  (dw-extra-info ulong-ptr))

(cffi:defctype mouse-input (:struct tag-mouse-input))

(defconstant +xbutton1+ #x0001)
(defconstant +xbutton2+ #x0002)

(defconstant +mouseeventf-move+ #x0001)
(defconstant +mouseeventf-leftdown+ #x0002)
(defconstant +mouseeventf-leftup+ #x0004)
(defconstant +mouseeventf-rightdown+ #x0008)
(defconstant +mouseeventf-rightup+ #x0010)
(defconstant +mouseeventf-middledown+ #x0020)
(defconstant +mouseeventf-middleup+ #x0040)
(defconstant +mouseeventf-xdown+ #x0080)
(defconstant +mouseeventf-xup+ #x0100)
(defconstant +mouseeventf-wheel+ #x0800)
(defconstant +mouseeventf-hwheel+ #x1000)
(defconstant +mouseeventf-virtualdesk+ #x4000)
(defconstant +mouseeventf-absolute+ #x8000)

(defconstant +wheel-delta+ 120 "The amount of mouse wheel movement for one wheel click.")

(cffi:defcstruct tag-keybd-input
  (w-vk word)
  (w-scan word)
  (dw-flags dword)
  (time dword)
  (dw-extra-info ulong-ptr))

(cffi:defctype keybd-input (:struct tag-keybd-input))

(cffi:defcstruct tag-hardware-input
  (u-msg dword)
  (w-param-l word)
  (w-param-h word))

(cffi:defctype hardware-input (:struct tag-hardware-input))

(cffi:defcunion input-union
  (mi mouse-input)
  (ki keybd-input)
  (hi hardware-input))

(cffi:defcstruct tag-input
  (type dword)
  (dummy-union-name (:union input-union)))

(cffi:defctype input (:struct tag-input))

(defconstant +input-mouse+ 0)
(defconstant +input-keyboard+ 1)
(defconstant +input-hardware+ 2)

(cffi:defcfun (%send-input "SendInput" :convention :stdcall) :uint
  "Send keyboard or mouse events and return the number of events sent successfully."
  (c-inputs :uint)
  (p-inputs :pointer)
  (cb-size :int))

;;; Define the GetDoubleClickTime function
(cffi:defcfun (%get-double-click-time "GetDoubleClickTime" :convention :stdcall) :uint
  "Return the maximum number of milliseconds that may occur between two clicks to count as a double-click.")

(defun virtual-screen-left ()
  "Return the coordinate for the left side of the virtual screen."
  (%get-system-metrics +sm-xvirtualscreen+))

(defun virtual-screen-top ()
  "Return the coordinate for the top of the virtual screen."
  (%get-system-metrics +sm-yvirtualscreen+))

(defun virtual-screen-width ()
  "Return the width of the virtual screen in pixels."
  (%get-system-metrics +sm-cxvirtualscreen+))

(defun virtual-screen-height ()
  "Return the height of the virtual screen in pixels."
  (%get-system-metrics +sm-cyvirtualscreen+))

(defmacro with-dpi-awareness (dpi-awareness-context &body body)
  "Set the thread DPI awareness to a given value, do some work, then revert it afterwards.
This can change how Windows reports display coordinates when the display scaling factor is not 100%.
For example with a 3200x1800 resolution display at 250% scaling, when the thread is DPI unaware, Windows reports
cursor position and screen size as if it was 1280x720 resolution.  But when it is DPI aware, it will report coordinates
as if it was 3200x1800."
  (let ((old-context (gensym)))
    `(let ((,old-context (%set-thread-dpi-awareness-context ,dpi-awareness-context)))
       (unwind-protect (progn ,@body)
         (%set-thread-dpi-awareness-context ,old-context)))))

(defmacro with-standard-dpi-awareness (&body body)
  "Set the thread DPI awareness to DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2 when doing work and revert when done."
  `(with-dpi-awareness *dpi-awareness-context-per-monitor-aware-v2*
     ,@body))

(defun get-cursor-position ()
  "Return the cursor's X and Y virtual screen coordinates in a cons."
  (with-standard-dpi-awareness
    (cffi:with-foreign-object (p '(:struct point))
      (%get-cursor-pos p)
      (cons (cffi:foreign-slot-value p '(:struct point) 'x)
            (cffi:foreign-slot-value p '(:struct point) 'y)))))

(defun send-mouse-input (&key (dx 0) (dy 0) (mouse-data 0) (dw-flags 0) (time 0) (dw-extra-info 0))
  "A convenience function to call SendInput using mouse input data.
Return T if the event was successfully sent."
  (cffi:with-foreign-object (in 'input)
    (let ((ptr (cffi:foreign-slot-pointer in 'input 'dummy-union-name)))
      (setf (cffi:foreign-slot-value in 'input 'type) +input-mouse+
            (cffi:foreign-slot-value ptr 'mouse-input 'dx) dx
            (cffi:foreign-slot-value ptr 'mouse-input 'dy) dy
            (cffi:foreign-slot-value ptr 'mouse-input 'mouse-data) mouse-data
            (cffi:foreign-slot-value ptr 'mouse-input 'dw-flags) dw-flags
            (cffi:foreign-slot-value ptr 'mouse-input 'time) time
            (cffi:foreign-slot-value ptr 'mouse-input 'dw-extra-info) dw-extra-info))
    (= 1 (%send-input 1 in (cffi:foreign-type-size 'input)))))

(defun normalized-absolute-coordinate (coordinate lowest-value width-or-height)
  "Convert a virtual screen pixel coordinate to an absolute value in the range [0, 65535]."
  (ceiling (* (- coordinate lowest-value) 65536) width-or-height))

(defun move-cursor (x y)
  "Move the cursor to the given (X, Y) coordinate.  Return T if the event was successfully sent."
  (with-standard-dpi-awareness
    (let* ((normalized-x (normalized-absolute-coordinate x (virtual-screen-left) (virtual-screen-width)))
           (normalized-y (normalized-absolute-coordinate y (virtual-screen-top) (virtual-screen-height)))
           (flags (logior +mouseeventf-move+ +mouseeventf-virtualdesk+ +mouseeventf-absolute+))
           (retval (send-mouse-input :dx normalized-x :dy normalized-y :dw-flags flags)))
      ;; Try it a second time if it doesn't appear at the right place.
      (destructuring-bind (real-x . real-y) (get-cursor-position)
        (when (or (/= real-x x) (/= real-y y))
          (setf retval (send-mouse-input :dx normalized-x :dy normalized-y :dw-flags flags))))
      retval)))

(defun mouse-buttons-swapped-p ()
  "Return T if the left and right mouse buttons are swapped."
  (= 1 (%get-system-metrics +sm-swapbutton+)))

(defun adjust-for-swap (button)
  "If the button is :PRIMARY or :SECONDARY, return :LEFT or :RIGHT, otherwise just return the button as is.
Normally :PRIMARY is :LEFT and :SECONDARY is :RIGHT, but if the buttons are swapped it will return the opposite."
  (let ((swapped (mouse-buttons-swapped-p)))
    (case button
      (:primary (if swapped :right :left))
      (:secondary (if swapped :left :right))
      (otherwise button))))

(defun press-mouse-button (button)
  "Press the given mouse button, one of :LEFT :MIDDLE :RIGHT :XBUTTON1 :XBUTTON2 :PRIMARY :SECONDARY.
:LEFT, :MIDDLE, and :RIGHT press the left/middle/right buttons.  :XBUTTON1 and :XBUTTON2 are the buttons that may be
on the side of the mouse.  :PRIMARY and :SECONDARY are equal to :LEFT and :RIGHT unless the mouse buttons are swapped.
Return T if the event was successfully sent."
  (setf button (adjust-for-swap button))
  (send-mouse-input :mouse-data (case button 
                                  (:xbutton1 +xbutton1+)
                                  (:xbutton2 +xbutton2+)
                                  (otherwise 0))
                    :dw-flags (ecase button
                                (:left +mouseeventf-leftdown+)
                                (:middle +mouseeventf-middledown+)
                                (:right +mouseeventf-rightdown+)
                                ((:xbutton1 :xbutton2) +mouseeventf-xdown+))))

(defun release-mouse-button (button)
  "Release the given mouse button, one of :LEFT :MIDDLE :RIGHT :XBUTTON1 :XBUTTON2 :PRIMARY :SECONDARY.
:LEFT, :MIDDLE, and :RIGHT release the left/middle/right buttons.  :XBUTTON1 and :XBUTTON2 are the buttons that may be
on the side of the mouse.  :PRIMARY and :SECONDARY are equal to :LEFT and :RIGHT unless the mouse buttons are swapped.
Return T if the event was successfully sent."
  (setf button (adjust-for-swap button))
  (send-mouse-input :mouse-data (case button
                                  (:xbutton1 +xbutton1+)
                                  (:xbutton2 +xbutton2+)
                                  (otherwise 0))
                    :dw-flags (ecase button
                                (:left +mouseeventf-leftup+)
                                (:middle +mouseeventf-middleup+)
                                (:right +mouseeventf-rightup+)
                                ((:xbutton1 :xbutton2) +mouseeventf-xup+))))

(defun get-double-click-time ()
  "Return the maximum number of seconds (a real number) delay between two clicks to count as a double-click."
  (/ (%get-double-click-time) 1000))

(defun rotate-mouse-wheel (clicks)
  "Rotate the mouse wheel the given number of clicks.  Positive values rotate the wheel forward, away from the user,
and negative values rotate the wheel backward, toward the user.  Return T if the event was successfully sent."
  (send-mouse-input :mouse-data (* clicks +wheel-delta+) :dw-flags +mouseeventf-wheel+))

(defun rotate-mouse-wheel-horizontally (clicks)
  "Rotate the mouse wheel horizontally the given numberr of clicks.  Positive values rotate the wheel to the right,
and negative values rotate the wheel to the left.  Return T if the event was successfully sent."
  (send-mouse-input :mouse-data (* clicks +wheel-delta+) :dw-flags +mouseeventf-hwheel+))
