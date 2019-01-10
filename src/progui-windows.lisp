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
(cffi:defctype word :unsigned-short)
(cffi:defctype dword :unsigned-long)
(cffi:defctype ulong-ptr #+:x86-64 :uint64 #-:x86-64 :unsigned-long)

(cffi:defcstruct tag-mouse-input
  (dx :long)
  (dy :long)
  (mouse-data dword)
  (dw-flags dword)
  (time dword)
  (dw-extra-info ulong-ptr))

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

(defconstant +keyeventf-extendedkey+ #x0001)
(defconstant +keyeventf-keyup+ #x0002)
(defconstant +keyeventf-scancode+ #x0008)
(defconstant +keyeventf-unicode+ #x0004)

(cffi:defcstruct tag-hardware-input
  (u-msg dword)
  (w-param-l word)
  (w-param-h word))

(cffi:defctype mi (:struct tag-mouse-input))
(cffi:defctype ki (:struct tag-keybd-input))
(cffi:defctype hi (:struct tag-hardware-input))

(cffi:defcunion input-union
  (mi mi)
  (ki ki)
  (hi hi))

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

;;; Define the Windows API GetDoubleClickTime function
(cffi:defcfun (%get-double-click-time "GetDoubleClickTime" :convention :stdcall) :uint
  "Return the maximum number of milliseconds that may occur between two clicks to count as a double-click.")

;;; Define the Windows API GetKeyboardLayout function
(cffi:defcfun (%get-keyboard-layout "GetKeyboardLayout" :convention :stdcall) :pointer
  "Retrieve the active input locale identifier, formerly called the keyboard layout."
  (id-thread dword))

;;; Define the Windows API MapVirtualKeyExW function
(cffi:defcfun (%map-virtual-key-ex-w "MapVirtualKeyExW" :convention :stdcall) :uint
  "Translate a virtual-key code to a scan code or the other way around."
  (u-code :uint)
  (u-map-type :uint)
  (dwhkl :pointer))

(defconstant +mapvk-vk-to-vsc+ 0)
(defconstant +mapvk-vsc-to-vk+ 1)
(defconstant +mapvk-vk-to-char+ 2)
(defconstant +mapvk-vsc-to-vk-ex+ 3)
(defconstant +mapvk-vk-to-vsc-ex+ 4)


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

(defstruct mouse-input
  "A Lisp version of the tag-mouse-input CFFI structure."
  (dx 0)
  (dy 0)
  (mouse-data 0)
  (dw-flags 0)
  (time 0)
  (dw-extra-info 0))

(defstruct keybd-input
  "A Lisp version of the tag-keybd-input CFFI structure."
  (w-vk 0)
  (w-scan 0)
  (dw-flags 0)
  (time 0)
  (dw-extra-info 0))

(defun send-inputs (inputs)
  "Call the Windows API SendInput function with a list of mouse-input/keybd-input structs."
  (let ((num-inputs (length inputs)))
    (cffi:with-foreign-object (in 'input num-inputs)
      (loop for i from 0 below num-inputs
            for input in inputs
            for input-ptr = (cffi:mem-aref in 'input i)
            for union-ptr = (cffi:foreign-slot-pointer input-ptr 'input 'dummy-union-name)
            do (etypecase input
                 (mouse-input
                  (setf (cffi:foreign-slot-value input-ptr 'input 'type) +input-mouse+
                        (cffi:foreign-slot-value union-ptr 'mi 'dx) (mouse-input-dx input)
                        (cffi:foreign-slot-value union-ptr 'mi 'dy) (mouse-input-dy input)
                        (cffi:foreign-slot-value union-ptr 'mi 'mouse-data) (mouse-input-mouse-data input)
                        (cffi:foreign-slot-value union-ptr 'mi 'dw-flags) (mouse-input-dw-flags input)
                        (cffi:foreign-slot-value union-ptr 'mi 'time) (mouse-input-time input)
                        (cffi:foreign-slot-value union-ptr 'mi 'dw-extra-info) (mouse-input-dw-extra-info input)))
                 (keybd-input
                  (setf (cffi:foreign-slot-value input-ptr 'input 'type) +input-keyboard+
                        (cffi:foreign-slot-value union-ptr 'ki 'w-vk) (keybd-input-w-vk input)
                        (cffi:foreign-slot-value union-ptr 'ki 'w-scan) (keybd-input-w-scan input)
                        (cffi:foreign-slot-value union-ptr 'ki 'dw-flags) (keybd-input-dw-flags input)
                        (cffi:foreign-slot-value union-ptr 'ki 'time) (keybd-input-time input)
                        (cffi:foreign-slot-value union-ptr 'ki 'dw-extra-info) (keybd-input-dw-extra-info input)))))
      (= num-inputs (%send-input num-inputs in (cffi:foreign-type-size 'input))))))

(defun send-input (input)
  "Call the Windows API SendInput function with a single mouse-input / keybd-input struct."
  (send-inputs (list input)))

(defun normalized-absolute-coordinate (coordinate lowest-value width-or-height)
  "Convert a virtual screen pixel coordinate to an absolute value in the range [0, 65535]."
  (ceiling (* (- coordinate lowest-value) 65536) width-or-height))

(defun move-cursor (x y)
  "Move the cursor to the given (X, Y) coordinate.  Return T if the event was successfully sent."
  (with-standard-dpi-awareness
    (let* ((normalized-x (normalized-absolute-coordinate x (virtual-screen-left) (virtual-screen-width)))
           (normalized-y (normalized-absolute-coordinate y (virtual-screen-top) (virtual-screen-height)))
           (flags (logior +mouseeventf-move+ +mouseeventf-virtualdesk+ +mouseeventf-absolute+))
           (mouse-input (make-mouse-input :dx normalized-x :dy normalized-y :dw-flags flags))
           (retval (send-input mouse-input)))
      ;; Try it a second time if it doesn't appear at the right place.
      (destructuring-bind (real-x . real-y) (get-cursor-position)
        (when (or (/= real-x x) (/= real-y y))
          (setf retval (send-input mouse-input))))
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

(defun button-click-mouse-data (button)
  "Return the value for the tag-mouse-input struct's mouse-data when a button is clicked."
  (case button
    (:xbutton1 +xbutton1+)
    (:xbutton2 +xbutton2+)
    (otherwise 0)))

(defun button-click-dw-flags (button &key pressp)
  "Return the value for the tag-mouse-input struct's dw-flags when a button is pressed or released.
PRESSP should be T when pressing the button or NIL when releasing the button."
  (ecase button
    (:left (if pressp +mouseeventf-leftdown+ +mouseeventf-leftup+))
    (:middle (if pressp +mouseeventf-middledown+ +mouseeventf-middleup+))
    (:right (if pressp +mouseeventf-rightdown+ +mouseeventf-rightup+))
    ((:xbutton1 :xbutton2) (if pressp +mouseeventf-xdown+ +mouseeventf-xup+))))

(defun press-mouse-button (button)
  "Press the given mouse button, one of :LEFT :MIDDLE :RIGHT :XBUTTON1 :XBUTTON2 :PRIMARY :SECONDARY.
:LEFT, :MIDDLE, and :RIGHT press the left/middle/right buttons.  :XBUTTON1 and :XBUTTON2 are the buttons that may be
on the side of the mouse.  :PRIMARY and :SECONDARY are equal to :LEFT and :RIGHT unless the mouse buttons are swapped.
Return T if the event was successfully sent."
  (let ((adjusted-button (adjust-for-swap button)))
    (send-input (make-mouse-input :mouse-data (button-click-mouse-data adjusted-button)
                                  :dw-flags (button-click-dw-flags adjusted-button :pressp t)))))

(defun release-mouse-button (button)
  "Release the given mouse button, one of :LEFT :MIDDLE :RIGHT :XBUTTON1 :XBUTTON2 :PRIMARY :SECONDARY.
:LEFT, :MIDDLE, and :RIGHT release the left/middle/right buttons.  :XBUTTON1 and :XBUTTON2 are the buttons that may be
on the side of the mouse.  :PRIMARY and :SECONDARY are equal to :LEFT and :RIGHT unless the mouse buttons are swapped.
Return T if the event was successfully sent."
  (let ((adjusted-button (adjust-for-swap button)))
    (send-input (make-mouse-input :mouse-data (button-click-mouse-data adjusted-button)
                                  :dw-flags (button-click-dw-flags adjusted-button :pressp nil)))))

(defun click-mouse-button (button num-times)
  "Click the given mouse button NUM-TIMES times, one of :LEFT :MIDDLE :RIGHT :XBUTTON1 :XBUTTON2 :PRIMARY :SECONDARY.
:LEFT, :MIDDLE, and :RIGHT press the left/middle/right buttons.  :XBUTTON1 and :XBUTTON2 are the buttons that may be
on the side of the mouse.  :PRIMARY and :SECONDARY are equal to :LEFT and :RIGHT unless the mouse buttons are swapped.
This function sends all the inputs to press and release in one call to SendInput, which makes sure the events are not
interspersed with other keyboard or mouse input events.
Return T if all events were successfully sent."
  (let* ((adjusted-button (adjust-for-swap button))
         (mouse-data (button-click-mouse-data adjusted-button))
         (press-flags (button-click-dw-flags adjusted-button :pressp t))
         (release-flags (button-click-dw-flags adjusted-button :pressp nil))
         (press (make-mouse-input :mouse-data mouse-data :dw-flags press-flags))
         (release (make-mouse-input :mouse-data mouse-data :dw-flags release-flags)))
    (send-inputs (loop repeat num-times nconc (list press release)))))

(defun get-double-click-time ()
  "Return the maximum number of seconds (a real number) delay between two clicks to count as a double-click."
  (/ (%get-double-click-time) 1000))

(defun rotate-mouse-wheel (clicks)
  "Rotate the mouse wheel the given number of clicks.  Positive values rotate the wheel forward, away from the user,
and negative values rotate the wheel backward, toward the user.  Return T if the event was successfully sent."
  (send-input (make-mouse-input :mouse-data (* clicks +wheel-delta+) :dw-flags +mouseeventf-wheel+)))

(defun rotate-mouse-wheel-horizontally (clicks)
  "Rotate the mouse wheel horizontally the given numberr of clicks.  Positive values rotate the wheel to the right,
and negative values rotate the wheel to the left.  Return T if the event was successfully sent."
  (send-input (make-mouse-input :mouse-data (* clicks +wheel-delta+) :dw-flags +mouseeventf-hwheel+)))

(defvar *keys*
  (let ((mapping (make-hash-table :test #'eq))
        (keys '(:VK-LBUTTON                         #x01
                :VK-RBUTTON                         #x02
                :VK-CANCEL                          #x03
                :VK-MBUTTON                         #x04
                :VK-XBUTTON1                        #x05
                :VK-XBUTTON2                        #x06
                :VK-BACK                            #x08
                :VK-TAB                             #x09
                :VK-CLEAR                           #x0C
                :VK-RETURN                          #x0D
                :VK-SHIFT                           #x10
                :VK-CONTROL                         #x11
                :VK-MENU                            #x12
                :VK-PAUSE                           #x13
                :VK-CAPITAL                         #x14
                :VK-KANA                            #x15
                :VK-HANGEUL                         #x15
                :VK-HANGUL                          #x15
                :VK-JUNJA                           #x17
                :VK-FINAL                           #x18
                :VK-HANJA                           #x19
                :VK-KANJI                           #x19
                :VK-ESCAPE                          #x1B
                :VK-CONVERT                         #x1C
                :VK-NONCONVERT                      #x1D
                :VK-ACCEPT                          #x1E
                :VK-MODECHANGE                      #x1F
                :VK-SPACE                           #x20
                :VK-PRIOR                           #x21
                :VK-NEXT                            #x22
                :VK-END                             #x23
                :VK-HOME                            #x24
                :VK-LEFT                            #x25
                :VK-UP                              #x26
                :VK-RIGHT                           #x27
                :VK-DOWN                            #x28
                :VK-SELECT                          #x29
                :VK-PRINT                           #x2A
                :VK-EXECUTE                         #x2B
                :VK-SNAPSHOT                        #x2C
                :VK-INSERT                          #x2D
                :VK-DELETE                          #x2E
                :VK-HELP                            #x2F
                :VK-0                               #x30
                :VK-1                               #x31
                :VK-2                               #x32
                :VK-3                               #x33
                :VK-4                               #x34
                :VK-5                               #x35
                :VK-6                               #x36
                :VK-7                               #x37
                :VK-8                               #x38
                :VK-9                               #x39
                :VK-A                               #x41
                :VK-B                               #x42
                :VK-C                               #x43
                :VK-D                               #x44
                :VK-E                               #x45
                :VK-F                               #x46
                :VK-G                               #x47
                :VK-H                               #x48
                :VK-I                               #x49
                :VK-J                               #x4A
                :VK-K                               #x4B
                :VK-L                               #x4C
                :VK-M                               #x4D
                :VK-N                               #x4E
                :VK-O                               #x4F
                :VK-P                               #x50
                :VK-Q                               #x51
                :VK-R                               #x52
                :VK-S                               #x53
                :VK-T                               #x54
                :VK-U                               #x55
                :VK-V                               #x56
                :VK-W                               #x57
                :VK-X                               #x58
                :VK-Y                               #x59
                :VK-Z                               #x5A
                :VK-LWIN                            #x5B
                :VK-RWIN                            #x5C
                :VK-APPS                            #x5D
                :VK-SLEEP                           #x5F
                :VK-NUMPAD0                         #x60
                :VK-NUMPAD1                         #x61
                :VK-NUMPAD2                         #x62
                :VK-NUMPAD3                         #x63
                :VK-NUMPAD4                         #x64
                :VK-NUMPAD5                         #x65
                :VK-NUMPAD6                         #x66
                :VK-NUMPAD7                         #x67
                :VK-NUMPAD8                         #x68
                :VK-NUMPAD9                         #x69
                :VK-MULTIPLY                        #x6A
                :VK-ADD                             #x6B
                :VK-SEPARATOR                       #x6C
                :VK-SUBTRACT                        #x6D
                :VK-DECIMAL                         #x6E
                :VK-DIVIDE                          #x6F
                :VK-F1                              #x70
                :VK-F2                              #x71
                :VK-F3                              #x72
                :VK-F4                              #x73
                :VK-F5                              #x74
                :VK-F6                              #x75
                :VK-F7                              #x76
                :VK-F8                              #x77
                :VK-F9                              #x78
                :VK-F10                             #x79
                :VK-F11                             #x7A
                :VK-F12                             #x7B
                :VK-F13                             #x7C
                :VK-F14                             #x7D
                :VK-F15                             #x7E
                :VK-F16                             #x7F
                :VK-F17                             #x80
                :VK-F18                             #x81
                :VK-F19                             #x82
                :VK-F20                             #x83
                :VK-F21                             #x84
                :VK-F22                             #x85
                :VK-F23                             #x86
                :VK-F24                             #x87
                :VK-NAVIGATION-VIEW                 #x88
                :VK-NAVIGATION-MENU                 #x89
                :VK-NAVIGATION-UP                   #x8A
                :VK-NAVIGATION-DOWN                 #x8B
                :VK-NAVIGATION-LEFT                 #x8C
                :VK-NAVIGATION-RIGHT                #x8D
                :VK-NAVIGATION-ACCEPT               #x8E
                :VK-NAVIGATION-CANCEL               #x8F
                :VK-NUMLOCK                         #x90
                :VK-SCROLL                          #x91
                :VK-OEM-NEC-EQUAL                   #x92
                :VK-OEM-FJ-JISHO                    #x92
                :VK-OEM-FJ-MASSHOU                  #x93
                :VK-OEM-FJ-TOUROKU                  #x94
                :VK-OEM-FJ-LOYA                     #x95
                :VK-OEM-FJ-ROYA                     #x96
                :VK-LSHIFT                          #xA0
                :VK-RSHIFT                          #xA1
                :VK-LCONTROL                        #xA2
                :VK-RCONTROL                        #xA3
                :VK-LMENU                           #xA4
                :VK-RMENU                           #xA5
                :VK-BROWSER-BACK                    #xA6
                :VK-BROWSER-FORWARD                 #xA7
                :VK-BROWSER-REFRESH                 #xA8
                :VK-BROWSER-STOP                    #xA9
                :VK-BROWSER-SEARCH                  #xAA
                :VK-BROWSER-FAVORITES               #xAB
                :VK-BROWSER-HOME                    #xAC
                :VK-VOLUME-MUTE                     #xAD
                :VK-VOLUME-DOWN                     #xAE
                :VK-VOLUME-UP                       #xAF
                :VK-MEDIA-NEXT-TRACK                #xB0
                :VK-MEDIA-PREV-TRACK                #xB1
                :VK-MEDIA-STOP                      #xB2
                :VK-MEDIA-PLAY-PAUSE                #xB3
                :VK-LAUNCH-MAIL                     #xB4
                :VK-LAUNCH-MEDIA-SELECT             #xB5
                :VK-LAUNCH-APP1                     #xB6
                :VK-LAUNCH-APP2                     #xB7
                :VK-OEM-1                           #xBA
                :VK-OEM-PLUS                        #xBB
                :VK-OEM-COMMA                       #xBC
                :VK-OEM-MINUS                       #xBD
                :VK-OEM-PERIOD                      #xBE
                :VK-OEM-2                           #xBF
                :VK-OEM-3                           #xC0
                :VK-GAMEPAD-A                       #xC3
                :VK-GAMEPAD-B                       #xC4
                :VK-GAMEPAD-X                       #xC5
                :VK-GAMEPAD-Y                       #xC6
                :VK-GAMEPAD-RIGHT-SHOULDER          #xC7
                :VK-GAMEPAD-LEFT-SHOULDER           #xC8
                :VK-GAMEPAD-LEFT-TRIGGER            #xC9
                :VK-GAMEPAD-RIGHT-TRIGGER           #xCA
                :VK-GAMEPAD-DPAD-UP                 #xCB
                :VK-GAMEPAD-DPAD-DOWN               #xCC
                :VK-GAMEPAD-DPAD-LEFT               #xCD
                :VK-GAMEPAD-DPAD-RIGHT              #xCE
                :VK-GAMEPAD-MENU                    #xCF
                :VK-GAMEPAD-VIEW                    #xD0
                :VK-GAMEPAD-LEFT-THUMBSTICK-BUTTON  #xD1
                :VK-GAMEPAD-RIGHT-THUMBSTICK-BUTTON #xD2
                :VK-GAMEPAD-LEFT-THUMBSTICK-UP      #xD3
                :VK-GAMEPAD-LEFT-THUMBSTICK-DOWN    #xD4
                :VK-GAMEPAD-LEFT-THUMBSTICK-RIGHT   #xD5
                :VK-GAMEPAD-LEFT-THUMBSTICK-LEFT    #xD6
                :VK-GAMEPAD-RIGHT-THUMBSTICK-UP     #xD7
                :VK-GAMEPAD-RIGHT-THUMBSTICK-DOWN   #xD8
                :VK-GAMEPAD-RIGHT-THUMBSTICK-RIGHT  #xD9
                :VK-GAMEPAD-RIGHT-THUMBSTICK-LEFT   #xDA
                :VK-OEM-4                           #xDB
                :VK-OEM-5                           #xDC
                :VK-OEM-6                           #xDD
                :VK-OEM-7                           #xDE
                :VK-OEM-8                           #xDF
                :VK-OEM-AX                          #xE1
                :VK-OEM-102                         #xE2
                :VK-ICO-HELP                        #xE3
                :VK-ICO-00                          #xE4
                :VK-PROCESSKEY                      #xE5
                :VK-ICO-CLEAR                       #xE6
                :VK-PACKET                          #xE7
                :VK-OEM-RESET                       #xE9
                :VK-OEM-JUMP                        #xEA
                :VK-OEM-PA1                         #xEB
                :VK-OEM-PA2                         #xEC
                :VK-OEM-PA3                         #xED
                :VK-OEM-WSCTRL                      #xEE
                :VK-OEM-CUSEL                       #xEF
                :VK-OEM-ATTN                        #xF0
                :VK-OEM-FINISH                      #xF1
                :VK-OEM-COPY                        #xF2
                :VK-OEM-AUTO                        #xF3
                :VK-OEM-ENLW                        #xF4
                :VK-OEM-BACKTAB                     #xF5
                :VK-ATTN                            #xF6
                :VK-CRSEL                           #xF7
                :VK-EXSEL                           #xF8
                :VK-EREOF                           #xF9
                :VK-PLAY                            #xFA
                :VK-ZOOM                            #xFB
                :VK-NONAME                          #xFC
                :VK-PA1                             #xFD
                :VK-OEM-CLEAR                       #xFE)))
        (loop for (k v) on keys by #'cddr
              do (setf (gethash k mapping) v)
              finally (return mapping)))
    "A mapping from virtual key code names to their values.")

(defun key->keybd-input (key &key (keyup nil))
  "Create a keyboard input using the given virtual key code's keyword symbol.
If KEYUP is NIL then the key is pressed down, otherwise it's released."
  (let* ((vk-code (gethash key *keys*))
         (scan-code (%map-virtual-key-ex-w vk-code +mapvk-vk-to-vsc-ex+ (%get-keyboard-layout 0)))
         (low-byte (logand #xff scan-code))
         (high-byte (ash scan-code -8)))
    (make-keybd-input :w-vk vk-code
                      :w-scan low-byte
                      :dw-flags (logior (if keyup +keyeventf-keyup+ 0)
                                        +keyeventf-scancode+
                                        (if (zerop high-byte) 0 +keyeventf-extendedkey+)))))

(defun press-key (key)
  "Press a key down, represented by the keyword symbol for the key."
  (send-input (key->keybd-input key :keyup nil)))

(defun release-key (key)
  "Release a key, represented by the keyword symbol for the key."
  (send-input (key->keybd-input key :keyup t)))