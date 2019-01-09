# progui
Programmatically control the mouse and keyboard.

This is a work in progress.  Currently I'm working on supporting Windows 10.
Once that's done, I can look into supporting other operating systems.

## The Virtual Screen
This code thinks of your displays the same way Windows 10 does: see
[The Virtual Screen](https://docs.microsoft.com/en-us/windows/desktop/gdi/the-virtual-screen)
for details.

The minimal bounding box around all displays is called the *virtual screen*.
The top left corner of the *primary monitor* has the coordinates (0, 0).
So (0, 0) isn't necessarily the top left corner of the *virtual screen* when
you have multiple displays.  The coordinates can even be negative.

The coordinate system always uses the displays' current resolution and ignores
Windows 10 display scaling.  For example, my laptop has a 3200x1800 display
and its resolution is the same 3200x1800 but with 250% scaling.  When an
application is DPI unaware, Windows will report the size as 1280x720.  But
this project ignores that and uses 3200x1800 coordinates.  On the other hand,
if I changed the actual display resolution to 1280x720 instead of the scaling
factor, then it will use coordinates within the 1280x720 range.

## Mouse Buttons
Mouse buttons in the functionality described later are one of the following
keywords:
* `:LEFT`
* `:MIDDLE`
* `:RIGHT`
* `:XBUTTON1`
* `:XBUTTON2`
* `:PRIMARY`
* `:SECONDARY`

`:LEFT` and `:RIGHT` are always the left and right mouse buttons regardless of
whether or not the user has the mouse buttons swapped.  `:PRIMARY` and
`:SECONDARY` are the same as `:LEFT` and `:RIGHT`, unless the user has the
mouse buttons swapped.  Then they'll be mapped the other way around.  Mouse
button clicking functions default to the `:PRIMARY` button unless another
button is explicitly passed in.

`:XBUTTON1` and `:XBUTTON2` are the two extra mouse buttons which may be on
the side of the mouse.  For example in web browsers these can be the back
and forward buttons, respectively.

## Functionality
These are in the `progui` package:

`rect`: A rectangle class containing left and top X,Y coordinates plus width
and height.  `rect-left`, `rect-top`, `rect-width`, and `rect-height` are
accessors.

`virtual-screen-rect`: Return a `rect` showing the bounding rectangle around
all the monitors.
```lisp
(progui:virtual-screen-rect)
=> #<PROGUI:RECT left: -1920 top: 0 width: 4480 height: 1440 4020012263>
```

`get-cursor-position`: Return the X, Y virtual screen coordinates of the cursor in a `cons`.
```lisp
(progui:get-cursor-position)
=> (138 . 463)
```

`move-cursor`: Move the cursor to the given (X, Y) coordinate.  Return T if
the event was successfully sent. 
```lisp
(progui:move-cursor -1200 234)
=> T
```

`move-cursor-smoothly`: Move the cursor from its current position to (X, Y)
over a duration given in seconds.  Return T if the cursor ends at the given
position.  Basically, this makes it so you can see the cursor gradually move
to its destination instead of instantly moving there.
```lisp
(progui:move-cursor-smoothly 0 0 3/4)
=> T ; moves the cursor to (0, 0) over 3/4 of a second duration

(progui:move-cursor-smoothly 1000 1000)
=> T ; moves the cursor to (1000, 1000) over half a second duration

(progui:get-cursor-position)
=> (0 . 0) ; position at the start
(progui:move-cursor-smoothly 10000 10000)
=> NIL ; out of range, can't move there
(progui:get-cursor-position)
=> (2559 . 1439) ; it tried to gradually move there but stopped at the corner
```

`press-mouse-button`: Press the given mouse button.
Return T if the event was successfully sent.
```lisp
(progui:press-mouse-button :primary)
=> T
```

`release-mouse-button`: Release the given mouse button.
Return T if the event was successfully sent.
```lisp
(progui:release-mouse-button :primary)
=> T
```

`get-double-click-time`: Return the maximum number of seconds delay between
two clicks to count as a double-click.
```lisp
(progui:get-double-click-time)
=> 1/2
```

`click-mouse-button`: Click (press and release) the given mouse button.
The optional `num-times` parameter says how many times to click.
Return T if all the events were successfully sent.
```lisp
(progui:click-mouse-button)
=> T ; clicks the :PRIMARY mouse button which is the same as :LEFT unless the
     ; user has the :LEFT and :RIGHT mouse buttons swapped

(progui:click-mouse-button :left)
=> T ; clicks the :LEFT mouse button regardless of the user's mouse button
     ; swap settings

(progui:click-mouse-button :left 2)
=> T ; clicks the left mouse button twice in a row (double-click)
```

`double-click-mouse-button`: Double click the given mouse button.
The optional `seconds-between-clicks` parameter is the length of time in
seconds to sleep between clicks.  Return T if all the events were successfully
sent.
```lisp
(progui:double-click-mouse-button)
=> T ; double click the :PRIMARY mouse button

(progui:double-click-mouse-button :middle 0.1)
=> T ; click the middle mouse button, wait 0.1 seconds, click it again
```

`rotate-mouse-wheel`: Rotate the mouse wheel vertically, forward or backward.
The parameter is the number of clicks.  Positive values rotate the wheel
forward, away from the user, and negative values rotate the wheel backward,
towards the user.  Return T if the event was successfully sent.

```lisp
(progui:rotate-mouse-wheel 12)
=> T
```

`rotate-mouse-wheel-horizontally`: Rotate the mouse wheel horizontally.  The
parameter is the number of clicks.  Positive values rotate the wheel to the
right, and negative values rotate the wheel to the left.  Return T if the event
was successfully sent.

```lisp
(progui:rotate-mouse-wheel-horizontally -4)
=> T
```
