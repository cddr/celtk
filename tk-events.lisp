(in-package :celtk)

#|
typedef struct {
    int type;
    unsigned long serial;   /* # of last request processed by server */
    Bool send_event;	    /* True if this came from a SendEvent request */
    Display *display;	    /* Display the event was read from */
    Window event;	    /* Window on which event was requested. */
    Window root;	    /* root window that the event occured on */
    Window subwindow;	    /* child window */
    Time time;		    /* milliseconds */
    int x, y;		    /* pointer x, y coordinates in event window */
    int x_root, y_root;	    /* coordinates relative to root */
    unsigned int state;	    /* key or button mask */
    Tk_Uid name;	    /* Name of virtual event. */
    Bool same_screen;	    /* same screen flag */
    Tcl_Obj *user_data;     /* application-specific data reference; Tk will
			     * decrement the reference count *once* when it
			     * has finished processing the event. */
} XVirtualEvent;
|#

(defctype Window-ptr :unsigned-long)
(defctype Time :unsigned-long)
(defctype Tk_Uid :string)

(defcstruct x-virtual-event
    (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display :pointer)
  (event-window Window-ptr)
  (root-window Window-ptr)
  (sub-window Window-ptr)
  (time Time)
  (x :int)
  (y :int)
  (x-root :int)
  (y-root :int)
  (state :unsigned-int)
  (name Tk_Uid)
  (same-screen :boolean)
  (user-data :string)
  )

(defcenum tcl-event-flag-values
    (:tcl-dont-wait         2)
  (:tcl-window-events     4)
  (:tcl-file-events       8)
  (:tcl-timer-events     16)
  (:tcl-idle-events      32)
  (:tcl-all-events       -3))

(defcfun ("Tcl_DoOneEvent" Tcl_DoOneEvent) :int
  (flags :int))

(defcfun ("Tcl_DoWhenIdle" tcl-do-when-idle) :void
  (tcl-idle-proc :pointer)
  (client-data :int))

(defcallback tcl-idle-proc :void ((client-data :int))
  (unless (c-stopped)
    (print (list :idle-proc :client-data client-data))))

;; Tk_MainLoop

(defcfun ("Tk_MainLoop" Tk_MainLoop) :void)



(defcfun ("Tk_CreateEventHandler" tk-create-event-handler) :void
  (tkwin :pointer)
  (mask :int)
  (proc :pointer)
  (client-data :int))

(defcallback tk-event-proc :void  ((client-data :int)(XEvent :pointer))
  (trc "yowza tk-event-proc" client-data XEvent (tk-event-type (mem-aref XEvent :int))
    (foreign-slot-value xevent 'X-Virtual-Event 'user-data)))

(defcenum tk-event-type
    (:KeyPress		2)
  :KeyRelease
  :ButtonPress		
  :ButtonRelease		
  :MotionNotify		
  :EnterNotify		
  :LeaveNotify		
  :FocusIn			
  :FocusOut		
  :KeymapNotify		
  :Expose			
  :GraphicsExpose		
  :NoExpose		
  :VisibilityNotify	
  :CreateNotify		
  :DestroyNotify		
  :UnmapNotify		
  :MapNotify		
  :MapRequest		
  :ReparentNotify		
  :ConfigureNotify		
  :ConfigureRequest	
  :GravityNotify		
  :ResizeRequest		
  :CirculateNotify		
  :CirculateRequest	
  :PropertyNotify		
  :SelectionClear		
  :SelectionRequest	
  :SelectionNotify		
  :ColormapNotify		
  :ClientMessage		
  :MappingNotify		
  :virtualEvent)

(defun tk-event-type (n)
  (ignore-errors 
   (foreign-enum-keyword 'tk-event-type n)))

