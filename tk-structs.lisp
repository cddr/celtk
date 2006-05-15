(in-package :celtk)

(defctype Window :unsigned-long) ;; <sigh> The XWindow pointer stored in the tkwin record
(defctype Time :unsigned-long)
(defctype Tk_Uid :string)

(defcstruct tk-fake-win
    "Used by macros to peek at tkwins (why use a fake window definition?)"
  (display :pointer)
  (dummy1 :pointer)
  (screen-num :int)
  (visual :pointer)
  (depth :int)
  (window Window)
  (dummy2 :pointer)
  (dummy3 :pointer)
  (parent-ptr Window)
  (dummy4 :pointer)
  (dummy5 :pointer)
  (pathName :string)
  ;;;    Tk_Uid nameUid;
  ;;;    Tk_Uid classUid;
  ;;;    XWindowChanges changes;
  ;;;    unsigned int dummy6;	/* dirtyChanges */
  ;;;    XSetWindowAttributes atts;
  ;;;    unsigned long dummy7;	/* dirtyAtts */
  ;;;    unsigned int flags;
  ;;;    char *dummy8;		/* handlerList */
  ;;;#ifdef TK_USE_INPUT_METHODS
  ;;;    XIC dummy9;			/* inputContext */
  ;;;#endif /* TK_USE_INPUT_METHODS */
  ;;;    ClientData *dummy10;	/* tagPtr */
  ;;;    int dummy11;		/* numTags */
  ;;;    int dummy12;		/* optionLevel */
  ;;;    char *dummy13;		/* selHandlerList */
  ;;;    char *dummy14;		/* geomMgrPtr */
  ;;;    ClientData dummy15;		/* geomData */
  ;;;    int reqWidth, reqHeight;
  ;;;    int internalBorderLeft;
  ;;;    char *dummy16;		/* wmInfoPtr */
  ;;;    char *dummy17;		/* classProcPtr */
  ;;;    ClientData dummy18;		/* instanceData */
  ;;;    char *dummy19;		/* privatePtr */
  ;;;    int internalBorderRight;
  ;;;    int internalBorderTop;
  ;;;    int internalBorderBottom;
  ;;;    int minReqWidth;
  ;;;    int minReqHeight;
  )

(defun tkwin-pathname (tkwin)
  (foreign-slot-value tkwin 'tk-fake-win 'pathname))

(defun tkwin-window (tkwin)
  "Get the (different!) XWindow pointer from the tkwin data structure.
Note that the Xwindow structure is not allocated straight away, not until
(I guess) the XWindow server has gotten involved with the widget."
  (foreign-slot-value tkwin 'tk-fake-win 'window))

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

(defcstruct x-virtual-event
    "Virtual event, OK?"
  (type :int)
  (serial :unsigned-long)
  (send-event :boolean)
  (display :pointer)
  (event-window Window)
  (root-window Window)
  (sub-window Window)
  (time Time)
  (x :int)
  (y :int)
  (x-root :int)
  (y-root :int)
  (state :unsigned-int)
  (name :string)
  (same-screen :boolean)
  (user-data :pointer)
  )

(defmacro xsv (slot-name xptr)
  `(foreign-slot-value ,xptr 'X-Virtual-Event ',slot-name))

(defun xevent-type (xe)
  (tk-event-type (xsv type xe)))

(defcenum tcl-event-flag-values
    (:tcl-dont-wait         2)
  (:tcl-window-events     4)
  (:tcl-file-events       8)
  (:tcl-timer-events     16)
  (:tcl-idle-events      32)
  (:tcl-all-events       -3))

(defcenum tcl-variable-related-flag
    "Flags passed to getvar, setvar, tracevar, etc"
  (:TCL_GLOBAL_ONLY      1)
  (:TCL_NAMESPACE_ONLY	 2)
  (:TCL_APPEND_VALUE	 4)
  (:TCL_LIST_ELEMENT	 8)
  (:TCL_TRACE_READS      #x10)
  (:TCL_TRACE_WRITES	 #x20)
  (:TCL_TRACE_UNSETS	 #x40)
  (:TCL_TRACE_DESTROYED	 #x80)
  (:TCL_INTERP_DESTROYED #x100)
  (:TCL_LEAVE_ERR_MSG	 #x200)
  (:TCL_TRACE_ARRAY      #x800)
  ;; Required to support old variable/vdelete/vinfo traces */
  (:TCL_TRACE_OLD_STYLE	 #x1000)
  ;; Indicate the semantics of the result of a trace */
  (:TCL_TRACE_RESULT_DYNAMIC #x8000)
  (:TCL_TRACE_RESULT_OBJECT  #x10000))

(defun var-flags (&rest kws)
  (apply '+ (loop for kw in kws
                  collecting (foreign-enum-value 'tcl-variable-related-flag kw))))

