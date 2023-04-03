
; The queue of functions that poll for events.
; form: (priority lambda), lambda retuns an event data, list of event data, or nil.
(defvar *pollers* nil)

; The event queue.
; form: (priority event data), data is arbitrary
(defvar *events* nil)

; The queue of handlers.
; form: (priority event handler)
(defvar *handlers* nil)

; Given a queue of entries with the first element of each the priority and another entry,
; returns a new queue with the element insterted in the appropriate position.
; Higher priority items get queued first.
(defun pq-insert (item pq)
    (if (null pq)
        (cons item pq)
        (if (> (first item) (first (first pq)))
            (cons item pq)
            (cons (first pq) (pq-insert item (rest pq))))))

; Wraps for (pq-insert) for event datas that adds to the *events* queue
(defun queue-event (e)
    (setq *events* (pq-insert e *events*))
    nil)

; Run through the *pollers* and pushes any events returned to the queue.
(defun gather-events ()
    (dolist (poller *pollers* nil)
        (let* ((pollfun (second poller)) (event (funcall pollfun)))
            (cond
                ((null event) nil)
                ((numberp (first event)) (queue-event event))
                ((listp (first event) (mapc queue-event event)))
                (t (error "malformed event: ~s" event))))))

; Dispatches the event to the handlers
(defun dispatch-event (event)
    (let ((event-name (second event)) (data (third event)))
        (dolist (handler *handlers* nil)
            (let ((handlerfun (third handler)) (handler-name (second handler)))
                (when (search event-name handler-name)
                    (let ((cancelled nil))
                        (setq cancelled (funcall handlerfun data))
                        (when cancelled (return t))))))))

; The main event scheduler
(defun event-loop ()
    (loop
        (gather-events)
        (mapc dispatch-event *events*)))