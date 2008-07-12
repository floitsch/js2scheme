(module jsre-RegExp-state
   (import jsre-RegExp-classes
	   jsre-RegExp-fsm
	   jsre-RegExp-dot)
   (export
    (final-class FSM-state
       clusters::vector
       backref-clusters::vector ;; duplicate of the relevant entries.
       (final-index (default #f))
       (collision?::bool (default #f))
       node::FSM-node
       (next-in-loop::FSM-state (default (FSM-state-nil)))
       (next-different-backref::FSM-state (default (FSM-state-nil))))
    (wide-class FSM-sleeping-state::FSM-state
       cycles-to-sleep::bint)))
