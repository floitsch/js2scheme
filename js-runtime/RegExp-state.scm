(module jsre-RegExp-state
   (import jsre-RegExp-classes
	   jsre-RegExp-fsm
	   jsre-RegExp-dot)
   (export
    (final-class FSM-loop-info
       (iterations::bint read-only)
       (index-time::bint read-only)
       (loop-exit::FSM-repeat-exit read-only))
    (final-class FSM-state
       clusters::vector
       backref-clusters::vector ;; duplicate of the relevant entries.
       (start-index::bint (default -1))
       (final-index::bint (default -1))
       node::FSM-node
       (loops::pair-nil (default '()))) ;; list of FSM-loop-infos
    (wide-class FSM-sleeping-state::FSM-state
       cycles-to-sleep::bint)))
