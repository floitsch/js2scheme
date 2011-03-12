;; Copyright (c) 2007-2011, Florian Loitsch
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in the
;;      documentation and/or other materials provided with the distribution.
;;    * Neither the name of the <organization> nor the
;;      names of its contributors may be used to endorse or promote products
;;      derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(module jsre-RegExp-state
   (import jsre-base-string
	   jsre-RegExp-classes
	   jsre-RegExp-fsm
	   jsre-RegExp-dot)
   (use jsre-conversion
	jsre-base-object)
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
       (loops::pair-nil (default '())) ;; list of FSM-loop-infos
       ;; optimization fields
       (sharing-clusters?::bool (default #f))
       (sharing-br-clusters?::bool (default #f))
       (still-needed?::bool (default #f)) ;; do not change the state.
       (occupying?::bool (default #f))) ;; state is occupying a node.
    (wide-class FSM-sleeping-state::FSM-state
       cycles-to-sleep::bint)))
