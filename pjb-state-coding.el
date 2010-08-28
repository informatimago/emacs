;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;******************************************************************************
;;;;FILE:               state-coding.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;	The purpose of this program is to generate a structure encoding the
;;;;	hierarchical structucture of states into a bit field.
;;;;
;;;;     A (AA AB AC) B C (CA) D (DA DB DC  DD (DDA DDB) DE DF)
;;;;
;;;;     00000011 top-level-states
;;;;     00000000 A
;;;;     00000001 B
;;;;     00000010 C
;;;;     00000011 D
;;;;
;;;;     00001111 A-substates
;;;;     00000100 AA
;;;;     00001000 AB
;;;;     00001100 AC
;;;;
;;;;     00000111 C-substates
;;;;     00000110 CA
;;;;
;;;;     00011111 D-substates
;;;;     00000111 DA
;;;;     00001011 DB
;;;;     00001111 DC
;;;;     00010011 DD
;;;;     00010111 DE
;;;;     00011011 DF
;;;;
;;;;     01111111 DD-substates
;;;;     00110011 DDA
;;;;     01010011 DDB
;;;;
;;;;     00011111 D-substates D abstract
;;;;     00000011 DA
;;;;     00000111 DB
;;;;     00001011 DC
;;;;     00001111 DD abstract
;;;;     00010011 DE
;;;;     00010111 DF
;;;;
;;;;     00111111 DD-substates DD abstract
;;;;     00001111 DDA
;;;;     00101111 DDB
;;;;
;;;;EXAMPLE:
;;;;
;;;;     (setq states 
;;;;           '(
;;;;             "EFTState" :abstract
;;;;             ("Idle")
;;;;             ("Releasing")
;;;;             ("WaitForListen")
;;;;             ("Listening")
;;;;             ("IncomingConnectionPending")
;;;;             ("OutgoingConnectionPending" :abstract
;;;;              ("OcpConnecting")
;;;;              ("OcpAssociating")
;;;;              ("OcpDisconnecting"))
;;;;             ("Connected" :abstract
;;;;              ("TransferIdle"
;;;;               ("TiAssociating")
;;;;               ("StuHangingUp" :abstract
;;;;                ("StuWaitingForRelease" :abstract
;;;;                 ("StuEndingAccess")
;;;;                 ("StuReleasing"))
;;;;                ("StuDisconnecting")
;;;;                ("StuEndedAccess")
;;;;                ("StuReleased")))
;;;;              ("Sending")
;;;;              ("SendingFile")
;;;;              ("SendAccessing")
;;;;              ("SendEndingAccess")
;;;;              ("WaitingEnd")
;;;;              ("Aborting")
;;;;              ("StartingReceive")
;;;;              ("AbortingReceive")
;;;;              ("EndingReceive")
;;;;              ("Receiving" :abstract
;;;;               ("Waiting")
;;;;               ("StartingFolder")
;;;;               ("StartingFile")
;;;;               ("ReceivingFile")
;;;;               ("EndingFolder")))
;;;;             ))
;;;;
;;;; (insert  (c-mask-and-value-enum-for-states states))
;;;;  
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon 
;;;;MODIFICATIONS
;;;;    1997-06-18 <PJB> Creation.
;;;;    2000-01-07 <PJB> Completed implementation. Adapted to emacs lisp.
;;;;BUGS
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 1990 - 2001
;;;;
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This library is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    Lesser General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Lesser General Public
;;;;    License along with this library; if not, write to the Free Software
;;;;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
;;;;
;;;;******************************************************************************
(require 'pjb-cl)


;; Emacs ;;
(defun string-concat (&rest args)
  (apply 'concat args));;string-concat

(defmacro print-format (format &rest args)
  (list 'insert (append `(format ,format) args)));;print-format

;;; (defun ** (x exp)
;;;   "Computes x**exp = x to the power of exp."
;;;   (cond ((< exp 0) (/ 1.0 (** x (- 0 exp))))
;;;         ((= exp 0) 1)
;;;         ((= exp 1) x)
;;;         (t         (if (= (% exp 2) 0)
;;;                        (let ((x2 (** x (/ exp 2))))
;;;                          (* x2 x2))
;;;                      (let ((x2 (** x (/ (- exp 1) 2))))
;;;                        (* x x2 x2))))))

(defvar code-states-format "{%s, #%s#, %s},\n")

;; clisp ;;
; (load "util")
;
; (defun string-concat (&rest args)
;   (apply 'concatenate (append '(string) args)))
;
; (defmacro print-format (format &rest args)
;   (append `(format t ,format) args))
;
; (setq code-states-format "{~a, #~a#, ~a},~%")
;;;;;;;;;;;


;
; grep '(defun' $file | sed -e 's/(defun/;/' | sort




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mask
;;
;; We implement masks as an ordered list of integer number of bit,
;; to be free from limits on integer values. High bit is first element.
;;

(defun remove-element (list element)
  (cond
   ((null list) nil)
   ((= (car list) element) (cdr list))
   (t (cons (car list) (remove-element (cdr list) element)))));;remove-element


(defun bits-for-count (count)
	(cond
		((< count 0) (error "bits-for-count expects a positive number."))
		((< count 2) 1)
		(t (1+ (bits-for-count (/ count 2))))));;bits-for-count

(defun make-mask-with-set-bits (nbits)
"
RETURN: a new mask with NBITS set from 0 to NBITS-1.
"
  (let ((mask)
        (i 0))
    (while (< i nbits)
      (setq mask (cons i mask)
            i    (1+ i)))
    mask));;make-mask-with-set-bits


(defun make-mask (&optional initial-value)
  "
RETURN: a new mask. If an initial-value is not specified, the new mask is null.
INITIAL-VALUE may be either a list of integer, each one being the number of a bit 
              set in the mask, or an integer atom with the bits of the mask.
"
  (cond
   ((listp initial-value)
    (remove-duplicates (sort initial-value '>)))
   ((integer-or-marker-p initial-value)
    (integer-to-mask initial-value))
   (t (error "The optional argument of make-mask must be either a list of integer or an integer."))));;make-mask

(defun mask-high-bit (mask)
  (car mask));;mask-high-bit

(defun mask-set-bit (mask bit)
  (if (member bit mask)
      mask
    (sort (cons bit mask) '>)));;mask-set-bit

(defun mask-clear-bit (mask bit)
  (remove-element mask bit));;mask-clear-bit
      
(defun mask-or (mask other-mask)
  (remove-duplicates (sort (append mask other-mask) '>)));;mask-or

(defun mask-and (mask other-mask)
  (cond
   ((null mask) nil)
   ((member (car mask) other-mask) 
    (cons (car mask) (mask-and (cdr mask) other-mask)))
   (t (mask-and (cdr mask) other-mask))));;mask-and

(defun mask-shift (mask shift)
  "
 0<shift  ==> left shift.
 0>shift  ==> right shift.
 (shift is added to each bit of mask).
"
  (mapcar (lambda (bit) (+ shift bit)) mask));;mask-shift


(defun integer-to-mask (int-value)
  "
RETURN: a new mask whose bits are thos of int-value.
"
  (let ((mask)
        (i 0)
        (m 1))
    (while (not (= 0 m)) ;; assuming that maxint*2==0 !!!
      (if (= m (logand m int-value))
          (setq mask (cons i mask)))
      (setq i (1+ i)
            m (* 2 m)))
    mask));;integer-to-mask


(defun mask-to-integer (mask)
  "
RETURN: an integer with the (positive) bits of mask set.
"
  (let ((imask 0))
    (while (and mask (<= 0 (car mask)))
      (let ((bit (expt 2 (car mask))))
        (if (= 0 bit)
            (error "Mask contains a bit that does not fit an integer."))
        (setq imask (logior imask bit)
              mask  (cdr mask))))
    imask));;mask-to-integer

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun substates (states)
	(cond
		((null states) ())
		((atom (car states)) (substates (cdr states)))
		(t (cons (car states) (substates (cdr states))))));;substates
		
(defun state-is-abstract (states)
	(member :abstract states));;state-is-abstract



(defun int-to-hex (n)
	(if (< n 16) 
		(nth n '("0" "1" "2" "3" "4" "5" "6" "7" 
                 "8" "9" "a" "b" "c" "d" "e" "f"))
		(string-concat (int-to-hex (truncate (/ n 16))) 
                       (int-to-hex (mod n 16)))));;int-to-hex

(defun complete-hex (str char len)
	(if (< 0 len)
		(complete-hex (string-concat char str) char (- len 1))
		str));;complete-hex
		
(defun mask-to-hex (mask)
	(let ((h (int-to-hex (mask-to-integer mask))))
		(if (< (length h) 8)
			(string-concat "0x" (complete-hex h "0" (- 8 (length h))))
          (string-concat "0x" h))));;mask-to-hex
			




(defun code-states (states current-mask current-bit)
  (let* ((subs (substates states))
         (next-bit (- current-bit (bits-for-count (length subs)))))
    (mapc
     (lambda (state)
       (code-states state (cons current-bit current-mask) next-bit)
       (setq next-bit (- next-bit 1)))
     subs))
  (print-format code-states-format 
                (mask-to-hex current-mask) (car states) (car states)))

;;     ( state & A_mask   ) == A_state
;;     ( state & D_mask   ) == D_state
;;     ( state & DD_mask  ) == DD_state
;;     ( state & DDA_mask ) == DDA_state




(defun compute-code-of-state (state ;; and its substates.
                              value-of-parent ;; (mask)
                              mask-of-state ;; (mask)
                              index-of-state ;; (integer) value 
                              base-of-state ;; (integer) N
                              )
  "
BASE-OF-STATE is a power of 2.
INDEX-OF-STATE is an integer such as:
          BASE-OF-STATE * INDEX-OF-STATE <= (mask-to-integer MASK-OF-STATE).
RETURN: a list of triplets (state-name state-mask state-value)
        (both state-mask and state-value are of type mask).
"
  (cond
   ((not (listp value-of-parent))
    (error "compute-code-of-state: VALUE-OF-PARENT must be a list of integer."))
   ((not (listp mask-of-state))
    (error "compute-code-of-state: MASK-OF-STATE must be a list of integer."))
   ((not (integer-or-marker-p index-of-state))
    (error "compute-code-of-state: INDEX-OF-STATE must be an integer."))
   ((not (integer-or-marker-p base-of-state))
    (error "compute-code-of-state: BASE-OF-STATE must be an integer."))
   ((< base-of-state 0) 
    (error "compute-code-of-state: BASE-OF-STATE must be positive."))
   (t)
   )
  (let* 
      ((value-of-state    (mask-or value-of-parent
                                   (mask-shift (integer-to-mask index-of-state)
                                               base-of-state)))
       (results           (list (list (car state) mask-of-state value-of-state)))
       (subs              (substates state))
       (index-of-substate (if (state-is-abstract state) 0 1))
       (bits-of-substate  (bits-for-count (+ index-of-substate (length subs))))
       (base-of-substate  (if mask-of-state (1+ (mask-high-bit mask-of-state)) 0))
       (mask-of-substate  (mask-or mask-of-state
                                   (mask-shift 
                                    (make-mask-with-set-bits bits-of-substate)
                                    base-of-substate))))
    (mapc (lambda (sub)
              (setq results (append results
                                    (compute-code-of-state sub 
                                                           value-of-state
                                                           mask-of-substate
                                                           index-of-substate
                                                           base-of-substate))
                    index-of-substate (1+ index-of-substate)))
            subs)
    results))





(defun c-mask-and-value-enum-for-states (states)
  (let* ((masks-and-values (compute-code-of-state states 
                                                  (make-mask) ;; value of parent
                                                  (make-mask) ;; mask
                                                  0 ;; index
                                                  0 ;; base
                                                  ))
         (margin           "    ")
         (first            (car masks-and-values))
         (sname            (car first)))
    (concat
     (format "%stypedef enum {\n" margin)
     (apply 'concat
            (mapcan (lambda (nmv)
                      (let ((name  (nth 0 nmv))
                            (mask  (nth 1 nmv))
                            (value (nth 2 nmv))
                            (margin (concatenate 'string margin margin)))
                        (list
                         (format "%s%-50s = %12s,\n" margin
                                 (format "%s_%s_mask" sname name) 
                                 (format "0x%08x" (mask-to-integer mask)))
                         (format "%s%-50s = %12s,\n" margin
                                 (format "%s_%s_state" sname name) 
                                 (format "0x%08x" (mask-to-integer value)))
                         )))
                    (cdr masks-and-values)))
     (format "%s} %s_t;\n" margin sname)
     "\n"
     "#define IN_STATE(state_var,state_name) \\\n"
     "    (((state_var)&state_name#_mask)==state_name##_state)\n"
     )));;c-mask-and-value-enum-for-states



;;;     (setq states 
;;;           '(
;;;             "EFTState" :abstract
;;;             ("Idle")
;;;             ("Releasing")
;;;             ("WaitForListen")
;;;             ("Listening")
;;;             ("IncomingConnectionPending")
;;;             ("OutgoingConnectionPending" :abstract
;;;              ("OcpConnecting")
;;;              ("OcpAssociating")
;;;              ("OcpDisconnecting"))
;;;             ("Connected" :abstract
;;;              ("TransferIdle"
;;;               ("TiAssociating")
;;;               ("StuHangingUp" :abstract
;;;                ("StuWaitingForRelease" :abstract
;;;                 ("StuEndingAccess")
;;;                 ("StuReleasing"))
;;;                ("StuDisconnecting")
;;;                ("StuEndedAccess")
;;;                ("StuReleased")))
;;;              ("Sending")
;;;              ("SendingFile")
;;;              ("SendAccessing")
;;;              ("SendEndingAccess")
;;;              ("WaitingEnd")
;;;              ("Aborting")
;;;              ("StartingReceive")
;;;              ("AbortingReceive")
;;;              ("EndingReceive")
;;;              ("Receiving" :abstract
;;;               ("Waiting")
;;;               ("StartingFolder")
;;;               ("StartingFile")
;;;               ("ReceivingFile")
;;;               ("EndingFolder")))
;;;             ))
  
;;;     (insert  (c-mask-and-value-enum-for-states states))


  
;;;; pjb-state-coding.el              -- 2003-12-04 05:33:31 -- pascal   ;;;;
