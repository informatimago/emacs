;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;******************************************************************************
;;;;FILE:               pjb-bourse.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    This module exports
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon 
;;;;MODIFICATIONS
;;;;    199?/??/?? <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 1990 - 2011
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
(require 'pjb-list)

(require 'pjb-utilities)
(require 'pjb-euro)
(require 'pjb-strings)
(require 'pjb-object)

(provide 'pjb-bourse)

(defun debug-tag () nil)

(defun percent (num denum)
  (if (or (null denum) (= denum 0.0))
    0.0
    (* 100.0 (/ num (+ 0.0 denum))))
  );;percent


(defun class-attributes (c)
  (aref (get c 'eieio-class-definition) class-public-a)
  );;class-attributes

;;------------------------------------------------------------------------------
;;
;;  +-----------+               +-----------+               +----------------+
;;  | Portfolio |---------------|   Line    |---------------|   Position     |
;;  +-----------+ 0,1       0,n +-----------+ 0,1       0,n +----------------+
;;        |                     | symbol    |               | buy-quantity   |
;;        |                     | devise    |               | sell-quantity  |
;;        |                     +-----------+               | buy-amount     |
;;        |                                                 | sell-ammount   |
;;        |                                                 | comissions     |
;;        |                                                 | nb-operations  |
;;        | n                                               | open-date      |
;;  +----------------+        +---------------+             | last-date      |
;;  | DeviseAccount  |        |   BuySellOp   |             | /state         |
;;  +----------------+        +---------------+             +----------------+
;;                                    |
;;                                   / \
;;                           +------+---+------+
;;                           |                 |
;;                      +----------+      +-----------+
;;                      |  BuyOp   |      |  SellOp   |
;;                      +----------+      +-----------+
;;
;;------------------------------------------------------------------------------
;;
;;
;;         +-------------------------------------------+
;;         |                Position                   |
;;         +-------------------------------------------+
;;         | buy-amount                                |
;;         | buy-quantity                              |
;;         | sell-ammount                              |
;;         | sell-quantity                             |
;;         | comissions                                |
;;         | nb-operations                             |
;;         | open-date                                 |
;;         | last-date                                 |
;;         +-------------------------------------------+
;;         | state                                     |
;;         | is-closed                                 |
;;         | is-running                                |
;;         | update-with-operation(operation)          |
;;         | amount-invested                           |
;;         | quantity                                  |
;;         | gain                                      |
;;         | paid-per-share                            |
;;         | owner-line                                |
;;         +-------------------------------------------+
;;                         1,n | positions {ordered}
;;                             |
;;                             |
;;                             |
;;                             |
;;                         0,1 | owner-line
;;         +-------------------------------------------+
;;         |                 Line                      |
;;         +-------------------------------------------+
;;         | symbol                                    |
;;         | devise                                    |
;;         +-------------------------------------------+
;;         | buy-amount       -->                      |
;;         | buy-quantity     -->                      |
;;         | sell-amount      -->                      |
;;         | sell-quantity    -->                      |
;;         | comission        -->                      |
;;         | nb-operations    -->                      |
;;         |                                           |
;;         | amount-invested  -->                      |
;;         | quantity         -->                      |
;;         | gain             -->                      |
;;         | paid-per-share   -->                      |
;;         |                                           |
;;         | last-position                             |
;;         | open-new-position                         |
;;         |                                           |
;;         | average-comission                         |
;;         | total-buy-amount                          |
;;         | total-gain                                |
;;         | update-with-operation                     |
;;         +-------------------------------------------+
;;                    1,1 [ symbol ] lines
;;                             |
;;                             |
;;                             |
;;                             |
;;                         0,1 |
;;         +-------------------------------------------+
;;         |                 Portfolio                 |
;;         +-------------------------------------------+
;;         | operations                                |
;;         | lines                                     |
;;         | total-credit                              |
;;         | total-debit                               |
;;         | total-comissions                          |
;;         | total-operations                          |
;;         +-------------------------------------------+
;;         | add-line(line)                            |
;;         | add-operation(operation)                  |
;;         | line-with-symbol(symbol)                  |
;;         | sort-portfolio-lines                      |
;;         | total-liquidite                           |
;;         | total-opcom                               |
;;         +-------------------------------------------+




;;;---------------------------------------------------------------------
;;;--- DeviseAccount ---------------------------------------------------
;;;---------------------------------------------------------------------

(defclass DeviseAccount (PjbObject)
  ((amounts  
    :initform nil
    :accessor amounts
    :type     list
    :documentation
    "The alist where the (devise . amount) are stored."))
  (:documentation
   "A DeviseAccount is an account where the amount is stored separately
for each devise [See: pjb-euro].")
  );;DeviseAccount


(defmethod amount-at-devise ((self DeviseAccount) devise)
  "Retourne le montant de la devise indiquée dans le devise-account.
Voir account-valuation pour la valeur totale du devise-account."
  (cdr (assoc devise (amounts self)))
  );;amount-at-devise

(defmethod devises ((self DeviseAccount))
  "Retourne la liste des devises dans le compte."
  (mapcar 'car (amounts self))
  );;devises

(defmethod account-valuation ((self DeviseAccount) devise)
  "Retourne la valeur du devise-account dans la devise indiquée.
Utilise pjb-euro, et nécessite pour les devises flottantes, des cours à jour."
  (let ((total-euro 0.0))
    (mapc (lambda (a)
            (setq total-euro (+ total-euro
                                (euro-from-value (cdr a) (car a)))))
          (amounts self))
    (euro-to-value devise total-euro))
  );;account-valuation


(defmethod account-add ((self DeviseAccount) devise montant)
  "Additionne au devise-account le montant indiqué dans la devise indiquée.
Pour additionner deux comptes, utiliser account-add-account.
Return: self."
  (let ((ligne (assoc devise (amounts self))))
    (if ligne
      (setcdr ligne (+ (cdr ligne) montant))
      (setf (slot-value self 'amounts)
            (cons (cons devise montant) (amounts self)))))
  self);;account-add


(defmethod account-sub ((self DeviseAccount) devise montant)
  "Soustrait du devise-account le montant indiqué dans la devise indiquée.
Pour soustraire deux comptes, utiliser account-sub-account.
Return: self."
  (let ((ligne (assoc devise (amounts self))))
    (if ligne
      (setcdr ligne (- (cdr ligne) montant))
      (setf (slot-value self 'amounts)
            (cons (cons devise (- 0 montant)) (amounts self)))))
  self);;account-sub


(defmethod account-mul ((self DeviseAccount) facteur)
  "Multiplie le devise-account par le facteur.
RETURN: self"
  (mapc (lambda (devise-amount)
          (setcdr devise-amount (* (cdr devise-amount) facteur)))
        (amounts self))
  self);;account-mul



(defun compare-sequal-sn-c  (a b)
  "Interne DeviseAccount."
  (string-equal (symbol-name (car a))
                (symbol-name (car b)))
  );;compare-sequal-sn-c

(defun compare-slessp-sn-c  (a b)
  "Interne DeviseAccount."
  (string-lessp (symbol-name (car a))
                (symbol-name (car b)))
  );;compare-slessp-sn-c

(defun compare-slessp-sn    (a b)
  "Interne DeviseAccount."
  (string-lessp (symbol-name  a)
                (symbol-name  b))
  );;compare-slessp-sn

(defmethod sorted-accounts ((self DeviseAccount))
  "Returns a list of assoc (devise . amount) sorted on the devise."
  (sort (copy-sequence (slot-value self 'amounts)) 'compare-slessp-sn-c)
  );;sorted-accounts


(defmethod account-operation-account ((self DeviseAccount)
                                      (other DeviseAccount) op-lambda)
  "Retourne un devise-account résultat de l'opération op-lambda sur
les paires de montants de même devise, et ajoute les montants-devise
restants."
  (let ((sort-a (sorted-accounts self))
        (sort-b (sorted-accounts other))
        (result nil)
        (oresult (make-instance DeviseAccount)))
    (while (or sort-a sort-b)
      (let ((a (car sort-a)) (b (car sort-b)))
        (setq result
              (cons
               (cond
                ((and a b (compare-sequal-sn-c a b)) ;; les deux sont égaux
                 (setq sort-a (cdr sort-a) ;; on les passe
                       sort-b (cdr sort-b))
                 ;; resultat : l'operation appliquee sur eux.
                 (cons (car a) (funcall op-lambda (cdr a) (cdr b))))

                ((or (null b)
                     (compare-slessp-sn-c a b)) ;; lequel est le plus petit ?
                 (setq sort-a (cdr sort-a)) ;; on le passe.
                 ;; resultat : l'operation appliquee sur lui.
                 (cons (car a) (funcall op-lambda (cdr a) 0.0)))

                (t ;; lequel reste ?
                 (setq sort-b (cdr sort-b)) ;; on le passe.
                 ;; resultat : l'operation appliquee sur lui.
                 (cons (car b) (funcall op-lambda 0.0 (cdr b)))))
               result))))
    (setf (slot-value oresult 'amounts) result)
    oresult)
  );;account-operation-account


(defmethod account-add-account ((self DeviseAccount) (other DeviseAccount))
  "Additionne les deux comptes-devises."
  (account-operation-account self other  '+)
  );;account-add-account

(defmethod account-sub-account ((self DeviseAccount) (other DeviseAccount))
  "Soustrait le devise-account compte-b du devise-account compte-a."
  (account-operation-account self other  '-)
  );;account-sub-account

;;;END DeviseAccount ---------------------------------------------------
;;;---------------------------------------------------------------------






;;;---------------------------------------------------------------------
;;;--- Operation -------------------------------------------------------
;;;---------------------------------------------------------------------

(defclass Operation (PjbObject)
  (
   (date 
    :initform nil
    :initarg  :date
    :accessor date
    :type     symbol
    :documentation "Date of the Operation.
This is a symbol with the format: YYYY-MM-DD.")
   (symbol 
    :initform nil
    :initarg  :symbol
    :accessor symbol
    :type     symbol
    :documentation "The ticker symbol of the shares handled in this operation.")
   )
  (:documentation "
Reification of a single operation.
This is an abstract class. Concrete subclasses may be buy or sell operations
or split operations.
Instances of theses subclasses are made by make-operation.
")
  );;Operation


(defmethod apply-to-position ((self Operation) (position Position))
  "NOTE:   This method must be overriden by subclasses.
PRE:    (equal (symbol self) (symbol (owner-line position)))
DO:     Apply self operation onto the position."
  (error "Method apply-to-line must be overriden by subclasses.")
  );;apply-to-position


;; ---------- --- ---------- ---- -------- ------ ------ ------------
;; DATE       DEV MONTANT    QUTE COURS    FRAIS  FRAIS% SYMBOL     
;; YYYY-MM-DD --ERROR:  OPERATION CLASS CANNOT DISPLAY-- SSSSSSSSSSSS
;; YYYY-MM-DD DEV 0000000.00 0000 00000.00 000.00 00.00% SSSSSSSSSSSS
;; YYYY-MM-DD SPLIT 0000 NEW SHARES FOR 0000 OLD SHARES  SSSSSSSSSSSS

(defconstant operation-format-bad 
  "%-10s --ERROR:  OPERATION CLASS CANNOT DISPLAY-- %s"
  "Format to print an abstract operation.");;operation-format-bad

(defconstant operation-format-buy-sell 
  "%-10s %-3s %10.2f %4d %8.2f %6.2f %5.2f%% %s"
  "Format to print a buy or sell operation.");;operation-format-buy-sell

(defconstant operation-format-split
  "%-10s SPLIT %4d NEW SHARES FOR %4d OLD SHARES  %s"
  "Format to print a split operation.");;operation-format-split

(defmethod as-string ((self Operation))
  "RETURN: A human readable string representing the operation."
  (format operation-format-bad (date self) (symbol self))
  );;as-string

;;;END Operation -------------------------------------------------------
;;;---------------------------------------------------------------------




;;;---------------------------------------------------------------------
;;;--- Position --------------------------------------------------------
;;;---------------------------------------------------------------------
(defclass Position (PjbObject)
  ((buy-quantity  
    :initform 0
    :initarg  :buy-quantity
    :accessor buy-quantity
    :type     number
    :documentation "Total number of share bought.
    buy-quantity>=0.")
   (buy-amount 
    :initform 0.0
    :initarg  :buy-amount
    :accessor buy-amount
    :type     number
    :documentation "Total amount paid.
    This includes the comission paid for the buys.
    (Expressed in the devise of the line).")
   (sell-quantity
    :initform 0
    :initarg  :sell-quantity
    :accessor sell-quantity
    :type     number
    :documentation "Total number of share sold.
    sell-quantity>=0.")
   (sell-amount  
    :initform 0.0
    :initarg  :sell-amount
    :accessor sell-amount
    :type     number
    :documentation "Total amount received for sells.
    This includes the deducted comission paid for the sells.
    (Expressed in the devise of the line).")
   (comission  
    :initform 0.0
    :initarg  :comission
    :accessor comission
    :type     number
    :documentation "Total comissions paid.
    (Expressed in the devise of the line).")
   (nb-operations
    :initform 0
    :initarg  :nb-operations
    :accessor nb-operations
    :type     number
    :documentation "Number of operations done.")
   (open-date 
    :initform '0000-00-00
    :initarg  :open-date
    :accessor open-date
    :type     symbol
    :documentation "Date of the first operation.")
   (last-date
    :initform '0000-00-00
    :initarg  :last-date
    :accessor last-date
    :type     symbol
    :documentation "Date of the last operation.")
   (owner-line  
    :initform nil
    :initarg  :owner-line
    :accessor owner-line
    :documentation "The Line instance that owns this position."))
  (:documentation "
Reification of a position. This is the summary of a range of
operations where the number of posseded shares only comes to 0 when
the position is closed.  A Line posses several successive Position
instances, of which all but the last must be closed.
")
  );;Position


(defmethod as-string ((self Position))
  "RETURN: a human readable string describing the position."
  ;; (insert (apply 'concat
  ;;         (mapcar (lambda (x) (format "(format \"    %-20s=%%S\\n\" (%s self))\n"
  ;;                                     (symbol-name x) (symbol-name x)))
  ;;                 (class-attributes Position))))
  (concat
   "Position {\n"
   (format "    buy-quantity        =%S\n" (buy-quantity self))
   (format "    buy-amount          =%S\n" (buy-amount self))
   (format "    sell-quantity       =%S\n" (sell-quantity self))
   (format "    sell-amount         =%S\n" (sell-amount self))
   (format "    comission           =%S\n" (comission self))
   (format "    nb-operations       =%S\n" (nb-operations self))
   ;;   (format "    open-date           =%S\n" (open-date self))
   ;;   (format "    last-date           =%S\n" (last-date self))
   ;;   (format "    owner-line          =%S\n" (owner-line self))
   "}\n")
  );;as-string




(defmethod state ((self Position))
  "RETURN: the state of the Position.
        Either: 'newborn when no operation has been included;
                'running when operations have been included,
                              but (quantity self) never has been 0;
                'closed  once (quantity self) reach 0."
  (cond
   ((= 0 (nb-operations self)) 'newborn)
   ((= 0 (quantity self))      'closed)
   (t                          'running))
  );;state


(defmethod is-closed ((self Position))
  "RETURN: Whether (equal (state self) 'closed)."
  (equal (state self) 'closed)
  );;is-closed


(defmethod is-running ((self Position))
  "RETURN: Whether (equal (state self) 'running)."
  (equal (state self) 'running)
  );;is-running


(defmethod update-with-operation ((self Position) (operation Operation))
  "PRE:    (not (is-closed self))
DO:     Updates this position with the given operation.
RETURN: self."
  (if (equal 'closed (state self))
    (let ((msg (format "POSITION FOR %s IS CLOSED.\n"
                 (symbol (owner-line self)))))
      (printf msg)
      (error msg)))
  (if (equal (state self) 'newborn)
    (setf (slot-value self 'open-date) (date operation)))
  (setf (slot-value self 'last-date) (date operation))
  (apply-to-position operation self)
  (setf (slot-value self 'nb-operations) (1+ (nb-operations self)))
  self);;update-with-operation



(defmethod amount-invested   ((self Position))
  "RETURN: The amount mobilized on this position.
        amount-invested =  (- (buy-amount self) (sell-amount self)).
        When is-closed, this is the lost (if positive)
                             or the gain (if negative)."
  (- (buy-amount self) (sell-amount self))
  );;amount-invested


(defmethod quantity ((self Position))
  "RETURN: The number of share remaining in the position.
        quantity = (- (buy-quantity self) (sell-quantity self))"
  (- (buy-quantity self) (sell-quantity self))
  );;quantity


(defmethod gain ((self Position))
  "RETURN: The gain on this position, negative if there's a loss.
        This is valid only when the position is closed:
        is-closed       => gain = (- sell-amount buy-amount)
        (not is-closed) => gain = 0.0"
  (if (is-closed self)
    (- (sell-amount self) (buy-amount self))
    0.0)
  );;gain


(defmethod paid-per-share ((self Position))
  "RETURN: The cost of the remaining shares.
        This is valid only when running.
        when the position is not running, or if the amount-invested is negative,
        then paid-per-share = 0.0
        else paid-per-share = (/ (amount-invested self) quantity)."
  (if (equal 'running (state self))
    (if (<= (amount-invested self) 0.0)
      ;; We already have a gain.
      0.0
      (/ (amount-invested self) (quantity self)))
    0.0)
  );;paid-per-share

;;;END Position --------------------------------------------------------
;;;---------------------------------------------------------------------





;;;---------------------------------------------------------------------
;;;--- SplitOp ---------------------------------------------------------
;;;---------------------------------------------------------------------

(defclass SplitOp (Operation)
  (
   (oldQuantity  
    :initform 0
    :initarg  :oldQuantity
    :accessor oldQuantity
    :type     number
    :documentation "Number of old shares.
oldQuantity>=0")
   (newQuantity  
    :initform 0
    :initarg  :newQuantity
    :accessor newQuantity
    :type     number
    :documentation "Number of new shares.
newQuantity>=0")
   )
  (:documentation "
A split operation. 
This kind of operation replaces oldQuantity shares by newQuantity shares.
An allocation of new (free) shares can be modelized by a split from the 
oldQuantity required to the newQuantity = number of new share + oldQuantity.
")
  );;SplitOp


(defmethod apply-to-position ((self SplitOp) (position Position))
  "NOTE:   This method must be overriden by subclasses.
PRE:    (equal (symbol self) (symbol (owner-line position)))
DO:     Apply this split operation onto the position."

  ;;DEBUG;;  (message (format "SplitOp::apply-to-position \n    split=%S \n%s"  self (as-string position)))

  ;; TODO: This should go into a Position::split method.
  (setf (slot-value position 'sell-quantity) 
        (/ (*  (sell-quantity position) (newQuantity self)) (oldQuantity self)))
  (setf (slot-value position 'buy-quantity) 
        (/ (*  (buy-quantity position) (newQuantity self)) (oldQuantity self)))

  ;;DEBUG;;  (message (format "%s\n" (as-string position)))

  );;apply-to-position


(defmethod as-string ((self SplitOp))
  "RETURN: A human readable string representing the operation."
  (format operation-format-split
    (date self) (newQuantity self)  (oldQuantity self) (symbol self))
  );;as-string

;;;END SplitOp ---------------------------------------------------------
;;;---------------------------------------------------------------------




;;;---------------------------------------------------------------------
;;;--- BuySellOp -------------------------------------------------------
;;;---------------------------------------------------------------------

(defclass BuySellOp (Operation)
  (
   (quantity   
    :initform 0
    :initarg  :quantity
    :accessor quantity
    :type     number
    :documentation "Number of share bought or sold.
quantity>=0")
   
   (devise   
    :initform nil
    :initarg  :devise
    :accessor devise
    :type     symbol
    :documentation "The devise symbol (see package pjb-euro).")

   (price       
    :initform 0.0
    :initarg  :price
    :accessor price
    :type     number
    :documentation "The price for one share on this operation.
The price is expressed in the devise of the operation.
price>0.0")

   (comission
    :initform 0.0
    :initarg  :comission
    :accessor comission
    :type     number
    :documentation "The comission value for this operation.
The comission is expressed in the devise of the operation.
comission>=0.0")

   )
  (:documentation "
Reification of a single buy or sell operation.
This is an abstract class. Concrete subclasses are BuyOp and SellOp.
Instances of theses subclasses are made by make-operation.
")
  );;BuySellOp




(defmethod amount-base ((self BuySellOp))
  "RETURN: The total amount of this operation, excluding the comission."
  (* (quantity self) (price self))
  );;amount-base


(defmethod comission-percent ((self BuySellOp))
  "RETURN: The percentage the comission represents relatively to the share value."
  (percent (comission self) (amount-base self))
  );;comission-percent


(defmethod amount-paid ((self BuySellOp))
  "RETURN: The amount paid for the operation. Negative when it's a sell operation."
  (+ (* (signed-quantity self) (price self)) (comission self))
  );;amount-paid


(defmethod signed-quantity ((self BuySellOp))
  "RETURN: The quantity.
NOTE:   Should be overriden by sell operation to return the opposite."
  (quantity self)
  );;signed-quantity

         
(defmethod as-string ((self BuySellOp))
  "RETURN: A human readable string representing the operation."
  (format operation-format-buy-sell
    (date self) (devise self) 
    (amount-paid self) (signed-quantity self) (price self) 
    (comission self) (comission-percent self) (symbol self))
  );;as-string

;;;END SellBuyOp -------------------------------------------------------
;;;---------------------------------------------------------------------



;;;---------------------------------------------------------------------
;;;--- BuyOp -----------------------------------------------------------
;;;---------------------------------------------------------------------

(defclass BuyOp (BuySellOp) ()
  (:documentation "
Reification of a buy operation.
")
  );;BuyOp


(defmethod amount  ((self BuySellOp))
  "RETURN: The total amount aid of this operation, including the comission.
        For buys, it's quantity*price+comission."
  (+ (* (quantity self) (price self)) (comission self))
  );;amount


(defmethod apply-to-position ((self BuyOp) (position Position))
  "PRE:    (equal (symbol self) (symbol (owner-line position))),
        (equal (devise self) (devise (owner-line position)))
DO:     Apply this buy operation onto the position."
  (if (not (equal (devise (owner-line position)) (devise self)))
    (let ((msg (format "DEVISE MISMATCH WITH LINE FOR %s: %s %s\n"
                 (symbol self) 
                 (devise (owner-line position)) (devise self))))
      (printf msg)
      (error msg)))

  ;;DEBUG;;  (message (format "BuyOp::apply-to-position \n    buy=%S \n%s" self (as-string position)))

  ;; TODO: This should go into a Position::buy method.
  (setf (slot-value position 'buy-quantity) (+ (buy-quantity  position) (quantity self)))
  (setf (slot-value position 'buy-amount)   (+ (buy-amount    position) (amount-paid self)))
  (setf (slot-value position 'comission)    (+ (comission     position) (comission self)))

  ;;DEBUG;;  (message (format "%s\n" (as-string position)))

  );;apply-to-position

;;;END BuyOp -----------------------------------------------------------
;;;---------------------------------------------------------------------



;;;---------------------------------------------------------------------
;;;--- SellOp ----------------------------------------------------------
;;;---------------------------------------------------------------------

(defclass SellOp (BuySellOp) ()
  (:documentation "
Reification of a sell operation.
")
  );;SellOp

(defmethod amount  ((self BuySellOp))
  "RETURN: The total amount paid for this operation, including the comission.
        For sells, it's quantity*price-comission"
  (- (* (quantity self) (price self)) (comission self))
  );;amount

(defmethod signed-quantity ((self SellOp))
  "RETURN: The opposite of the quantity, to denote a sell."
  (- 0 (quantity self))
  );;signed-quantity


(defmethod apply-to-position ((self SellOp) (position Position))
  "PRE:    (equal (symbol self) (symbol (owner-line position))),
        (equal (devise self) (devise (owner-line position)))
DO:     Apply this sell operation onto the position."
  (if (not (equal (devise (owner-line position)) (devise self)))
    (let ((msg (format "DEVISE MISMATCH WITH LINE FOR %s: %s %s\n"
                 (symbol self) 
                 (devise (owner-line position)) (devise self))))
      (printf msg)
      (error msg)))

  ;;DEBUG;;  (message (format "Sell::apply-to-position \n    buy=%S \n%s"  self (as-string position)))

  ;; TODO: This should go into a Position::sell method.
  (setf (slot-value position 'sell-quantity) (+ (sell-quantity position) (quantity self)))
  (setf (slot-value position 'sell-amount)   (- (sell-amount   position) (amount-paid self)))
  ;; amount-paid < 0 == sell-amount is incremented.
  ;; (Note if quantity*price < commission then amount-paid > 0)
  (setf (slot-value position 'comission)     (+ (comission     position) (comission self)))

  ;;DEBUG;;  (message (format "%s\n" (as-string position)))

  );;apply-to-position

;;;END SellOp ----------------------------------------------------------
;;;---------------------------------------------------------------------


(defun make-operation (attributes)
  "RETURN:  either a new BuyOp (quantity>=0)
         or a new SellOp (quantity<0) instance built from the given attributes."
  (if (eq 'SPLIT (nth 1 attributes))
    (let ((date         (nth 0 attributes))
          (newQuantity  (nth 2 attributes))
          (oldQuantity  (nth 3 attributes))
          (symbol       (nth 4 attributes)))
      (make-instance (class-constructor SplitOp)
        (format "%s-%s" symbol date)
        'date date
        'newQuantity newQuantity
        'oldQuantity oldQuantity
        'symbol symbol))
    (let ((date      (nth 0 attributes))
          (quantity  (nth 1 attributes))
          (devise    (nth 2 attributes))
          (price     (nth 3 attributes))
          (comission (nth 4 attributes))
          (symbol    (nth 5 attributes)))
      (make-instance
          (if (< 0 quantity) (class-constructor BuyOp) (class-constructor SellOp))
        (format "%s-%s" symbol date)
        :date date
        :quantity (abs quantity)
        :devise devise
        :price price
        :comission comission
        :symbol symbol)))
  );;make-operation




;;;---------------------------------------------------------------------
;;;--- Line ------------------------------------------------------------
;;;---------------------------------------------------------------------

(defclass Line (PjbObject)
  ((symbol  
    :initform nil
    :initarg  :symbol
    :accessor symbol
    :type     symbol
    :documentation
    "The ticker symbol of this line (stored as a lisp symbol).")
   (devise     
    :initform nil
    :initarg  :devise
    :accessor devise
    :type     symbol
    :documentation
    "The devise in which the shares of this line are dealt.
 (See: pjb-euro).")
   (positions      
    :initform nil
    :accessor positions
    :type     list
    :documentation
    "The list of successive positions for this line.
They're stored the last one first."))
  (:documentation   "
Reification of a Portfolio Line, where all the operations regarding a share
are accumulated.
")
  );;Line


(defmethod update-with-operation ((self Line) (operation BuySellOp))
  "PRE:    (and (or (null (devise self)) (equal (devise self) (devise operation)))
             (or (null (symbol self)) (equal (symbol self) (symbol operation))))
POST:   (and (equal (devise self) (devise operation))
             (equal (symbol self) (symbol operation)))
DO:     Updates this line with the given operation.
RETURN: self."
  (if (null (devise self))
    (setf (slot-value self 'devise) (devise operation)))
  (if (null (symbol self))
    (setf (slot-value self 'symbol) (symbol operation)))
  (if (not (equal (symbol self) (symbol operation)))
    (let ((msg (format "SYMBOL MISMATCH WITH LINE FOR %s: %s\n"
                 (symbol self) (symbol operation))))
      (printf msg)
      (error msg)))

  (if (is-closed (last-position self))
    (open-new-position self))
  (update-with-operation (last-position self) operation)
  );;update-with-operation


(defmethod open-new-position ((self Line))
  "PRE:   (is-closed (last-position self))
POST:   self has a new newbord position ready to be filled with operations.
RETURN: self"
  (let ((p (car (slot-value self 'positions))))
    (if (and p (not (is-closed p)))
      (error "The last position is not closed."))
    (setf (slot-value self 'positions) 
          (cons (make-instance Position
                  (format "%s-%d" (symbol self) (length p))
                  :owner-line self)
                (slot-value self 'positions))))
  self);;open-new-position


(defmethod last-position ((self Line))
  "RETURN: The last position of the line."
  (if (null (slot-value self 'positions))
    (open-new-position self))
  (car (slot-value self 'positions))
  );;last-position


;; Last position data:

(defmethod quantity ((self Line))
  "RETURN: The number of share of the last postion.
        (All the previous positions have a number of 0 share remaining...)."
  (quantity (last-position self))
  );;quantity

(defmethod amount-invested ((self Line))
  "RETURN: The amount-invested of the last position."
  (if (is-running (last-position self))
    (amount-invested (last-position self))
    0.0)
  );;amount-invested

(defmethod comission ((self Line))
  "RETURN: The comission paid for the last position."
  (if (is-running (last-position self))
    (comission (last-position self))
    0.0)
  );;comission

(defmethod nb-operations ((self Line))
  "RETURN: The number of operation of the last position.
        (used for average-comission)"
  (nb-operations (last-position self))
  );;nb-operations


(defmethod average-comission ((self Line))
  "RETURN: The average comission paid for the last position."
  (/ (comission self) (nb-operations self))
  );;average-comission


(defmethod paid-per-share ((self Line))
  "RETURN: The price paid per share for the last position."
  (paid-per-share (last-position self))
  );;paid-per-share


(defmethod gain ((self Line))
  "RETURN: The gain of the last position."
  (gain (last-position self))
  );;gain


(defmethod buy-amount ((self Line))
  "RETURN: The buy amount of the last position of the line."
  (buy-amount (last-position self))
  );;buy-amount


(defmethod sell-amount ((self Line))
  "RETURN: The sell amount of the last position of the line."
  (sell-amount (last-position self))
  );;sell-amount


(defmethod buy-quantity ((self Line))
  "RETURN: The buy quantity of the last position of the line."
  (buy-quantity (last-position self))
  );;buy-quantity


(defmethod sell-quantity ((self Line))
  "RETURN: The sell quantity of the last position of the line."
  (sell-quantity (last-position self))
  );;sell-quantity


;; Totals over closed positions:

(defmethod closed-buy-amount ((self Line))
  "RETURN: The sum over closed positions of (amount-base position).
        (used to compute the percentage gain)."
  (apply '+ (mapcar (lambda (pos) (buy-amount pos)) 
                    (if (is-closed (last-position self))
                      (positions self)
                      (cdr (positions self)))))
  );;closed-buy-amount

(defmethod closed-gain ((self Line))
  "RETURN: The sum over closed positions of (gain position)."
  (apply '+ (mapcar (lambda (pos) (gain pos)) 
                    (if (is-closed (last-position self))
                      (positions self)
                      (cdr (positions self)))))
  );;closed-gain

;;;END Line ------------------------------------------------------------
;;;---------------------------------------------------------------------






;;------------------------------------------------------------------------
;;--- Portfolio ----------------------------------------------------------
;;------------------------------------------------------------------------

(defclass Portfolio (PjbObject)
  (
   (operations  
    :initform nil
    :accessor operations
    :type     list
    :documentation
    "The list of operations applied to this portfolio.")

   (lines  
    :initform nil
    :accessor lines
    :type     list
    :documentation "The alist of (symbol . Line).")

   ;; BuySellOp totals:
   ;; -----------------
   ;; total-opcom    total-comissions   total-comissions/total-amount-base (%)
   ;; total-credit   total-debit

   (total-opcom   
    :initform (lambda () (make-instance DeviseAccount))
    :accessor total-opcom
    :documentation 
    "The sum over operations of (amount operation).")

   (total-amount-base 
    :initform (lambda () (make-instance DeviseAccount))
    :accessor total-amount-base
    :documentation
    "The sum over operations of (amount-base operation).")
   
   (total-comissions 
    :initform (lambda () (make-instance DeviseAccount))
    :accessor total-comissions
    :documentation 
    "The sum over operations of (comission operation).")

   (total-credit 
    :initform (lambda () (make-instance DeviseAccount))
    :accessor total-credit
    :documentation 
    "The sum over sell operations of (amount operation).")

   (total-debit 
    :initform (lambda () (make-instance DeviseAccount))
    :accessor total-debit
    :documentation 
    "The sum over buy operations of (amount operation).")

   ;; Line row:
   ;; ---------
   ;; SYMBOL       QUTE  DEV PAYE       FRAIS  REVIENT   BENEFICE  BENEFI%
   ;;              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^
   ;;              Over running position                 Over closed positions.
   ;; Line totals:
   ;; ------------
   ;;                        PAYE                        BENEFICE  BENEFI%
   ;;              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^
   ;;                   Total over running positions     Total over closed pos.
   ;; total-invested  total-closed-gains  total-closed-gains/total-closedbase (%)

   (total-invested 
    :initform (lambda () (make-instance DeviseAccount))
    :accessor total-invested
    :documentation 
    "The sum over lines of (amount-invested line).")

   (total-closed-gains 
    :initform (lambda () (make-instance DeviseAccount))
    :accessor total-closed-gains
    :documentation 
    "The sum over lines of (closed-gain line).")

   (total-closed-base 
    :initform (lambda () (make-instance DeviseAccount))
    :accessor total-closed-base
    :documentation 
    "The sum over lines of (closed-buy-amount line).")
   )
  (:documentation "
Reification of a Portfolio.
")
  );;Portfolio


(defmethod add-operation ((self Portfolio) (op BuySellOp))
  "DO:     Add an operation.
PRE:    (date (last (operations self))) <= (date operation)
        to ensure that the positions in the lines are not closed unduly.
POST:   operation is appended to operations.
RETURN: self."

  ;; Append the operation.
  (if (operations self)
    (nconc (operations self) op)
    (setf (slot-value self 'operations) (cons op nil)))

  ;; Update the line.
  (let ((line (line-with-symbol self (symbol op))))
    (if (null line)
      (progn
        (setq line (make-instance Line (symbol-name (symbol op))
                                  :symbol (symbol op)
                                  :devise (devise op)))
        (add-line self line))
      ) ;;if
    (update-with-operation line op)
    )

  self);;add-operation


(defmethod compute-operation-totals ((self Portfolio))
  "DO:     compute the following totals from the operations.
        total-opcom, total-amount-base, total-comissions, 
        total-credit, total-debit 
RETURN: self."

  ;; 1- Reset the totals.
  (setf (slot-value self 'total-opcom)       (make-instance DeviseAccount))
  (setf (slot-value self 'total-amount-base) (make-instance DeviseAccount))
  (setf (slot-value self 'total-comissions)  (make-instance DeviseAccount))
  (setf (slot-value self 'total-credit)      (make-instance DeviseAccount))
  (setf (slot-value self 'total-debit)       (make-instance DeviseAccount))

  ;; 2- Loop over the operations.
  (let ((ops (operations self))
        (op))
    (while ops
      (setq op  (car ops)
            ops (cdr ops))
      ;; TODO: We should move this into the Operation class and subclasses...
      (if (not (eq (class-of op) SplitOp))
        (let ((dev (devise op)))
          ;; Update totals.
          (account-add (total-opcom self)       dev (amount op))
          (account-add (total-amount-base self) dev (amount-base op))
          (account-add (total-comissions self)  dev (comission op))
          (if (eq (class-of op) SellOp)
            (account-add (total-credit self) dev (amount op))
            (account-add (total-debit self)  dev (amount op)))
          )
        ) ;;if
      )
    ) ;;while ops
  self);;compute-operation-totals




(defmethod add-line ((self Portfolio) (line Line))
  "
DO:   Add the line to the portfolio.
PRE:  No other line with the same symbol should exist in the portfolio.
POST: (eq (line-with-symbol self (symbol line)) line)"
  (if (line-with-symbol self (symbol line))
    (error "There is already a line for the symbol %s in the portfolio."
           (symbol line)))
  (setf (slot-value self 'lines) (cons (cons (symbol line) line) (lines self)))
  self);;add-line


(defmethod sort-portfolio-lines ((self Portfolio))
  ;; sort-lines is an emacs function!
  "DO:     Sort the lines list on the symbol.
RETURN: self."
  (setf (slot-value self 'lines) 
        (sort (lines self)
              (lambda (a b)
                (setq a (symbol-name (car a))
                      b (symbol-name (car b)))
                (let ((ia (string-index a "."))
                      (ib (string-index b ".")))
                  (if ia
                    (if ib
                      (if (string-equal (substring a ia) (substring b ib))
                        (string-lessp a b)
                        (string-lessp  (substring a ia) (substring b ib)))
                      nil)
                    (if ib
                      t
                      (string-lessp a b)))))))
  self);;sort-portfolio-lines


(defmethod compute-line-totals ((self Portfolio))
  "DO:     compute the following totals from the lines.
           total-invested, total-closed-gains, total-closed-base.
RETURN: self."

  ;; 1- Reset the totals.
  (setf (slot-value self 'total-invested)     (make-instance DeviseAccount))
  (setf (slot-value self 'total-closed-gains) (make-instance DeviseAccount))
  (setf (slot-value self 'total-closed-base)  (make-instance DeviseAccount))

  ;; 2- Loop over the lines.
  (let ((lines (lines self))
        (line) (dev))
    (while lines
      (setq line  (cdr (car lines))
            lines (cdr lines)
            dev   (devise line))
      ;; Update totals.
      (account-add (total-invested self)     dev (amount-invested   line))
      (account-add (total-closed-gains self) dev (closed-gain       line))
      (account-add (total-closed-base self)  dev (closed-buy-amount line))
      )
    ) ;;while lines
  self);;compute-line-totals


(defmethod line-with-symbol ((self Portfolio) symbol)
  "RETURN: the line whose symbol is SYMBOL,
        or nil if none exist in the portfolio."
  (cdr (assoc symbol (lines self)))
  );;line-with-symbol

;;;END Portfolio -------------------------------------------------------
;;;---------------------------------------------------------------------






;;----------------------------------------------------------------------
;;--- Main Functions ---------------------------------------------------
;;----------------------------------------------------------------------

(defun portefeuille (operations &rest options)
  "
OPERATIONS est une liste d'opérations boursières au format :
     ( (DATE   QUANTITE   DEVISE COURS   COMISSION SYMBOL)
       (2000-10-16  20    EUR     8.98   8.20      ES0132580319) )
Achat : quantité > 0
Vente : quantité < 0

On peut placer 0, 1 ou plusieurs options:

    'efface-reste-du-tampon   --> efface le reste du tampon à partir du point
                                  d'insertion avant d'afficher les résultats.

    les symboles monétaires définis
    dans (euro-get-devises)   --> affiche les totaux dans la devise indiquée
                                  (que les cours soient à jour!).
"

  (let ((portfolio (make-instance Portfolio))

        (jlin "---------- --- ---------- ---- -------- ------ ------ ------------\n")
        (jtit "DATE       DEV MONTANT    QUTE COURS    FRAIS  FRAIS% SYMBOL      \n")
        (jlco "---------- --- ---------- ---------- --------- ------\n")
        (jcom "%-10s %-3s %10.2f            %9.2f %5.2f%%\n")
        (jsol "%-10s %-3s %10.2f \n")
        (plin "------------ ----- --- ---------- ------ --------- --------- -------\n")
        (ptit "SYMBOL       QUTE  DEV PAYE       FRAIS  REVIENT   BENEFICE  BENEFI%\n")
        (pfor "%-13s%5d %-3s %10.2f %6.2f %9.2f %9.2f %6.2f%%\n")
        (ptot "%-13s      %-3s %10.2f                  %9.2f %6.2f%%\n")
        (devises-totaux       nil)
        (toutes-les-devises)
        )

    ;;; Process options.
    (mapc (lambda (opt)
            (cond ((equal opt 'efface-reste-du-tampon)
                   (delete-region (point) (point-max)))
                  ((member opt (euro-get-devises))
                   (setq devises-totaux (cons opt devises-totaux)))
                  (t (error "Option inconnue '%s'." opt))))
          options)

    (setq devises-totaux
          (if devises-totaux
            (remove-duplicates (sort devises-totaux 'compare-slessp-sn))
            '(EUR)))


    ;;; ============================================================ ;;;
    ;;; ===                        OPERATIONS                    === ;;;
    ;;; ============================================================ ;;;

    (printf "\n")
    (printf "------------------------------------------------------------------\n")
    (printf "                          OPERATIONS\n")
    (printf "%s" jlin)
    (printf "%s" jtit)
    (printf "%s" jlin)

    ;;; Make the BuySellOp objects from the input lists.
    (setq operations
          (mapcar 'make-operation (sort operations 'compare-slessp-sn-c)))

    (let ((op))
      (while operations
        (setq op         (car operations)
              operations (cdr operations))
        ;;; Print the operation.
        (printf "%s\n" (as-string op))
        ;;; Update portfolio & line.
        (add-operation portfolio op)
        )
      ) ;;while operations
    (printf "%s" jlin)

    ;; Compute the operation totals.
    (compute-operation-totals portfolio)

    ;;; Print the operation totals.
    (let ( ;; all these totals are DeviseAccount instances.
          (t-opcom       (total-opcom       portfolio))
          (t-amount-base (total-amount-base portfolio))
          (t-comissions  (total-comissions  portfolio))
          )

      (setq toutes-les-devises (sort (remove-duplicates (devises t-opcom)) 
                                     'compare-slessp-sn))
      (mapc
       (lambda (devise)
         (printf jcom "INVEST/DEV"  devise
                 (amount-at-devise t-opcom       devise)
                 (amount-at-devise t-comissions  devise)
                 (percent
                  (amount-at-devise t-comissions  devise)
                  (amount-at-devise t-amount-base devise))
                 ""))
       toutes-les-devises)
      (printf "%s" jlco)

      (mapc
       (lambda (devise)
         (printf jcom "INVESTI" devise
                 (account-valuation t-opcom       devise)
                 (account-valuation t-comissions  devise)
                 (percent
                  (account-valuation t-comissions  devise)
                  (account-valuation t-amount-base devise))
                 ""))
       devises-totaux)
      (printf "%s" jlco)
      ) ;;let

    (let* ((devise EUR)
           (total-credit-v (account-valuation (total-credit portfolio) devise))
           (total-debit-v  (account-valuation (total-debit  portfolio) devise)))

      (printf "%-10s %3s %10.2f %10.2f\n"
              "LIQUIDITE" devise total-credit-v total-debit-v)
      (if (< total-credit-v total-debit-v)
        (printf "%10s %3s            %10.2f\n"
                "" devise (- total-debit-v total-credit-v))
        (printf "%-10s %3s %10.2f\n"
                "" devise (- total-credit-v total-debit-v)))
      ) ;;let*

    (printf "\n")


    ;;; ============================================================ ;;;
    ;;; ===                      PORTFOLIO                       === ;;;
    ;;; ============================================================ ;;;

    (printf "%s\n" (make-string (- (length plin) 1) (string-to-char "-")))
    (printf "                            PORTFOLIO\n")
    (printf "%s" plin)
    (printf "%s" ptit)
    (printf "%s" plin)

    ;;; List the portfolio.
    (sort-portfolio-lines portfolio)
    (let ((p-lines (lines portfolio))
          (line))
      (while p-lines
        (setq line (cdr (car p-lines))
              p-lines (cdr p-lines))
        ;;; Print a line.
        (printf pfor (symbol line) 
                ;; over running position:
                (quantity line) (devise line) (amount-invested line)
                (comission line) (paid-per-share line)
                ;; over closed positions:
                (closed-gain line) 
                (percent (closed-gain line) (closed-buy-amount line)))
        )
      ) ;;while p-lines
    (printf "%s" plin)

    ;; Compute the line totals.
    (compute-line-totals portfolio)

    ;; Print totals.
    (let ( ;; all these totals are DeviseAccount instances.
          (t-invested     (total-invested     portfolio))
          (t-closed-gains (total-closed-gains portfolio))
          (t-closed-base  (total-closed-base  portfolio))
          )
      (mapc (lambda (devise)
                (printf ptot "TOTAL/DEV" devise
                        (amount-at-devise t-invested     devise)
                        (amount-at-devise t-closed-gains devise)
                        (percent
                         (amount-at-devise t-closed-gains devise)
                         (amount-at-devise t-closed-base  devise))))
              toutes-les-devises)
      (printf "%s" plin)

      (mapc (lambda (devise)
                (printf ptot "TOTAL" devise
                        (account-valuation t-invested     devise)
                        (account-valuation t-closed-gains devise)
                        (percent
                         (account-valuation t-closed-gains devise)
                         (account-valuation t-closed-base  devise))))
              devises-totaux)
      (printf "%s" plin)
      ) ;;let
    (printf "\n")

    portfolio)
  );;portefeuille




(defun affiche-vente (portefeuille taux-benef)
  (let ((plin "------------ ----- --- --------- ------ --------- -------\n")
        (ptit "SYMBOL       QUTE  DEV COURS     FRAIS  BENEFICE  BENEFI%\n")
        (pfor "%-13s%5d %3s %9.2f %6.2f %9.2f %6.2f%%\n")
        (p-lines (lines portefeuille))
        )

    (printf "%s" plin)
    (printf "%s" ptit)
    (printf "%s" plin)


    (while p-lines
      (let* ((line (cdr (car p-lines)))
             (avg-comission    (/ (comission line) (nb-operations line)))
             (amount           (- (buy-amount line) (sell-amount line)))
             (quantity         (- (buy-quantity line) (sell-quantity line)))
             (gain         0.0)
             (sell-price       0.0)
             )
        (if (< 0 quantity)
          (progn
            (setq sell-price (+ (* (+ 1.0 taux-benef) (paid-per-share line))
                                (/ avg-comission (quantity line))))
            (setq gain   (* taux-benef (buy-amount line)))
            (printf pfor (symbol line) (quantity line) (devise line)
                    sell-price avg-comission gain (* 100.0 taux-benef))
            ))
        ) ;;let*
      (setq p-lines (cdr p-lines))
      ) ;;while portefeuille
    (printf "%s" plin)
    ) ;;let
  );;affiche-vente



(defun show-positions (portefeuille symbol)
  (let* ((line (cdr (assoc symbol (lines portefeuille))))
         (last (last-position line)))
    (insert (format "%s\n" (make-string 75 ?-)))
    (insert (format "%s\n" symbol))
    (insert (format "%4s %8s %4s %8s %7s %3s %10s %10s %4s %8s\n"
              "----" "--------" "----" "--------" "-------" "---"
              "----------" "----------" "----" "--------"))
    (insert (format "%4s %8s %4s %8s %7s %3s %10s %10s %4s %8s\n"
              "BuQt" "Buy Amt"
              "SeQt" "Sel Amt"
              "Comm"
              "NOp"
              "Open Date"
              "Last Date"
              "Rest"
              "Benef."))
    (insert (format "%4s %8s %4s %8s %7s %3s %10s %10s %4s %8s\n"
              "----" "--------" "----" "--------" "-------" "---"
              "----------" "----------" "----" "--------"))
    (if (not (is-closed last))
      (insert (format "%66s %8.2f\n" "" 
                      (/ (- (sell-amount last) (buy-amount last))
                         (- (sell-quantity last) (buy-quantity last))))))
    (mapc
     (lambda (p) 
       (insert (format "%4d %8.2f %4d %8.2f %7.2f %3d %10s %10s %4d %8.2f\n"
                 (buy-quantity p)
                 (buy-amount p)
                 (sell-quantity p)
                 (sell-amount p)
                 (comission p)
                 (nb-operations p)
                 (open-date p)
                 (last-date p)
                 (- (buy-quantity p) (sell-quantity p) )
                 (- (sell-amount p) (buy-amount p)) )))
     (positions line))
    (insert (format "%s\n" (make-string 75 ?-)))
    )
  );;show-positions


;;;; pjb-bourse.el                    --                     --          ;;;;
