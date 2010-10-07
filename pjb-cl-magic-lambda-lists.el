
(setf *cl-lambda-lists* (make-hash-table :test (function equal) :size 978))

(setf (gethash "*" *cl-lambda-lists*) (quote [cl-struct-lambda-list * :FUNCTION (&REST ARGS) 0 0 ((&REST ARGS)) nil nil ((:rest . "ARGS...")) [lambda-list-accept]]))

(setf (gethash "+" *cl-lambda-lists*) (quote [cl-struct-lambda-list + :FUNCTION (&REST ARGS) 0 0 ((&REST ARGS)) nil nil ((:rest . "ARGS...")) [lambda-list-accept]]))

(setf (gethash "-" *cl-lambda-lists*) (quote [cl-struct-lambda-list - :FUNCTION (NUMBER &REST MORE-NUMBERS) 2 0 ((&REST MORE-NUMBERS)) nil nil ((:mandatory . "NUMBER") (:rest . "MORE-NUMBERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "/" *cl-lambda-lists*) (quote [cl-struct-lambda-list / :FUNCTION (NUMBER &REST MORE-NUMBERS) 2 0 ((&REST MORE-NUMBERS)) nil nil ((:mandatory . "NUMBER") (:rest . "MORE-NUMBERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "/=" *cl-lambda-lists*) (quote [cl-struct-lambda-list /= :FUNCTION (NUMBER &REST MORE-NUMBERS) 2 0 ((&REST MORE-NUMBERS)) nil nil ((:mandatory . "NUMBER") (:rest . "MORE-NUMBERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "1+" *cl-lambda-lists*) (quote [cl-struct-lambda-list 1+ :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "1-" *cl-lambda-lists*) (quote [cl-struct-lambda-list 1- :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "<" *cl-lambda-lists*) (quote [cl-struct-lambda-list < :FUNCTION (NUMBER &REST MORE-NUMBERS) 2 0 ((&REST MORE-NUMBERS)) nil nil ((:mandatory . "NUMBER") (:rest . "MORE-NUMBERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "<=" *cl-lambda-lists*) (quote [cl-struct-lambda-list <= :FUNCTION (NUMBER &REST MORE-NUMBERS) 2 0 ((&REST MORE-NUMBERS)) nil nil ((:mandatory . "NUMBER") (:rest . "MORE-NUMBERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "=" *cl-lambda-lists*) (quote [cl-struct-lambda-list = :FUNCTION (NUMBER &REST MORE-NUMBERS) 2 0 ((&REST MORE-NUMBERS)) nil nil ((:mandatory . "NUMBER") (:rest . "MORE-NUMBERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash ">" *cl-lambda-lists*) (quote [cl-struct-lambda-list > :FUNCTION (NUMBER &REST MORE-NUMBERS) 2 0 ((&REST MORE-NUMBERS)) nil nil ((:mandatory . "NUMBER") (:rest . "MORE-NUMBERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash ">=" *cl-lambda-lists*) (quote [cl-struct-lambda-list >= :FUNCTION (NUMBER &REST MORE-NUMBERS) 2 0 ((&REST MORE-NUMBERS)) nil nil ((:mandatory . "NUMBER") (:rest . "MORE-NUMBERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "ABORT" *cl-lambda-lists*) (quote [cl-struct-lambda-list ABORT :FUNCTION (&OPTIONAL CONDITION) 0 1 nil nil nil ((:optional . "[CONDITION]")) [lambda-list-accept lambda-list-close]]))

(setf (gethash "ABS" *cl-lambda-lists*) (quote [cl-struct-lambda-list ABS :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ACONS" *cl-lambda-lists*) (quote [cl-struct-lambda-list ACONS :FUNCTION (KEY DATUM ALIST) 4 0 nil nil nil ((:mandatory . "KEY") (:mandatory . "DATUM") (:mandatory . "ALIST")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ACOS" *cl-lambda-lists*) (quote [cl-struct-lambda-list ACOS :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ACOSH" *cl-lambda-lists*) (quote [cl-struct-lambda-list ACOSH :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ADD-METHOD" *cl-lambda-lists*) (quote [cl-struct-lambda-list ADD-METHOD :GENERIC (GENERIC-FUNCTION METHOD) 3 0 nil nil nil ((:mandatory . "GENERIC-FUNCTION") (:mandatory . "METHOD")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ADJOIN" *cl-lambda-lists*) (quote [cl-struct-lambda-list ADJOIN :FUNCTION (ITEM LIST &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT nil NOTP)) 3 0 nil nil (TEST-NOT TEST KEY) ((:mandatory . "ITEM") (:mandatory . "LIST") (:key . ":KEY") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "ADJUST-ARRAY" *cl-lambda-lists*) (quote [cl-struct-lambda-list ADJUST-ARRAY :FUNCTION (ARRAY DIMENSIONS &KEY (ELEMENT-TYPE (ARRAY-ELEMENT-TYPE ARRAY)) (INITIAL-ELEMENT nil INITIAL-ELEMENT-P) (INITIAL-CONTENTS nil INITIAL-CONTENTS-P) FILL-POINTER DISPLACED-TO DISPLACED-INDEX-OFFSET) 3 0 nil nil (DISPLACED-INDEX-OFFSET DISPLACED-TO FILL-POINTER INITIAL-CONTENTS INITIAL-ELEMENT ELEMENT-TYPE) ((:mandatory . "ARRAY") (:mandatory . "DIMENSIONS") (:key . ":ELEMENT-TYPE") (:key . ":INITIAL-ELEMENT") (:key . ":INITIAL-CONTENTS") (:key . ":FILL-POINTER") (:key . ":DISPLACED-TO") (:key . ":DISPLACED-INDEX-OFFSET")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "ADJUSTABLE-ARRAY-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list ADJUSTABLE-ARRAY-P :FUNCTION (ARRAY) 2 0 nil nil nil ((:mandatory . "ARRAY")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ALLOCATE-INSTANCE" *cl-lambda-lists*) (quote [cl-struct-lambda-list ALLOCATE-INSTANCE :GENERIC (CLASS &REST INITARGS) 2 0 ((&REST INITARGS)) nil nil ((:mandatory . "CLASS") (:rest . "INITARGS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "ALPHA-CHAR-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list ALPHA-CHAR-P :FUNCTION (CHAR) 2 0 nil nil nil ((:mandatory . "CHAR")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ALPHANUMERICP" *cl-lambda-lists*) (quote [cl-struct-lambda-list ALPHANUMERICP :FUNCTION (CHAR) 2 0 nil nil nil ((:mandatory . "CHAR")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "AND" *cl-lambda-lists*) (quote [cl-struct-lambda-list AND :MACRO (&REST FORMS) 0 0 ((&REST FORMS)) nil nil ((:rest . "FORMS...")) [lambda-list-accept]]))

(setf (gethash "APPEND" *cl-lambda-lists*) (quote [cl-struct-lambda-list APPEND :FUNCTION (&REST LISTS) 0 0 ((&REST LISTS)) nil nil ((:rest . "LISTS...")) [lambda-list-accept]]))

(setf (gethash "APPLY" *cl-lambda-lists*) (quote [cl-struct-lambda-list APPLY :FUNCTION (FUNCTION ARG &REST ARGUMENTS) 3 0 ((&REST ARGUMENTS)) nil nil ((:mandatory . "FUNCTION") (:mandatory . "ARG") (:rest . "ARGUMENTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "APROPOS" *cl-lambda-lists*) (quote [cl-struct-lambda-list APROPOS :FUNCTION (STRING-DESIGNATOR &OPTIONAL PACKAGE EXTERNAL-ONLY) 2 2 nil nil nil ((:mandatory . "STRING-DESIGNATOR") (:optional . "[PACKAGE]") (:optional . "[EXTERNAL-ONLY]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "APROPOS-LIST" *cl-lambda-lists*) (quote [cl-struct-lambda-list APROPOS-LIST :FUNCTION (STRING-DESIGNATOR &OPTIONAL PACKAGE-DESIGNATOR EXTERNAL-ONLY) 2 2 nil nil nil ((:mandatory . "STRING-DESIGNATOR") (:optional . "[PACKAGE-DESIGNATOR]") (:optional . "[EXTERNAL-ONLY]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "AREF" *cl-lambda-lists*) (quote [cl-struct-lambda-list AREF :FUNCTION (ARRAY &REST SUBSCRIPTS) 2 0 ((&REST SUBSCRIPTS)) nil nil ((:mandatory . "ARRAY") (:rest . "SUBSCRIPTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "ARITHMETIC-ERROR-OPERANDS" *cl-lambda-lists*) (quote [cl-struct-lambda-list ARITHMETIC-ERROR-OPERANDS :FUNCTION (CONDITION) 2 0 nil nil nil ((:mandatory . "CONDITION")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ARITHMETIC-ERROR-OPERATION" *cl-lambda-lists*) (quote [cl-struct-lambda-list ARITHMETIC-ERROR-OPERATION :FUNCTION (CONDITION) 2 0 nil nil nil ((:mandatory . "CONDITION")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ARRAY-DIMENSION" *cl-lambda-lists*) (quote [cl-struct-lambda-list ARRAY-DIMENSION :FUNCTION (ARRAY AXIS-NUMBER) 3 0 nil nil nil ((:mandatory . "ARRAY") (:mandatory . "AXIS-NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ARRAY-DIMENSIONS" *cl-lambda-lists*) (quote [cl-struct-lambda-list ARRAY-DIMENSIONS :FUNCTION (ARRAY) 2 0 nil nil nil ((:mandatory . "ARRAY")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ARRAY-DISPLACEMENT" *cl-lambda-lists*) (quote [cl-struct-lambda-list ARRAY-DISPLACEMENT :FUNCTION (ARRAY) 2 0 nil nil nil ((:mandatory . "ARRAY")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ARRAY-ELEMENT-TYPE" *cl-lambda-lists*) (quote [cl-struct-lambda-list ARRAY-ELEMENT-TYPE :FUNCTION (ARRAY) 2 0 nil nil nil ((:mandatory . "ARRAY")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ARRAY-HAS-FILL-POINTER-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list ARRAY-HAS-FILL-POINTER-P :FUNCTION (ARRAY) 2 0 nil nil nil ((:mandatory . "ARRAY")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ARRAY-IN-BOUNDS-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list ARRAY-IN-BOUNDS-P :FUNCTION (ARRAY &REST SUBSCRIPTS) 2 0 ((&REST SUBSCRIPTS)) nil nil ((:mandatory . "ARRAY") (:rest . "SUBSCRIPTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "ARRAY-RANK" *cl-lambda-lists*) (quote [cl-struct-lambda-list ARRAY-RANK :FUNCTION (ARRAY) 2 0 nil nil nil ((:mandatory . "ARRAY")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ARRAY-ROW-MAJOR-INDEX" *cl-lambda-lists*) (quote [cl-struct-lambda-list ARRAY-ROW-MAJOR-INDEX :FUNCTION (ARRAY &REST SUBSCRIPTS) 2 0 ((&REST SUBSCRIPTS)) nil nil ((:mandatory . "ARRAY") (:rest . "SUBSCRIPTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "ARRAY-TOTAL-SIZE" *cl-lambda-lists*) (quote [cl-struct-lambda-list ARRAY-TOTAL-SIZE :FUNCTION (ARRAY) 2 0 nil nil nil ((:mandatory . "ARRAY")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ARRAYP" *cl-lambda-lists*) (quote [cl-struct-lambda-list ARRAYP :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ASH" *cl-lambda-lists*) (quote [cl-struct-lambda-list ASH :FUNCTION (INTEGER COUNT) 3 0 nil nil nil ((:mandatory . "INTEGER") (:mandatory . "COUNT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ASIN" *cl-lambda-lists*) (quote [cl-struct-lambda-list ASIN :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ASINH" *cl-lambda-lists*) (quote [cl-struct-lambda-list ASINH :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ASSERT" *cl-lambda-lists*) (quote [cl-struct-lambda-list ASSERT :MACRO (TEST-FORM &OPTIONAL PLACES DATUM &REST ARGUMENTS) 2 2 ((&REST ARGUMENTS)) nil nil ((:mandatory . "TEST-FORM") (:optional . "[PLACES]") (:optional . "[DATUM]") (:rest . "ARGUMENTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "ASSOC" *cl-lambda-lists*) (quote [cl-struct-lambda-list ASSOC :FUNCTION (ITEM ALIST &KEY KEY (TEST nil TESTP) (TEST-NOT nil NOTP)) 3 0 nil nil (TEST-NOT TEST KEY) ((:mandatory . "ITEM") (:mandatory . "ALIST") (:key . ":KEY") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "ASSOC-IF" *cl-lambda-lists*) (quote [cl-struct-lambda-list ASSOC-IF :FUNCTION (PREDICATE ALIST &KEY KEY) 3 0 nil nil (KEY) ((:mandatory . "PREDICATE") (:mandatory . "ALIST") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "ASSOC-IF-NOT" *cl-lambda-lists*) (quote [cl-struct-lambda-list ASSOC-IF-NOT :FUNCTION (PREDICATE ALIST &KEY KEY) 3 0 nil nil (KEY) ((:mandatory . "PREDICATE") (:mandatory . "ALIST") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "ATAN" *cl-lambda-lists*) (quote [cl-struct-lambda-list ATAN :FUNCTION (Y &OPTIONAL (X nil XP)) 2 1 nil nil nil ((:mandatory . "Y") (:optional . "[X]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ATANH" *cl-lambda-lists*) (quote [cl-struct-lambda-list ATANH :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ATOM" *cl-lambda-lists*) (quote [cl-struct-lambda-list ATOM :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "BIT" *cl-lambda-lists*) (quote [cl-struct-lambda-list BIT :FUNCTION (BIT-ARRAY &REST SUBSCRIPTS) 2 0 ((&REST SUBSCRIPTS)) nil nil ((:mandatory . "BIT-ARRAY") (:rest . "SUBSCRIPTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "BIT-AND" *cl-lambda-lists*) (quote [cl-struct-lambda-list BIT-AND :FUNCTION (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY) 3 1 nil nil nil ((:mandatory . "BIT-ARRAY-1") (:mandatory . "BIT-ARRAY-2") (:optional . "[RESULT-BIT-ARRAY]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "BIT-ANDC1" *cl-lambda-lists*) (quote [cl-struct-lambda-list BIT-ANDC1 :FUNCTION (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY) 3 1 nil nil nil ((:mandatory . "BIT-ARRAY-1") (:mandatory . "BIT-ARRAY-2") (:optional . "[RESULT-BIT-ARRAY]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "BIT-ANDC2" *cl-lambda-lists*) (quote [cl-struct-lambda-list BIT-ANDC2 :FUNCTION (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY) 3 1 nil nil nil ((:mandatory . "BIT-ARRAY-1") (:mandatory . "BIT-ARRAY-2") (:optional . "[RESULT-BIT-ARRAY]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "BIT-EQV" *cl-lambda-lists*) (quote [cl-struct-lambda-list BIT-EQV :FUNCTION (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY) 3 1 nil nil nil ((:mandatory . "BIT-ARRAY-1") (:mandatory . "BIT-ARRAY-2") (:optional . "[RESULT-BIT-ARRAY]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "BIT-IOR" *cl-lambda-lists*) (quote [cl-struct-lambda-list BIT-IOR :FUNCTION (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY) 3 1 nil nil nil ((:mandatory . "BIT-ARRAY-1") (:mandatory . "BIT-ARRAY-2") (:optional . "[RESULT-BIT-ARRAY]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "BIT-NAND" *cl-lambda-lists*) (quote [cl-struct-lambda-list BIT-NAND :FUNCTION (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY) 3 1 nil nil nil ((:mandatory . "BIT-ARRAY-1") (:mandatory . "BIT-ARRAY-2") (:optional . "[RESULT-BIT-ARRAY]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "BIT-NOR" *cl-lambda-lists*) (quote [cl-struct-lambda-list BIT-NOR :FUNCTION (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY) 3 1 nil nil nil ((:mandatory . "BIT-ARRAY-1") (:mandatory . "BIT-ARRAY-2") (:optional . "[RESULT-BIT-ARRAY]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "BIT-NOT" *cl-lambda-lists*) (quote [cl-struct-lambda-list BIT-NOT :FUNCTION (BIT-ARRAY &OPTIONAL RESULT-BIT-ARRAY) 2 1 nil nil nil ((:mandatory . "BIT-ARRAY") (:optional . "[RESULT-BIT-ARRAY]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "BIT-ORC1" *cl-lambda-lists*) (quote [cl-struct-lambda-list BIT-ORC1 :FUNCTION (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY) 3 1 nil nil nil ((:mandatory . "BIT-ARRAY-1") (:mandatory . "BIT-ARRAY-2") (:optional . "[RESULT-BIT-ARRAY]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "BIT-ORC2" *cl-lambda-lists*) (quote [cl-struct-lambda-list BIT-ORC2 :FUNCTION (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY) 3 1 nil nil nil ((:mandatory . "BIT-ARRAY-1") (:mandatory . "BIT-ARRAY-2") (:optional . "[RESULT-BIT-ARRAY]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "BIT-VECTOR-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list BIT-VECTOR-P :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "BIT-XOR" *cl-lambda-lists*) (quote [cl-struct-lambda-list BIT-XOR :FUNCTION (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY) 3 1 nil nil nil ((:mandatory . "BIT-ARRAY-1") (:mandatory . "BIT-ARRAY-2") (:optional . "[RESULT-BIT-ARRAY]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "BLOCK" *cl-lambda-lists*) (quote [cl-struct-lambda-list BLOCK :SPECIAL-OPERATOR (NAME &REST FORMS) 2 0 ((&REST FORMS)) nil nil ((:mandatory . "NAME") (:rest . "FORMS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "BOOLE" *cl-lambda-lists*) (quote [cl-struct-lambda-list BOOLE :FUNCTION (OP INTEGER1 INTEGER2) 4 0 nil nil nil ((:mandatory . "OP") (:mandatory . "INTEGER1") (:mandatory . "INTEGER2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "BOTH-CASE-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list BOTH-CASE-P :FUNCTION (CHAR) 2 0 nil nil nil ((:mandatory . "CHAR")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "BOUNDP" *cl-lambda-lists*) (quote [cl-struct-lambda-list BOUNDP :FUNCTION (SYMBOL) 2 0 nil nil nil ((:mandatory . "SYMBOL")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "BREAK" *cl-lambda-lists*) (quote [cl-struct-lambda-list BREAK :FUNCTION (&OPTIONAL (DATUM "break") &REST ARGUMENTS) 0 1 ((&REST ARGUMENTS)) nil nil ((:optional . "[DATUM]") (:rest . "ARGUMENTS...")) [lambda-list-accept lambda-list-accept]]))

(setf (gethash "BROADCAST-STREAM-STREAMS" *cl-lambda-lists*) (quote [cl-struct-lambda-list BROADCAST-STREAM-STREAMS :FUNCTION (INSTANCE) 2 0 nil nil nil ((:mandatory . "INSTANCE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "BUTLAST" *cl-lambda-lists*) (quote [cl-struct-lambda-list BUTLAST :FUNCTION (LIST &OPTIONAL (N 1)) 2 1 nil nil nil ((:mandatory . "LIST") (:optional . "[N]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "BYTE" *cl-lambda-lists*) (quote [cl-struct-lambda-list BYTE :FUNCTION (SIZE POSITION) 3 0 nil nil nil ((:mandatory . "SIZE") (:mandatory . "POSITION")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "BYTE-POSITION" *cl-lambda-lists*) (quote [cl-struct-lambda-list BYTE-POSITION :FUNCTION (BYTESPEC) 2 0 nil nil nil ((:mandatory . "BYTESPEC")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "BYTE-SIZE" *cl-lambda-lists*) (quote [cl-struct-lambda-list BYTE-SIZE :FUNCTION (BYTESPEC) 2 0 nil nil nil ((:mandatory . "BYTESPEC")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CAAAAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CAAAAR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CAAADR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CAAADR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CAAAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CAAAR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CAADAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CAADAR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CAADDR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CAADDR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CAADR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CAADR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CAAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CAAR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CADAAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CADAAR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CADADR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CADADR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CADAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CADAR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CADDAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CADDAR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CADDDR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CADDDR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CADDR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CADDR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CADR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CADR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CALL-METHOD" *cl-lambda-lists*) (quote [cl-struct-lambda-list CALL-METHOD :MACRO (&REST ARGS) 0 0 ((&REST ARGS)) nil nil ((:rest . "ARGS...")) [lambda-list-accept]]))

(setf (gethash "CAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CAR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CASE" *cl-lambda-lists*) (quote [cl-struct-lambda-list CASE :MACRO (KEYFORM &BODY CASES) 2 0 ((&BODY CASES)) nil nil ((:mandatory . "KEYFORM") (:body . "CASES...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "CATCH" *cl-lambda-lists*) (quote [cl-struct-lambda-list CATCH :SPECIAL-OPERATOR (TAG &BODY BODY) 2 0 ((&BODY BODY)) nil nil ((:mandatory . "TAG") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "CCASE" *cl-lambda-lists*) (quote [cl-struct-lambda-list CCASE :MACRO (KEYFORM &BODY CASES) 2 0 ((&BODY CASES)) nil nil ((:mandatory . "KEYFORM") (:body . "CASES...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "CDAAAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CDAAAR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CDAADR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CDAADR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CDAAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CDAAR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CDADAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CDADAR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CDADDR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CDADDR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CDADR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CDADR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CDAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CDAR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CDDAAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CDDAAR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CDDADR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CDDADR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CDDAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CDDAR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CDDDAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CDDDAR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CDDDDR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CDDDDR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CDDDR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CDDDR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CDDR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CDDR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CDR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CDR :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CEILING" *cl-lambda-lists*) (quote [cl-struct-lambda-list CEILING :FUNCTION (NUMBER &OPTIONAL (DIVISOR 1)) 2 1 nil nil nil ((:mandatory . "NUMBER") (:optional . "[DIVISOR]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CELL-ERROR-NAME" *cl-lambda-lists*) (quote [cl-struct-lambda-list CELL-ERROR-NAME :FUNCTION (CONDITION) 2 0 nil nil nil ((:mandatory . "CONDITION")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CERROR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CERROR :FUNCTION (CONTINUE-STRING DATUM &REST ARGUMENTS) 3 0 ((&REST ARGUMENTS)) nil nil ((:mandatory . "CONTINUE-STRING") (:mandatory . "DATUM") (:rest . "ARGUMENTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "CHANGE-CLASS" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHANGE-CLASS :GENERIC (INSTANCE NEW-CLASS &REST INITARGS) 3 0 ((&REST INITARGS)) nil nil ((:mandatory . "INSTANCE") (:mandatory . "NEW-CLASS") (:rest . "INITARGS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "CHAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHAR :FUNCTION (STRING INDEX) 3 0 nil nil nil ((:mandatory . "STRING") (:mandatory . "INDEX")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CHAR-CODE" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHAR-CODE :FUNCTION (CHAR) 2 0 nil nil nil ((:mandatory . "CHAR")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CHAR-DOWNCASE" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHAR-DOWNCASE :FUNCTION (CHAR) 2 0 nil nil nil ((:mandatory . "CHAR")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CHAR-EQUAL" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHAR-EQUAL :FUNCTION (CHARACTER &REST MORE-CHARACTERS) 2 0 ((&REST MORE-CHARACTERS)) nil nil ((:mandatory . "CHARACTER") (:rest . "MORE-CHARACTERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "CHAR-GREATERP" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHAR-GREATERP :FUNCTION (CHARACTER &REST MORE-CHARACTERS) 2 0 ((&REST MORE-CHARACTERS)) nil nil ((:mandatory . "CHARACTER") (:rest . "MORE-CHARACTERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "CHAR-INT" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHAR-INT :FUNCTION (CHAR) 2 0 nil nil nil ((:mandatory . "CHAR")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CHAR-LESSP" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHAR-LESSP :FUNCTION (CHARACTER &REST MORE-CHARACTERS) 2 0 ((&REST MORE-CHARACTERS)) nil nil ((:mandatory . "CHARACTER") (:rest . "MORE-CHARACTERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "CHAR-NAME" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHAR-NAME :FUNCTION (CHAR) 2 0 nil nil nil ((:mandatory . "CHAR")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CHAR-NOT-EQUAL" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHAR-NOT-EQUAL :FUNCTION (CHARACTER &REST MORE-CHARACTERS) 2 0 ((&REST MORE-CHARACTERS)) nil nil ((:mandatory . "CHARACTER") (:rest . "MORE-CHARACTERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "CHAR-NOT-GREATERP" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHAR-NOT-GREATERP :FUNCTION (CHARACTER &REST MORE-CHARACTERS) 2 0 ((&REST MORE-CHARACTERS)) nil nil ((:mandatory . "CHARACTER") (:rest . "MORE-CHARACTERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "CHAR-NOT-LESSP" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHAR-NOT-LESSP :FUNCTION (CHARACTER &REST MORE-CHARACTERS) 2 0 ((&REST MORE-CHARACTERS)) nil nil ((:mandatory . "CHARACTER") (:rest . "MORE-CHARACTERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "CHAR-UPCASE" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHAR-UPCASE :FUNCTION (CHAR) 2 0 nil nil nil ((:mandatory . "CHAR")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CHAR/=" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHAR/= :FUNCTION (CHARACTER &REST MORE-CHARACTERS) 2 0 ((&REST MORE-CHARACTERS)) nil nil ((:mandatory . "CHARACTER") (:rest . "MORE-CHARACTERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "CHAR<" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHAR< :FUNCTION (CHARACTER &REST MORE-CHARACTERS) 2 0 ((&REST MORE-CHARACTERS)) nil nil ((:mandatory . "CHARACTER") (:rest . "MORE-CHARACTERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "CHAR<=" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHAR<= :FUNCTION (CHARACTER &REST MORE-CHARACTERS) 2 0 ((&REST MORE-CHARACTERS)) nil nil ((:mandatory . "CHARACTER") (:rest . "MORE-CHARACTERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "CHAR=" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHAR= :FUNCTION (CHARACTER &REST MORE-CHARACTERS) 2 0 ((&REST MORE-CHARACTERS)) nil nil ((:mandatory . "CHARACTER") (:rest . "MORE-CHARACTERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "CHAR>" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHAR> :FUNCTION (CHARACTER &REST MORE-CHARACTERS) 2 0 ((&REST MORE-CHARACTERS)) nil nil ((:mandatory . "CHARACTER") (:rest . "MORE-CHARACTERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "CHAR>=" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHAR>= :FUNCTION (CHARACTER &REST MORE-CHARACTERS) 2 0 ((&REST MORE-CHARACTERS)) nil nil ((:mandatory . "CHARACTER") (:rest . "MORE-CHARACTERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "CHARACTER" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHARACTER :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CHARACTERP" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHARACTERP :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CHECK-TYPE" *cl-lambda-lists*) (quote [cl-struct-lambda-list CHECK-TYPE :MACRO (PLACE TYPE &OPTIONAL TYPE-STRING) 3 1 nil nil nil ((:mandatory . "PLACE") (:mandatory . "TYPE") (:optional . "[TYPE-STRING]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CIS" *cl-lambda-lists*) (quote [cl-struct-lambda-list CIS :FUNCTION (THETA) 2 0 nil nil nil ((:mandatory . "THETA")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CLASS-NAME" *cl-lambda-lists*) (quote [cl-struct-lambda-list CLASS-NAME :GENERIC (CLASS) 2 0 nil nil nil ((:mandatory . "CLASS")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CLASS-OF" *cl-lambda-lists*) (quote [cl-struct-lambda-list CLASS-OF :FUNCTION (X) 2 0 nil nil nil ((:mandatory . "X")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CLEAR-INPUT" *cl-lambda-lists*) (quote [cl-struct-lambda-list CLEAR-INPUT :FUNCTION (&OPTIONAL (STREAM *STANDARD-INPUT*)) 0 1 nil nil nil ((:optional . "[STREAM]")) [lambda-list-accept lambda-list-close]]))

(setf (gethash "CLEAR-OUTPUT" *cl-lambda-lists*) (quote [cl-struct-lambda-list CLEAR-OUTPUT :FUNCTION (&OPTIONAL (STREAM *STANDARD-OUTPUT*)) 0 1 nil nil nil ((:optional . "[STREAM]")) [lambda-list-accept lambda-list-close]]))

(setf (gethash "CLOSE" *cl-lambda-lists*) (quote [cl-struct-lambda-list CLOSE :GENERIC (STREAM &KEY ABORT) 2 0 nil nil (ABORT) ((:mandatory . "STREAM") (:key . ":ABORT")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "CLRHASH" *cl-lambda-lists*) (quote [cl-struct-lambda-list CLRHASH :FUNCTION (HASH-TABLE) 2 0 nil nil nil ((:mandatory . "HASH-TABLE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CODE-CHAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list CODE-CHAR :FUNCTION (CODE) 2 0 nil nil nil ((:mandatory . "CODE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "COERCE" *cl-lambda-lists*) (quote [cl-struct-lambda-list COERCE :FUNCTION (OBJECT OUTPUT-TYPE-SPEC) 3 0 nil nil nil ((:mandatory . "OBJECT") (:mandatory . "OUTPUT-TYPE-SPEC")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "COMPILE" *cl-lambda-lists*) (quote [cl-struct-lambda-list COMPILE :FUNCTION (NAME &OPTIONAL (DEFINITION (OR (MACRO-FUNCTION NAME) (FDEFINITION NAME)))) 2 1 nil nil nil ((:mandatory . "NAME") (:optional . "[DEFINITION]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "COMPILE-FILE" *cl-lambda-lists*) (quote [cl-struct-lambda-list COMPILE-FILE :FUNCTION (INPUT-FILE &KEY (OUTPUT-FILE (CFP-OUTPUT-FILE-DEFAULT INPUT-FILE)) ((VERBOSE *COMPILE-VERBOSE*) *COMPILE-VERBOSE*) ((PRINT *COMPILE-PRINT*) *COMPILE-PRINT*) (EXTERNAL-FORMAT DEFAULT) (TRACE-FILE nil) ((BLOCK-COMPILE *BLOCK-COMPILE-ARG*) nil)) 2 0 nil nil (BLOCK-COMPILE TRACE-FILE EXTERNAL-FORMAT PRINT VERBOSE OUTPUT-FILE) ((:mandatory . "INPUT-FILE") (:key . ":OUTPUT-FILE") (:key . "VERBOSE") (:key . "PRINT") (:key . ":EXTERNAL-FORMAT") (:key . ":TRACE-FILE") (:key . "BLOCK-COMPILE")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "COMPILE-FILE-PATHNAME" *cl-lambda-lists*) (quote [cl-struct-lambda-list COMPILE-FILE-PATHNAME :FUNCTION (INPUT-FILE &KEY (OUTPUT-FILE (CFP-OUTPUT-FILE-DEFAULT INPUT-FILE)) &ALLOW-OTHER-KEYS) 2 0 nil ((&ALLOW-OTHER-KEYS)) (OUTPUT-FILE) ((:mandatory . "INPUT-FILE") (:key . ":OUTPUT-FILE")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "COMPILED-FUNCTION-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list COMPILED-FUNCTION-P :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "COMPILER-MACRO-FUNCTION" *cl-lambda-lists*) (quote [cl-struct-lambda-list COMPILER-MACRO-FUNCTION :FUNCTION (NAME &OPTIONAL ENV) 2 1 nil nil nil ((:mandatory . "NAME") (:optional . "[ENV]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "COMPLEMENT" *cl-lambda-lists*) (quote [cl-struct-lambda-list COMPLEMENT :FUNCTION (FUNCTION) 2 0 nil nil nil ((:mandatory . "FUNCTION")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "COMPLEX" *cl-lambda-lists*) (quote [cl-struct-lambda-list COMPLEX :FUNCTION (REALPART &OPTIONAL (IMAGPART 0)) 2 1 nil nil nil ((:mandatory . "REALPART") (:optional . "[IMAGPART]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "COMPLEXP" *cl-lambda-lists*) (quote [cl-struct-lambda-list COMPLEXP :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "COMPUTE-APPLICABLE-METHODS" *cl-lambda-lists*) (quote [cl-struct-lambda-list COMPUTE-APPLICABLE-METHODS :GENERIC (GENERIC-FUNCTION ARGUMENTS) 3 0 nil nil nil ((:mandatory . "GENERIC-FUNCTION") (:mandatory . "ARGUMENTS")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "COMPUTE-RESTARTS" *cl-lambda-lists*) (quote [cl-struct-lambda-list COMPUTE-RESTARTS :FUNCTION (&OPTIONAL CONDITION) 0 1 nil nil nil ((:optional . "[CONDITION]")) [lambda-list-accept lambda-list-close]]))

(setf (gethash "CONCATENATE" *cl-lambda-lists*) (quote [cl-struct-lambda-list CONCATENATE :FUNCTION (OUTPUT-TYPE-SPEC &REST SEQUENCES) 2 0 ((&REST SEQUENCES)) nil nil ((:mandatory . "OUTPUT-TYPE-SPEC") (:rest . "SEQUENCES...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "CONCATENATED-STREAM-STREAMS" *cl-lambda-lists*) (quote [cl-struct-lambda-list CONCATENATED-STREAM-STREAMS :FUNCTION (INSTANCE) 2 0 nil nil nil ((:mandatory . "INSTANCE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "COND" *cl-lambda-lists*) (quote [cl-struct-lambda-list COND :MACRO (&REST CLAUSES) 0 0 ((&REST CLAUSES)) nil nil ((:rest . "CLAUSES...")) [lambda-list-accept]]))

(setf (gethash "CONJUGATE" *cl-lambda-lists*) (quote [cl-struct-lambda-list CONJUGATE :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CONS" *cl-lambda-lists*) (quote [cl-struct-lambda-list CONS :FUNCTION (SE1 SE2) 3 0 nil nil nil ((:mandatory . "SE1") (:mandatory . "SE2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CONSP" *cl-lambda-lists*) (quote [cl-struct-lambda-list CONSP :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CONSTANTLY" *cl-lambda-lists*) (quote [cl-struct-lambda-list CONSTANTLY :FUNCTION (VALUE) 2 0 nil nil nil ((:mandatory . "VALUE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CONSTANTP" *cl-lambda-lists*) (quote [cl-struct-lambda-list CONSTANTP :FUNCTION (OBJECT &OPTIONAL ENVIRONMENT) 2 1 nil nil nil ((:mandatory . "OBJECT") (:optional . "[ENVIRONMENT]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "CONTINUE" *cl-lambda-lists*) (quote [cl-struct-lambda-list CONTINUE :FUNCTION (&OPTIONAL CONDITION) 0 1 nil nil nil ((:optional . "[CONDITION]")) [lambda-list-accept lambda-list-close]]))

(setf (gethash "COPY-ALIST" *cl-lambda-lists*) (quote [cl-struct-lambda-list COPY-ALIST :FUNCTION (ALIST) 2 0 nil nil nil ((:mandatory . "ALIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "COPY-LIST" *cl-lambda-lists*) (quote [cl-struct-lambda-list COPY-LIST :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "COPY-PPRINT-DISPATCH" *cl-lambda-lists*) (quote [cl-struct-lambda-list COPY-PPRINT-DISPATCH :FUNCTION (&OPTIONAL (TABLE *PRINT-PPRINT-DISPATCH*)) 0 1 nil nil nil ((:optional . "[TABLE]")) [lambda-list-accept lambda-list-close]]))

(setf (gethash "COPY-READTABLE" *cl-lambda-lists*) (quote [cl-struct-lambda-list COPY-READTABLE :FUNCTION (&OPTIONAL (FROM-READTABLE *READTABLE*) TO-READTABLE) 0 2 nil nil nil ((:optional . "[FROM-READTABLE]") (:optional . "[TO-READTABLE]")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "COPY-SEQ" *cl-lambda-lists*) (quote [cl-struct-lambda-list COPY-SEQ :FUNCTION (SEQUENCE) 2 0 nil nil nil ((:mandatory . "SEQUENCE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "COPY-STRUCTURE" *cl-lambda-lists*) (quote [cl-struct-lambda-list COPY-STRUCTURE :FUNCTION (STRUCTURE) 2 0 nil nil nil ((:mandatory . "STRUCTURE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "COPY-SYMBOL" *cl-lambda-lists*) (quote [cl-struct-lambda-list COPY-SYMBOL :FUNCTION (SYMBOL &OPTIONAL (COPY-PROPS nil) &AUX NEW-SYMBOL) 2 1 nil nil nil ((:mandatory . "SYMBOL") (:optional . "[COPY-PROPS]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "COPY-TREE" *cl-lambda-lists*) (quote [cl-struct-lambda-list COPY-TREE :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "COS" *cl-lambda-lists*) (quote [cl-struct-lambda-list COS :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "COSH" *cl-lambda-lists*) (quote [cl-struct-lambda-list COSH :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "COUNT" *cl-lambda-lists*) (quote [cl-struct-lambda-list COUNT :FUNCTION (ITEM SEQUENCE &KEY FROM-END (START 0) (END nil) (KEY nil) (TEST (FUNCTION EQL) TEST-P) (TEST-NOT nil TEST-NOT-P)) 3 0 nil nil (TEST-NOT TEST KEY END START FROM-END) ((:mandatory . "ITEM") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":START") (:key . ":END") (:key . ":KEY") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "COUNT-IF" *cl-lambda-lists*) (quote [cl-struct-lambda-list COUNT-IF :FUNCTION (PRED SEQUENCE &KEY FROM-END (START 0) (END nil) (KEY nil)) 3 0 nil nil (KEY END START FROM-END) ((:mandatory . "PRED") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":START") (:key . ":END") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "COUNT-IF-NOT" *cl-lambda-lists*) (quote [cl-struct-lambda-list COUNT-IF-NOT :FUNCTION (PRED SEQUENCE &KEY FROM-END (START 0) (END nil) (KEY nil)) 3 0 nil nil (KEY END START FROM-END) ((:mandatory . "PRED") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":START") (:key . ":END") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "CTYPECASE" *cl-lambda-lists*) (quote [cl-struct-lambda-list CTYPECASE :MACRO (KEYFORM &BODY CASES) 2 0 ((&BODY CASES)) nil nil ((:mandatory . "KEYFORM") (:body . "CASES...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "DECF" *cl-lambda-lists*) (quote [cl-struct-lambda-list DECF :MACRO (G299 &OPTIONAL (DELTA 1) &ENVIRONMENT G298) 2 1 nil nil nil ((:mandatory . "G299") (:optional . "[DELTA]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "DECLAIM" *cl-lambda-lists*) (quote [cl-struct-lambda-list DECLAIM :MACRO (&REST SPECS) 0 0 ((&REST SPECS)) nil nil ((:rest . "SPECS...")) [lambda-list-accept]]))

(setf (gethash "DECODE-FLOAT" *cl-lambda-lists*) (quote [cl-struct-lambda-list DECODE-FLOAT :FUNCTION (F) 2 0 nil nil nil ((:mandatory . "F")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "DECODE-UNIVERSAL-TIME" *cl-lambda-lists*) (quote [cl-struct-lambda-list DECODE-UNIVERSAL-TIME :FUNCTION (UNIVERSAL-TIME &OPTIONAL TIME-ZONE) 2 1 nil nil nil ((:mandatory . "UNIVERSAL-TIME") (:optional . "[TIME-ZONE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "DEFCLASS" *cl-lambda-lists*) (quote [cl-struct-lambda-list DEFCLASS :MACRO (&ENVIRONMENT ENV NAME %DIRECT-SUPERCLASSES %DIRECT-SLOTS &REST %OPTIONS) 4 0 ((&REST %OPTIONS) (&ENVIRONMENT ENV)) nil nil ((:mandatory . "NAME") (:mandatory . "%DIRECT-SUPERCLASSES") (:mandatory . "%DIRECT-SLOTS") (:rest . "%OPTIONS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "DEFCONSTANT" *cl-lambda-lists*) (quote [cl-struct-lambda-list DEFCONSTANT :MACRO (NAME VALUE &OPTIONAL DOCUMENTATION) 3 1 nil nil nil ((:mandatory . "NAME") (:mandatory . "VALUE") (:optional . "[DOCUMENTATION]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "DEFGENERIC" *cl-lambda-lists*) (quote [cl-struct-lambda-list DEFGENERIC :MACRO (FUN-NAME LAMBDA-LIST &BODY OPTIONS) 3 0 ((&BODY OPTIONS)) nil nil ((:mandatory . "FUN-NAME") (:mandatory . "LAMBDA-LIST") (:body . "OPTIONS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "DEFINE-COMPILER-MACRO" *cl-lambda-lists*) (quote [cl-struct-lambda-list DEFINE-COMPILER-MACRO :MACRO (NAME LAMBDA-LIST &BODY BODY) 3 0 ((&BODY BODY)) nil nil ((:mandatory . "NAME") (:mandatory . "LAMBDA-LIST") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "DEFINE-CONDITION" *cl-lambda-lists*) (quote [cl-struct-lambda-list DEFINE-CONDITION :MACRO (NAME (&REST PARENT-TYPES) (&REST SLOT-SPECS) &BODY OPTIONS) 4 0 ((&BODY OPTIONS)) nil nil ((:mandatory . "NAME") (:mandatory . "(&REST PARENT-TYPES)") (:mandatory . "(&REST SLOT-SPECS)") (:body . "OPTIONS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "DEFINE-METHOD-COMBINATION" *cl-lambda-lists*) (quote [cl-struct-lambda-list DEFINE-METHOD-COMBINATION :MACRO (&WHOLE FORM &REST ARGS) 0 0 ((&REST ARGS) (&WHOLE FORM)) nil nil ((:rest . "ARGS...")) [lambda-list-accept]]))

(setf (gethash "DEFINE-MODIFY-MACRO" *cl-lambda-lists*) (quote [cl-struct-lambda-list DEFINE-MODIFY-MACRO :MACRO (NAME LAMBDA-LIST FUNCTION &OPTIONAL DOC-STRING) 4 1 nil nil nil ((:mandatory . "NAME") (:mandatory . "LAMBDA-LIST") (:mandatory . "FUNCTION") (:optional . "[DOC-STRING]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "DEFINE-SETF-EXPANDER" *cl-lambda-lists*) (quote [cl-struct-lambda-list DEFINE-SETF-EXPANDER :MACRO (ACCESS-FN LAMBDA-LIST &BODY BODY) 3 0 ((&BODY BODY)) nil nil ((:mandatory . "ACCESS-FN") (:mandatory . "LAMBDA-LIST") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "DEFINE-SYMBOL-MACRO" *cl-lambda-lists*) (quote [cl-struct-lambda-list DEFINE-SYMBOL-MACRO :MACRO (NAME EXPANSION) 3 0 nil nil nil ((:mandatory . "NAME") (:mandatory . "EXPANSION")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "DEFMACRO" *cl-lambda-lists*) (quote [cl-struct-lambda-list DEFMACRO :MACRO (NAME LAMBDA-LIST &REST BODY) 3 0 ((&REST BODY)) nil nil ((:mandatory . "NAME") (:mandatory . "LAMBDA-LIST") (:rest . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "DEFMETHOD" *cl-lambda-lists*) (quote [cl-struct-lambda-list DEFMETHOD :MACRO (&REST ARGS &ENVIRONMENT ENV) 0 0 ((&REST ARGS) (&ENVIRONMENT ENV)) nil nil ((:rest . "ARGS...")) [lambda-list-accept]]))

(setf (gethash "DEFPACKAGE" *cl-lambda-lists*) (quote [cl-struct-lambda-list DEFPACKAGE :MACRO (PACKAGE &REST OPTIONS) 2 0 ((&REST OPTIONS)) nil nil ((:mandatory . "PACKAGE") (:rest . "OPTIONS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "DEFPARAMETER" *cl-lambda-lists*) (quote [cl-struct-lambda-list DEFPARAMETER :MACRO (VAR VAL &OPTIONAL (DOC nil DOCP)) 3 1 nil nil nil ((:mandatory . "VAR") (:mandatory . "VAL") (:optional . "[DOC]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "DEFSETF" *cl-lambda-lists*) (quote [cl-struct-lambda-list DEFSETF :MACRO (ACCESS-FN &REST REST) 2 0 ((&REST REST)) nil nil ((:mandatory . "ACCESS-FN") (:rest . "REST...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "DEFSTRUCT" *cl-lambda-lists*) (quote [cl-struct-lambda-list DEFSTRUCT :MACRO (NAME-AND-OPTIONS &REST SLOT-DESCRIPTIONS) 2 0 ((&REST SLOT-DESCRIPTIONS)) nil nil ((:mandatory . "NAME-AND-OPTIONS") (:rest . "SLOT-DESCRIPTIONS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "DEFTYPE" *cl-lambda-lists*) (quote [cl-struct-lambda-list DEFTYPE :MACRO (NAME ARGLIST &BODY BODY) 3 0 ((&BODY BODY)) nil nil ((:mandatory . "NAME") (:mandatory . "ARGLIST") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "DEFUN" *cl-lambda-lists*) (quote [cl-struct-lambda-list DEFUN :MACRO (&ENVIRONMENT ENV NAME ARGS &BODY BODY) 3 0 ((&BODY BODY) (&ENVIRONMENT ENV)) nil nil ((:mandatory . "NAME") (:mandatory . "ARGS") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "DEFVAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list DEFVAR :MACRO (VAR &OPTIONAL (VAL nil VALP) (DOC nil DOCP)) 2 2 nil nil nil ((:mandatory . "VAR") (:optional . "[VAL]") (:optional . "[DOC]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "DELETE" *cl-lambda-lists*) (quote [cl-struct-lambda-list DELETE :FUNCTION (ITEM SEQUENCE &KEY FROM-END (TEST (FUNCTION EQL)) (TEST-NOT nil) (START 0) (END nil) (COUNT nil) (KEY nil)) 3 0 nil nil (KEY COUNT END START TEST-NOT TEST FROM-END) ((:mandatory . "ITEM") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":TEST") (:key . ":TEST-NOT") (:key . ":START") (:key . ":END") (:key . ":COUNT") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "DELETE-DUPLICATES" *cl-lambda-lists*) (quote [cl-struct-lambda-list DELETE-DUPLICATES :FUNCTION (SEQUENCE &KEY (TEST (FUNCTION EQL)) (TEST-NOT nil) (START 0) (END nil) FROM-END (KEY nil)) 2 0 nil nil (KEY FROM-END END START TEST-NOT TEST) ((:mandatory . "SEQUENCE") (:key . ":TEST") (:key . ":TEST-NOT") (:key . ":START") (:key . ":END") (:key . ":FROM-END") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "DELETE-FILE" *cl-lambda-lists*) (quote [cl-struct-lambda-list DELETE-FILE :FUNCTION (FILE) 2 0 nil nil nil ((:mandatory . "FILE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "DELETE-IF" *cl-lambda-lists*) (quote [cl-struct-lambda-list DELETE-IF :FUNCTION (PREDICATE SEQUENCE &KEY FROM-END (START 0) (KEY nil) (END nil) (COUNT nil)) 3 0 nil nil (COUNT END KEY START FROM-END) ((:mandatory . "PREDICATE") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":START") (:key . ":KEY") (:key . ":END") (:key . ":COUNT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "DELETE-IF-NOT" *cl-lambda-lists*) (quote [cl-struct-lambda-list DELETE-IF-NOT :FUNCTION (PREDICATE SEQUENCE &KEY FROM-END (START 0) (END nil) (KEY nil) (COUNT nil)) 3 0 nil nil (COUNT KEY END START FROM-END) ((:mandatory . "PREDICATE") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":START") (:key . ":END") (:key . ":KEY") (:key . ":COUNT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "DELETE-PACKAGE" *cl-lambda-lists*) (quote [cl-struct-lambda-list DELETE-PACKAGE :FUNCTION (PACKAGE-OR-NAME) 2 0 nil nil nil ((:mandatory . "PACKAGE-OR-NAME")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "DENOMINATOR" *cl-lambda-lists*) (quote [cl-struct-lambda-list DENOMINATOR :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "DEPOSIT-FIELD" *cl-lambda-lists*) (quote [cl-struct-lambda-list DEPOSIT-FIELD :FUNCTION (NEWBYTE BYTESPEC INTEGER) 4 0 nil nil nil ((:mandatory . "NEWBYTE") (:mandatory . "BYTESPEC") (:mandatory . "INTEGER")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "DESCRIBE" *cl-lambda-lists*) (quote [cl-struct-lambda-list DESCRIBE :FUNCTION (X &OPTIONAL (STREAM-DESIGNATOR *STANDARD-OUTPUT*)) 2 1 nil nil nil ((:mandatory . "X") (:optional . "[STREAM-DESIGNATOR]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "DESCRIBE-OBJECT" *cl-lambda-lists*) (quote [cl-struct-lambda-list DESCRIBE-OBJECT :GENERIC (PACKAGE STREAM) 3 0 nil nil nil ((:mandatory . "PACKAGE") (:mandatory . "STREAM")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "DESTRUCTURING-BIND" *cl-lambda-lists*) (quote [cl-struct-lambda-list DESTRUCTURING-BIND :MACRO (LAMBDA-LIST ARG-LIST &REST BODY) 3 0 ((&REST BODY)) nil nil ((:mandatory . "LAMBDA-LIST") (:mandatory . "ARG-LIST") (:rest . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "DIGIT-CHAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list DIGIT-CHAR :FUNCTION (WEIGHT &OPTIONAL (RADIX 10)) 2 1 nil nil nil ((:mandatory . "WEIGHT") (:optional . "[RADIX]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "DIGIT-CHAR-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list DIGIT-CHAR-P :FUNCTION (CHAR &OPTIONAL (RADIX 10)) 2 1 nil nil nil ((:mandatory . "CHAR") (:optional . "[RADIX]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "DIRECTORY" *cl-lambda-lists*) (quote [cl-struct-lambda-list DIRECTORY :FUNCTION (PATHNAME &KEY) 2 0 nil nil nil ((:mandatory . "PATHNAME")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "DIRECTORY-NAMESTRING" *cl-lambda-lists*) (quote [cl-struct-lambda-list DIRECTORY-NAMESTRING :FUNCTION (PATHNAME) 2 0 nil nil nil ((:mandatory . "PATHNAME")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "DISASSEMBLE" *cl-lambda-lists*) (quote [cl-struct-lambda-list DISASSEMBLE :FUNCTION (OBJECT &KEY (STREAM *STANDARD-OUTPUT*) (USE-LABELS T)) 2 0 nil nil (USE-LABELS STREAM) ((:mandatory . "OBJECT") (:key . ":STREAM") (:key . ":USE-LABELS")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "DO" *cl-lambda-lists*) (quote [cl-struct-lambda-list DO :MACRO (VARLIST ENDLIST &BODY BODY) 3 0 ((&BODY BODY)) nil nil ((:mandatory . "VARLIST") (:mandatory . "ENDLIST") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "DO*" *cl-lambda-lists*) (quote [cl-struct-lambda-list DO* :MACRO (VARLIST ENDLIST &BODY BODY) 3 0 ((&BODY BODY)) nil nil ((:mandatory . "VARLIST") (:mandatory . "ENDLIST") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "DO-ALL-SYMBOLS" *cl-lambda-lists*) (quote [cl-struct-lambda-list DO-ALL-SYMBOLS :MACRO ((VAR &OPTIONAL RESULT-FORM) &BODY BODY-DECLS) 2 0 ((&BODY BODY-DECLS)) nil nil ((:mandatory . "(VAR &OPTIONAL RESULT-FORM)") (:body . "BODY-DECLS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "DO-EXTERNAL-SYMBOLS" *cl-lambda-lists*) (quote [cl-struct-lambda-list DO-EXTERNAL-SYMBOLS :MACRO ((VAR &OPTIONAL (PACKAGE (QUOTE *PACKAGE*)) RESULT-FORM) &BODY BODY-DECLS) 2 0 ((&BODY BODY-DECLS)) nil nil ((:mandatory . "(VAR &OPTIONAL (PACKAGE (QUOTE *PACKAGE*)) RESULT-FORM)") (:body . "BODY-DECLS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "DO-SYMBOLS" *cl-lambda-lists*) (quote [cl-struct-lambda-list DO-SYMBOLS :MACRO ((VAR &OPTIONAL (PACKAGE (QUOTE *PACKAGE*)) RESULT-FORM) &BODY BODY-DECLS) 2 0 ((&BODY BODY-DECLS)) nil nil ((:mandatory . "(VAR &OPTIONAL (PACKAGE (QUOTE *PACKAGE*)) RESULT-FORM)") (:body . "BODY-DECLS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "DOCUMENTATION" *cl-lambda-lists*) (quote [cl-struct-lambda-list DOCUMENTATION :GENERIC (SLOTD DOC-TYPE) 3 0 nil nil nil ((:mandatory . "SLOTD") (:mandatory . "DOC-TYPE")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "DOLIST" *cl-lambda-lists*) (quote [cl-struct-lambda-list DOLIST :MACRO ((VAR LIST &OPTIONAL (RESULT nil)) &BODY BODY) 2 0 ((&BODY BODY)) nil nil ((:mandatory . "(VAR LIST &OPTIONAL (RESULT nil))") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "DOTIMES" *cl-lambda-lists*) (quote [cl-struct-lambda-list DOTIMES :MACRO ((VAR COUNT &OPTIONAL (RESULT nil)) &BODY BODY) 2 0 ((&BODY BODY)) nil nil ((:mandatory . "(VAR COUNT &OPTIONAL (RESULT nil))") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "DPB" *cl-lambda-lists*) (quote [cl-struct-lambda-list DPB :FUNCTION (NEWBYTE BYTESPEC INTEGER) 4 0 nil nil nil ((:mandatory . "NEWBYTE") (:mandatory . "BYTESPEC") (:mandatory . "INTEGER")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "DRIBBLE" *cl-lambda-lists*) (quote [cl-struct-lambda-list DRIBBLE :FUNCTION (&OPTIONAL PATHNAME &KEY (IF-EXISTS APPEND)) 0 1 nil nil (IF-EXISTS) ((:optional . "[PATHNAME]") (:key . ":IF-EXISTS")) [lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "ECASE" *cl-lambda-lists*) (quote [cl-struct-lambda-list ECASE :MACRO (KEYFORM &BODY CASES) 2 0 ((&BODY CASES)) nil nil ((:mandatory . "KEYFORM") (:body . "CASES...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "ECHO-STREAM-INPUT-STREAM" *cl-lambda-lists*) (quote [cl-struct-lambda-list ECHO-STREAM-INPUT-STREAM :FUNCTION (INSTANCE) 2 0 nil nil nil ((:mandatory . "INSTANCE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ECHO-STREAM-OUTPUT-STREAM" *cl-lambda-lists*) (quote [cl-struct-lambda-list ECHO-STREAM-OUTPUT-STREAM :FUNCTION (INSTANCE) 2 0 nil nil nil ((:mandatory . "INSTANCE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ED" *cl-lambda-lists*) (quote [cl-struct-lambda-list ED :FUNCTION (&OPTIONAL X) 0 1 nil nil nil ((:optional . "[X]")) [lambda-list-accept lambda-list-close]]))

(setf (gethash "EIGHTH" *cl-lambda-lists*) (quote [cl-struct-lambda-list EIGHTH :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ELT" *cl-lambda-lists*) (quote [cl-struct-lambda-list ELT :FUNCTION (SEQUENCE INDEX) 3 0 nil nil nil ((:mandatory . "SEQUENCE") (:mandatory . "INDEX")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ENCODE-UNIVERSAL-TIME" *cl-lambda-lists*) (quote [cl-struct-lambda-list ENCODE-UNIVERSAL-TIME :FUNCTION (SECOND MINUTE HOUR DATE MONTH YEAR &OPTIONAL TIME-ZONE) 7 1 nil nil nil ((:mandatory . "SECOND") (:mandatory . "MINUTE") (:mandatory . "HOUR") (:mandatory . "DATE") (:mandatory . "MONTH") (:mandatory . "YEAR") (:optional . "[TIME-ZONE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ENDP" *cl-lambda-lists*) (quote [cl-struct-lambda-list ENDP :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ENOUGH-NAMESTRING" *cl-lambda-lists*) (quote [cl-struct-lambda-list ENOUGH-NAMESTRING :FUNCTION (PATHNAME &OPTIONAL (DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*)) 2 1 nil nil nil ((:mandatory . "PATHNAME") (:optional . "[DEFAULTS]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ENSURE-DIRECTORIES-EXIST" *cl-lambda-lists*) (quote [cl-struct-lambda-list ENSURE-DIRECTORIES-EXIST :FUNCTION (PATHSPEC &KEY VERBOSE (MODE 511)) 2 0 nil nil (MODE VERBOSE) ((:mandatory . "PATHSPEC") (:key . ":VERBOSE") (:key . ":MODE")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "ENSURE-GENERIC-FUNCTION" *cl-lambda-lists*) (quote [cl-struct-lambda-list ENSURE-GENERIC-FUNCTION :FUNCTION (FUN-NAME &REST ALL-KEYS &KEY ENVIRONMENT &ALLOW-OTHER-KEYS) 2 0 ((&REST ALL-KEYS) (&KEY ENVIRONMENT) (&ALLOW-OTHER-KEYS)) ((&ALLOW-OTHER-KEYS)) (ENVIRONMENT) ((:mandatory . "FUN-NAME") (:key . ":ENVIRONMENT") (:rest . "ALL-KEYS...")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-accept]]))

(setf (gethash "EQ" *cl-lambda-lists*) (quote [cl-struct-lambda-list EQ :FUNCTION (OBJ1 OBJ2) 3 0 nil nil nil ((:mandatory . "OBJ1") (:mandatory . "OBJ2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "EQL" *cl-lambda-lists*) (quote [cl-struct-lambda-list EQL :FUNCTION (OBJ1 OBJ2) 3 0 nil nil nil ((:mandatory . "OBJ1") (:mandatory . "OBJ2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "EQUAL" *cl-lambda-lists*) (quote [cl-struct-lambda-list EQUAL :FUNCTION (X Y) 3 0 nil nil nil ((:mandatory . "X") (:mandatory . "Y")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "EQUALP" *cl-lambda-lists*) (quote [cl-struct-lambda-list EQUALP :FUNCTION (X Y) 3 0 nil nil nil ((:mandatory . "X") (:mandatory . "Y")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ERROR" *cl-lambda-lists*) (quote [cl-struct-lambda-list ERROR :FUNCTION (DATUM &REST ARGUMENTS) 2 0 ((&REST ARGUMENTS)) nil nil ((:mandatory . "DATUM") (:rest . "ARGUMENTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "ETYPECASE" *cl-lambda-lists*) (quote [cl-struct-lambda-list ETYPECASE :MACRO (KEYFORM &BODY CASES) 2 0 ((&BODY CASES)) nil nil ((:mandatory . "KEYFORM") (:body . "CASES...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "EVAL" *cl-lambda-lists*) (quote [cl-struct-lambda-list EVAL :FUNCTION (ORIGINAL-EXP) 2 0 nil nil nil ((:mandatory . "ORIGINAL-EXP")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "EVAL-WHEN" *cl-lambda-lists*) (quote [cl-struct-lambda-list EVAL-WHEN :SPECIAL-OPERATOR (SITUATIONS &REST FORMS) 2 0 ((&REST FORMS)) nil nil ((:mandatory . "SITUATIONS") (:rest . "FORMS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "EVENP" *cl-lambda-lists*) (quote [cl-struct-lambda-list EVENP :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "EVERY" *cl-lambda-lists*) (quote [cl-struct-lambda-list EVERY :FUNCTION (PRED FIRST-SEQ &REST MORE-SEQS) 3 0 ((&REST MORE-SEQS)) nil nil ((:mandatory . "PRED") (:mandatory . "FIRST-SEQ") (:rest . "MORE-SEQS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "EXP" *cl-lambda-lists*) (quote [cl-struct-lambda-list EXP :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "EXPORT" *cl-lambda-lists*) (quote [cl-struct-lambda-list EXPORT :FUNCTION (SYMBOLS &OPTIONAL (PACKAGE (SANE-PACKAGE))) 2 1 nil nil nil ((:mandatory . "SYMBOLS") (:optional . "[PACKAGE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "EXPT" *cl-lambda-lists*) (quote [cl-struct-lambda-list EXPT :FUNCTION (BASE POWER) 3 0 nil nil nil ((:mandatory . "BASE") (:mandatory . "POWER")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FBOUNDP" *cl-lambda-lists*) (quote [cl-struct-lambda-list FBOUNDP :FUNCTION (NAME) 2 0 nil nil nil ((:mandatory . "NAME")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FCEILING" *cl-lambda-lists*) (quote [cl-struct-lambda-list FCEILING :FUNCTION (NUMBER &OPTIONAL (DIVISOR 1)) 2 1 nil nil nil ((:mandatory . "NUMBER") (:optional . "[DIVISOR]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FDEFINITION" *cl-lambda-lists*) (quote [cl-struct-lambda-list FDEFINITION :FUNCTION (NAME) 2 0 nil nil nil ((:mandatory . "NAME")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FFLOOR" *cl-lambda-lists*) (quote [cl-struct-lambda-list FFLOOR :FUNCTION (NUMBER &OPTIONAL (DIVISOR 1)) 2 1 nil nil nil ((:mandatory . "NUMBER") (:optional . "[DIVISOR]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FIFTH" *cl-lambda-lists*) (quote [cl-struct-lambda-list FIFTH :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FILE-AUTHOR" *cl-lambda-lists*) (quote [cl-struct-lambda-list FILE-AUTHOR :FUNCTION (FILE) 2 0 nil nil nil ((:mandatory . "FILE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FILE-ERROR-PATHNAME" *cl-lambda-lists*) (quote [cl-struct-lambda-list FILE-ERROR-PATHNAME :FUNCTION (CONDITION) 2 0 nil nil nil ((:mandatory . "CONDITION")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FILE-LENGTH" *cl-lambda-lists*) (quote [cl-struct-lambda-list FILE-LENGTH :FUNCTION (STREAM) 2 0 nil nil nil ((:mandatory . "STREAM")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FILE-NAMESTRING" *cl-lambda-lists*) (quote [cl-struct-lambda-list FILE-NAMESTRING :FUNCTION (PATHNAME) 2 0 nil nil nil ((:mandatory . "PATHNAME")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FILE-POSITION" *cl-lambda-lists*) (quote [cl-struct-lambda-list FILE-POSITION :FUNCTION (STREAM &OPTIONAL POSITION) 2 1 nil nil nil ((:mandatory . "STREAM") (:optional . "[POSITION]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FILE-STRING-LENGTH" *cl-lambda-lists*) (quote [cl-struct-lambda-list FILE-STRING-LENGTH :FUNCTION (STREAM OBJECT) 3 0 nil nil nil ((:mandatory . "STREAM") (:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FILE-WRITE-DATE" *cl-lambda-lists*) (quote [cl-struct-lambda-list FILE-WRITE-DATE :FUNCTION (FILE) 2 0 nil nil nil ((:mandatory . "FILE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FILL" *cl-lambda-lists*) (quote [cl-struct-lambda-list FILL :FUNCTION (SEQUENCE ITEM &KEY (START 0) (END nil)) 3 0 nil nil (END START) ((:mandatory . "SEQUENCE") (:mandatory . "ITEM") (:key . ":START") (:key . ":END")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "FILL-POINTER" *cl-lambda-lists*) (quote [cl-struct-lambda-list FILL-POINTER :FUNCTION (VECTOR) 2 0 nil nil nil ((:mandatory . "VECTOR")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FIND" *cl-lambda-lists*) (quote [cl-struct-lambda-list FIND :FUNCTION (ITEM SEQUENCE &KEY FROM-END (START 0) END KEY TEST TEST-NOT) 3 0 nil nil (TEST-NOT TEST KEY END START FROM-END) ((:mandatory . "ITEM") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":START") (:key . ":END") (:key . ":KEY") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "FIND-ALL-SYMBOLS" *cl-lambda-lists*) (quote [cl-struct-lambda-list FIND-ALL-SYMBOLS :FUNCTION (STRING-OR-SYMBOL) 2 0 nil nil nil ((:mandatory . "STRING-OR-SYMBOL")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FIND-CLASS" *cl-lambda-lists*) (quote [cl-struct-lambda-list FIND-CLASS :FUNCTION (SYMBOL &OPTIONAL (ERRORP T) ENVIRONMENT) 2 2 nil nil nil ((:mandatory . "SYMBOL") (:optional . "[ERRORP]") (:optional . "[ENVIRONMENT]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FIND-IF" *cl-lambda-lists*) (quote [cl-struct-lambda-list FIND-IF :FUNCTION (PREDICATE SEQUENCE &KEY FROM-END (START 0) END KEY) 3 0 nil nil (KEY END START FROM-END) ((:mandatory . "PREDICATE") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":START") (:key . ":END") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "FIND-IF-NOT" *cl-lambda-lists*) (quote [cl-struct-lambda-list FIND-IF-NOT :FUNCTION (PREDICATE SEQUENCE &KEY FROM-END (START 0) END KEY) 3 0 nil nil (KEY END START FROM-END) ((:mandatory . "PREDICATE") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":START") (:key . ":END") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "FIND-METHOD" *cl-lambda-lists*) (quote [cl-struct-lambda-list FIND-METHOD :GENERIC (GENERIC-FUNCTION QUALIFIERS SPECIALIZERS &OPTIONAL (ERRORP T)) 4 1 nil nil nil ((:mandatory . "GENERIC-FUNCTION") (:mandatory . "QUALIFIERS") (:mandatory . "SPECIALIZERS") (:optional . "[ERRORP]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FIND-PACKAGE" *cl-lambda-lists*) (quote [cl-struct-lambda-list FIND-PACKAGE :FUNCTION (PACKAGE-DESIGNATOR) 2 0 nil nil nil ((:mandatory . "PACKAGE-DESIGNATOR")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FIND-RESTART" *cl-lambda-lists*) (quote [cl-struct-lambda-list FIND-RESTART :FUNCTION (NAME &OPTIONAL CONDITION) 2 1 nil nil nil ((:mandatory . "NAME") (:optional . "[CONDITION]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FIND-SYMBOL" *cl-lambda-lists*) (quote [cl-struct-lambda-list FIND-SYMBOL :FUNCTION (NAME &OPTIONAL (PACKAGE (SANE-PACKAGE))) 2 1 nil nil nil ((:mandatory . "NAME") (:optional . "[PACKAGE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FINISH-OUTPUT" *cl-lambda-lists*) (quote [cl-struct-lambda-list FINISH-OUTPUT :FUNCTION (&OPTIONAL (STREAM *STANDARD-OUTPUT*)) 0 1 nil nil nil ((:optional . "[STREAM]")) [lambda-list-accept lambda-list-close]]))

(setf (gethash "FIRST" *cl-lambda-lists*) (quote [cl-struct-lambda-list FIRST :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FLET" *cl-lambda-lists*) (quote [cl-struct-lambda-list FLET :SPECIAL-OPERATOR (DEFINITIONS &BODY BODY) 2 0 ((&BODY BODY)) nil nil ((:mandatory . "DEFINITIONS") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "FLOAT" *cl-lambda-lists*) (quote [cl-struct-lambda-list FLOAT :FUNCTION (NUMBER &OPTIONAL (OTHER nil OTHERP)) 2 1 nil nil nil ((:mandatory . "NUMBER") (:optional . "[OTHER]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FLOAT-DIGITS" *cl-lambda-lists*) (quote [cl-struct-lambda-list FLOAT-DIGITS :FUNCTION (F) 2 0 nil nil nil ((:mandatory . "F")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FLOAT-PRECISION" *cl-lambda-lists*) (quote [cl-struct-lambda-list FLOAT-PRECISION :FUNCTION (F) 2 0 nil nil nil ((:mandatory . "F")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FLOAT-RADIX" *cl-lambda-lists*) (quote [cl-struct-lambda-list FLOAT-RADIX :FUNCTION (X) 2 0 nil nil nil ((:mandatory . "X")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FLOAT-SIGN" *cl-lambda-lists*) (quote [cl-struct-lambda-list FLOAT-SIGN :FUNCTION (FLOAT1 &OPTIONAL (FLOAT2 (FLOAT 1 FLOAT1))) 2 1 nil nil nil ((:mandatory . "FLOAT1") (:optional . "[FLOAT2]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FLOATP" *cl-lambda-lists*) (quote [cl-struct-lambda-list FLOATP :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FLOOR" *cl-lambda-lists*) (quote [cl-struct-lambda-list FLOOR :FUNCTION (NUMBER &OPTIONAL (DIVISOR 1)) 2 1 nil nil nil ((:mandatory . "NUMBER") (:optional . "[DIVISOR]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FMAKUNBOUND" *cl-lambda-lists*) (quote [cl-struct-lambda-list FMAKUNBOUND :FUNCTION (NAME) 2 0 nil nil nil ((:mandatory . "NAME")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FORCE-OUTPUT" *cl-lambda-lists*) (quote [cl-struct-lambda-list FORCE-OUTPUT :FUNCTION (&OPTIONAL (STREAM *STANDARD-OUTPUT*)) 0 1 nil nil nil ((:optional . "[STREAM]")) [lambda-list-accept lambda-list-close]]))

(setf (gethash "FORMAT" *cl-lambda-lists*) (quote [cl-struct-lambda-list FORMAT :FUNCTION (DESTINATION CONTROL-STRING &REST FORMAT-ARGUMENTS) 3 0 ((&REST FORMAT-ARGUMENTS)) nil nil ((:mandatory . "DESTINATION") (:mandatory . "CONTROL-STRING") (:rest . "FORMAT-ARGUMENTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "FORMATTER" *cl-lambda-lists*) (quote [cl-struct-lambda-list FORMATTER :MACRO (CONTROL-STRING) 2 0 nil nil nil ((:mandatory . "CONTROL-STRING")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FOURTH" *cl-lambda-lists*) (quote [cl-struct-lambda-list FOURTH :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FRESH-LINE" *cl-lambda-lists*) (quote [cl-struct-lambda-list FRESH-LINE :FUNCTION (&OPTIONAL (STREAM *STANDARD-OUTPUT*)) 0 1 nil nil nil ((:optional . "[STREAM]")) [lambda-list-accept lambda-list-close]]))

(setf (gethash "FROUND" *cl-lambda-lists*) (quote [cl-struct-lambda-list FROUND :FUNCTION (NUMBER &OPTIONAL (DIVISOR 1)) 2 1 nil nil nil ((:mandatory . "NUMBER") (:optional . "[DIVISOR]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FTRUNCATE" *cl-lambda-lists*) (quote [cl-struct-lambda-list FTRUNCATE :FUNCTION (NUMBER &OPTIONAL (DIVISOR 1)) 2 1 nil nil nil ((:mandatory . "NUMBER") (:optional . "[DIVISOR]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FUNCALL" *cl-lambda-lists*) (quote [cl-struct-lambda-list FUNCALL :FUNCTION (FUNCTION &REST ARGUMENTS) 2 0 ((&REST ARGUMENTS)) nil nil ((:mandatory . "FUNCTION") (:rest . "ARGUMENTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "FUNCTION" *cl-lambda-lists*) (quote [cl-struct-lambda-list FUNCTION :SPECIAL-OPERATOR (THING) 2 0 nil nil nil ((:mandatory . "THING")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FUNCTION-KEYWORDS" *cl-lambda-lists*) (quote [cl-struct-lambda-list FUNCTION-KEYWORDS :GENERIC (METHOD) 2 0 nil nil nil ((:mandatory . "METHOD")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FUNCTION-LAMBDA-EXPRESSION" *cl-lambda-lists*) (quote [cl-struct-lambda-list FUNCTION-LAMBDA-EXPRESSION :FUNCTION (FUN) 2 0 nil nil nil ((:mandatory . "FUN")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "FUNCTIONP" *cl-lambda-lists*) (quote [cl-struct-lambda-list FUNCTIONP :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "GCD" *cl-lambda-lists*) (quote [cl-struct-lambda-list GCD :FUNCTION (&REST NUMBERS) 0 0 ((&REST NUMBERS)) nil nil ((:rest . "NUMBERS...")) [lambda-list-accept]]))

(setf (gethash "GENSYM" *cl-lambda-lists*) (quote [cl-struct-lambda-list GENSYM :FUNCTION (&OPTIONAL (THING "G")) 0 1 nil nil nil ((:optional . "[THING]")) [lambda-list-accept lambda-list-close]]))

(setf (gethash "GENTEMP" *cl-lambda-lists*) (quote [cl-struct-lambda-list GENTEMP :FUNCTION (&OPTIONAL (PREFIX "T") (PACKAGE (SANE-PACKAGE))) 0 2 nil nil nil ((:optional . "[PREFIX]") (:optional . "[PACKAGE]")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "GET" *cl-lambda-lists*) (quote [cl-struct-lambda-list GET :FUNCTION (SYMBOL INDICATOR &OPTIONAL (DEFAULT nil)) 3 1 nil nil nil ((:mandatory . "SYMBOL") (:mandatory . "INDICATOR") (:optional . "[DEFAULT]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "GET-DECODED-TIME" *cl-lambda-lists*) (quote [cl-struct-lambda-list GET-DECODED-TIME :FUNCTION nil 1 0 nil nil nil nil [lambda-list-accept lambda-list-close]]))

(setf (gethash "GET-DISPATCH-MACRO-CHARACTER" *cl-lambda-lists*) (quote [cl-struct-lambda-list GET-DISPATCH-MACRO-CHARACTER :FUNCTION (DISP-CHAR SUB-CHAR &OPTIONAL (RT *READTABLE*)) 3 1 nil nil nil ((:mandatory . "DISP-CHAR") (:mandatory . "SUB-CHAR") (:optional . "[RT]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "GET-INTERNAL-REAL-TIME" *cl-lambda-lists*) (quote [cl-struct-lambda-list GET-INTERNAL-REAL-TIME :FUNCTION nil 1 0 nil nil nil nil [lambda-list-accept lambda-list-close]]))

(setf (gethash "GET-INTERNAL-RUN-TIME" *cl-lambda-lists*) (quote [cl-struct-lambda-list GET-INTERNAL-RUN-TIME :FUNCTION nil 1 0 nil nil nil nil [lambda-list-accept lambda-list-close]]))

(setf (gethash "GET-MACRO-CHARACTER" *cl-lambda-lists*) (quote [cl-struct-lambda-list GET-MACRO-CHARACTER :FUNCTION (CHAR &OPTIONAL (READTABLE *READTABLE*)) 2 1 nil nil nil ((:mandatory . "CHAR") (:optional . "[READTABLE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "GET-OUTPUT-STREAM-STRING" *cl-lambda-lists*) (quote [cl-struct-lambda-list GET-OUTPUT-STREAM-STRING :FUNCTION (STREAM) 2 0 nil nil nil ((:mandatory . "STREAM")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "GET-PROPERTIES" *cl-lambda-lists*) (quote [cl-struct-lambda-list GET-PROPERTIES :FUNCTION (PLACE INDICATOR-LIST) 3 0 nil nil nil ((:mandatory . "PLACE") (:mandatory . "INDICATOR-LIST")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "GET-SETF-EXPANSION" *cl-lambda-lists*) (quote [cl-struct-lambda-list GET-SETF-EXPANSION :FUNCTION (FORM &OPTIONAL ENVIRONMENT) 2 1 nil nil nil ((:mandatory . "FORM") (:optional . "[ENVIRONMENT]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "GET-UNIVERSAL-TIME" *cl-lambda-lists*) (quote [cl-struct-lambda-list GET-UNIVERSAL-TIME :FUNCTION nil 1 0 nil nil nil nil [lambda-list-accept lambda-list-close]]))

(setf (gethash "GETF" *cl-lambda-lists*) (quote [cl-struct-lambda-list GETF :FUNCTION (PLACE INDICATOR &OPTIONAL (DEFAULT nil)) 3 1 nil nil nil ((:mandatory . "PLACE") (:mandatory . "INDICATOR") (:optional . "[DEFAULT]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "GETHASH" *cl-lambda-lists*) (quote [cl-struct-lambda-list GETHASH :FUNCTION (KEY HASH-TABLE &OPTIONAL DEFAULT) 3 1 nil nil nil ((:mandatory . "KEY") (:mandatory . "HASH-TABLE") (:optional . "[DEFAULT]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "GO" *cl-lambda-lists*) (quote [cl-struct-lambda-list GO :SPECIAL-OPERATOR (TAG) 2 0 nil nil nil ((:mandatory . "TAG")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "GRAPHIC-CHAR-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list GRAPHIC-CHAR-P :FUNCTION (CHAR) 2 0 nil nil nil ((:mandatory . "CHAR")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "HANDLER-BIND" *cl-lambda-lists*) (quote [cl-struct-lambda-list HANDLER-BIND :MACRO (BINDINGS &BODY FORMS) 2 0 ((&BODY FORMS)) nil nil ((:mandatory . "BINDINGS") (:body . "FORMS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "HANDLER-CASE" *cl-lambda-lists*) (quote [cl-struct-lambda-list HANDLER-CASE :MACRO (FORM &REST CASES) 2 0 ((&REST CASES)) nil nil ((:mandatory . "FORM") (:rest . "CASES...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "HASH-TABLE-COUNT" *cl-lambda-lists*) (quote [cl-struct-lambda-list HASH-TABLE-COUNT :FUNCTION (HASH-TABLE) 2 0 nil nil nil ((:mandatory . "HASH-TABLE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "HASH-TABLE-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list HASH-TABLE-P :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "HASH-TABLE-REHASH-SIZE" *cl-lambda-lists*) (quote [cl-struct-lambda-list HASH-TABLE-REHASH-SIZE :FUNCTION (INSTANCE) 2 0 nil nil nil ((:mandatory . "INSTANCE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "HASH-TABLE-REHASH-THRESHOLD" *cl-lambda-lists*) (quote [cl-struct-lambda-list HASH-TABLE-REHASH-THRESHOLD :FUNCTION (INSTANCE) 2 0 nil nil nil ((:mandatory . "INSTANCE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "HASH-TABLE-SIZE" *cl-lambda-lists*) (quote [cl-struct-lambda-list HASH-TABLE-SIZE :FUNCTION (HASH-TABLE) 2 0 nil nil nil ((:mandatory . "HASH-TABLE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "HASH-TABLE-TEST" *cl-lambda-lists*) (quote [cl-struct-lambda-list HASH-TABLE-TEST :FUNCTION (INSTANCE) 2 0 nil nil nil ((:mandatory . "INSTANCE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "HOST-NAMESTRING" *cl-lambda-lists*) (quote [cl-struct-lambda-list HOST-NAMESTRING :FUNCTION (PATHNAME) 2 0 nil nil nil ((:mandatory . "PATHNAME")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "IDENTITY" *cl-lambda-lists*) (quote [cl-struct-lambda-list IDENTITY :FUNCTION (THING) 2 0 nil nil nil ((:mandatory . "THING")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "IF" *cl-lambda-lists*) (quote [cl-struct-lambda-list IF :SPECIAL-OPERATOR (TEST THEN &OPTIONAL ELSE) 3 1 nil nil nil ((:mandatory . "TEST") (:mandatory . "THEN") (:optional . "[ELSE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "IGNORE-ERRORS" *cl-lambda-lists*) (quote [cl-struct-lambda-list IGNORE-ERRORS :MACRO (&REST FORMS) 0 0 ((&REST FORMS)) nil nil ((:rest . "FORMS...")) [lambda-list-accept]]))

(setf (gethash "IMAGPART" *cl-lambda-lists*) (quote [cl-struct-lambda-list IMAGPART :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "IMPORT" *cl-lambda-lists*) (quote [cl-struct-lambda-list IMPORT :FUNCTION (SYMBOLS &OPTIONAL (PACKAGE (SANE-PACKAGE))) 2 1 nil nil nil ((:mandatory . "SYMBOLS") (:optional . "[PACKAGE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "IN-PACKAGE" *cl-lambda-lists*) (quote [cl-struct-lambda-list IN-PACKAGE :MACRO (PACKAGE-DESIGNATOR) 2 0 nil nil nil ((:mandatory . "PACKAGE-DESIGNATOR")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "INCF" *cl-lambda-lists*) (quote [cl-struct-lambda-list INCF :MACRO (G280 &OPTIONAL (DELTA 1) &ENVIRONMENT G279) 2 1 nil nil nil ((:mandatory . "G280") (:optional . "[DELTA]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "INITIALIZE-INSTANCE" *cl-lambda-lists*) (quote [cl-struct-lambda-list INITIALIZE-INSTANCE :GENERIC (INSTANCE &REST INITARGS) 2 0 ((&REST INITARGS)) nil nil ((:mandatory . "INSTANCE") (:rest . "INITARGS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "INPUT-STREAM-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list INPUT-STREAM-P :GENERIC (NON-STREAM) 2 0 nil nil nil ((:mandatory . "NON-STREAM")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "INSPECT" *cl-lambda-lists*) (quote [cl-struct-lambda-list INSPECT :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "INTEGER-DECODE-FLOAT" *cl-lambda-lists*) (quote [cl-struct-lambda-list INTEGER-DECODE-FLOAT :FUNCTION (X) 2 0 nil nil nil ((:mandatory . "X")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "INTEGER-LENGTH" *cl-lambda-lists*) (quote [cl-struct-lambda-list INTEGER-LENGTH :FUNCTION (INTEGER) 2 0 nil nil nil ((:mandatory . "INTEGER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "INTEGERP" *cl-lambda-lists*) (quote [cl-struct-lambda-list INTEGERP :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "INTERACTIVE-STREAM-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list INTERACTIVE-STREAM-P :FUNCTION (STREAM) 2 0 nil nil nil ((:mandatory . "STREAM")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "INTERN" *cl-lambda-lists*) (quote [cl-struct-lambda-list INTERN :FUNCTION (NAME &OPTIONAL (PACKAGE (SANE-PACKAGE))) 2 1 nil nil nil ((:mandatory . "NAME") (:optional . "[PACKAGE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "INTERSECTION" *cl-lambda-lists*) (quote [cl-struct-lambda-list INTERSECTION :FUNCTION (LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT nil NOTP)) 3 0 nil nil (TEST-NOT TEST KEY) ((:mandatory . "LIST1") (:mandatory . "LIST2") (:key . ":KEY") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "INVALID-METHOD-ERROR" *cl-lambda-lists*) (quote [cl-struct-lambda-list INVALID-METHOD-ERROR :FUNCTION (METHOD FORMAT-CONTROL &REST FORMAT-ARGUMENTS) 3 0 ((&REST FORMAT-ARGUMENTS)) nil nil ((:mandatory . "METHOD") (:mandatory . "FORMAT-CONTROL") (:rest . "FORMAT-ARGUMENTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "INVOKE-DEBUGGER" *cl-lambda-lists*) (quote [cl-struct-lambda-list INVOKE-DEBUGGER :FUNCTION (CONDITION) 2 0 nil nil nil ((:mandatory . "CONDITION")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "INVOKE-RESTART" *cl-lambda-lists*) (quote [cl-struct-lambda-list INVOKE-RESTART :FUNCTION (RESTART &REST VALUES) 2 0 ((&REST VALUES)) nil nil ((:mandatory . "RESTART") (:rest . "VALUES...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "INVOKE-RESTART-INTERACTIVELY" *cl-lambda-lists*) (quote [cl-struct-lambda-list INVOKE-RESTART-INTERACTIVELY :FUNCTION (RESTART) 2 0 nil nil nil ((:mandatory . "RESTART")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ISQRT" *cl-lambda-lists*) (quote [cl-struct-lambda-list ISQRT :FUNCTION (N) 2 0 nil nil nil ((:mandatory . "N")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "KEYWORDP" *cl-lambda-lists*) (quote [cl-struct-lambda-list KEYWORDP :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LABELS" *cl-lambda-lists*) (quote [cl-struct-lambda-list LABELS :SPECIAL-OPERATOR (DEFINITIONS &BODY BODY) 2 0 ((&BODY BODY)) nil nil ((:mandatory . "DEFINITIONS") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "LAMBDA" *cl-lambda-lists*) (quote [cl-struct-lambda-list LAMBDA :MACRO (&WHOLE WHOLE ARGS &BODY BODY) 2 0 ((&BODY BODY) (&WHOLE WHOLE)) nil nil ((:mandatory . "ARGS") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "LAST" *cl-lambda-lists*) (quote [cl-struct-lambda-list LAST :FUNCTION (LIST &OPTIONAL (N 1)) 2 1 nil nil nil ((:mandatory . "LIST") (:optional . "[N]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LCM" *cl-lambda-lists*) (quote [cl-struct-lambda-list LCM :FUNCTION (&REST NUMBERS) 0 0 ((&REST NUMBERS)) nil nil ((:rest . "NUMBERS...")) [lambda-list-accept]]))

(setf (gethash "LDB" *cl-lambda-lists*) (quote [cl-struct-lambda-list LDB :FUNCTION (BYTESPEC INTEGER) 3 0 nil nil nil ((:mandatory . "BYTESPEC") (:mandatory . "INTEGER")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LDB-TEST" *cl-lambda-lists*) (quote [cl-struct-lambda-list LDB-TEST :FUNCTION (BYTESPEC INTEGER) 3 0 nil nil nil ((:mandatory . "BYTESPEC") (:mandatory . "INTEGER")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LDIFF" *cl-lambda-lists*) (quote [cl-struct-lambda-list LDIFF :FUNCTION (LIST OBJECT) 3 0 nil nil nil ((:mandatory . "LIST") (:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LENGTH" *cl-lambda-lists*) (quote [cl-struct-lambda-list LENGTH :FUNCTION (SEQUENCE) 2 0 nil nil nil ((:mandatory . "SEQUENCE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LET" *cl-lambda-lists*) (quote [cl-struct-lambda-list LET :SPECIAL-OPERATOR (BINDINGS &BODY BODY) 2 0 ((&BODY BODY)) nil nil ((:mandatory . "BINDINGS") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "LET*" *cl-lambda-lists*) (quote [cl-struct-lambda-list LET* :SPECIAL-OPERATOR (BINDINGS &BODY BODY) 2 0 ((&BODY BODY)) nil nil ((:mandatory . "BINDINGS") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "LISP-IMPLEMENTATION-TYPE" *cl-lambda-lists*) (quote [cl-struct-lambda-list LISP-IMPLEMENTATION-TYPE :FUNCTION nil 1 0 nil nil nil nil [lambda-list-accept lambda-list-close]]))

(setf (gethash "LISP-IMPLEMENTATION-VERSION" *cl-lambda-lists*) (quote [cl-struct-lambda-list LISP-IMPLEMENTATION-VERSION :FUNCTION nil 1 0 nil nil nil nil [lambda-list-accept lambda-list-close]]))

(setf (gethash "LIST" *cl-lambda-lists*) (quote [cl-struct-lambda-list LIST :FUNCTION (&REST ARGS) 0 0 ((&REST ARGS)) nil nil ((:rest . "ARGS...")) [lambda-list-accept]]))

(setf (gethash "LIST*" *cl-lambda-lists*) (quote [cl-struct-lambda-list LIST* :FUNCTION (ARG &REST OTHERS) 2 0 ((&REST OTHERS)) nil nil ((:mandatory . "ARG") (:rest . "OTHERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "LIST-ALL-PACKAGES" *cl-lambda-lists*) (quote [cl-struct-lambda-list LIST-ALL-PACKAGES :FUNCTION nil 1 0 nil nil nil nil [lambda-list-accept lambda-list-close]]))

(setf (gethash "LIST-LENGTH" *cl-lambda-lists*) (quote [cl-struct-lambda-list LIST-LENGTH :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LISTEN" *cl-lambda-lists*) (quote [cl-struct-lambda-list LISTEN :FUNCTION (&OPTIONAL (STREAM *STANDARD-INPUT*)) 0 1 nil nil nil ((:optional . "[STREAM]")) [lambda-list-accept lambda-list-close]]))

(setf (gethash "LISTP" *cl-lambda-lists*) (quote [cl-struct-lambda-list LISTP :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LOAD" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOAD :FUNCTION (FILESPEC &KEY (VERBOSE *LOAD-VERBOSE*) (PRINT *LOAD-PRINT*) (IF-DOES-NOT-EXIST T) (EXTERNAL-FORMAT DEFAULT)) 2 0 nil nil (EXTERNAL-FORMAT IF-DOES-NOT-EXIST PRINT VERBOSE) ((:mandatory . "FILESPEC") (:key . ":VERBOSE") (:key . ":PRINT") (:key . ":IF-DOES-NOT-EXIST") (:key . ":EXTERNAL-FORMAT")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "LOAD-LOGICAL-PATHNAME-TRANSLATIONS" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOAD-LOGICAL-PATHNAME-TRANSLATIONS :FUNCTION (HOST) 2 0 nil nil nil ((:mandatory . "HOST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LOAD-TIME-VALUE" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOAD-TIME-VALUE :SPECIAL-OPERATOR (FORM &OPTIONAL READ-ONLY-P) 2 1 nil nil nil ((:mandatory . "FORM") (:optional . "[READ-ONLY-P]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LOCALLY" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOCALLY :SPECIAL-OPERATOR (&BODY BODY) 0 0 ((&BODY BODY)) nil nil ((:body . "BODY...")) [lambda-list-accept]]))

(setf (gethash "LOG" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOG :FUNCTION (NUMBER &OPTIONAL (BASE nil BASE-P)) 2 1 nil nil nil ((:mandatory . "NUMBER") (:optional . "[BASE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LOGAND" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOGAND :FUNCTION (&REST INTEGERS) 0 0 ((&REST INTEGERS)) nil nil ((:rest . "INTEGERS...")) [lambda-list-accept]]))

(setf (gethash "LOGANDC1" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOGANDC1 :FUNCTION (INTEGER1 INTEGER2) 3 0 nil nil nil ((:mandatory . "INTEGER1") (:mandatory . "INTEGER2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LOGANDC2" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOGANDC2 :FUNCTION (INTEGER1 INTEGER2) 3 0 nil nil nil ((:mandatory . "INTEGER1") (:mandatory . "INTEGER2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LOGBITP" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOGBITP :FUNCTION (INDEX INTEGER) 3 0 nil nil nil ((:mandatory . "INDEX") (:mandatory . "INTEGER")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LOGCOUNT" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOGCOUNT :FUNCTION (INTEGER) 2 0 nil nil nil ((:mandatory . "INTEGER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LOGEQV" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOGEQV :FUNCTION (&REST INTEGERS) 0 0 ((&REST INTEGERS)) nil nil ((:rest . "INTEGERS...")) [lambda-list-accept]]))

(setf (gethash "LOGICAL-PATHNAME" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOGICAL-PATHNAME :FUNCTION (PATHSPEC) 2 0 nil nil nil ((:mandatory . "PATHSPEC")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LOGICAL-PATHNAME-TRANSLATIONS" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOGICAL-PATHNAME-TRANSLATIONS :FUNCTION (HOST) 2 0 nil nil nil ((:mandatory . "HOST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LOGIOR" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOGIOR :FUNCTION (&REST INTEGERS) 0 0 ((&REST INTEGERS)) nil nil ((:rest . "INTEGERS...")) [lambda-list-accept]]))

(setf (gethash "LOGNAND" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOGNAND :FUNCTION (INTEGER1 INTEGER2) 3 0 nil nil nil ((:mandatory . "INTEGER1") (:mandatory . "INTEGER2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LOGNOR" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOGNOR :FUNCTION (INTEGER1 INTEGER2) 3 0 nil nil nil ((:mandatory . "INTEGER1") (:mandatory . "INTEGER2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LOGNOT" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOGNOT :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LOGORC1" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOGORC1 :FUNCTION (INTEGER1 INTEGER2) 3 0 nil nil nil ((:mandatory . "INTEGER1") (:mandatory . "INTEGER2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LOGORC2" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOGORC2 :FUNCTION (INTEGER1 INTEGER2) 3 0 nil nil nil ((:mandatory . "INTEGER1") (:mandatory . "INTEGER2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LOGTEST" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOGTEST :FUNCTION (INTEGER1 INTEGER2) 3 0 nil nil nil ((:mandatory . "INTEGER1") (:mandatory . "INTEGER2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "LOGXOR" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOGXOR :FUNCTION (&REST INTEGERS) 0 0 ((&REST INTEGERS)) nil nil ((:rest . "INTEGERS...")) [lambda-list-accept]]))

(setf (gethash "LONG-SITE-NAME" *cl-lambda-lists*) (quote [cl-struct-lambda-list LONG-SITE-NAME :FUNCTION nil 1 0 nil nil nil nil [lambda-list-accept lambda-list-close]]))

(setf (gethash "LOOP" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOOP :MACRO (&ENVIRONMENT ENV &REST KEYWORDS-AND-FORMS) 0 0 ((&REST KEYWORDS-AND-FORMS) (&ENVIRONMENT ENV)) nil nil ((:rest . "KEYWORDS-AND-FORMS...")) [lambda-list-accept]]))

(setf (gethash "LOOP-FINISH" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOOP-FINISH :MACRO nil 1 0 nil nil nil nil [lambda-list-accept lambda-list-close]]))

(setf (gethash "LOWER-CASE-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list LOWER-CASE-P :FUNCTION (CHAR) 2 0 nil nil nil ((:mandatory . "CHAR")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "MACHINE-INSTANCE" *cl-lambda-lists*) (quote [cl-struct-lambda-list MACHINE-INSTANCE :FUNCTION nil 1 0 nil nil nil nil [lambda-list-accept lambda-list-close]]))

(setf (gethash "MACHINE-TYPE" *cl-lambda-lists*) (quote [cl-struct-lambda-list MACHINE-TYPE :FUNCTION nil 1 0 nil nil nil nil [lambda-list-accept lambda-list-close]]))

(setf (gethash "MACHINE-VERSION" *cl-lambda-lists*) (quote [cl-struct-lambda-list MACHINE-VERSION :FUNCTION nil 1 0 nil nil nil nil [lambda-list-accept lambda-list-close]]))

(setf (gethash "MACRO-FUNCTION" *cl-lambda-lists*) (quote [cl-struct-lambda-list MACRO-FUNCTION :FUNCTION (SYMBOL &OPTIONAL ENV) 2 1 nil nil nil ((:mandatory . "SYMBOL") (:optional . "[ENV]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "MACROEXPAND" *cl-lambda-lists*) (quote [cl-struct-lambda-list MACROEXPAND :FUNCTION (FORM &OPTIONAL ENV) 2 1 nil nil nil ((:mandatory . "FORM") (:optional . "[ENV]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "MACROEXPAND-1" *cl-lambda-lists*) (quote [cl-struct-lambda-list MACROEXPAND-1 :FUNCTION (FORM &OPTIONAL ENV) 2 1 nil nil nil ((:mandatory . "FORM") (:optional . "[ENV]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "MACROLET" *cl-lambda-lists*) (quote [cl-struct-lambda-list MACROLET :SPECIAL-OPERATOR (DEFINITIONS &REST BODY) 2 0 ((&REST BODY)) nil nil ((:mandatory . "DEFINITIONS") (:rest . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "MAKE-ARRAY" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-ARRAY :FUNCTION (DIMENSIONS &KEY (ELEMENT-TYPE T) (INITIAL-ELEMENT nil INITIAL-ELEMENT-P) (INITIAL-CONTENTS nil INITIAL-CONTENTS-P) ADJUSTABLE FILL-POINTER DISPLACED-TO DISPLACED-INDEX-OFFSET) 2 0 nil nil (DISPLACED-INDEX-OFFSET DISPLACED-TO FILL-POINTER ADJUSTABLE INITIAL-CONTENTS INITIAL-ELEMENT ELEMENT-TYPE) ((:mandatory . "DIMENSIONS") (:key . ":ELEMENT-TYPE") (:key . ":INITIAL-ELEMENT") (:key . ":INITIAL-CONTENTS") (:key . ":ADJUSTABLE") (:key . ":FILL-POINTER") (:key . ":DISPLACED-TO") (:key . ":DISPLACED-INDEX-OFFSET")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "MAKE-BROADCAST-STREAM" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-BROADCAST-STREAM :FUNCTION (&REST STREAMS) 0 0 ((&REST STREAMS)) nil nil ((:rest . "STREAMS...")) [lambda-list-accept]]))

(setf (gethash "MAKE-CONCATENATED-STREAM" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-CONCATENATED-STREAM :FUNCTION (&REST STREAMS) 0 0 ((&REST STREAMS)) nil nil ((:rest . "STREAMS...")) [lambda-list-accept]]))

(setf (gethash "MAKE-CONDITION" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-CONDITION :FUNCTION (THING &REST ARGS) 2 0 ((&REST ARGS)) nil nil ((:mandatory . "THING") (:rest . "ARGS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "MAKE-DISPATCH-MACRO-CHARACTER" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-DISPATCH-MACRO-CHARACTER :FUNCTION (CHAR &OPTIONAL (NON-TERMINATING-P nil) (RT *READTABLE*)) 2 2 nil nil nil ((:mandatory . "CHAR") (:optional . "[NON-TERMINATING-P]") (:optional . "[RT]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "MAKE-ECHO-STREAM" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-ECHO-STREAM :FUNCTION (INPUT-STREAM OUTPUT-STREAM) 3 0 nil nil nil ((:mandatory . "INPUT-STREAM") (:mandatory . "OUTPUT-STREAM")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "MAKE-HASH-TABLE" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-HASH-TABLE :FUNCTION (&KEY (TEST (QUOTE EQL)) (SIZE +MIN-HASH-TABLE-SIZE+) (REHASH-SIZE 1.5) (REHASH-THRESHOLD 1) (WEAK-P nil)) 0 0 nil nil (WEAK-P REHASH-THRESHOLD REHASH-SIZE SIZE TEST) ((:key . ":TEST") (:key . ":SIZE") (:key . ":REHASH-SIZE") (:key . ":REHASH-THRESHOLD") (:key . ":WEAK-P")) [lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "MAKE-INSTANCE" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-INSTANCE :GENERIC (CLASS &REST INITARGS) 2 0 ((&REST INITARGS)) nil nil ((:mandatory . "CLASS") (:rest . "INITARGS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "MAKE-INSTANCES-OBSOLETE" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-INSTANCES-OBSOLETE :GENERIC (CLASS) 2 0 nil nil nil ((:mandatory . "CLASS")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "MAKE-LIST" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-LIST :FUNCTION (SIZE &KEY INITIAL-ELEMENT) 2 0 nil nil (INITIAL-ELEMENT) ((:mandatory . "SIZE") (:key . ":INITIAL-ELEMENT")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "MAKE-LOAD-FORM" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-LOAD-FORM :GENERIC (RANDOM-STATE &OPTIONAL ENVIRONMENT) 2 1 nil nil nil ((:mandatory . "RANDOM-STATE") (:optional . "[ENVIRONMENT]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "MAKE-LOAD-FORM-SAVING-SLOTS" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-LOAD-FORM-SAVING-SLOTS :FUNCTION (OBJECT &KEY SLOT-NAMES ENVIRONMENT) 2 0 nil nil (ENVIRONMENT SLOT-NAMES) ((:mandatory . "OBJECT") (:key . ":SLOT-NAMES") (:key . ":ENVIRONMENT")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "MAKE-PACKAGE" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-PACKAGE :FUNCTION (NAME &KEY (USE (QUOTE nil)) NICKNAMES (INTERNAL-SYMBOLS 10) (EXTERNAL-SYMBOLS 10)) 2 0 nil nil (EXTERNAL-SYMBOLS INTERNAL-SYMBOLS NICKNAMES USE) ((:mandatory . "NAME") (:key . ":USE") (:key . ":NICKNAMES") (:key . ":INTERNAL-SYMBOLS") (:key . ":EXTERNAL-SYMBOLS")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "MAKE-PATHNAME" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-PATHNAME :FUNCTION (&KEY HOST (DEVICE nil DEVP) (DIRECTORY nil DIRP) (NAME nil NAMEP) (TYPE nil TYPEP) (VERSION nil VERSIONP) DEFAULTS (CASE LOCAL)) 0 0 nil nil (CASE DEFAULTS VERSION TYPE NAME DIRECTORY DEVICE HOST) ((:key . ":HOST") (:key . ":DEVICE") (:key . ":DIRECTORY") (:key . ":NAME") (:key . ":TYPE") (:key . ":VERSION") (:key . ":DEFAULTS") (:key . ":CASE")) [lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "MAKE-RANDOM-STATE" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-RANDOM-STATE :FUNCTION (&OPTIONAL STATE) 0 1 nil nil nil ((:optional . "[STATE]")) [lambda-list-accept lambda-list-close]]))

(setf (gethash "MAKE-SEQUENCE" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-SEQUENCE :FUNCTION (TYPE LENGTH &KEY (INITIAL-ELEMENT nil IEP)) 3 0 nil nil (INITIAL-ELEMENT) ((:mandatory . "TYPE") (:mandatory . "LENGTH") (:key . ":INITIAL-ELEMENT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "MAKE-STRING" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-STRING :FUNCTION (COUNT &KEY (ELEMENT-TYPE (QUOTE CHARACTER)) ((INITIAL-ELEMENT FILL-CHAR))) 2 0 nil nil (INITIAL-ELEMENT ELEMENT-TYPE) ((:mandatory . "COUNT") (:key . ":ELEMENT-TYPE") (:key . "INITIAL-ELEMENT")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "MAKE-STRING-INPUT-STREAM" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-STRING-INPUT-STREAM :FUNCTION (STRING &OPTIONAL (START 0) END) 2 2 nil nil nil ((:mandatory . "STRING") (:optional . "[START]") (:optional . "[END]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "MAKE-STRING-OUTPUT-STREAM" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-STRING-OUTPUT-STREAM :FUNCTION (&KEY (ELEMENT-TYPE (QUOTE CHARACTER)) &AUX (STRING (MAKE-STRING 40))) 0 0 nil nil (ELEMENT-TYPE) ((:key . ":ELEMENT-TYPE")) [lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "MAKE-SYMBOL" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-SYMBOL :FUNCTION (STRING) 2 0 nil nil nil ((:mandatory . "STRING")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "MAKE-SYNONYM-STREAM" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-SYNONYM-STREAM :FUNCTION (SYMBOL) 2 0 nil nil nil ((:mandatory . "SYMBOL")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "MAKE-TWO-WAY-STREAM" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKE-TWO-WAY-STREAM :FUNCTION (INPUT-STREAM OUTPUT-STREAM) 3 0 nil nil nil ((:mandatory . "INPUT-STREAM") (:mandatory . "OUTPUT-STREAM")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "MAKUNBOUND" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAKUNBOUND :FUNCTION (SYMBOL) 2 0 nil nil nil ((:mandatory . "SYMBOL")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "MAP" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAP :FUNCTION (RESULT-TYPE FUNCTION FIRST-SEQUENCE &REST MORE-SEQUENCES) 4 0 ((&REST MORE-SEQUENCES)) nil nil ((:mandatory . "RESULT-TYPE") (:mandatory . "FUNCTION") (:mandatory . "FIRST-SEQUENCE") (:rest . "MORE-SEQUENCES...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "MAP-INTO" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAP-INTO :FUNCTION (RESULT-SEQUENCE FUNCTION &REST SEQUENCES) 3 0 ((&REST SEQUENCES)) nil nil ((:mandatory . "RESULT-SEQUENCE") (:mandatory . "FUNCTION") (:rest . "SEQUENCES...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "MAPC" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAPC :FUNCTION (FUNCTION LIST &REST MORE-LISTS) 3 0 ((&REST MORE-LISTS)) nil nil ((:mandatory . "FUNCTION") (:mandatory . "LIST") (:rest . "MORE-LISTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "MAPCAN" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAPCAN :FUNCTION (FUNCTION LIST &REST MORE-LISTS) 3 0 ((&REST MORE-LISTS)) nil nil ((:mandatory . "FUNCTION") (:mandatory . "LIST") (:rest . "MORE-LISTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "MAPCAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAPCAR :FUNCTION (FUNCTION LIST &REST MORE-LISTS) 3 0 ((&REST MORE-LISTS)) nil nil ((:mandatory . "FUNCTION") (:mandatory . "LIST") (:rest . "MORE-LISTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "MAPCON" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAPCON :FUNCTION (FUNCTION LIST &REST MORE-LISTS) 3 0 ((&REST MORE-LISTS)) nil nil ((:mandatory . "FUNCTION") (:mandatory . "LIST") (:rest . "MORE-LISTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "MAPHASH" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAPHASH :FUNCTION (FUNCTION-DESIGNATOR HASH-TABLE) 3 0 nil nil nil ((:mandatory . "FUNCTION-DESIGNATOR") (:mandatory . "HASH-TABLE")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "MAPL" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAPL :FUNCTION (FUNCTION LIST &REST MORE-LISTS) 3 0 ((&REST MORE-LISTS)) nil nil ((:mandatory . "FUNCTION") (:mandatory . "LIST") (:rest . "MORE-LISTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "MAPLIST" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAPLIST :FUNCTION (FUNCTION LIST &REST MORE-LISTS) 3 0 ((&REST MORE-LISTS)) nil nil ((:mandatory . "FUNCTION") (:mandatory . "LIST") (:rest . "MORE-LISTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "MASK-FIELD" *cl-lambda-lists*) (quote [cl-struct-lambda-list MASK-FIELD :FUNCTION (BYTESPEC INTEGER) 3 0 nil nil nil ((:mandatory . "BYTESPEC") (:mandatory . "INTEGER")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "MAX" *cl-lambda-lists*) (quote [cl-struct-lambda-list MAX :FUNCTION (NUMBER &REST MORE-NUMBERS) 2 0 ((&REST MORE-NUMBERS)) nil nil ((:mandatory . "NUMBER") (:rest . "MORE-NUMBERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "MEMBER" *cl-lambda-lists*) (quote [cl-struct-lambda-list MEMBER :FUNCTION (ITEM LIST &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT (FUNCTION EQL) NOTP)) 3 0 nil nil (TEST-NOT TEST KEY) ((:mandatory . "ITEM") (:mandatory . "LIST") (:key . ":KEY") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "MEMBER-IF" *cl-lambda-lists*) (quote [cl-struct-lambda-list MEMBER-IF :FUNCTION (TEST LIST &KEY KEY) 3 0 nil nil (KEY) ((:mandatory . "TEST") (:mandatory . "LIST") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "MEMBER-IF-NOT" *cl-lambda-lists*) (quote [cl-struct-lambda-list MEMBER-IF-NOT :FUNCTION (TEST LIST &KEY KEY) 3 0 nil nil (KEY) ((:mandatory . "TEST") (:mandatory . "LIST") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "MERGE" *cl-lambda-lists*) (quote [cl-struct-lambda-list MERGE :FUNCTION (RESULT-TYPE SEQUENCE1 SEQUENCE2 PREDICATE &KEY KEY) 5 0 nil nil (KEY) ((:mandatory . "RESULT-TYPE") (:mandatory . "SEQUENCE1") (:mandatory . "SEQUENCE2") (:mandatory . "PREDICATE") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "MERGE-PATHNAMES" *cl-lambda-lists*) (quote [cl-struct-lambda-list MERGE-PATHNAMES :FUNCTION (PATHNAME &OPTIONAL (DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*) (DEFAULT-VERSION NEWEST)) 2 2 nil nil nil ((:mandatory . "PATHNAME") (:optional . "[DEFAULTS]") (:optional . "[DEFAULT-VERSION]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "METHOD-COMBINATION-ERROR" *cl-lambda-lists*) (quote [cl-struct-lambda-list METHOD-COMBINATION-ERROR :FUNCTION (FORMAT-CONTROL &REST FORMAT-ARGUMENTS) 2 0 ((&REST FORMAT-ARGUMENTS)) nil nil ((:mandatory . "FORMAT-CONTROL") (:rest . "FORMAT-ARGUMENTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "METHOD-QUALIFIERS" *cl-lambda-lists*) (quote [cl-struct-lambda-list METHOD-QUALIFIERS :GENERIC (METHOD) 2 0 nil nil nil ((:mandatory . "METHOD")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "MIN" *cl-lambda-lists*) (quote [cl-struct-lambda-list MIN :FUNCTION (NUMBER &REST MORE-NUMBERS) 2 0 ((&REST MORE-NUMBERS)) nil nil ((:mandatory . "NUMBER") (:rest . "MORE-NUMBERS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "MINUSP" *cl-lambda-lists*) (quote [cl-struct-lambda-list MINUSP :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "MISMATCH" *cl-lambda-lists*) (quote [cl-struct-lambda-list MISMATCH :FUNCTION (SEQUENCE1 SEQUENCE2 &KEY FROM-END (TEST (FUNCTION EQL)) (TEST-NOT nil) (START1 0) (END1 nil) (START2 0) (END2 nil) (KEY nil)) 3 0 nil nil (KEY END2 START2 END1 START1 TEST-NOT TEST FROM-END) ((:mandatory . "SEQUENCE1") (:mandatory . "SEQUENCE2") (:key . ":FROM-END") (:key . ":TEST") (:key . ":TEST-NOT") (:key . ":START1") (:key . ":END1") (:key . ":START2") (:key . ":END2") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "MOD" *cl-lambda-lists*) (quote [cl-struct-lambda-list MOD :FUNCTION (NUMBER DIVISOR) 3 0 nil nil nil ((:mandatory . "NUMBER") (:mandatory . "DIVISOR")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "MUFFLE-WARNING" *cl-lambda-lists*) (quote [cl-struct-lambda-list MUFFLE-WARNING :FUNCTION (&OPTIONAL CONDITION) 0 1 nil nil nil ((:optional . "[CONDITION]")) [lambda-list-accept lambda-list-close]]))

(setf (gethash "MULTIPLE-VALUE-BIND" *cl-lambda-lists*) (quote [cl-struct-lambda-list MULTIPLE-VALUE-BIND :MACRO (VARS VALUE-FORM &BODY BODY) 3 0 ((&BODY BODY)) nil nil ((:mandatory . "VARS") (:mandatory . "VALUE-FORM") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "MULTIPLE-VALUE-CALL" *cl-lambda-lists*) (quote [cl-struct-lambda-list MULTIPLE-VALUE-CALL :SPECIAL-OPERATOR (FUN &REST ARGS) 2 0 ((&REST ARGS)) nil nil ((:mandatory . "FUN") (:rest . "ARGS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "MULTIPLE-VALUE-LIST" *cl-lambda-lists*) (quote [cl-struct-lambda-list MULTIPLE-VALUE-LIST :MACRO (VALUE-FORM) 2 0 nil nil nil ((:mandatory . "VALUE-FORM")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "MULTIPLE-VALUE-PROG1" *cl-lambda-lists*) (quote [cl-struct-lambda-list MULTIPLE-VALUE-PROG1 :SPECIAL-OPERATOR (VALUES-FORM &REST FORMS) 2 0 ((&REST FORMS)) nil nil ((:mandatory . "VALUES-FORM") (:rest . "FORMS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "MULTIPLE-VALUE-SETQ" *cl-lambda-lists*) (quote [cl-struct-lambda-list MULTIPLE-VALUE-SETQ :MACRO (VARS VALUE-FORM) 3 0 nil nil nil ((:mandatory . "VARS") (:mandatory . "VALUE-FORM")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "NAME-CHAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list NAME-CHAR :FUNCTION (NAME) 2 0 nil nil nil ((:mandatory . "NAME")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "NAMESTRING" *cl-lambda-lists*) (quote [cl-struct-lambda-list NAMESTRING :FUNCTION (PATHNAME) 2 0 nil nil nil ((:mandatory . "PATHNAME")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "NBUTLAST" *cl-lambda-lists*) (quote [cl-struct-lambda-list NBUTLAST :FUNCTION (LIST &OPTIONAL (N 1)) 2 1 nil nil nil ((:mandatory . "LIST") (:optional . "[N]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "NCONC" *cl-lambda-lists*) (quote [cl-struct-lambda-list NCONC :FUNCTION (&REST LISTS) 0 0 ((&REST LISTS)) nil nil ((:rest . "LISTS...")) [lambda-list-accept]]))

(setf (gethash "NINTERSECTION" *cl-lambda-lists*) (quote [cl-struct-lambda-list NINTERSECTION :FUNCTION (LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT nil NOTP)) 3 0 nil nil (TEST-NOT TEST KEY) ((:mandatory . "LIST1") (:mandatory . "LIST2") (:key . ":KEY") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "NINTH" *cl-lambda-lists*) (quote [cl-struct-lambda-list NINTH :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "NO-APPLICABLE-METHOD" *cl-lambda-lists*) (quote [cl-struct-lambda-list NO-APPLICABLE-METHOD :GENERIC (GENERIC-FUNCTION &REST ARGS) 2 0 ((&REST ARGS)) nil nil ((:mandatory . "GENERIC-FUNCTION") (:rest . "ARGS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "NO-NEXT-METHOD" *cl-lambda-lists*) (quote [cl-struct-lambda-list NO-NEXT-METHOD :GENERIC (GENERIC-FUNCTION METHOD &REST ARGS) 3 0 ((&REST ARGS)) nil nil ((:mandatory . "GENERIC-FUNCTION") (:mandatory . "METHOD") (:rest . "ARGS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "NOT" *cl-lambda-lists*) (quote [cl-struct-lambda-list NOT :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "NOTANY" *cl-lambda-lists*) (quote [cl-struct-lambda-list NOTANY :FUNCTION (PRED FIRST-SEQ &REST MORE-SEQS) 3 0 ((&REST MORE-SEQS)) nil nil ((:mandatory . "PRED") (:mandatory . "FIRST-SEQ") (:rest . "MORE-SEQS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "NOTEVERY" *cl-lambda-lists*) (quote [cl-struct-lambda-list NOTEVERY :FUNCTION (PRED FIRST-SEQ &REST MORE-SEQS) 3 0 ((&REST MORE-SEQS)) nil nil ((:mandatory . "PRED") (:mandatory . "FIRST-SEQ") (:rest . "MORE-SEQS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "NRECONC" *cl-lambda-lists*) (quote [cl-struct-lambda-list NRECONC :FUNCTION (X Y) 3 0 nil nil nil ((:mandatory . "X") (:mandatory . "Y")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "NREVERSE" *cl-lambda-lists*) (quote [cl-struct-lambda-list NREVERSE :FUNCTION (SEQUENCE) 2 0 nil nil nil ((:mandatory . "SEQUENCE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "NSET-DIFFERENCE" *cl-lambda-lists*) (quote [cl-struct-lambda-list NSET-DIFFERENCE :FUNCTION (LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT nil NOTP)) 3 0 nil nil (TEST-NOT TEST KEY) ((:mandatory . "LIST1") (:mandatory . "LIST2") (:key . ":KEY") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "NSET-EXCLUSIVE-OR" *cl-lambda-lists*) (quote [cl-struct-lambda-list NSET-EXCLUSIVE-OR :FUNCTION (LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT (FUNCTION EQL) NOTP)) 3 0 nil nil (TEST-NOT TEST KEY) ((:mandatory . "LIST1") (:mandatory . "LIST2") (:key . ":KEY") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "NSTRING-CAPITALIZE" *cl-lambda-lists*) (quote [cl-struct-lambda-list NSTRING-CAPITALIZE :FUNCTION (STRING &KEY (START 0) END) 2 0 nil nil (END START) ((:mandatory . "STRING") (:key . ":START") (:key . ":END")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "NSTRING-DOWNCASE" *cl-lambda-lists*) (quote [cl-struct-lambda-list NSTRING-DOWNCASE :FUNCTION (STRING &KEY (START 0) END) 2 0 nil nil (END START) ((:mandatory . "STRING") (:key . ":START") (:key . ":END")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "NSTRING-UPCASE" *cl-lambda-lists*) (quote [cl-struct-lambda-list NSTRING-UPCASE :FUNCTION (STRING &KEY (START 0) END) 2 0 nil nil (END START) ((:mandatory . "STRING") (:key . ":START") (:key . ":END")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "NSUBLIS" *cl-lambda-lists*) (quote [cl-struct-lambda-list NSUBLIS :FUNCTION (ALIST TREE &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT (FUNCTION EQL) NOTP)) 3 0 nil nil (TEST-NOT TEST KEY) ((:mandatory . "ALIST") (:mandatory . "TREE") (:key . ":KEY") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "NSUBST" *cl-lambda-lists*) (quote [cl-struct-lambda-list NSUBST :FUNCTION (NEW OLD TREE &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT (FUNCTION EQL) NOTP)) 4 0 nil nil (TEST-NOT TEST KEY) ((:mandatory . "NEW") (:mandatory . "OLD") (:mandatory . "TREE") (:key . ":KEY") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "NSUBST-IF" *cl-lambda-lists*) (quote [cl-struct-lambda-list NSUBST-IF :FUNCTION (NEW TEST TREE &KEY KEY) 4 0 nil nil (KEY) ((:mandatory . "NEW") (:mandatory . "TEST") (:mandatory . "TREE") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "NSUBST-IF-NOT" *cl-lambda-lists*) (quote [cl-struct-lambda-list NSUBST-IF-NOT :FUNCTION (NEW TEST TREE &KEY KEY) 4 0 nil nil (KEY) ((:mandatory . "NEW") (:mandatory . "TEST") (:mandatory . "TREE") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "NSUBSTITUTE" *cl-lambda-lists*) (quote [cl-struct-lambda-list NSUBSTITUTE :FUNCTION (NEW OLD SEQUENCE &KEY FROM-END (TEST (FUNCTION EQL)) (TEST-NOT nil) (END nil) (COUNT nil) (KEY nil) (START 0)) 4 0 nil nil (START KEY COUNT END TEST-NOT TEST FROM-END) ((:mandatory . "NEW") (:mandatory . "OLD") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":TEST") (:key . ":TEST-NOT") (:key . ":END") (:key . ":COUNT") (:key . ":KEY") (:key . ":START")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "NSUBSTITUTE-IF" *cl-lambda-lists*) (quote [cl-struct-lambda-list NSUBSTITUTE-IF :FUNCTION (NEW PRED SEQUENCE &KEY FROM-END (START 0) (END nil) (COUNT nil) (KEY nil)) 4 0 nil nil (KEY COUNT END START FROM-END) ((:mandatory . "NEW") (:mandatory . "PRED") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":START") (:key . ":END") (:key . ":COUNT") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "NSUBSTITUTE-IF-NOT" *cl-lambda-lists*) (quote [cl-struct-lambda-list NSUBSTITUTE-IF-NOT :FUNCTION (NEW PRED SEQUENCE &KEY FROM-END (START 0) (END nil) (COUNT nil) (KEY nil)) 4 0 nil nil (KEY COUNT END START FROM-END) ((:mandatory . "NEW") (:mandatory . "PRED") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":START") (:key . ":END") (:key . ":COUNT") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "NTH" *cl-lambda-lists*) (quote [cl-struct-lambda-list NTH :FUNCTION (N LIST) 3 0 nil nil nil ((:mandatory . "N") (:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "NTH-VALUE" *cl-lambda-lists*) (quote [cl-struct-lambda-list NTH-VALUE :MACRO (N FORM) 3 0 nil nil nil ((:mandatory . "N") (:mandatory . "FORM")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "NTHCDR" *cl-lambda-lists*) (quote [cl-struct-lambda-list NTHCDR :FUNCTION (N LIST) 3 0 nil nil nil ((:mandatory . "N") (:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "NULL" *cl-lambda-lists*) (quote [cl-struct-lambda-list NULL :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "NUMBERP" *cl-lambda-lists*) (quote [cl-struct-lambda-list NUMBERP :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "NUMERATOR" *cl-lambda-lists*) (quote [cl-struct-lambda-list NUMERATOR :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "NUNION" *cl-lambda-lists*) (quote [cl-struct-lambda-list NUNION :FUNCTION (LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT nil NOTP)) 3 0 nil nil (TEST-NOT TEST KEY) ((:mandatory . "LIST1") (:mandatory . "LIST2") (:key . ":KEY") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "ODDP" *cl-lambda-lists*) (quote [cl-struct-lambda-list ODDP :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "OPEN" *cl-lambda-lists*) (quote [cl-struct-lambda-list OPEN :FUNCTION (FILENAME &KEY (DIRECTION INPUT) (ELEMENT-TYPE (QUOTE BASE-CHAR)) (IF-EXISTS nil IF-EXISTS-GIVEN) (IF-DOES-NOT-EXIST nil IF-DOES-NOT-EXIST-GIVEN) (EXTERNAL-FORMAT DEFAULT) &AUX (DIRECTION DIRECTION) (IF-DOES-NOT-EXIST IF-DOES-NOT-EXIST) (IF-EXISTS IF-EXISTS)) 2 0 nil nil (EXTERNAL-FORMAT IF-DOES-NOT-EXIST IF-EXISTS ELEMENT-TYPE DIRECTION) ((:mandatory . "FILENAME") (:key . ":DIRECTION") (:key . ":ELEMENT-TYPE") (:key . ":IF-EXISTS") (:key . ":IF-DOES-NOT-EXIST") (:key . ":EXTERNAL-FORMAT")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "OPEN-STREAM-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list OPEN-STREAM-P :GENERIC (NON-STREAM) 2 0 nil nil nil ((:mandatory . "NON-STREAM")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "OR" *cl-lambda-lists*) (quote [cl-struct-lambda-list OR :MACRO (&REST FORMS) 0 0 ((&REST FORMS)) nil nil ((:rest . "FORMS...")) [lambda-list-accept]]))

(setf (gethash "OUTPUT-STREAM-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list OUTPUT-STREAM-P :GENERIC (NON-STREAM) 2 0 nil nil nil ((:mandatory . "NON-STREAM")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PACKAGE-ERROR-PACKAGE" *cl-lambda-lists*) (quote [cl-struct-lambda-list PACKAGE-ERROR-PACKAGE :FUNCTION (CONDITION) 2 0 nil nil nil ((:mandatory . "CONDITION")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PACKAGE-NAME" *cl-lambda-lists*) (quote [cl-struct-lambda-list PACKAGE-NAME :FUNCTION (PACKAGE-DESIGNATOR) 2 0 nil nil nil ((:mandatory . "PACKAGE-DESIGNATOR")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PACKAGE-NICKNAMES" *cl-lambda-lists*) (quote [cl-struct-lambda-list PACKAGE-NICKNAMES :FUNCTION (X) 2 0 nil nil nil ((:mandatory . "X")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PACKAGE-SHADOWING-SYMBOLS" *cl-lambda-lists*) (quote [cl-struct-lambda-list PACKAGE-SHADOWING-SYMBOLS :FUNCTION (X) 2 0 nil nil nil ((:mandatory . "X")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PACKAGE-USE-LIST" *cl-lambda-lists*) (quote [cl-struct-lambda-list PACKAGE-USE-LIST :FUNCTION (X) 2 0 nil nil nil ((:mandatory . "X")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PACKAGE-USED-BY-LIST" *cl-lambda-lists*) (quote [cl-struct-lambda-list PACKAGE-USED-BY-LIST :FUNCTION (X) 2 0 nil nil nil ((:mandatory . "X")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PACKAGEP" *cl-lambda-lists*) (quote [cl-struct-lambda-list PACKAGEP :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PAIRLIS" *cl-lambda-lists*) (quote [cl-struct-lambda-list PAIRLIS :FUNCTION (KEYS DATA &OPTIONAL (ALIST (QUOTE nil))) 3 1 nil nil nil ((:mandatory . "KEYS") (:mandatory . "DATA") (:optional . "[ALIST]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PARSE-INTEGER" *cl-lambda-lists*) (quote [cl-struct-lambda-list PARSE-INTEGER :FUNCTION (STRING &KEY (START 0) END (RADIX 10) JUNK-ALLOWED) 2 0 nil nil (JUNK-ALLOWED RADIX END START) ((:mandatory . "STRING") (:key . ":START") (:key . ":END") (:key . ":RADIX") (:key . ":JUNK-ALLOWED")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "PARSE-NAMESTRING" *cl-lambda-lists*) (quote [cl-struct-lambda-list PARSE-NAMESTRING :FUNCTION (THING &OPTIONAL HOST (DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*) &KEY (START 0) END JUNK-ALLOWED) 2 2 nil nil (JUNK-ALLOWED END START) ((:mandatory . "THING") (:optional . "[HOST]") (:optional . "[DEFAULTS]") (:key . ":START") (:key . ":END") (:key . ":JUNK-ALLOWED")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "PATHNAME" *cl-lambda-lists*) (quote [cl-struct-lambda-list PATHNAME :FUNCTION (THING) 2 0 nil nil nil ((:mandatory . "THING")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PATHNAME-DEVICE" *cl-lambda-lists*) (quote [cl-struct-lambda-list PATHNAME-DEVICE :FUNCTION (PATHNAME &KEY (CASE LOCAL)) 2 0 nil nil (CASE) ((:mandatory . "PATHNAME") (:key . ":CASE")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "PATHNAME-DIRECTORY" *cl-lambda-lists*) (quote [cl-struct-lambda-list PATHNAME-DIRECTORY :FUNCTION (PATHNAME &KEY (CASE LOCAL)) 2 0 nil nil (CASE) ((:mandatory . "PATHNAME") (:key . ":CASE")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "PATHNAME-HOST" *cl-lambda-lists*) (quote [cl-struct-lambda-list PATHNAME-HOST :FUNCTION (PATHNAME &KEY (CASE LOCAL)) 2 0 nil nil (CASE) ((:mandatory . "PATHNAME") (:key . ":CASE")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "PATHNAME-MATCH-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list PATHNAME-MATCH-P :FUNCTION (IN-PATHNAME IN-WILDNAME) 3 0 nil nil nil ((:mandatory . "IN-PATHNAME") (:mandatory . "IN-WILDNAME")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PATHNAME-NAME" *cl-lambda-lists*) (quote [cl-struct-lambda-list PATHNAME-NAME :FUNCTION (PATHNAME &KEY (CASE LOCAL)) 2 0 nil nil (CASE) ((:mandatory . "PATHNAME") (:key . ":CASE")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "PATHNAME-TYPE" *cl-lambda-lists*) (quote [cl-struct-lambda-list PATHNAME-TYPE :FUNCTION (PATHNAME &KEY (CASE LOCAL)) 2 0 nil nil (CASE) ((:mandatory . "PATHNAME") (:key . ":CASE")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "PATHNAME-VERSION" *cl-lambda-lists*) (quote [cl-struct-lambda-list PATHNAME-VERSION :FUNCTION (PATHNAME) 2 0 nil nil nil ((:mandatory . "PATHNAME")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PATHNAMEP" *cl-lambda-lists*) (quote [cl-struct-lambda-list PATHNAMEP :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PEEK-CHAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list PEEK-CHAR :FUNCTION (&OPTIONAL (PEEK-TYPE nil) (STREAM *STANDARD-INPUT*) (EOF-ERROR-P T) EOF-VALUE RECURSIVE-P) 0 5 nil nil nil ((:optional . "[PEEK-TYPE]") (:optional . "[STREAM]") (:optional . "[EOF-ERROR-P]") (:optional . "[EOF-VALUE]") (:optional . "[RECURSIVE-P]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PHASE" *cl-lambda-lists*) (quote [cl-struct-lambda-list PHASE :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PLUSP" *cl-lambda-lists*) (quote [cl-struct-lambda-list PLUSP :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "POP" *cl-lambda-lists*) (quote [cl-struct-lambda-list POP :MACRO (PLACE &ENVIRONMENT ENV) 2 0 nil nil nil ((:mandatory . "PLACE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "POSITION" *cl-lambda-lists*) (quote [cl-struct-lambda-list POSITION :FUNCTION (ITEM SEQUENCE &KEY FROM-END (START 0) END KEY TEST TEST-NOT) 3 0 nil nil (TEST-NOT TEST KEY END START FROM-END) ((:mandatory . "ITEM") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":START") (:key . ":END") (:key . ":KEY") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "POSITION-IF" *cl-lambda-lists*) (quote [cl-struct-lambda-list POSITION-IF :FUNCTION (PREDICATE SEQUENCE &KEY FROM-END (START 0) END KEY) 3 0 nil nil (KEY END START FROM-END) ((:mandatory . "PREDICATE") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":START") (:key . ":END") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "POSITION-IF-NOT" *cl-lambda-lists*) (quote [cl-struct-lambda-list POSITION-IF-NOT :FUNCTION (PREDICATE SEQUENCE &KEY FROM-END (START 0) END KEY) 3 0 nil nil (KEY END START FROM-END) ((:mandatory . "PREDICATE") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":START") (:key . ":END") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "PPRINT" *cl-lambda-lists*) (quote [cl-struct-lambda-list PPRINT :FUNCTION (OBJECT &OPTIONAL STREAM) 2 1 nil nil nil ((:mandatory . "OBJECT") (:optional . "[STREAM]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PPRINT-DISPATCH" *cl-lambda-lists*) (quote [cl-struct-lambda-list PPRINT-DISPATCH :FUNCTION (OBJECT &OPTIONAL (TABLE *PRINT-PPRINT-DISPATCH*)) 2 1 nil nil nil ((:mandatory . "OBJECT") (:optional . "[TABLE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PPRINT-EXIT-IF-LIST-EXHAUSTED" *cl-lambda-lists*) (quote [cl-struct-lambda-list PPRINT-EXIT-IF-LIST-EXHAUSTED :MACRO nil 1 0 nil nil nil nil [lambda-list-accept lambda-list-close]]))

(setf (gethash "PPRINT-FILL" *cl-lambda-lists*) (quote [cl-struct-lambda-list PPRINT-FILL :FUNCTION (STREAM LIST &OPTIONAL (COLON-P T) ATSIGN-P) 3 2 nil nil nil ((:mandatory . "STREAM") (:mandatory . "LIST") (:optional . "[COLON-P]") (:optional . "[ATSIGN-P]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PPRINT-INDENT" *cl-lambda-lists*) (quote [cl-struct-lambda-list PPRINT-INDENT :FUNCTION (RELATIVE-TO N &OPTIONAL STREAM) 3 1 nil nil nil ((:mandatory . "RELATIVE-TO") (:mandatory . "N") (:optional . "[STREAM]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PPRINT-LINEAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list PPRINT-LINEAR :FUNCTION (STREAM LIST &OPTIONAL (COLON-P T) ATSIGN-P) 3 2 nil nil nil ((:mandatory . "STREAM") (:mandatory . "LIST") (:optional . "[COLON-P]") (:optional . "[ATSIGN-P]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PPRINT-LOGICAL-BLOCK" *cl-lambda-lists*) (quote [cl-struct-lambda-list PPRINT-LOGICAL-BLOCK :MACRO ((STREAM-SYMBOL OBJECT &KEY (PREFIX nil PREFIXP) (PER-LINE-PREFIX nil PER-LINE-PREFIX-P) (SUFFIX "" SUFFIXP)) &BODY BODY) 2 0 ((&BODY BODY)) nil nil ((:mandatory . "(STREAM-SYMBOL OBJECT &KEY (PREFIX nil PREFIXP) (PER-LINE-PREFIX nil PER-LINE-PREFIX-P) (SUFFIX  SUFFIXP))") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "PPRINT-NEWLINE" *cl-lambda-lists*) (quote [cl-struct-lambda-list PPRINT-NEWLINE :FUNCTION (KIND &OPTIONAL STREAM) 2 1 nil nil nil ((:mandatory . "KIND") (:optional . "[STREAM]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PPRINT-POP" *cl-lambda-lists*) (quote [cl-struct-lambda-list PPRINT-POP :MACRO nil 1 0 nil nil nil nil [lambda-list-accept lambda-list-close]]))

(setf (gethash "PPRINT-TAB" *cl-lambda-lists*) (quote [cl-struct-lambda-list PPRINT-TAB :FUNCTION (KIND COLNUM COLINC &OPTIONAL STREAM) 4 1 nil nil nil ((:mandatory . "KIND") (:mandatory . "COLNUM") (:mandatory . "COLINC") (:optional . "[STREAM]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PPRINT-TABULAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list PPRINT-TABULAR :FUNCTION (STREAM LIST &OPTIONAL (COLON-P T) ATSIGN-P TABSIZE) 3 3 nil nil nil ((:mandatory . "STREAM") (:mandatory . "LIST") (:optional . "[COLON-P]") (:optional . "[ATSIGN-P]") (:optional . "[TABSIZE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PRIN1" *cl-lambda-lists*) (quote [cl-struct-lambda-list PRIN1 :FUNCTION (OBJECT &OPTIONAL STREAM) 2 1 nil nil nil ((:mandatory . "OBJECT") (:optional . "[STREAM]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PRIN1-TO-STRING" *cl-lambda-lists*) (quote [cl-struct-lambda-list PRIN1-TO-STRING :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PRINC" *cl-lambda-lists*) (quote [cl-struct-lambda-list PRINC :FUNCTION (OBJECT &OPTIONAL STREAM) 2 1 nil nil nil ((:mandatory . "OBJECT") (:optional . "[STREAM]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PRINC-TO-STRING" *cl-lambda-lists*) (quote [cl-struct-lambda-list PRINC-TO-STRING :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PRINT" *cl-lambda-lists*) (quote [cl-struct-lambda-list PRINT :FUNCTION (OBJECT &OPTIONAL STREAM) 2 1 nil nil nil ((:mandatory . "OBJECT") (:optional . "[STREAM]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PRINT-NOT-READABLE-OBJECT" *cl-lambda-lists*) (quote [cl-struct-lambda-list PRINT-NOT-READABLE-OBJECT :FUNCTION (CONDITION) 2 0 nil nil nil ((:mandatory . "CONDITION")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PRINT-OBJECT" *cl-lambda-lists*) (quote [cl-struct-lambda-list PRINT-OBJECT :GENERIC (SELF OUT) 3 0 nil nil nil ((:mandatory . "SELF") (:mandatory . "OUT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PRINT-UNREADABLE-OBJECT" *cl-lambda-lists*) (quote [cl-struct-lambda-list PRINT-UNREADABLE-OBJECT :MACRO ((OBJECT STREAM &KEY TYPE IDENTITY) &BODY BODY) 2 0 ((&BODY BODY)) nil nil ((:mandatory . "(OBJECT STREAM &KEY TYPE IDENTITY)") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "PROBE-FILE" *cl-lambda-lists*) (quote [cl-struct-lambda-list PROBE-FILE :FUNCTION (PATHNAME) 2 0 nil nil nil ((:mandatory . "PATHNAME")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PROCLAIM" *cl-lambda-lists*) (quote [cl-struct-lambda-list PROCLAIM :FUNCTION (RAW-FORM) 2 0 nil nil nil ((:mandatory . "RAW-FORM")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PROG" *cl-lambda-lists*) (quote [cl-struct-lambda-list PROG :MACRO (VARLIST &BODY BODY-DECLS) 2 0 ((&BODY BODY-DECLS)) nil nil ((:mandatory . "VARLIST") (:body . "BODY-DECLS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "PROG*" *cl-lambda-lists*) (quote [cl-struct-lambda-list PROG* :MACRO (VARLIST &BODY BODY-DECLS) 2 0 ((&BODY BODY-DECLS)) nil nil ((:mandatory . "VARLIST") (:body . "BODY-DECLS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "PROG1" *cl-lambda-lists*) (quote [cl-struct-lambda-list PROG1 :MACRO (RESULT &BODY BODY) 2 0 ((&BODY BODY)) nil nil ((:mandatory . "RESULT") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "PROG2" *cl-lambda-lists*) (quote [cl-struct-lambda-list PROG2 :MACRO (FORM1 RESULT &BODY BODY) 3 0 ((&BODY BODY)) nil nil ((:mandatory . "FORM1") (:mandatory . "RESULT") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "PROGN" *cl-lambda-lists*) (quote [cl-struct-lambda-list PROGN :SPECIAL-OPERATOR (&REST FORMS) 0 0 ((&REST FORMS)) nil nil ((:rest . "FORMS...")) [lambda-list-accept]]))

(setf (gethash "PROGV" *cl-lambda-lists*) (quote [cl-struct-lambda-list PROGV :SPECIAL-OPERATOR (VARS VALS &BODY BODY) 3 0 ((&BODY BODY)) nil nil ((:mandatory . "VARS") (:mandatory . "VALS") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "PROVIDE" *cl-lambda-lists*) (quote [cl-struct-lambda-list PROVIDE :FUNCTION (MODULE-NAME) 2 0 nil nil nil ((:mandatory . "MODULE-NAME")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PSETF" *cl-lambda-lists*) (quote [cl-struct-lambda-list PSETF :MACRO (&REST ARGS &ENVIRONMENT ENV) 0 0 ((&REST ARGS) (&ENVIRONMENT ENV)) nil nil ((:rest . "ARGS...")) [lambda-list-accept]]))

(setf (gethash "PSETQ" *cl-lambda-lists*) (quote [cl-struct-lambda-list PSETQ :MACRO (&REST PAIRS) 0 0 ((&REST PAIRS)) nil nil ((:rest . "PAIRS...")) [lambda-list-accept]]))

(setf (gethash "PUSH" *cl-lambda-lists*) (quote [cl-struct-lambda-list PUSH :MACRO (OBJ PLACE &ENVIRONMENT ENV) 3 0 nil nil nil ((:mandatory . "OBJ") (:mandatory . "PLACE")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "PUSHNEW" *cl-lambda-lists*) (quote [cl-struct-lambda-list PUSHNEW :MACRO (OBJ PLACE &REST KEYS &ENVIRONMENT ENV) 3 0 ((&REST KEYS) (&ENVIRONMENT ENV)) nil nil ((:mandatory . "OBJ") (:mandatory . "PLACE") (:rest . "KEYS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "QUOTE" *cl-lambda-lists*) (quote [cl-struct-lambda-list QUOTE :SPECIAL-OPERATOR (THING) 2 0 nil nil nil ((:mandatory . "THING")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "RANDOM" *cl-lambda-lists*) (quote [cl-struct-lambda-list RANDOM :FUNCTION (ARG &OPTIONAL (STATE *RANDOM-STATE*)) 2 1 nil nil nil ((:mandatory . "ARG") (:optional . "[STATE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "RANDOM-STATE-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list RANDOM-STATE-P :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "RASSOC" *cl-lambda-lists*) (quote [cl-struct-lambda-list RASSOC :FUNCTION (ITEM ALIST &KEY KEY (TEST nil TESTP) (TEST-NOT nil NOTP)) 3 0 nil nil (TEST-NOT TEST KEY) ((:mandatory . "ITEM") (:mandatory . "ALIST") (:key . ":KEY") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "RASSOC-IF" *cl-lambda-lists*) (quote [cl-struct-lambda-list RASSOC-IF :FUNCTION (PREDICATE ALIST &KEY KEY) 3 0 nil nil (KEY) ((:mandatory . "PREDICATE") (:mandatory . "ALIST") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "RASSOC-IF-NOT" *cl-lambda-lists*) (quote [cl-struct-lambda-list RASSOC-IF-NOT :FUNCTION (PREDICATE ALIST &KEY KEY) 3 0 nil nil (KEY) ((:mandatory . "PREDICATE") (:mandatory . "ALIST") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "RATIONAL" *cl-lambda-lists*) (quote [cl-struct-lambda-list RATIONAL :FUNCTION (X) 2 0 nil nil nil ((:mandatory . "X")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "RATIONALIZE" *cl-lambda-lists*) (quote [cl-struct-lambda-list RATIONALIZE :FUNCTION (X) 2 0 nil nil nil ((:mandatory . "X")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "RATIONALP" *cl-lambda-lists*) (quote [cl-struct-lambda-list RATIONALP :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "READ" *cl-lambda-lists*) (quote [cl-struct-lambda-list READ :FUNCTION (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERROR-P T) (EOF-VALUE nil) (RECURSIVEP nil)) 0 4 nil nil nil ((:optional . "[STREAM]") (:optional . "[EOF-ERROR-P]") (:optional . "[EOF-VALUE]") (:optional . "[RECURSIVEP]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "READ-BYTE" *cl-lambda-lists*) (quote [cl-struct-lambda-list READ-BYTE :FUNCTION (STREAM &OPTIONAL (EOF-ERROR-P T) EOF-VALUE) 2 2 nil nil nil ((:mandatory . "STREAM") (:optional . "[EOF-ERROR-P]") (:optional . "[EOF-VALUE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "READ-CHAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list READ-CHAR :FUNCTION (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERROR-P T) EOF-VALUE RECURSIVE-P) 0 4 nil nil nil ((:optional . "[STREAM]") (:optional . "[EOF-ERROR-P]") (:optional . "[EOF-VALUE]") (:optional . "[RECURSIVE-P]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "READ-CHAR-NO-HANG" *cl-lambda-lists*) (quote [cl-struct-lambda-list READ-CHAR-NO-HANG :FUNCTION (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERROR-P T) EOF-VALUE RECURSIVE-P) 0 4 nil nil nil ((:optional . "[STREAM]") (:optional . "[EOF-ERROR-P]") (:optional . "[EOF-VALUE]") (:optional . "[RECURSIVE-P]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "READ-DELIMITED-LIST" *cl-lambda-lists*) (quote [cl-struct-lambda-list READ-DELIMITED-LIST :FUNCTION (ENDCHAR &OPTIONAL (INPUT-STREAM *STANDARD-INPUT*) RECURSIVE-P) 2 2 nil nil nil ((:mandatory . "ENDCHAR") (:optional . "[INPUT-STREAM]") (:optional . "[RECURSIVE-P]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "READ-FROM-STRING" *cl-lambda-lists*) (quote [cl-struct-lambda-list READ-FROM-STRING :FUNCTION (STRING &OPTIONAL EOF-ERROR-P EOF-VALUE &KEY (START 0) END PRESERVE-WHITESPACE) 2 2 nil nil (PRESERVE-WHITESPACE END START) ((:mandatory . "STRING") (:optional . "[EOF-ERROR-P]") (:optional . "[EOF-VALUE]") (:key . ":START") (:key . ":END") (:key . ":PRESERVE-WHITESPACE")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "READ-LINE" *cl-lambda-lists*) (quote [cl-struct-lambda-list READ-LINE :FUNCTION (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERROR-P T) EOF-VALUE RECURSIVE-P) 0 4 nil nil nil ((:optional . "[STREAM]") (:optional . "[EOF-ERROR-P]") (:optional . "[EOF-VALUE]") (:optional . "[RECURSIVE-P]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "READ-PRESERVING-WHITESPACE" *cl-lambda-lists*) (quote [cl-struct-lambda-list READ-PRESERVING-WHITESPACE :FUNCTION (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERROR-P T) (EOF-VALUE nil) (RECURSIVEP nil)) 0 4 nil nil nil ((:optional . "[STREAM]") (:optional . "[EOF-ERROR-P]") (:optional . "[EOF-VALUE]") (:optional . "[RECURSIVEP]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "READ-SEQUENCE" *cl-lambda-lists*) (quote [cl-struct-lambda-list READ-SEQUENCE :FUNCTION (SEQ STREAM &KEY (START 0) END) 3 0 nil nil (END START) ((:mandatory . "SEQ") (:mandatory . "STREAM") (:key . ":START") (:key . ":END")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "READTABLE-CASE" *cl-lambda-lists*) (quote [cl-struct-lambda-list READTABLE-CASE :FUNCTION (INSTANCE) 2 0 nil nil nil ((:mandatory . "INSTANCE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "READTABLEP" *cl-lambda-lists*) (quote [cl-struct-lambda-list READTABLEP :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "REALP" *cl-lambda-lists*) (quote [cl-struct-lambda-list REALP :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "REALPART" *cl-lambda-lists*) (quote [cl-struct-lambda-list REALPART :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "REDUCE" *cl-lambda-lists*) (quote [cl-struct-lambda-list REDUCE :FUNCTION (FUNCTION SEQUENCE &KEY (KEY nil) FROM-END (START 0) (END nil) (INITIAL-VALUE nil IVP)) 3 0 nil nil (INITIAL-VALUE END START FROM-END KEY) ((:mandatory . "FUNCTION") (:mandatory . "SEQUENCE") (:key . ":KEY") (:key . ":FROM-END") (:key . ":START") (:key . ":END") (:key . ":INITIAL-VALUE")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "REINITIALIZE-INSTANCE" *cl-lambda-lists*) (quote [cl-struct-lambda-list REINITIALIZE-INSTANCE :GENERIC (INSTANCE &REST INITARGS) 2 0 ((&REST INITARGS)) nil nil ((:mandatory . "INSTANCE") (:rest . "INITARGS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "REM" *cl-lambda-lists*) (quote [cl-struct-lambda-list REM :FUNCTION (NUMBER DIVISOR) 3 0 nil nil nil ((:mandatory . "NUMBER") (:mandatory . "DIVISOR")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "REMF" *cl-lambda-lists*) (quote [cl-struct-lambda-list REMF :MACRO (PLACE INDICATOR &ENVIRONMENT ENV) 3 0 nil nil nil ((:mandatory . "PLACE") (:mandatory . "INDICATOR")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "REMHASH" *cl-lambda-lists*) (quote [cl-struct-lambda-list REMHASH :FUNCTION (KEY HASH-TABLE) 3 0 nil nil nil ((:mandatory . "KEY") (:mandatory . "HASH-TABLE")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "REMOVE" *cl-lambda-lists*) (quote [cl-struct-lambda-list REMOVE :FUNCTION (ITEM SEQUENCE &KEY FROM-END (TEST (FUNCTION EQL)) (TEST-NOT nil) (START 0) (END nil) (COUNT nil) (KEY nil)) 3 0 nil nil (KEY COUNT END START TEST-NOT TEST FROM-END) ((:mandatory . "ITEM") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":TEST") (:key . ":TEST-NOT") (:key . ":START") (:key . ":END") (:key . ":COUNT") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "REMOVE-DUPLICATES" *cl-lambda-lists*) (quote [cl-struct-lambda-list REMOVE-DUPLICATES :FUNCTION (SEQUENCE &KEY (TEST (FUNCTION EQL)) (TEST-NOT nil) (START 0) (END nil) FROM-END (KEY nil)) 2 0 nil nil (KEY FROM-END END START TEST-NOT TEST) ((:mandatory . "SEQUENCE") (:key . ":TEST") (:key . ":TEST-NOT") (:key . ":START") (:key . ":END") (:key . ":FROM-END") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "REMOVE-IF" *cl-lambda-lists*) (quote [cl-struct-lambda-list REMOVE-IF :FUNCTION (PREDICATE SEQUENCE &KEY FROM-END (START 0) (END nil) (COUNT nil) (KEY nil)) 3 0 nil nil (KEY COUNT END START FROM-END) ((:mandatory . "PREDICATE") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":START") (:key . ":END") (:key . ":COUNT") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "REMOVE-IF-NOT" *cl-lambda-lists*) (quote [cl-struct-lambda-list REMOVE-IF-NOT :FUNCTION (PREDICATE SEQUENCE &KEY FROM-END (START 0) (END nil) (COUNT nil) (KEY nil)) 3 0 nil nil (KEY COUNT END START FROM-END) ((:mandatory . "PREDICATE") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":START") (:key . ":END") (:key . ":COUNT") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "REMOVE-METHOD" *cl-lambda-lists*) (quote [cl-struct-lambda-list REMOVE-METHOD :GENERIC (GENERIC-FUNCTION METHOD) 3 0 nil nil nil ((:mandatory . "GENERIC-FUNCTION") (:mandatory . "METHOD")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "REMPROP" *cl-lambda-lists*) (quote [cl-struct-lambda-list REMPROP :FUNCTION (SYMBOL INDICATOR) 3 0 nil nil nil ((:mandatory . "SYMBOL") (:mandatory . "INDICATOR")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "RENAME-FILE" *cl-lambda-lists*) (quote [cl-struct-lambda-list RENAME-FILE :FUNCTION (FILE NEW-NAME) 3 0 nil nil nil ((:mandatory . "FILE") (:mandatory . "NEW-NAME")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "RENAME-PACKAGE" *cl-lambda-lists*) (quote [cl-struct-lambda-list RENAME-PACKAGE :FUNCTION (PACKAGE NAME &OPTIONAL (NICKNAMES nil)) 3 1 nil nil nil ((:mandatory . "PACKAGE") (:mandatory . "NAME") (:optional . "[NICKNAMES]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "REPLACE" *cl-lambda-lists*) (quote [cl-struct-lambda-list REPLACE :FUNCTION (SEQUENCE1 SEQUENCE2 &KEY (START1 0) (END1 nil) (START2 0) (END2 nil)) 3 0 nil nil (END2 START2 END1 START1) ((:mandatory . "SEQUENCE1") (:mandatory . "SEQUENCE2") (:key . ":START1") (:key . ":END1") (:key . ":START2") (:key . ":END2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "REQUIRE" *cl-lambda-lists*) (quote [cl-struct-lambda-list REQUIRE :FUNCTION (MODULE-NAME &OPTIONAL PATHNAMES) 2 1 nil nil nil ((:mandatory . "MODULE-NAME") (:optional . "[PATHNAMES]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "REST" *cl-lambda-lists*) (quote [cl-struct-lambda-list REST :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "RESTART-BIND" *cl-lambda-lists*) (quote [cl-struct-lambda-list RESTART-BIND :MACRO (BINDINGS &BODY FORMS) 2 0 ((&BODY FORMS)) nil nil ((:mandatory . "BINDINGS") (:body . "FORMS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "RESTART-CASE" *cl-lambda-lists*) (quote [cl-struct-lambda-list RESTART-CASE :MACRO (EXPRESSION &BODY CLAUSES &ENVIRONMENT ENV) 2 0 ((&BODY CLAUSES) (&ENVIRONMENT ENV)) nil nil ((:mandatory . "EXPRESSION") (:body . "CLAUSES...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "RESTART-NAME" *cl-lambda-lists*) (quote [cl-struct-lambda-list RESTART-NAME :FUNCTION (INSTANCE) 2 0 nil nil nil ((:mandatory . "INSTANCE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "RETURN" *cl-lambda-lists*) (quote [cl-struct-lambda-list RETURN :MACRO (&OPTIONAL (VALUE nil)) 0 1 nil nil nil ((:optional . "[VALUE]")) [lambda-list-accept lambda-list-close]]))

(setf (gethash "RETURN-FROM" *cl-lambda-lists*) (quote [cl-struct-lambda-list RETURN-FROM :SPECIAL-OPERATOR (NAME &OPTIONAL VALUE) 2 1 nil nil nil ((:mandatory . "NAME") (:optional . "[VALUE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "REVAPPEND" *cl-lambda-lists*) (quote [cl-struct-lambda-list REVAPPEND :FUNCTION (X Y) 3 0 nil nil nil ((:mandatory . "X") (:mandatory . "Y")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "REVERSE" *cl-lambda-lists*) (quote [cl-struct-lambda-list REVERSE :FUNCTION (SEQUENCE) 2 0 nil nil nil ((:mandatory . "SEQUENCE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ROOM" *cl-lambda-lists*) (quote [cl-struct-lambda-list ROOM :FUNCTION (&OPTIONAL (VERBOSITY DEFAULT)) 0 1 nil nil nil ((:optional . "[VERBOSITY]")) [lambda-list-accept lambda-list-close]]))

(setf (gethash "ROTATEF" *cl-lambda-lists*) (quote [cl-struct-lambda-list ROTATEF :MACRO (&REST ARGS &ENVIRONMENT ENV) 0 0 ((&REST ARGS) (&ENVIRONMENT ENV)) nil nil ((:rest . "ARGS...")) [lambda-list-accept]]))

(setf (gethash "ROUND" *cl-lambda-lists*) (quote [cl-struct-lambda-list ROUND :FUNCTION (NUMBER &OPTIONAL (DIVISOR 1)) 2 1 nil nil nil ((:mandatory . "NUMBER") (:optional . "[DIVISOR]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "ROW-MAJOR-AREF" *cl-lambda-lists*) (quote [cl-struct-lambda-list ROW-MAJOR-AREF :FUNCTION (ARRAY INDEX) 3 0 nil nil nil ((:mandatory . "ARRAY") (:mandatory . "INDEX")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "RPLACA" *cl-lambda-lists*) (quote [cl-struct-lambda-list RPLACA :FUNCTION (X Y) 3 0 nil nil nil ((:mandatory . "X") (:mandatory . "Y")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "RPLACD" *cl-lambda-lists*) (quote [cl-struct-lambda-list RPLACD :FUNCTION (X Y) 3 0 nil nil nil ((:mandatory . "X") (:mandatory . "Y")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SBIT" *cl-lambda-lists*) (quote [cl-struct-lambda-list SBIT :FUNCTION (SIMPLE-BIT-ARRAY &REST SUBSCRIPTS) 2 0 ((&REST SUBSCRIPTS)) nil nil ((:mandatory . "SIMPLE-BIT-ARRAY") (:rest . "SUBSCRIPTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "SCALE-FLOAT" *cl-lambda-lists*) (quote [cl-struct-lambda-list SCALE-FLOAT :FUNCTION (F EX) 3 0 nil nil nil ((:mandatory . "F") (:mandatory . "EX")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SCHAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list SCHAR :FUNCTION (STRING INDEX) 3 0 nil nil nil ((:mandatory . "STRING") (:mandatory . "INDEX")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SEARCH" *cl-lambda-lists*) (quote [cl-struct-lambda-list SEARCH :FUNCTION (SEQUENCE1 SEQUENCE2 &KEY FROM-END (TEST (FUNCTION EQL)) (TEST-NOT nil) (START1 0) (END1 nil) (START2 0) (END2 nil) (KEY nil)) 3 0 nil nil (KEY END2 START2 END1 START1 TEST-NOT TEST FROM-END) ((:mandatory . "SEQUENCE1") (:mandatory . "SEQUENCE2") (:key . ":FROM-END") (:key . ":TEST") (:key . ":TEST-NOT") (:key . ":START1") (:key . ":END1") (:key . ":START2") (:key . ":END2") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "SECOND" *cl-lambda-lists*) (quote [cl-struct-lambda-list SECOND :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SET" *cl-lambda-lists*) (quote [cl-struct-lambda-list SET :FUNCTION (SYMBOL NEW-VALUE) 3 0 nil nil nil ((:mandatory . "SYMBOL") (:mandatory . "NEW-VALUE")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SET-DIFFERENCE" *cl-lambda-lists*) (quote [cl-struct-lambda-list SET-DIFFERENCE :FUNCTION (LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT nil NOTP)) 3 0 nil nil (TEST-NOT TEST KEY) ((:mandatory . "LIST1") (:mandatory . "LIST2") (:key . ":KEY") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "SET-DISPATCH-MACRO-CHARACTER" *cl-lambda-lists*) (quote [cl-struct-lambda-list SET-DISPATCH-MACRO-CHARACTER :FUNCTION (DISP-CHAR SUB-CHAR FUNCTION &OPTIONAL (RT *READTABLE*)) 4 1 nil nil nil ((:mandatory . "DISP-CHAR") (:mandatory . "SUB-CHAR") (:mandatory . "FUNCTION") (:optional . "[RT]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SET-EXCLUSIVE-OR" *cl-lambda-lists*) (quote [cl-struct-lambda-list SET-EXCLUSIVE-OR :FUNCTION (LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT (FUNCTION EQL) NOTP)) 3 0 nil nil (TEST-NOT TEST KEY) ((:mandatory . "LIST1") (:mandatory . "LIST2") (:key . ":KEY") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "SET-MACRO-CHARACTER" *cl-lambda-lists*) (quote [cl-struct-lambda-list SET-MACRO-CHARACTER :FUNCTION (CHAR FUNCTION &OPTIONAL (NON-TERMINATINGP nil) (READTABLE *READTABLE*)) 3 2 nil nil nil ((:mandatory . "CHAR") (:mandatory . "FUNCTION") (:optional . "[NON-TERMINATINGP]") (:optional . "[READTABLE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SET-PPRINT-DISPATCH" *cl-lambda-lists*) (quote [cl-struct-lambda-list SET-PPRINT-DISPATCH :FUNCTION (TYPE FUNCTION &OPTIONAL (PRIORITY 0) (TABLE *PRINT-PPRINT-DISPATCH*)) 3 2 nil nil nil ((:mandatory . "TYPE") (:mandatory . "FUNCTION") (:optional . "[PRIORITY]") (:optional . "[TABLE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SET-SYNTAX-FROM-CHAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list SET-SYNTAX-FROM-CHAR :FUNCTION (TO-CHAR FROM-CHAR &OPTIONAL (TO-READTABLE *READTABLE*) (FROM-READTABLE nil)) 3 2 nil nil nil ((:mandatory . "TO-CHAR") (:mandatory . "FROM-CHAR") (:optional . "[TO-READTABLE]") (:optional . "[FROM-READTABLE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SETF" *cl-lambda-lists*) (quote [cl-struct-lambda-list SETF :MACRO (&REST ARGS &ENVIRONMENT ENV) 0 0 ((&REST ARGS) (&ENVIRONMENT ENV)) nil nil ((:rest . "ARGS...")) [lambda-list-accept]]))

(setf (gethash "SETQ" *cl-lambda-lists*) (quote [cl-struct-lambda-list SETQ :SPECIAL-OPERATOR (&WHOLE SOURCE &REST THINGS) 0 0 ((&REST THINGS) (&WHOLE SOURCE)) nil nil ((:rest . "THINGS...")) [lambda-list-accept]]))

(setf (gethash "SEVENTH" *cl-lambda-lists*) (quote [cl-struct-lambda-list SEVENTH :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SHADOW" *cl-lambda-lists*) (quote [cl-struct-lambda-list SHADOW :FUNCTION (SYMBOLS &OPTIONAL (PACKAGE (SANE-PACKAGE))) 2 1 nil nil nil ((:mandatory . "SYMBOLS") (:optional . "[PACKAGE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SHADOWING-IMPORT" *cl-lambda-lists*) (quote [cl-struct-lambda-list SHADOWING-IMPORT :FUNCTION (SYMBOLS &OPTIONAL (PACKAGE (SANE-PACKAGE))) 2 1 nil nil nil ((:mandatory . "SYMBOLS") (:optional . "[PACKAGE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SHARED-INITIALIZE" *cl-lambda-lists*) (quote [cl-struct-lambda-list SHARED-INITIALIZE :GENERIC (INSTANCE SLOT-NAMES &REST INITARGS &ALLOW-OTHER-KEYS) 3 0 ((&REST INITARGS) (&ALLOW-OTHER-KEYS)) ((&ALLOW-OTHER-KEYS)) nil ((:mandatory . "INSTANCE") (:mandatory . "SLOT-NAMES") (:rest . "INITARGS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "SHIFTF" *cl-lambda-lists*) (quote [cl-struct-lambda-list SHIFTF :MACRO (&WHOLE FORM &REST ARGS &ENVIRONMENT ENV) 0 0 ((&REST ARGS) (&ENVIRONMENT ENV) (&WHOLE FORM)) nil nil ((:rest . "ARGS...")) [lambda-list-accept]]))

(setf (gethash "SHORT-SITE-NAME" *cl-lambda-lists*) (quote [cl-struct-lambda-list SHORT-SITE-NAME :FUNCTION nil 1 0 nil nil nil nil [lambda-list-accept lambda-list-close]]))

(setf (gethash "SIGNAL" *cl-lambda-lists*) (quote [cl-struct-lambda-list SIGNAL :FUNCTION (DATUM &REST ARGUMENTS) 2 0 ((&REST ARGUMENTS)) nil nil ((:mandatory . "DATUM") (:rest . "ARGUMENTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "SIGNUM" *cl-lambda-lists*) (quote [cl-struct-lambda-list SIGNUM :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SIMPLE-BIT-VECTOR-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list SIMPLE-BIT-VECTOR-P :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SIMPLE-CONDITION-FORMAT-ARGUMENTS" *cl-lambda-lists*) (quote [cl-struct-lambda-list SIMPLE-CONDITION-FORMAT-ARGUMENTS :FUNCTION (CONDITION) 2 0 nil nil nil ((:mandatory . "CONDITION")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SIMPLE-CONDITION-FORMAT-CONTROL" *cl-lambda-lists*) (quote [cl-struct-lambda-list SIMPLE-CONDITION-FORMAT-CONTROL :FUNCTION (CONDITION) 2 0 nil nil nil ((:mandatory . "CONDITION")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SIMPLE-STRING-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list SIMPLE-STRING-P :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SIMPLE-VECTOR-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list SIMPLE-VECTOR-P :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SIN" *cl-lambda-lists*) (quote [cl-struct-lambda-list SIN :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SINH" *cl-lambda-lists*) (quote [cl-struct-lambda-list SINH :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SIXTH" *cl-lambda-lists*) (quote [cl-struct-lambda-list SIXTH :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SLEEP" *cl-lambda-lists*) (quote [cl-struct-lambda-list SLEEP :FUNCTION (N) 2 0 nil nil nil ((:mandatory . "N")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SLOT-BOUNDP" *cl-lambda-lists*) (quote [cl-struct-lambda-list SLOT-BOUNDP :FUNCTION (OBJECT SLOT-NAME) 3 0 nil nil nil ((:mandatory . "OBJECT") (:mandatory . "SLOT-NAME")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SLOT-EXISTS-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list SLOT-EXISTS-P :FUNCTION (OBJECT SLOT-NAME) 3 0 nil nil nil ((:mandatory . "OBJECT") (:mandatory . "SLOT-NAME")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SLOT-MAKUNBOUND" *cl-lambda-lists*) (quote [cl-struct-lambda-list SLOT-MAKUNBOUND :FUNCTION (OBJECT SLOT-NAME) 3 0 nil nil nil ((:mandatory . "OBJECT") (:mandatory . "SLOT-NAME")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SLOT-MISSING" *cl-lambda-lists*) (quote [cl-struct-lambda-list SLOT-MISSING :GENERIC (CLASS INSTANCE SLOT-NAME OPERATION &OPTIONAL NEW-VALUE) 5 1 nil nil nil ((:mandatory . "CLASS") (:mandatory . "INSTANCE") (:mandatory . "SLOT-NAME") (:mandatory . "OPERATION") (:optional . "[NEW-VALUE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SLOT-UNBOUND" *cl-lambda-lists*) (quote [cl-struct-lambda-list SLOT-UNBOUND :GENERIC (CLASS INSTANCE SLOT-NAME) 4 0 nil nil nil ((:mandatory . "CLASS") (:mandatory . "INSTANCE") (:mandatory . "SLOT-NAME")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SLOT-VALUE" *cl-lambda-lists*) (quote [cl-struct-lambda-list SLOT-VALUE :FUNCTION (OBJECT SLOT-NAME) 3 0 nil nil nil ((:mandatory . "OBJECT") (:mandatory . "SLOT-NAME")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SOFTWARE-TYPE" *cl-lambda-lists*) (quote [cl-struct-lambda-list SOFTWARE-TYPE :FUNCTION nil 1 0 nil nil nil nil [lambda-list-accept lambda-list-close]]))

(setf (gethash "SOFTWARE-VERSION" *cl-lambda-lists*) (quote [cl-struct-lambda-list SOFTWARE-VERSION :FUNCTION nil 1 0 nil nil nil nil [lambda-list-accept lambda-list-close]]))

(setf (gethash "SOME" *cl-lambda-lists*) (quote [cl-struct-lambda-list SOME :FUNCTION (PRED FIRST-SEQ &REST MORE-SEQS) 3 0 ((&REST MORE-SEQS)) nil nil ((:mandatory . "PRED") (:mandatory . "FIRST-SEQ") (:rest . "MORE-SEQS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "SORT" *cl-lambda-lists*) (quote [cl-struct-lambda-list SORT :FUNCTION (SEQUENCE PREDICATE &KEY KEY) 3 0 nil nil (KEY) ((:mandatory . "SEQUENCE") (:mandatory . "PREDICATE") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "SPECIAL-OPERATOR-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list SPECIAL-OPERATOR-P :FUNCTION (SYMBOL) 2 0 nil nil nil ((:mandatory . "SYMBOL")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SQRT" *cl-lambda-lists*) (quote [cl-struct-lambda-list SQRT :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "STABLE-SORT" *cl-lambda-lists*) (quote [cl-struct-lambda-list STABLE-SORT :FUNCTION (SEQUENCE PREDICATE &KEY KEY) 3 0 nil nil (KEY) ((:mandatory . "SEQUENCE") (:mandatory . "PREDICATE") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "STANDARD-CHAR-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list STANDARD-CHAR-P :FUNCTION (CHAR) 2 0 nil nil nil ((:mandatory . "CHAR")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "STEP" *cl-lambda-lists*) (quote [cl-struct-lambda-list STEP :MACRO (FORM) 2 0 nil nil nil ((:mandatory . "FORM")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "STORE-VALUE" *cl-lambda-lists*) (quote [cl-struct-lambda-list STORE-VALUE :FUNCTION (VALUE &OPTIONAL CONDITION) 2 1 nil nil nil ((:mandatory . "VALUE") (:optional . "[CONDITION]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "STREAM-ELEMENT-TYPE" *cl-lambda-lists*) (quote [cl-struct-lambda-list STREAM-ELEMENT-TYPE :GENERIC (NON-STREAM) 2 0 nil nil nil ((:mandatory . "NON-STREAM")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "STREAM-ERROR-STREAM" *cl-lambda-lists*) (quote [cl-struct-lambda-list STREAM-ERROR-STREAM :FUNCTION (CONDITION) 2 0 nil nil nil ((:mandatory . "CONDITION")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "STREAM-EXTERNAL-FORMAT" *cl-lambda-lists*) (quote [cl-struct-lambda-list STREAM-EXTERNAL-FORMAT :FUNCTION (STREAM) 2 0 nil nil nil ((:mandatory . "STREAM")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "STREAMP" *cl-lambda-lists*) (quote [cl-struct-lambda-list STREAMP :FUNCTION (STREAM) 2 0 nil nil nil ((:mandatory . "STREAM")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "STRING" *cl-lambda-lists*) (quote [cl-struct-lambda-list STRING :FUNCTION (X) 2 0 nil nil nil ((:mandatory . "X")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "STRING-CAPITALIZE" *cl-lambda-lists*) (quote [cl-struct-lambda-list STRING-CAPITALIZE :FUNCTION (STRING &KEY (START 0) END) 2 0 nil nil (END START) ((:mandatory . "STRING") (:key . ":START") (:key . ":END")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "STRING-DOWNCASE" *cl-lambda-lists*) (quote [cl-struct-lambda-list STRING-DOWNCASE :FUNCTION (STRING &KEY (START 0) END) 2 0 nil nil (END START) ((:mandatory . "STRING") (:key . ":START") (:key . ":END")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "STRING-EQUAL" *cl-lambda-lists*) (quote [cl-struct-lambda-list STRING-EQUAL :FUNCTION (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) 3 0 nil nil (END2 START2 END1 START1) ((:mandatory . "STRING1") (:mandatory . "STRING2") (:key . ":START1") (:key . ":END1") (:key . ":START2") (:key . ":END2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "STRING-GREATERP" *cl-lambda-lists*) (quote [cl-struct-lambda-list STRING-GREATERP :FUNCTION (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) 3 0 nil nil (END2 START2 END1 START1) ((:mandatory . "STRING1") (:mandatory . "STRING2") (:key . ":START1") (:key . ":END1") (:key . ":START2") (:key . ":END2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "STRING-LEFT-TRIM" *cl-lambda-lists*) (quote [cl-struct-lambda-list STRING-LEFT-TRIM :FUNCTION (CHAR-BAG STRING) 3 0 nil nil nil ((:mandatory . "CHAR-BAG") (:mandatory . "STRING")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "STRING-LESSP" *cl-lambda-lists*) (quote [cl-struct-lambda-list STRING-LESSP :FUNCTION (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) 3 0 nil nil (END2 START2 END1 START1) ((:mandatory . "STRING1") (:mandatory . "STRING2") (:key . ":START1") (:key . ":END1") (:key . ":START2") (:key . ":END2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "STRING-NOT-EQUAL" *cl-lambda-lists*) (quote [cl-struct-lambda-list STRING-NOT-EQUAL :FUNCTION (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) 3 0 nil nil (END2 START2 END1 START1) ((:mandatory . "STRING1") (:mandatory . "STRING2") (:key . ":START1") (:key . ":END1") (:key . ":START2") (:key . ":END2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "STRING-NOT-GREATERP" *cl-lambda-lists*) (quote [cl-struct-lambda-list STRING-NOT-GREATERP :FUNCTION (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) 3 0 nil nil (END2 START2 END1 START1) ((:mandatory . "STRING1") (:mandatory . "STRING2") (:key . ":START1") (:key . ":END1") (:key . ":START2") (:key . ":END2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "STRING-NOT-LESSP" *cl-lambda-lists*) (quote [cl-struct-lambda-list STRING-NOT-LESSP :FUNCTION (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) 3 0 nil nil (END2 START2 END1 START1) ((:mandatory . "STRING1") (:mandatory . "STRING2") (:key . ":START1") (:key . ":END1") (:key . ":START2") (:key . ":END2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "STRING-RIGHT-TRIM" *cl-lambda-lists*) (quote [cl-struct-lambda-list STRING-RIGHT-TRIM :FUNCTION (CHAR-BAG STRING) 3 0 nil nil nil ((:mandatory . "CHAR-BAG") (:mandatory . "STRING")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "STRING-TRIM" *cl-lambda-lists*) (quote [cl-struct-lambda-list STRING-TRIM :FUNCTION (CHAR-BAG STRING) 3 0 nil nil nil ((:mandatory . "CHAR-BAG") (:mandatory . "STRING")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "STRING-UPCASE" *cl-lambda-lists*) (quote [cl-struct-lambda-list STRING-UPCASE :FUNCTION (STRING &KEY (START 0) END) 2 0 nil nil (END START) ((:mandatory . "STRING") (:key . ":START") (:key . ":END")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "STRING/=" *cl-lambda-lists*) (quote [cl-struct-lambda-list STRING/= :FUNCTION (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) 3 0 nil nil (END2 START2 END1 START1) ((:mandatory . "STRING1") (:mandatory . "STRING2") (:key . ":START1") (:key . ":END1") (:key . ":START2") (:key . ":END2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "STRING<" *cl-lambda-lists*) (quote [cl-struct-lambda-list STRING< :FUNCTION (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) 3 0 nil nil (END2 START2 END1 START1) ((:mandatory . "STRING1") (:mandatory . "STRING2") (:key . ":START1") (:key . ":END1") (:key . ":START2") (:key . ":END2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "STRING<=" *cl-lambda-lists*) (quote [cl-struct-lambda-list STRING<= :FUNCTION (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) 3 0 nil nil (END2 START2 END1 START1) ((:mandatory . "STRING1") (:mandatory . "STRING2") (:key . ":START1") (:key . ":END1") (:key . ":START2") (:key . ":END2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "STRING=" *cl-lambda-lists*) (quote [cl-struct-lambda-list STRING= :FUNCTION (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) 3 0 nil nil (END2 START2 END1 START1) ((:mandatory . "STRING1") (:mandatory . "STRING2") (:key . ":START1") (:key . ":END1") (:key . ":START2") (:key . ":END2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "STRING>" *cl-lambda-lists*) (quote [cl-struct-lambda-list STRING> :FUNCTION (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) 3 0 nil nil (END2 START2 END1 START1) ((:mandatory . "STRING1") (:mandatory . "STRING2") (:key . ":START1") (:key . ":END1") (:key . ":START2") (:key . ":END2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "STRING>=" *cl-lambda-lists*) (quote [cl-struct-lambda-list STRING>= :FUNCTION (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) 3 0 nil nil (END2 START2 END1 START1) ((:mandatory . "STRING1") (:mandatory . "STRING2") (:key . ":START1") (:key . ":END1") (:key . ":START2") (:key . ":END2")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "STRINGP" *cl-lambda-lists*) (quote [cl-struct-lambda-list STRINGP :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SUBLIS" *cl-lambda-lists*) (quote [cl-struct-lambda-list SUBLIS :FUNCTION (ALIST TREE &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT (FUNCTION EQL) NOTP)) 3 0 nil nil (TEST-NOT TEST KEY) ((:mandatory . "ALIST") (:mandatory . "TREE") (:key . ":KEY") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "SUBSEQ" *cl-lambda-lists*) (quote [cl-struct-lambda-list SUBSEQ :FUNCTION (SEQUENCE START &OPTIONAL END) 3 1 nil nil nil ((:mandatory . "SEQUENCE") (:mandatory . "START") (:optional . "[END]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SUBSETP" *cl-lambda-lists*) (quote [cl-struct-lambda-list SUBSETP :FUNCTION (LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT nil NOTP)) 3 0 nil nil (TEST-NOT TEST KEY) ((:mandatory . "LIST1") (:mandatory . "LIST2") (:key . ":KEY") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "SUBST" *cl-lambda-lists*) (quote [cl-struct-lambda-list SUBST :FUNCTION (NEW OLD TREE &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT (FUNCTION EQL) NOTP)) 4 0 nil nil (TEST-NOT TEST KEY) ((:mandatory . "NEW") (:mandatory . "OLD") (:mandatory . "TREE") (:key . ":KEY") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "SUBST-IF" *cl-lambda-lists*) (quote [cl-struct-lambda-list SUBST-IF :FUNCTION (NEW TEST TREE &KEY KEY) 4 0 nil nil (KEY) ((:mandatory . "NEW") (:mandatory . "TEST") (:mandatory . "TREE") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "SUBST-IF-NOT" *cl-lambda-lists*) (quote [cl-struct-lambda-list SUBST-IF-NOT :FUNCTION (NEW TEST TREE &KEY KEY) 4 0 nil nil (KEY) ((:mandatory . "NEW") (:mandatory . "TEST") (:mandatory . "TREE") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "SUBSTITUTE" *cl-lambda-lists*) (quote [cl-struct-lambda-list SUBSTITUTE :FUNCTION (NEW OLD SEQUENCE &KEY FROM-END (TEST (FUNCTION EQL)) (TEST-NOT nil) (START 0) (COUNT nil) (END nil) (KEY nil)) 4 0 nil nil (KEY END COUNT START TEST-NOT TEST FROM-END) ((:mandatory . "NEW") (:mandatory . "OLD") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":TEST") (:key . ":TEST-NOT") (:key . ":START") (:key . ":COUNT") (:key . ":END") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "SUBSTITUTE-IF" *cl-lambda-lists*) (quote [cl-struct-lambda-list SUBSTITUTE-IF :FUNCTION (NEW PRED SEQUENCE &KEY FROM-END (START 0) (END nil) (COUNT nil) (KEY nil)) 4 0 nil nil (KEY COUNT END START FROM-END) ((:mandatory . "NEW") (:mandatory . "PRED") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":START") (:key . ":END") (:key . ":COUNT") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "SUBSTITUTE-IF-NOT" *cl-lambda-lists*) (quote [cl-struct-lambda-list SUBSTITUTE-IF-NOT :FUNCTION (NEW PRED SEQUENCE &KEY FROM-END (START 0) (END nil) (COUNT nil) (KEY nil)) 4 0 nil nil (KEY COUNT END START FROM-END) ((:mandatory . "NEW") (:mandatory . "PRED") (:mandatory . "SEQUENCE") (:key . ":FROM-END") (:key . ":START") (:key . ":END") (:key . ":COUNT") (:key . ":KEY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "SUBTYPEP" *cl-lambda-lists*) (quote [cl-struct-lambda-list SUBTYPEP :FUNCTION (TYPE1 TYPE2 &OPTIONAL ENVIRONMENT) 3 1 nil nil nil ((:mandatory . "TYPE1") (:mandatory . "TYPE2") (:optional . "[ENVIRONMENT]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SVREF" *cl-lambda-lists*) (quote [cl-struct-lambda-list SVREF :FUNCTION (SIMPLE-VECTOR INDEX) 3 0 nil nil nil ((:mandatory . "SIMPLE-VECTOR") (:mandatory . "INDEX")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SXHASH" *cl-lambda-lists*) (quote [cl-struct-lambda-list SXHASH :FUNCTION (X) 2 0 nil nil nil ((:mandatory . "X")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SYMBOL-FUNCTION" *cl-lambda-lists*) (quote [cl-struct-lambda-list SYMBOL-FUNCTION :FUNCTION (SYMBOL) 2 0 nil nil nil ((:mandatory . "SYMBOL")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SYMBOL-MACROLET" *cl-lambda-lists*) (quote [cl-struct-lambda-list SYMBOL-MACROLET :SPECIAL-OPERATOR (MACROBINDINGS &BODY BODY) 2 0 ((&BODY BODY)) nil nil ((:mandatory . "MACROBINDINGS") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "SYMBOL-NAME" *cl-lambda-lists*) (quote [cl-struct-lambda-list SYMBOL-NAME :FUNCTION (SYMBOL) 2 0 nil nil nil ((:mandatory . "SYMBOL")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SYMBOL-PACKAGE" *cl-lambda-lists*) (quote [cl-struct-lambda-list SYMBOL-PACKAGE :FUNCTION (SYMBOL) 2 0 nil nil nil ((:mandatory . "SYMBOL")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SYMBOL-PLIST" *cl-lambda-lists*) (quote [cl-struct-lambda-list SYMBOL-PLIST :FUNCTION (SYMBOL) 2 0 nil nil nil ((:mandatory . "SYMBOL")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SYMBOL-VALUE" *cl-lambda-lists*) (quote [cl-struct-lambda-list SYMBOL-VALUE :FUNCTION (SYMBOL) 2 0 nil nil nil ((:mandatory . "SYMBOL")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SYMBOLP" *cl-lambda-lists*) (quote [cl-struct-lambda-list SYMBOLP :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "SYNONYM-STREAM-SYMBOL" *cl-lambda-lists*) (quote [cl-struct-lambda-list SYNONYM-STREAM-SYMBOL :FUNCTION (INSTANCE) 2 0 nil nil nil ((:mandatory . "INSTANCE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "TAGBODY" *cl-lambda-lists*) (quote [cl-struct-lambda-list TAGBODY :SPECIAL-OPERATOR (&REST STATEMENTS) 0 0 ((&REST STATEMENTS)) nil nil ((:rest . "STATEMENTS...")) [lambda-list-accept]]))

(setf (gethash "TAILP" *cl-lambda-lists*) (quote [cl-struct-lambda-list TAILP :FUNCTION (OBJECT LIST) 3 0 nil nil nil ((:mandatory . "OBJECT") (:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "TAN" *cl-lambda-lists*) (quote [cl-struct-lambda-list TAN :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "TANH" *cl-lambda-lists*) (quote [cl-struct-lambda-list TANH :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "TENTH" *cl-lambda-lists*) (quote [cl-struct-lambda-list TENTH :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "TERPRI" *cl-lambda-lists*) (quote [cl-struct-lambda-list TERPRI :FUNCTION (&OPTIONAL (STREAM *STANDARD-OUTPUT*)) 0 1 nil nil nil ((:optional . "[STREAM]")) [lambda-list-accept lambda-list-close]]))

(setf (gethash "THE" *cl-lambda-lists*) (quote [cl-struct-lambda-list THE :SPECIAL-OPERATOR (TYPE VALUE) 3 0 nil nil nil ((:mandatory . "TYPE") (:mandatory . "VALUE")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "THIRD" *cl-lambda-lists*) (quote [cl-struct-lambda-list THIRD :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "THROW" *cl-lambda-lists*) (quote [cl-struct-lambda-list THROW :SPECIAL-OPERATOR (TAG RESULT) 3 0 nil nil nil ((:mandatory . "TAG") (:mandatory . "RESULT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "TIME" *cl-lambda-lists*) (quote [cl-struct-lambda-list TIME :MACRO (FORM) 2 0 nil nil nil ((:mandatory . "FORM")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "TRACE" *cl-lambda-lists*) (quote [cl-struct-lambda-list TRACE :MACRO (&REST SPECS) 0 0 ((&REST SPECS)) nil nil ((:rest . "SPECS...")) [lambda-list-accept]]))

(setf (gethash "TRANSLATE-LOGICAL-PATHNAME" *cl-lambda-lists*) (quote [cl-struct-lambda-list TRANSLATE-LOGICAL-PATHNAME :FUNCTION (PATHNAME &KEY) 2 0 nil nil nil ((:mandatory . "PATHNAME")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "TRANSLATE-PATHNAME" *cl-lambda-lists*) (quote [cl-struct-lambda-list TRANSLATE-PATHNAME :FUNCTION (SOURCE FROM-WILDNAME TO-WILDNAME &KEY) 4 0 nil nil nil ((:mandatory . "SOURCE") (:mandatory . "FROM-WILDNAME") (:mandatory . "TO-WILDNAME")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "TREE-EQUAL" *cl-lambda-lists*) (quote [cl-struct-lambda-list TREE-EQUAL :FUNCTION (X Y &KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT nil NOTP)) 3 0 nil nil (TEST-NOT TEST) ((:mandatory . "X") (:mandatory . "Y") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "TRUENAME" *cl-lambda-lists*) (quote [cl-struct-lambda-list TRUENAME :FUNCTION (PATHNAME) 2 0 nil nil nil ((:mandatory . "PATHNAME")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "TRUNCATE" *cl-lambda-lists*) (quote [cl-struct-lambda-list TRUNCATE :FUNCTION (NUMBER &OPTIONAL (DIVISOR 1)) 2 1 nil nil nil ((:mandatory . "NUMBER") (:optional . "[DIVISOR]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "TWO-WAY-STREAM-INPUT-STREAM" *cl-lambda-lists*) (quote [cl-struct-lambda-list TWO-WAY-STREAM-INPUT-STREAM :FUNCTION (INSTANCE) 2 0 nil nil nil ((:mandatory . "INSTANCE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "TWO-WAY-STREAM-OUTPUT-STREAM" *cl-lambda-lists*) (quote [cl-struct-lambda-list TWO-WAY-STREAM-OUTPUT-STREAM :FUNCTION (INSTANCE) 2 0 nil nil nil ((:mandatory . "INSTANCE")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "TYPE-ERROR-DATUM" *cl-lambda-lists*) (quote [cl-struct-lambda-list TYPE-ERROR-DATUM :FUNCTION (CONDITION) 2 0 nil nil nil ((:mandatory . "CONDITION")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "TYPE-ERROR-EXPECTED-TYPE" *cl-lambda-lists*) (quote [cl-struct-lambda-list TYPE-ERROR-EXPECTED-TYPE :FUNCTION (CONDITION) 2 0 nil nil nil ((:mandatory . "CONDITION")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "TYPE-OF" *cl-lambda-lists*) (quote [cl-struct-lambda-list TYPE-OF :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "TYPECASE" *cl-lambda-lists*) (quote [cl-struct-lambda-list TYPECASE :MACRO (KEYFORM &BODY CASES) 2 0 ((&BODY CASES)) nil nil ((:mandatory . "KEYFORM") (:body . "CASES...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "TYPEP" *cl-lambda-lists*) (quote [cl-struct-lambda-list TYPEP :FUNCTION (OBJECT TYPE &OPTIONAL ENVIRONMENT) 3 1 nil nil nil ((:mandatory . "OBJECT") (:mandatory . "TYPE") (:optional . "[ENVIRONMENT]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "UNBOUND-SLOT-INSTANCE" *cl-lambda-lists*) (quote [cl-struct-lambda-list UNBOUND-SLOT-INSTANCE :FUNCTION (CONDITION) 2 0 nil nil nil ((:mandatory . "CONDITION")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "UNEXPORT" *cl-lambda-lists*) (quote [cl-struct-lambda-list UNEXPORT :FUNCTION (SYMBOLS &OPTIONAL (PACKAGE (SANE-PACKAGE))) 2 1 nil nil nil ((:mandatory . "SYMBOLS") (:optional . "[PACKAGE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "UNINTERN" *cl-lambda-lists*) (quote [cl-struct-lambda-list UNINTERN :FUNCTION (SYMBOL &OPTIONAL (PACKAGE (SANE-PACKAGE))) 2 1 nil nil nil ((:mandatory . "SYMBOL") (:optional . "[PACKAGE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "UNION" *cl-lambda-lists*) (quote [cl-struct-lambda-list UNION :FUNCTION (LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT nil NOTP)) 3 0 nil nil (TEST-NOT TEST KEY) ((:mandatory . "LIST1") (:mandatory . "LIST2") (:key . ":KEY") (:key . ":TEST") (:key . ":TEST-NOT")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "UNLESS" *cl-lambda-lists*) (quote [cl-struct-lambda-list UNLESS :MACRO (TEST &BODY FORMS) 2 0 ((&BODY FORMS)) nil nil ((:mandatory . "TEST") (:body . "FORMS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "UNREAD-CHAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list UNREAD-CHAR :FUNCTION (CHARACTER &OPTIONAL (STREAM *STANDARD-INPUT*)) 2 1 nil nil nil ((:mandatory . "CHARACTER") (:optional . "[STREAM]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "UNTRACE" *cl-lambda-lists*) (quote [cl-struct-lambda-list UNTRACE :MACRO (&REST SPECS) 0 0 ((&REST SPECS)) nil nil ((:rest . "SPECS...")) [lambda-list-accept]]))

(setf (gethash "UNUSE-PACKAGE" *cl-lambda-lists*) (quote [cl-struct-lambda-list UNUSE-PACKAGE :FUNCTION (PACKAGES-TO-UNUSE &OPTIONAL (PACKAGE (SANE-PACKAGE))) 2 1 nil nil nil ((:mandatory . "PACKAGES-TO-UNUSE") (:optional . "[PACKAGE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "UNWIND-PROTECT" *cl-lambda-lists*) (quote [cl-struct-lambda-list UNWIND-PROTECT :SPECIAL-OPERATOR (PROTECTED &BODY CLEANUP) 2 0 ((&BODY CLEANUP)) nil nil ((:mandatory . "PROTECTED") (:body . "CLEANUP...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "UPDATE-INSTANCE-FOR-DIFFERENT-CLASS" *cl-lambda-lists*) (quote [cl-struct-lambda-list UPDATE-INSTANCE-FOR-DIFFERENT-CLASS :GENERIC (PREVIOUS CURRENT &REST INITARGS) 3 0 ((&REST INITARGS)) nil nil ((:mandatory . "PREVIOUS") (:mandatory . "CURRENT") (:rest . "INITARGS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "UPDATE-INSTANCE-FOR-REDEFINED-CLASS" *cl-lambda-lists*) (quote [cl-struct-lambda-list UPDATE-INSTANCE-FOR-REDEFINED-CLASS :GENERIC (INSTANCE ADDED-SLOTS DISCARDED-SLOTS PROPERTY-LIST &REST INITARGS) 5 0 ((&REST INITARGS)) nil nil ((:mandatory . "INSTANCE") (:mandatory . "ADDED-SLOTS") (:mandatory . "DISCARDED-SLOTS") (:mandatory . "PROPERTY-LIST") (:rest . "INITARGS...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "UPGRADED-ARRAY-ELEMENT-TYPE" *cl-lambda-lists*) (quote [cl-struct-lambda-list UPGRADED-ARRAY-ELEMENT-TYPE :FUNCTION (SPEC &OPTIONAL ENVIRONMENT) 2 1 nil nil nil ((:mandatory . "SPEC") (:optional . "[ENVIRONMENT]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "UPGRADED-COMPLEX-PART-TYPE" *cl-lambda-lists*) (quote [cl-struct-lambda-list UPGRADED-COMPLEX-PART-TYPE :FUNCTION (SPEC &OPTIONAL ENVIRONMENT) 2 1 nil nil nil ((:mandatory . "SPEC") (:optional . "[ENVIRONMENT]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "UPPER-CASE-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list UPPER-CASE-P :FUNCTION (CHAR) 2 0 nil nil nil ((:mandatory . "CHAR")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "USE-PACKAGE" *cl-lambda-lists*) (quote [cl-struct-lambda-list USE-PACKAGE :FUNCTION (PACKAGES-TO-USE &OPTIONAL (PACKAGE (SANE-PACKAGE))) 2 1 nil nil nil ((:mandatory . "PACKAGES-TO-USE") (:optional . "[PACKAGE]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "USE-VALUE" *cl-lambda-lists*) (quote [cl-struct-lambda-list USE-VALUE :FUNCTION (VALUE &OPTIONAL CONDITION) 2 1 nil nil nil ((:mandatory . "VALUE") (:optional . "[CONDITION]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "USER-HOMEDIR-PATHNAME" *cl-lambda-lists*) (quote [cl-struct-lambda-list USER-HOMEDIR-PATHNAME :FUNCTION (&OPTIONAL HOST) 0 1 nil nil nil ((:optional . "[HOST]")) [lambda-list-accept lambda-list-close]]))

(setf (gethash "VALUES" *cl-lambda-lists*) (quote [cl-struct-lambda-list VALUES :FUNCTION (&REST VALUES) 0 0 ((&REST VALUES)) nil nil ((:rest . "VALUES...")) [lambda-list-accept]]))

(setf (gethash "VALUES-LIST" *cl-lambda-lists*) (quote [cl-struct-lambda-list VALUES-LIST :FUNCTION (LIST) 2 0 nil nil nil ((:mandatory . "LIST")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "VECTOR" *cl-lambda-lists*) (quote [cl-struct-lambda-list VECTOR :FUNCTION (&REST OBJECTS) 0 0 ((&REST OBJECTS)) nil nil ((:rest . "OBJECTS...")) [lambda-list-accept]]))

(setf (gethash "VECTOR-POP" *cl-lambda-lists*) (quote [cl-struct-lambda-list VECTOR-POP :FUNCTION (ARRAY) 2 0 nil nil nil ((:mandatory . "ARRAY")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "VECTOR-PUSH" *cl-lambda-lists*) (quote [cl-struct-lambda-list VECTOR-PUSH :FUNCTION (NEW-EL ARRAY) 3 0 nil nil nil ((:mandatory . "NEW-EL") (:mandatory . "ARRAY")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "VECTOR-PUSH-EXTEND" *cl-lambda-lists*) (quote [cl-struct-lambda-list VECTOR-PUSH-EXTEND :FUNCTION (NEW-ELEMENT VECTOR &OPTIONAL (EXTENSION (1+ (LENGTH VECTOR)))) 3 1 nil nil nil ((:mandatory . "NEW-ELEMENT") (:mandatory . "VECTOR") (:optional . "[EXTENSION]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "VECTORP" *cl-lambda-lists*) (quote [cl-struct-lambda-list VECTORP :FUNCTION (OBJECT) 2 0 nil nil nil ((:mandatory . "OBJECT")) [lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "WARN" *cl-lambda-lists*) (quote [cl-struct-lambda-list WARN :FUNCTION (DATUM &REST ARGUMENTS) 2 0 ((&REST ARGUMENTS)) nil nil ((:mandatory . "DATUM") (:rest . "ARGUMENTS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "WHEN" *cl-lambda-lists*) (quote [cl-struct-lambda-list WHEN :MACRO (TEST &BODY FORMS) 2 0 ((&BODY FORMS)) nil nil ((:mandatory . "TEST") (:body . "FORMS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "WILD-PATHNAME-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list WILD-PATHNAME-P :FUNCTION (PATHNAME &OPTIONAL FIELD-KEY) 2 1 nil nil nil ((:mandatory . "PATHNAME") (:optional . "[FIELD-KEY]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "WITH-ACCESSORS" *cl-lambda-lists*) (quote [cl-struct-lambda-list WITH-ACCESSORS :MACRO (SLOTS INSTANCE &BODY BODY) 3 0 ((&BODY BODY)) nil nil ((:mandatory . "SLOTS") (:mandatory . "INSTANCE") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "WITH-COMPILATION-UNIT" *cl-lambda-lists*) (quote [cl-struct-lambda-list WITH-COMPILATION-UNIT :MACRO (OPTIONS &BODY BODY) 2 0 ((&BODY BODY)) nil nil ((:mandatory . "OPTIONS") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "WITH-CONDITION-RESTARTS" *cl-lambda-lists*) (quote [cl-struct-lambda-list WITH-CONDITION-RESTARTS :MACRO (CONDITION-FORM RESTARTS-FORM &BODY BODY) 3 0 ((&BODY BODY)) nil nil ((:mandatory . "CONDITION-FORM") (:mandatory . "RESTARTS-FORM") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "WITH-HASH-TABLE-ITERATOR" *cl-lambda-lists*) (quote [cl-struct-lambda-list WITH-HASH-TABLE-ITERATOR :MACRO ((FUNCTION HASH-TABLE) &BODY BODY) 2 0 ((&BODY BODY)) nil nil ((:mandatory . "(FUNCTION HASH-TABLE)") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "WITH-INPUT-FROM-STRING" *cl-lambda-lists*) (quote [cl-struct-lambda-list WITH-INPUT-FROM-STRING :MACRO ((VAR STRING &KEY INDEX START END) &BODY FORMS-DECLS) 2 0 ((&BODY FORMS-DECLS)) nil nil ((:mandatory . "(VAR STRING &KEY INDEX START END)") (:body . "FORMS-DECLS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "WITH-OPEN-FILE" *cl-lambda-lists*) (quote [cl-struct-lambda-list WITH-OPEN-FILE :MACRO ((STREAM FILESPEC &REST OPTIONS) &BODY BODY) 2 0 ((&BODY BODY)) nil nil ((:mandatory . "(STREAM FILESPEC &REST OPTIONS)") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "WITH-OPEN-STREAM" *cl-lambda-lists*) (quote [cl-struct-lambda-list WITH-OPEN-STREAM :MACRO ((VAR STREAM) &BODY FORMS-DECLS) 2 0 ((&BODY FORMS-DECLS)) nil nil ((:mandatory . "(VAR STREAM)") (:body . "FORMS-DECLS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "WITH-OUTPUT-TO-STRING" *cl-lambda-lists*) (quote [cl-struct-lambda-list WITH-OUTPUT-TO-STRING :MACRO ((VAR &OPTIONAL STRING &KEY (ELEMENT-TYPE (QUOTE (QUOTE CHARACTER)))) &BODY FORMS-DECLS) 2 0 ((&BODY FORMS-DECLS)) nil nil ((:mandatory . "(VAR &OPTIONAL STRING &KEY (ELEMENT-TYPE (QUOTE (QUOTE CHARACTER))))") (:body . "FORMS-DECLS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "WITH-PACKAGE-ITERATOR" *cl-lambda-lists*) (quote [cl-struct-lambda-list WITH-PACKAGE-ITERATOR :MACRO ((MNAME PACKAGE-LIST &REST SYMBOL-TYPES) &BODY BODY) 2 0 ((&BODY BODY)) nil nil ((:mandatory . "(MNAME PACKAGE-LIST &REST SYMBOL-TYPES)") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "WITH-SIMPLE-RESTART" *cl-lambda-lists*) (quote [cl-struct-lambda-list WITH-SIMPLE-RESTART :MACRO ((RESTART-NAME FORMAT-STRING &REST FORMAT-ARGUMENTS) &BODY FORMS) 2 0 ((&BODY FORMS)) nil nil ((:mandatory . "(RESTART-NAME FORMAT-STRING &REST FORMAT-ARGUMENTS)") (:body . "FORMS...")) [lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "WITH-SLOTS" *cl-lambda-lists*) (quote [cl-struct-lambda-list WITH-SLOTS :MACRO (SLOTS INSTANCE &BODY BODY) 3 0 ((&BODY BODY)) nil nil ((:mandatory . "SLOTS") (:mandatory . "INSTANCE") (:body . "BODY...")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-accept]]))

(setf (gethash "WITH-STANDARD-IO-SYNTAX" *cl-lambda-lists*) (quote [cl-struct-lambda-list WITH-STANDARD-IO-SYNTAX :MACRO (&BODY BODY) 0 0 ((&BODY BODY)) nil nil ((:body . "BODY...")) [lambda-list-accept]]))

(setf (gethash "WRITE" *cl-lambda-lists*) (quote [cl-struct-lambda-list WRITE :FUNCTION (OBJECT &KEY ((STREAM STREAM) *STANDARD-OUTPUT*) ((ESCAPE *PRINT-ESCAPE*) *PRINT-ESCAPE*) ((RADIX *PRINT-RADIX*) *PRINT-RADIX*) ((BASE *PRINT-BASE*) *PRINT-BASE*) ((CIRCLE *PRINT-CIRCLE*) *PRINT-CIRCLE*) ((PRETTY *PRINT-PRETTY*) *PRINT-PRETTY*) ((LEVEL *PRINT-LEVEL*) *PRINT-LEVEL*) ((LENGTH *PRINT-LENGTH*) *PRINT-LENGTH*) ((CASE *PRINT-CASE*) *PRINT-CASE*) ((ARRAY *PRINT-ARRAY*) *PRINT-ARRAY*) ((GENSYM *PRINT-GENSYM*) *PRINT-GENSYM*) ((READABLY *PRINT-READABLY*) *PRINT-READABLY*) ((RIGHT-MARGIN *PRINT-RIGHT-MARGIN*) *PRINT-RIGHT-MARGIN*) ((MISER-WIDTH *PRINT-MISER-WIDTH*) *PRINT-MISER-WIDTH*) ((LINES *PRINT-LINES*) *PRINT-LINES*) ((PPRINT-DISPATCH *PRINT-PPRINT-DISPATCH*) *PRINT-PPRINT-DISPATCH*)) 2 0 nil nil (PPRINT-DISPATCH LINES MISER-WIDTH RIGHT-MARGIN READABLY GENSYM ARRAY CASE LENGTH LEVEL PRETTY CIRCLE BASE RADIX ESCAPE STREAM) ((:mandatory . "OBJECT") (:key . "STREAM") (:key . "ESCAPE") (:key . "RADIX") (:key . "BASE") (:key . "CIRCLE") (:key . "PRETTY") (:key . "LEVEL") (:key . "LENGTH") (:key . "CASE") (:key . "ARRAY") (:key . "GENSYM") (:key . "READABLY") (:key . "RIGHT-MARGIN") (:key . "MISER-WIDTH") (:key . "LINES") (:key . "PPRINT-DISPATCH")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "WRITE-BYTE" *cl-lambda-lists*) (quote [cl-struct-lambda-list WRITE-BYTE :FUNCTION (INTEGER STREAM) 3 0 nil nil nil ((:mandatory . "INTEGER") (:mandatory . "STREAM")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "WRITE-CHAR" *cl-lambda-lists*) (quote [cl-struct-lambda-list WRITE-CHAR :FUNCTION (CHARACTER &OPTIONAL (STREAM *STANDARD-OUTPUT*)) 2 1 nil nil nil ((:mandatory . "CHARACTER") (:optional . "[STREAM]")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-close]]))

(setf (gethash "WRITE-LINE" *cl-lambda-lists*) (quote [cl-struct-lambda-list WRITE-LINE :FUNCTION (STRING &OPTIONAL (STREAM *STANDARD-OUTPUT*) &KEY (START 0) END) 2 1 nil nil (END START) ((:mandatory . "STRING") (:optional . "[STREAM]") (:key . ":START") (:key . ":END")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "WRITE-SEQUENCE" *cl-lambda-lists*) (quote [cl-struct-lambda-list WRITE-SEQUENCE :FUNCTION (SEQ STREAM &KEY (START 0) (END nil)) 3 0 nil nil (END START) ((:mandatory . "SEQ") (:mandatory . "STREAM") (:key . ":START") (:key . ":END")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "WRITE-STRING" *cl-lambda-lists*) (quote [cl-struct-lambda-list WRITE-STRING :FUNCTION (STRING &OPTIONAL (STREAM *STANDARD-OUTPUT*) &KEY (START 0) END) 2 1 nil nil (END START) ((:mandatory . "STRING") (:optional . "[STREAM]") (:key . ":START") (:key . ":END")) [lambda-list-accept lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "WRITE-TO-STRING" *cl-lambda-lists*) (quote [cl-struct-lambda-list WRITE-TO-STRING :FUNCTION (OBJECT &KEY ((ESCAPE *PRINT-ESCAPE*) *PRINT-ESCAPE*) ((RADIX *PRINT-RADIX*) *PRINT-RADIX*) ((BASE *PRINT-BASE*) *PRINT-BASE*) ((CIRCLE *PRINT-CIRCLE*) *PRINT-CIRCLE*) ((PRETTY *PRINT-PRETTY*) *PRINT-PRETTY*) ((LEVEL *PRINT-LEVEL*) *PRINT-LEVEL*) ((LENGTH *PRINT-LENGTH*) *PRINT-LENGTH*) ((CASE *PRINT-CASE*) *PRINT-CASE*) ((ARRAY *PRINT-ARRAY*) *PRINT-ARRAY*) ((GENSYM *PRINT-GENSYM*) *PRINT-GENSYM*) ((READABLY *PRINT-READABLY*) *PRINT-READABLY*) ((RIGHT-MARGIN *PRINT-RIGHT-MARGIN*) *PRINT-RIGHT-MARGIN*) ((MISER-WIDTH *PRINT-MISER-WIDTH*) *PRINT-MISER-WIDTH*) ((LINES *PRINT-LINES*) *PRINT-LINES*) ((PPRINT-DISPATCH *PRINT-PPRINT-DISPATCH*) *PRINT-PPRINT-DISPATCH*)) 2 0 nil nil (PPRINT-DISPATCH LINES MISER-WIDTH RIGHT-MARGIN READABLY GENSYM ARRAY CASE LENGTH LEVEL PRETTY CIRCLE BASE RADIX ESCAPE) ((:mandatory . "OBJECT") (:key . "ESCAPE") (:key . "RADIX") (:key . "BASE") (:key . "CIRCLE") (:key . "PRETTY") (:key . "LEVEL") (:key . "LENGTH") (:key . "CASE") (:key . "ARRAY") (:key . "GENSYM") (:key . "READABLY") (:key . "RIGHT-MARGIN") (:key . "MISER-WIDTH") (:key . "LINES") (:key . "PPRINT-DISPATCH")) [lambda-list-accept lambda-list-accept lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-check-current-keyword lambda-list-check-previous-keyword lambda-list-close]]))

(setf (gethash "Y-OR-N-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list Y-OR-N-P :FUNCTION (&OPTIONAL FORMAT-STRING &REST ARGUMENTS) 0 1 ((&REST ARGUMENTS)) nil nil ((:optional . "[FORMAT-STRING]") (:rest . "ARGUMENTS...")) [lambda-list-accept lambda-list-accept]]))

(setf (gethash "YES-OR-NO-P" *cl-lambda-lists*) (quote [cl-struct-lambda-list YES-OR-NO-P :FUNCTION (&OPTIONAL FORMAT-STRING &REST ARGUMENTS) 0 1 ((&REST ARGUMENTS)) nil nil ((:optional . "[FORMAT-STRING]") (:rest . "ARGUMENTS...")) [lambda-list-accept lambda-list-accept]]))

(setf (gethash "ZEROP" *cl-lambda-lists*) (quote [cl-struct-lambda-list ZEROP :FUNCTION (NUMBER) 2 0 nil nil nil ((:mandatory . "NUMBER")) [lambda-list-accept lambda-list-accept lambda-list-close]]))
