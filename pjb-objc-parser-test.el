;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-obj-parser-test.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pbourguignon@dxo.com>
;;;;MODIFICATIONS
;;;;    2013-01-28 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2013 - 2013
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(require 'pjb-objc-parser)

(defun test/pjb-objc-parser--eat-sexp ()
  (with-temp-buffer
    (lisp-mode)
    (insert "
sexp-1  sexp-2
")
    (goto-char (point-min))
    (let ((sexp (pjb-objc-parser--eat-sexp)))
      (assert (equal sexp 'sexp-1) t)
      (assert (= (point) 8) t)))
    (with-temp-buffer
      (lisp-mode)
      (insert "; test 2
\(sexp-1  sexp-2)
")
    (goto-char (point-min))
    (let ((sexp (pjb-objc-parser--eat-sexp)))
      (assert (equal sexp '(sexp-1 sexp-2)) t)
      (assert (= (point) 26) t)))
  :success)


(defun test/pjb-objc-parser--eat-looked ()
  (with-temp-buffer
    (lisp-mode)
    (insert "aaaabbbaaaabbbbaaacccddaaaddaaadd")
    (goto-char (point-min))
    (looking-at "\\(a+b+\\)*a*c+")
    (pjb-objc-parser--eat-looked)
    (assert (= (point) 22) t))
  :success)


(defun test/pjb-objc-parser--parse-parameter ()
  (with-temp-buffer
    (objc-mode)
    (insert "(int)anInt
\(NSString *)aString
\(^(void)(int a,NSString* b))aBlock;
")
    (goto-char (point-min))
    (let ((p (pjb-objc-parser--parse-parameter)))
      (assert (equal '(int) (pjb-objc-parameter-type p)) t)
      (assert (equal 'anInt (pjb-objc-parameter-name p)) t))
    (let ((p (pjb-objc-parser--parse-parameter)))
      (assert (equal '(NSString *) (pjb-objc-parameter-type p)) t)
      (assert (equal 'aString (pjb-objc-parameter-name p)) t))
    (let ((p (pjb-objc-parser--parse-parameter)))
      ;; TODO: this type is parsed anyhow, until we have a true type parser rule.
      (assert (equal '(^(void)(int a,NSString* b)) (pjb-objc-parameter-type p)) t)
      (assert (equal 'aBlock (pjb-objc-parameter-name p)) t)))
  :success)


(defun test/pjb-objc-parser--next ()
  (cond
    ((pjb-objc-parser--looking-at-semicolon)
     (pjb-objc-parser--eat-looked))
    ((pjb-objc-parser--looking-at-block)
     (forward-sexp))
    ((pjb-objc-parser--looking-at-comment)
     (c-forward-comments))))


(defun test/pjb-objc-parser--parse-method-signature ()
  (with-temp-buffer
    (objc-mode)
    (insert "+(void)aClassMethodSignature;
-(id)simpleMethod:(int)anInt;
- (NSString *)aMethod:(NSString *)aString with:(^(void)(int a,NSString* b))aBlock
{
  return nil;
}
-(NSString*)aMethod:(NSString *)aString with:(^(void)(int a,NSString* b))aBlock andSome:(int)code :(int)count,...;
")
    (goto-char (point-min))
    (let ((s (pjb-objc-parser--parse-method-signature)))
      (assert (equal :class (pjb-objc-method-signature-object s)) t)
      (assert (equal '(void) (pjb-objc-method-signature-result-type s)) t)
      (assert (equalp (pjb-objc-selector '(aClassMethodSignature))
                      (pjb-objc-method-signature-selector s)) t)
      (assert (equalp '()
                      (pjb-objc-method-signature-parameters s)) t))
    (test/pjb-objc-parser--next)
    (let ((s (pjb-objc-parser--parse-method-signature)))
      (assert (equal :instance (pjb-objc-method-signature-object s)) t)
      (assert (equal '(id) (pjb-objc-method-signature-result-type s)) t)
      (assert (equalp (pjb-objc-selector '(simpleMethod:))
                      (pjb-objc-method-signature-selector s)) t)
      (assert (equalp (list (pjb-objc-parameter :name 'anInt :type '(int)))
                      (pjb-objc-method-signature-parameters s)) t))
    (test/pjb-objc-parser--next)
    (let ((s (pjb-objc-parser--parse-method-signature)))
      (assert (equal :instance (pjb-objc-method-signature-object s)) t)
      (assert (equal '(NSString *) (pjb-objc-method-signature-result-type s)) t)
      (assert (equalp (pjb-objc-selector '(aMethod: with:))
                      (pjb-objc-method-signature-selector s)) t)
      (assert (equalp (list (pjb-objc-parameter :name 'aString :type '(NSString *))
                            (pjb-objc-parameter :name 'aBlock :type '(^(void)(int a,NSString* b))))
                      (pjb-objc-method-signature-parameters s)) t))
    (test/pjb-objc-parser--next)
    (let ((s (pjb-objc-parser--parse-method-signature)))
      (assert (equal :instance (pjb-objc-method-signature-object s)) t)
      ;; TODO: this type is parsed anyhow, until we have a true type parser rule.
      (assert (equal '(NSString*) (pjb-objc-method-signature-result-type s)) t)
      (assert (equalp (pjb-objc-selector '(aMethod: with: andSome: :))
                      (pjb-objc-method-signature-selector s)) t)
      (assert (equalp (list (pjb-objc-parameter :name 'aString :type '(NSString *))
                            (pjb-objc-parameter :name 'aBlock :type '(^(void)(int a,NSString* b)))
                            (pjb-objc-parameter :name 'code :type '(int))
                            (pjb-objc-parameter :name 'count :type '(int))
                            (pjb-objc-parameter :name '\... :type nil))
                      (pjb-objc-method-signature-parameters s)) t))
    (test/pjb-objc-parser--next))
  :success)


(defun test/pjb-objc-parser ()
  (test/pjb-objc-parser--eat-sexp)
  (test/pjb-objc-parser--eat-looked)
  (test/pjb-objc-parser--parse-parameter)
  (test/pjb-objc-parser--parse-method-signature))


(test/pjb-objc-parser)
