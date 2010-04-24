;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-worldfact.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Functions to process the CIA Worldfact.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2009-02-01 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2009 - 2009
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************
;;;;    

;; GDP: /ultimo/pascal/factbook2002/fields/2001.html
;; Budget: /ultimo/pascal/factbook2002/fields/2056.html
;; Inflation rate: /ultimo/pascal/factbook2002/fields/2092.html
;; Unemployment rate: /ultimo/pascal/factbook2002/fields/2129.html
;; GDP/capita: /ultimo/pascal/factbook2002/fields/2004.html

(require 'pjb-sources)
(require 'pjb-cl)

(defparameter *world-factbook-base* "/rest/mirrors/cia-factbook")

(pjb-defclass Country ()
  (:att name                    string "Name of this country.")
   (:att GDP                     number "Gross Domestic Product.")
   (:att GDP-per-capita          number "Gross Domestic Product per Capita.")
   (:att budget-revenues         number "Budget revenues.")
   (:att budget-expenditures     number "Budget expenditures.")
   (:att inflation-rate          number "Inflation Rate.")
   (:att unemployment-rate       number "Unemployment Rate.")
   (:att population              number "Population.")
   (:att life-expectancy-both    number "Life Expectancy (both).")
   (:att life-expectancy-male    number "Life Expectancy (male).")
   (:att life-expectancy-female  number "Life Expectancy (female).")
   (:att mil-expenditure-dollar  number "Military expenditures - dollar figure")
   (:att mil-expenditure-per-GDP number "Military expenditures - percent of GDP")
   (:doc "Data about a country."))


(defmethod budget-deficit ((self Country))
  "
RETURN:  The budget-deficit if it's known or else nil.
"
  (if (and (budget-revenues self) (budget-expenditures self))
    (- (budget-revenues self) (budget-expenditures self))
    nil))





(defvar countries nil "An alist of (name . Country).")

(defun country-named (name &optional create)
  "
RETURN: The Country instance named `name', 
        or nil if none exists and not create.
"
  (if name
      (let ((country  (cdr (assoc name countries))))
        (if (and create (null country))
            (progn 
              (setq country (make-instance Country :name name))
              (push (cons name country) countries)))
        country)
    nil))


(defun mapcountries (fun)
  "
DO:     Apply the `function' on each country in the `countries' list.
"
  (mapcar (lambda (country) (funcall fun (cdr country)))
          countries))




(defun clean-fields (field)
  "
RETURN:  nil or a list of  numbers, the valuees described in the field string.
"
  (if field
      (progn
        (dolist (replacement
                 '(
                   ("purchasing power parity - "   ""                 t t)
                   ("total population:"            ""                 t t)
                   ("([A-Za-z]* *[0-9]+ est.)"     ""                 t nil)
                   ("This page was last updated.*" ""                 t nil)
                   ("note:.*"                      ""                 t nil)
                   ("(FY[0-9][0-9]\\(/[0-9][0-9]\\)?\\( est.\\)?)" "" t nil)
                   (" (\\([A-Z][a-z]* \\)?20[0-9][0-9]\\( est.\\)?)" 
                    ""                                                t nil)
                   (" (19[0-9][0-9]\\( est.\\)?)"  ""                 t nil)
                   (" *mill?ion *"                 "e6"               t nil)
                   (" *bill?ion *"                 "e9"               t nil)
                   (" *trill?ion *"                "e12"              t nil)
                   ("; note.*"                     ""                 t nil)
                   ("years"                        ""                 t t)
                   ("\\$"                          ""                 t t)
                   ("Greek Cypriot area\\( - \\|: \\)\\?"    
                    "(\\+ "                                   t t)
                   ("; Turkish Cypriot area\\( - \\|: \\)\\([0-9]+e[0-9]+\\)" 
                    " \\1)"                                   t nil)
                   (", including capital .*"       ""         t nil)
                   ("revenues:"                    ""         t t)
                   ("expenditures:"                ":"        t t)
                   ("male:"                        ":"        t t)
                   ("female:"                      ":"        t t)
                   ("NA *"                         "nil"      t nil)
                   (","                            ""         t t)
                   (" *(Ministry of Defense expenditures)" "" t nil)
                   ))
          (setq field (funcall (function replace-regexp-in-string)
                               (nth 0 replacement) (nth 1 replacement)
                               field 
                               (nth 2 replacement) (nth 3 replacement)))
          ) ;;dolist
        (mapcar (lambda (item)
                  (setq item (chop-spaces item))
                  (cond
                   ((STRING= "nil" item)   
                    nil)
                   ((string-match "[e.]" item) 
                    (string-to-number item))
                   (t                          
                    (string-to-number (concat item ".0")))))
                 (split-string field ":")))
    nil))




(defun load-table (attributes table)
  "
DO:      Loads data from one table into the given attributes
         of the `countries'.
"
  (mapcar (lambda (line)
            (let* ((fields (split-string line "|"))
                   (country (country-named (nth 0 fields) t))
                   (data    (nth 1 fields))
                   (values  (clean-fields data))
                   )
              (when country
                (loop for att in attributes
                      for val in values
                      when val
                      do (setf (slot-value country att) val))
                )))
          (split-string
           (shell-command-to-string 
            (format "html-get-tables %s|sed -e '1,/Country/d'" table))
           "\n"))
  (values))



(defun url-field (page-number)
  (format "%s/fields/%d.html" *world-factbook-base* page-number))

(defun load-countries ()
  "
DO:      Loads data about the countries.
POST:    The `countries' alist is filled.
"
  (load-table '(GDP)                                   (url-field 2001))
  (load-table '(budget-revenues  budget-expenditures ) (url-field 2056))
  (load-table '(inflation-rate)                        (url-field 2092))
  (load-table '(unemployment-rate)                     (url-field 2129))
  (load-table '(population)                            (url-field 2119))
  (load-table '(life-expectancy-both 
                life-expectancy-male 
                life-expectancy-female)                (url-field 2102))
  (load-table '(GDP-per-capita)                        (url-field 2004))
  (load-table '(mil-expenditure-dollar)                (url-field 2067))
  (load-table '(mil-expenditure-per-GDP)               (url-field 2034)))



(defun countries-unemployment-budget ()
  (printf "%7s %13s %s\n" "-------" "-------------" "--------------------")
  (printf "%7s %13s %s\n" "chomage" "depenses" "pays")
  (printf "%7s %13s %s\n" "-------" "-------------" "--------------------")
  (mapc (lambda (country)
          (printf "%6.0f%% %12.3f%% %s\n" 
                  (unemployment-rate country) 
                  (* 100 (/ (budget-expenditures country)
                            (GDP country)))
                  (name country)))
        (sort
         (nremove-nil
          (flatten
           (mapcountries (lambda (country) 
                           (if (and
                                (unemployment-rate country)
                                (budget-expenditures country)
                                (GDP country)
                                (/= 0 (GDP country)))
                               country nil)))))
         (lambda (a b) (<= (/ (budget-expenditures a) (GDP a))
                           (/ (budget-expenditures b) (GDP b))))))
  (printf "%7s %13s %s\n" "-------" "-------------" "--------------------"))



(defun countries-life-expectancy ()
  (printf "%12s    %s\n" "------------" "--------------------")
  (printf "%12s    %s\n" "esperance" "pays")
  (printf "%12s    %s\n" "------------" "--------------------")
  (mapc (lambda (country)
          (printf "%6.1f years    %s\n" 
                  (life-expectancy-male country) 
                  (name country)))
        (sort
         (nremove-nil
          (flatten
           (mapcountries (lambda (country) 
                           (if (life-expectancy-both country) country nil)))))
         (lambda (a b) (>= (life-expectancy-both a) 
                           (life-expectancy-both b) ))))
  (printf "%12s    %s\n" "------------" "--------------------"))




(defun countries-GDP-per-capita ()
  (printf "%12s %12s %s\n" "------------" "------------" "--------------------")
  (printf "%12s %12s %s\n" "GDP/cap/year" "GDP/cap/month" "country")
  (printf "%12s %12s %s\n" "------------" "------------" "--------------------")
  (mapc (lambda (country)
          (printf "%12.2f %12.2f %s\n" 
                  (GDP-per-capita country) 
                  (/ (GDP-per-capita country) 12.0)
                  (name country)) )
        (sort
         (nremove-nil
          (flatten
           (mapcountries (lambda (country) 
                           (if (and
                                (GDP-per-capita country)
                                ;;(<= (GDP-per-capita country) 2400.0)
                                )
                               country nil)))))
         (lambda (a b) (>= (GDP-per-capita a)
                           (GDP-per-capita b)))) )
  (printf "%12s %12s %s\n" "------------" "------------" "--------------------"))


(when nil

  (load-countries)
  (countries-unemployment-budget)
  (countries-life-expectancy)
  (countries-GDP-per-capita)

  (clean-fields "$20.048 billion (2002); note - this is the officially announced figure, but actual defense spending more likely ranges from $45 billion to $65 billion for 2002")
  (clean-fields "$4,027,970 (January 2002)")

  (mil-expenditure-dollar (country-named "France"))
  (mil-expenditure-dollar (country-named "Fiji"))
  (mil-expenditure-dollar (country-named "Ireland"))
  (mil-expenditure-dollar (country-named "India"))
  (mil-expenditure-dollar (country-named "Israel"))

  (let ((other 0.0))
    (mapcountries (lambda (country)
                    (if (and (not (STRING= "United States" (name country)))
                             (numberp (mil-expenditure-dollar country)))
                      (incf other (mil-expenditure-dollar country)))
                    (printf "%20S   %6S %%   %s\n" 
                            (mil-expenditure-dollar country)
                            (mil-expenditure-per-GDP country)
                            (name country))))
    (printf "%20S\n" other))

  )

;;;; pjb-worldfact.el                 --                     --          ;;;;
