(asdf-load :cl-ppcre :split-sequence)


(defparameter *pl1-keywords*
  "a
abnormal
act(ivate)?
addbuff
alias
aligned
alloc(ate)?
anycond(ition)?
area
ascii
assignable|asgn
asm|assembler
attach
attention|attn
auto(matic)?
b
b1
b2
b3
b4
backwards
based
begin
bigendian
bin(ary)?
bit
bkwd
blksize
buf(fered)?
buff(ers)?
buffoff
bufnd
bufni
bufsp
builtin
by
byaddr
byvalue
bx
c
call
cdecl
cell
char(acter)?
charg(raphic)?
check
close
cobol
col(umn)?
complex|cplx
conn(ected)?
cond(ition)?
consecutive
const(ant)?
ctlasa
ctl360
controlled|ctl
conv(ersion)?
copy
d
db
data
date
dcl|declare
deact(ivate)?
dec(imal)?
dft|default
delay
delete
define
def(ined)?
descriptor
descriptors
detach
dim(ension)?
direct
display
do
downthru
e
edit
else
endendfile
endpage
entry
env(ironment)?
error
event
excl(usive)?
exit
exports
ext(ernal)?
f
fb
fs
fbs
fetch
fetchable
file
finish
fixed
fixedoverflow|fofl
float
flush
free
forever
fortran
format
from
fromalien
g
generic
genkey
get
go
goto
graphic
gx
handle
hexadec
i
ieee
if
ignore
imported
in
include
indexarea
indexed
init(ial)?
inline
input
inter
interactive
int(ernal)?
into
invalidop
irred(ucible)?
iterate
key
keyed
keyfrom
keylength
keyloc
keyto
label
leave
limited
like
line
linesize
linkage
list
littleendian
local
locate
loop
m
main
name
ncp
nocharg(raphic)?
nocheck
noconv(ersion)?
nodescriptor
noexecops
nof(ixedoverflow|ofl)
nolock
nonassignable|nonasgn
nonconn(ected)?
none
nonvar(ying)?
non_quick
no_quick_blocks
noinit
noinline
noinvalidop
noo(verflow|fl)
noprint
normal
nosize
nosub(scriptrange|rg)
nostr(ingrange|g)
nostr(ingsize|z)
note
nou(nderflow|fl)
nowrite
noz(erodivide|div)
offset
on
open
optional
options
optlink
order
ordinal
other(wise)?
output
overflow|ofl
p
package
packed_decimal
packed
page
pagesize
par(m|ameter)
password
pending
pic(ture)?
pointer|ptr
pos(ition)?
prec(ision)?
print
priority
proc(edure)?
put
r
range
read
real
record
recsize
recursive
red(ucible)?
reentrant
refer
regional
release
rename
reorder
repeat
replace
reply
reread
reserved
reserves
resignal
retcode
return
returns
reuse
revert
rewrite
scalarvarying
select
separate_static
set
sequential|seql
signal
signed
sis
size
skip
snap
static
stdcall
storage
stop
stream
string
stringrange|strg
stringsize|strz
stringvalue
structure
sub
subscriptrange|subrg
support
system
task
then
thread
title
to
total
tp
transient
transmit
trkofl
tstack
type
u
unal(igned)?
unbuf(fered)?
unconnected
undefinedfile|undf
underflow|ufl
union
unlock
uns(igned)?
until
update
upthru
v
validate
value
variable
var(ying)?
var(ying)?z
vb
vbs
vs
vsam
wait
when
w(ide)?char
winmain
while
write
wx
x
xn
xu
zerodivide|zdiv
")





(cl-ppcre::parse-string "zerodivide|zdiv") --> (:ALTERNATION "zerodivide" "zdiv")
(cl-ppcre::parse-string "var(ying)?z")     --> (:SEQUENCE "var" (:GREEDY-REPETITION 0 1 (:REGISTER "ying")) #\z)
(cl-ppcre::parse-string "par(m|ameter)")   --> (:SEQUENCE "par" (:REGISTER (:ALTERNATION #\m "ameter")))

(defgrammar lex-regexp ()
  (regexp ::= term terms)
  (terms  ::= (alt (seq term terms)
                   empty))
  (empty  ::=)
  (term   ::=  )
  )


(defun parse-lex-regexp (regexp)
  (let ((result '()))
   (labels ((remove-register (expr)
              (cond
                ((atom expr) expr)
                ((eq :register (first expr)) (remove-register (second expr)))
                (t (mapcar (function remove-register) expr))))
            (generate-keywords (expr)
              (cond
                ((characterp expr) (list (string expr)))
                ((stringp    expr) (list expr))
                ((atom expr) (error "~S unexpected ~S" 'GENERATE-KEYWORDS expr))
                (t (ecase (first expr)
                     ((:alternation)
                      (mapcan (function generate-keywords) (rest expr)))
                     ((:sequence)
                      (mapcar (lambda (seq)
                                (apply (function concatenate) 'string seq))
                              (apply (function COM.INFORMATIMAGO.COMMON-LISP.LIST:COMBINE)
                                     (mapcar (function generate-keywords) (rest expr)))))
                     ((:greedy-repetition)
                      (destructuring-bind (g min max subexpr) expr
                        (if (and (integerp min)
                                 (integerp max)
                                 (<= min max))
                            (apply (function COM.INFORMATIMAGO.COMMON-LISP.LIST:COMBINE)
                                   (generate-keywords subexpr))
                            (error "~S unexpected bounds in ~S" 'GENERATE-KEYWORDS expr)))))))))
     (generate-keywords (remove-register (cl-ppcre::parse-string regexp))))))




;; (mapcar 'parse-lex-regexp
;;         '("write" "zerodivide|zdiv" "var(ying)?z" "var((yi)(ng))?z" "par(m|ameter)"
;;           "((a|b)(c|d))" "beg((a|b)(c|d))?end"))


(defparameter *expanded-keywords*
  (mapcar (lambda (regexp) (parse-lex-regexp (string-trim "     " regexp)))
         (split-sequence:split-sequence  #\newline  *pl1-keywords* :remove-empty-subseqs t)))

;; (reduce (function append) *expanded-keywords*)

;; (("a") ("abnormal") ("activate") ("addbuff") ("alias") ("aligned") ("allocate")
;;  ("anycondition") ("area") ("ascii") ("assignable" "asgn") ("asm" "assembler")
;;  ("attach") ("attention" "attn") ("automatic") ("b") ("b1") ("b2") ("b3")
;;  ("b4") ("backwards") ("based") ("begin") ("bigendian") ("binary") ("bit")
;;  ("bkwd") ("blksize") ("buffered") ("buffers") ("buffoff") ("bufnd") ("bufni")
;;  ("bufsp") ("builtin") ("by") ("byaddr") ("byvalue") ("bx") ("c") ("call")
;;  ("cdecl") ("cell") ("character") ("chargraphic") ("check") ("close") ("cobol")
;;  ("column") ("complex" "cplx") ("connected") ("condition") ("consecutive")
;;  ("constant") ("ctlasa") ("ctl360") ("controlled" "ctl") ("conversion")
;;  ("copy") ("d") ("db") ("data") ("date") ("dcl" "declare") ("deactivate")
;;  ("decimal") ("dft" "default") ("delay") ("delete") ("define") ("defined")
;;  ("descriptor") ("descriptors") ("detach") ("dimension") ("direct") ("display")
;;  ("do") ("downthru") ("e") ("edit") ("else") ("endendfile") ("endpage")
;;  ("entry") ("environment") ("error") ("event") ("exclusive") ("exit")
;;  ("exports") ("external") ("f") ("fb") ("fs") ("fbs") ("fetch") ("fetchable")
;;  ("file") ("finish") ("fixed") ("fixedoverflow" "fofl") ("float") ("flush")
;;  ("free") ("forever") ("fortran") ("format") ("from") ("fromalien") ("g")
;;  ("generic") ("genkey") ("get") ("go") ("goto") ("graphic") ("gx") ("handle")
;;  ("hexadec") ("i") ("ieee") ("if") ("ignore") ("imported") ("in") ("include")
;;  ("indexarea") ("indexed") ("initial") ("inline") ("input") ("inter")
;;  ("interactive") ("internal") ("into") ("invalidop") ("irreducible")
;;  ("iterate") ("key") ("keyed") ("keyfrom") ("keylength") ("keyloc") ("keyto")
;;  ("label") ("leave") ("limited") ("like") ("line") ("linesize") ("linkage")
;;  ("list") ("littleendian") ("local") ("locate") ("loop") ("m") ("main")
;;  ("name") ("ncp") ("nochargraphic") ("nocheck") ("noconversion")
;;  ("nodescriptor") ("noexecops") ("nofixedoverflow" "nofofl") ("nolock")
;;  ("nonassignable" "nonasgn") ("nonconnected") ("none") ("nonvarying")
;;  ("non_quick") ("no_quick_blocks") ("noinit") ("noinline") ("noinvalidop")
;;  ("nooverflow" "noofl") ("noprint") ("normal") ("nosize")
;;  ("nosubscriptrange" "nosubrg") ("nostringrange" "nostrg")
;;  ("nostringsize" "nostrz") ("note") ("nounderflow" "noufl") ("nowrite")
;;  ("nozerodivide" "nozdiv") ("offset") ("on") ("open") ("optional") ("options")
;;  ("optlink") ("order") ("ordinal") ("otherwise") ("output") ("overflow" "ofl")
;;  ("p") ("package") ("packed_decimal") ("packed") ("page") ("pagesize")
;;  ("parm" "parameter") ("password") ("pending") ("picture") ("pointer" "ptr")
;;  ("position") ("precision") ("print") ("priority") ("procedure") ("put") ("r")
;;  ("range") ("read") ("real") ("record") ("recsize") ("recursive") ("reducible")
;;  ("reentrant") ("refer") ("regional") ("release") ("rename") ("reorder")
;;  ("repeat") ("replace") ("reply") ("reread") ("reserved") ("reserves")
;;  ("resignal") ("retcode") ("return") ("returns") ("reuse") ("revert")
;;  ("rewrite") ("scalarvarying") ("select") ("separate_static") ("set")
;;  ("sequential" "seql") ("signal") ("signed") ("sis") ("size") ("skip") ("snap")
;;  ("static") ("stdcall") ("storage") ("stop") ("stream") ("string")
;;  ("stringrange" "strg") ("stringsize" "strz") ("stringvalue") ("structure")
;;  ("sub") ("subscriptrange" "subrg") ("support") ("system") ("task") ("then")
;;  ("thread") ("title") ("to") ("total") ("tp") ("transient") ("transmit")
;;  ("trkofl") ("tstack") ("type") ("u") ("unaligned") ("unbuffered")
;;  ("unconnected") ("undefinedfile" "undf") ("underflow" "ufl") ("union")
;;  ("unlock") ("unsigned") ("until") ("update") ("upthru") ("v") ("validate")
;;  ("value") ("variable") ("varying") ("varyingz") ("vb") ("vbs") ("vs") ("vsam")
;;  ("wait") ("when") ("widechar") ("winmain") ("while") ("write") ("wx") ("x")
;;  ("xn") ("xu") ("zerodivide" "zdiv"))






