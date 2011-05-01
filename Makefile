#******************************************************************************
#FILE:              Makefile
#LANGUAGE:          make
#SYSTEM:            UNIX
#USER-INTERFACE:    None
#DESCRIPTION
#USAGE
#AUTHORS
#    <PJB> Pascal J. Bourguignon
#MODIFICATIONS
#    2003-01-17 <PJB> Added this header comment.
#BUGS
#LEGAL
#    GPL
#    
#    Copyright Pascal J. Bourguignon 2003 - 2011
#    
#    This program is free software; you can redistribute it and/or
#    modify it under the terms of the GNU General Public License
#    as published by the Free Software Foundation; either version
#    2 of the License, or (at your option) any later version.
#    
#    This program is distributed in the hope that it will be
#    useful, but WITHOUT ANY WARRANTY; without even the implied
#    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#    PURPOSE.  See the GNU General Public License for more details.
#    
#    You should have received a copy of the GNU General Public
#    License along with this program; if not, write to the Free
#    Software Foundation, Inc., 59 Temple Place, Suite 330,
#    Boston, MA 02111-1307 USA
#******************************************************************************
TARGET  := $(shell uname)
PREFIX  := /usr/local
MAKEDIR := $(HOME)/src/public/common/makedir

SHARE_LISP=$(shell get-directory SHARE_LISP)

MODULE=emacs
PACKAGE_PATH=com/informatimago/emacs
PACKAGES=$(SHARE_LISP)/packages

EMACS_FLAGS= -l .emacs # -l pjb-cl.el

AWK = $(shell which awk)

# .el.elc: ; 	@echo '' ; echo Compiling $< ; emacs -batch -q $(EMACSFLAGS) -f batch-byte-compile $< 2>&1 | egrep -v -e 'Loading .*/fns-'
# See also .emacs in this directory with the load-path used to compile.


EMACS_SOURCES=\
	pjb-advices.el \
	pjb-asm7090.el \
	pjb-banks.el \
	pjb-bourse.el \
	pjb-c.el \
	pjb-cl.el \
	pjb-cl-faces.el \
	pjb-cl-magic.el \
	pjb-class.el \
	pjb-constants.el \
	pjb-cvs.el \
	pjb-cvspass.el \
	pjb-erc.el \
	pjb-font.el \
	pjb-dot.el \
	pjb-dodo.el \
	pjb-emacs.el \
	pjb-computer-paper.el \
	pjb-euro.el \
	pjb-graph.el \
	pjb-i2p-expression.el \
	pjb-layers.el \
	pjb-list.el \
	pjb-mail.el \
	pjb-make-depends.el \
	pjb-objc-mode.el \
	pjb-object.el \
	pjb-pgp.el \
	pjb-queue.el \
	pjb-roman.el \
	pjb-s2p-expression.el \
	pjb-secouer.el \
	pjb-selftrade.el \
	pjb-server.el \
	pjb-sources.el \
	pjb-state-coding.el \
	pjb-shell.el \
	pjb-strings.el \
	pjb-transpose.el \
	pjb-utilities.el \
	pjb-vm-kill-file.el \
	pjb-work.el \
	pjb-worldfact.el 

EMACS_OBJECTS=$(EMACS_SOURCES:.el=.elc)
LISP_OBJECTS=$(EMACS_OBJECTS) 

all::$(EMACS_OBJECTS) summary.html
	-@chmod a+r *

install::install-packages




TRACE=@
LISP_SOURCE_SUFFIXES= .el   .lisp
LISP_OBJECT_SUFFIXES= .elc  .fas .lib   .x86f   .fasl 

.SUFFIXES:: $(LISP_SOURCE_SUFFIXES) $(LISP_OBJECT_SUFFIXES)


# ------------------------------------------------------------------------
# EMACS:
# ------------------------------------------------------------------------
EMACS=emacs
EMACS_FLAGS_DEFAULT=
%.elc : %.el
	@ echo "Generating $@" 
	$(TRACE) $(EMACS) -batch -q $(EMACS_FLAGS) -f batch-byte-compile $< 2>&1 \
	| $(AWK) 'BEGIN{s=0;} /Loading .*fns-/{next;} /Warning: Function .* from cl package/{s=1;next;} {if(s==0){print $0}else{s=0;next;}}'
# See also .emacs in this directory with the load-path used to compile.


# ------------------------------------------------------------------------
# Compiling & installing lisp packages:
#
# Targets: install-packages
# Input:   LISP_SOURCES CLISP_SOURCES CMUCL_SOURCES SBCL_SOURCES EMACS_SOURCES 
# Output:  $(PACKAGES)/$(PACKAGE_PATH)/*
install-packages :: do-install
do-install ::	$(EMACS_SOURCES) \
	   			$(CMUCL_SOURCES) \
				$(LISP_SOURCES) \
				$(SBCL_SOURCES)
	@ echo "Installing packages: $(LISP_SOURCES) $(EMACS_SOURCES) $(CLISP_SOURCES) $(CMUCL_SOURCES) $(SBCL_SOURCES)" | fmt
	$(TRACE) umask 022 ;\
	for f in 	$(LISP_SOURCES) $(LISP_OBJECTS)     \
				$(EMACS_SOURCES) $(CLISP_SOURCES)    \
	    		$(CMUCL_SOURCES) $(SBCL_SOURCES) ; do \
		install -v -p -m 644 $$f $(PACKAGES)/$(PACKAGE_PATH)/$$f ;\
	done


summary summary.html:$(EMACS_SOURCES)
	@ true echo $(EMACS_SOURCES) ; echo $(EMACS_SOURCES:.el=)
	@ clisp -q -x "(defparameter *sources* (quote ($(EMACS_SOURCES:.el=))))" \
	           -x "(load \"compile.lisp\")" -on-error debug


# ------------------------------------------------------------------------
# Cleaning:

clean::
	-rm -f $(LISP_OBJECT_SUFFIXES:%=*%)

cleanall:: 
	$(TRACE) for p in $(LISP_PROGRAMS) $(CLISP_PROGRAMS) $(SBCL_PROGRAMS) \
			  $(CMUCL_PROGRAMS) $(EMACS_PROGRAMS) NONE ; do \
		[ "$$p" = NONE ] || $(MAKE) $(MFLAGS) $${p}-clean PGMNAME=$$p ;\
	done



#include $(MAKEDIR)/lisp
#-include Makefile.depend
#
#install::install-packages
#
# depend Makefile.depend :: $(EMACS_SOURCES)
# 	touch Makefile.depend
# depend Makefile.depend :: $(EMACS_SOURCES)
# 	./make-depends -I. $(EMACS_SOURCES:.el=.elc) >> Makefile.depend

#### Makefile                         --                     --          ####


