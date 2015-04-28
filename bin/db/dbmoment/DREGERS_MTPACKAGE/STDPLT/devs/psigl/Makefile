#
# Makefile for postscript mohochrome printer.
#
# +D tells compiler to use large memory model.
NAME=	psigl
HEADERS=	../../h/igl.h devpar.h
SYMBOLHEADERS=	../com/symboldata.h
CFLAGS = -O -Wno-implicit-int
DEVDIR=	../..
PRINT=	lpr -p

MANDIR = /usr/man
MANEXT = l

# Code used by both PS and PSIGL
POBJ = bye.o cgen.o cmdlist.o compile.o dashline.o fontdata.o getspcpar.o \
       getstdpar.o glo.o opt.o par.o symbol.o text.o unixmain.o

# Code used by PS
PSOBJ = sherr.o shinp.o shout.o shmain.o

# Code used by PSIGL
PSIGLOBJ = saerr.o sainp.o saout.o samain.o


${NAME}:	$(POBJ) $(PSOBJ)
	gcc -o ${NAME} $(POBJ) $(PSOBJ) -lm
	mv psigl ../../../BIN/psigl

$(POBJ) $(PSOBJ) $(PSIGLOBJ):	const.h types.h
glo.o:		${HEADERS}
symbol.o:	${SYMBOLHEADERS}

install:	${NAME}
	install -c -m 755 ${NAME} ${DEVDIR}

xinstall:	${NAME}
	install -c -m 755 ${NAME} ${DEVDIR}/x${NAME}

man:	psigl.man
	install -c psigl.man ${MANDIR}/man${MANEXT}/psigl.${MANEXT}

list:
	${PRINT} makefile devpar.h $(SOURCE)

cleanup:
	-rm -f ${NAME} *.o a.out core
