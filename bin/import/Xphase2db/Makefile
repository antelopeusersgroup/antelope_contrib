BIN=Xphase2db		# commands to be placed in bin
PF=Xphase2db.pf		# parameter files to be placed in data/pf

MAN1=		# man pages for commands
MAN3=		# man pages for library routines
MAN5=		# man pages for file formats

CLEAN= 		# Extra files which should be removed during a "make clean"

cflags=-g	# -g or -O is the most common option here and in fflags
fflags= 	# 
ldflags= 	# flags to linker/loader
ldlibs= $(DBLIBS) -lstock		

include $(ANTELOPEMAKE)  	# This line must appear following the definitions above, 
			# and before the DIRS definition below..
DIRS=			# Omit this line to run make in subdirectories, or
			# fill in to run make only in certain subdirectories

OBJS=	Xphase2db.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
