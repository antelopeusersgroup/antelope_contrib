BIN=mwapbavg		# commands to be placed in bin
INCLUDE=	# include files to be placed in include
#PF=mwap.pf		# parameter files to be placed in data/pf

MAN1=		# man pages for commands
#MAN3=		# man pages for library routines
#MAN5=		# man pages for file formats

CLEAN= 		# Extra files which should be removed during a "make clean"

#cflags= -DSP5		# -g or -O is the most common option here and in fflags
fflags= 	# 
ldflags= 	# flags to linker/loader
ldlibs=		 -lmultiwavelet -xlic_lib=sunperf $(DBLIBS) $(TRLIBS) -lxtra $(GPLLIBS) -lgenloc -ltt -lstock -ldl -lm 

include $(ANTELOPEMAKE)  	# This line must appear following the definitions above, 
			# and before the DIRS definition below..
DIRS=			# Omit this line to run make in subdirectories, or
			# fill in to run make only in certain subdirectories

OBJS=	mwapbavg.o dbarrival_view.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
