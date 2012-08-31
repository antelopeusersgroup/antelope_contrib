

BIN=		dbmapevents
MAN1=		dbmapevents.1

ldlibs=		-lev -lscv2 -ldbl2 -lgrx -lmap $(X11LIBS) $(TRLIBS) 

include $(ANTELOPEMAKE)  	

DIRS=		

OBJS=	dbmapevents.F edprojmap.o
$(BIN) : $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

# $Id$ 
