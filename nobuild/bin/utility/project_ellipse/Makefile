BIN= project_ellipse

MAN1= project_ellipse.1

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS= $(BIN).o

$(BIN) : $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
