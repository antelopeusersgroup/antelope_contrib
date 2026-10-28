
# Build environment can be configured the following
# environment variables:
#   CC : Specify the C compiler to use
#   CFLAGS : Specify compiler options to use
#   LDFLAGS : Specify linker options to use
#   CPPFLAGS : Specify c-preprocessor options to use

# Required compiler parameters
CFLAGS += -I..

LDFLAGS = -L..
LDLIBS = -lslink

# For Windows w/ Unix-like build environments uncomment the following line
# This is needed for MinGW but not for Cygwin
#LDLIBS += -lslink -lws2_32

# For SunOS/Solaris uncomment the following line
#LDLIBS += -lslink -lsocket -lnsl -lrt

# Build all *.c source as independent programs
SRCS := $(sort $(wildcard *.c))
BINS := $(SRCS:%.c=%)

all: $(BINS)

# Build programs and check for executable
$(BINS) : % : %.c
	@printf 'Building $<\n';
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $< $(LDFLAGS) $(LDLIBS)

clean:
	rm -rf *.o $(BINS) *.dSYM

install:
	@echo
	@echo "No install target, copy the executable(s) as needed"
	@echo
