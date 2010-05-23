: # use perl
eval 'exec perl -S $0 "$@"'
if 0;

$arch = "-arch x86_64";

print "Adding $arch\n";

$self->{LDDLFLAGS} = "$arch $Config{lddlflags}";

