DATADIR = schemas/css3.1.ext

DATA = \
	adoption \
	comm \
	deployment \
	dlsite

CLEAN	= \
	comm \
	deployment \
	dlsite
	

SUBDIR=/contrib

include $(ANTELOPEMAKE)


comm : ../comm
	$(RM) comm 
	cp ../comm  .

deployment : ../deployment
	$(RM) deployment
	cp ../deployment  .

dlsite : ../dlsite
	$(RM) dlsite  
	cp ../dlsite  .

