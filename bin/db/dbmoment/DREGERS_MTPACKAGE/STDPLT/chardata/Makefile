FinalFiles=roman1.final  italic.final  bold1.final   newmath.ascii \
	   roman2.final  italic.final  bold2.final   newmath.ascii \
	   italian.ascii script2.ascii english.ascii cyrillic.ascii

KeyNames=  Rs            Is            Bs             Ss \
           Rc            Ic            Bc             Sc \
           Rx            Ix            Bx             Sx

chardata.h: $(FinalFiles) char_compile
	/bin/rm chardata.h
	do_compiles chardata.h "$(KeyNames)" $(FinalFiles)
	\rm *.final char_compile.o char_compile

char_compile:  char_compile.c
	$(CC) $(CFLAGS) char_compile.c -o char_compile $(LDFLAGS)

roman1.final: roman1.ascii additional
	cat roman1.ascii additional > roman1.final

italic.final: italic1.ascii additional
	cat italic1.ascii additional > italic.final

bold1.final: roman2.ascii additional
	cat roman2.ascii additional > bold1.final

roman2.final: roman3.ascii additional
	cat roman3.ascii additional > roman2.final

bold2.final: roman4.ascii additional
	cat roman4.ascii additional > bold2.final

clean:
	rm chardata.h
