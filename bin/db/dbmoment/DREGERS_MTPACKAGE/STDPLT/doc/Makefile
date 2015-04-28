TROFF=	ptroff -t
ALL=	title toc intro user driver appA appB

what:	null
	echo SGP Documentation

title:	null
	-eqn param title | ${TROFF} -ms 

toc:	null
	-eqn param toc | ${TROFF} -ms 
	
intro:	null
	-eqn param intro | ${TROFF} -ms 

user:	null
	-eqn param user | ${TROFF} -ms 

driver:	null
	-eqn param driver | ${TROFF} -ms 

appA:	null
	-eqn param appA | ${TROFF} -ms 

appB:	null
	-eqn param appB | ${TROFF} -ms 

#
all:	 null
	-eqn param ${ALL} | ${TROFF} -ms 

null:
.SILENT:
