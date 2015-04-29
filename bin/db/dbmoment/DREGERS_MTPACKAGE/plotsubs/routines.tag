box		plot.c	/^box(x1,y1,x2,y2)$/
ubox		plot.c	/^ubox(x1,y1,x2,y2)$/

endplot		erase.c	/^endplot()$/

freebrush_	fortran.c	/^freebrush_ (pibrush)$/

freepen_	fortran.c	/^freepen_ (pipen)$/

getbrushcolor_	fortran.c	/^getbrushcolor_ ()$/

getbrushmode_	fortran.c	/^getbrushmode_ ()$/

getfillmode	table.c	/^getfillmode()$/

getpencolor_	fortran.c	/^getpencolor_ ()$/

getsymbolnames	symbol.c	/^struct symbolname *getsymbolnames(nsymbol)$/

plotlabel	text.c	/^plotlabel(fmt,ARGS)$/
labelplot_	fortran.c	/^labelplot_ (str,islen)$/

origin_		fortran.c	/^origin_ (px,py)$/

setpattern_	fortran.c	/^setpattern_ (pipat)$/

drawbox_	fortran.c	/^drawbox_ (px1,py1,px2,py2)$/
udrawbox_	fortran.c	/^udrawbox_ (px1,py1,px2,py2)$/

plotdot		dot.c	/^plotdot(xinch,yinch,size,imode)$/

uplotdot	dot.c	/^uplotdot(xuser,yuser,size,imode)$/
