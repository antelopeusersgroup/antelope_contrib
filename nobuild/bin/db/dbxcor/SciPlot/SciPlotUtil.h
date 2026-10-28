#if __STDC__ || defined(__cplusplus)
#define P_(s) s
#else
#define P_(s) ()
#endif

#ifdef __cplusplus
extern "C"
{
#endif


/* SciPlotUtil.c */
Widget SciPlotDialog P_((Widget parent, char *title));
void SciPlotDialogPopup P_((Widget w));
void SciPlotReadDataFile P_((Widget parent, FILE *fd));


#ifdef __cplusplus
}
#endif

#undef P_
