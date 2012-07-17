
#include <stdio.h>
#include <X11/Xlib.h>

#define	MAX_PIXELS	10000
#define	MAX_POINTS	20000

#define	QPL_SCALE_FIXED	0
#define	QPL_SCALE_AUTO	1
#define	QPL_SCALE_AUTO0	2

#define	REFRESH_TIME	5

XPoint xps[MAX_POINTS];
float  xpl[MAX_POINTS];
float  ypl[MAX_POINTS];

typedef struct qpl_ {
	int npixels;
	int np;
	int istart;
	float *min;
	float *max;
	float *in;
	float *out;
	int *numb;
	int overlap;
	int noverlaps;
	struct qpl_ *overlaps;
} QPlot;

QPlot *
qpbin (ts, dt, nsamp, data, tmin, twin, npixels, qpl)

double ts;
double     dt;
int            nsamp;
float *               data;
double                      tmin;
double                            twin;
int                                     npixels;
QPlot *                                          qpl;

{
	double pdt, samp, dt2, dat, fsamp;
	int j0, j1, i, j, np, npix, ist, i2;
	QPlot *qov;
	QPlot *qpls;

	if (tmin > ts + dt * (nsamp - 1)) {
		if (qpl == NULL) return (NULL);
		else return (qpl);
	}
	if (tmin+twin < ts) {
		if (qpl == NULL) return (NULL);
		else return (qpl);
	}
	if (qpl == NULL) {
		qpls = (QPlot *) malloc (sizeof(QPlot));
		if (qpls == NULL) {
			fprintf (stderr, "qpbin: Malloc error.\n");
			return (NULL);
		}
		qpls->npixels = 0;
		qpls->min = NULL;
		qpls->max = NULL;
		qpls->in = NULL;
		qpls->out = NULL;
		qpls->numb = NULL;
		qpls->overlap = 0;
		qpls->noverlaps = 0;
		qpls->overlaps = NULL;
		qpls->min = (float *) malloc (npixels*sizeof(float));
		if (qpls->min == NULL) {
			fprintf (stderr, "qpbin: Malloc error.\n");
			qpfree (qpls);
			return (NULL);
		}
		qpls->max = (float *) malloc (npixels*sizeof(float));
		if (qpls->max == NULL) {
			fprintf (stderr, "qpbin: Malloc error.\n");
			qpfree (qpls);
			return (NULL);
		}
		qpls->in = (float *) malloc (npixels*sizeof(float));
		if (qpls->in == NULL) {
			fprintf (stderr, "qpbin: Malloc error.\n");
			qpfree (qpls);
			return (NULL);
		}
		qpls->out = (float *) malloc (npixels*sizeof(float));
		if (qpls->out == NULL) {
			fprintf (stderr, "qpbin: Malloc error.\n");
			qpfree (qpls);
			return (NULL);
		}
		qpls->numb = (int *) malloc (npixels*sizeof(int));
		if (qpls->numb == NULL) {
			fprintf (stderr, "qpbin: Malloc error.\n");
			qpfree (qpls);
			return (NULL);
		}
		for (i=0; i<npixels; i++) {
			qpls->min[i] = 0.0;
			qpls->max[i] = 0.0;
			qpls->in[i] = 0.0;
			qpls->out[i] = 0.0;
			qpls->numb[i] = -1;
		}
		qpls->npixels = npixels;
		qpls->np = 0;
		qpls->istart = 0;
	} else {
		qpls = qpl;
	}
	pdt = twin / (double) npixels;

	/* Look for overlap first. */

	samp = (tmin-ts) / dt;
	dt2 = pdt / dt;
	samp -= dt2;
	j0 = samp;
	if (samp < 0.0) j0--;
	j0++;
	samp += dt2;
	j1 = samp;
	np = 0;
	npix = 0;
	ist = -1;
	for (i=0; i<npixels; i++, samp += dt2, j0 = j1 + 1, j1 = samp) {
		if (samp < 0.0) j1--;
		if (j1 < 0) continue;
		if (j0 < 0) j0 = 0;
		if (j0 > nsamp-1) break;
		if (j1 > nsamp-1) j1 = nsamp - 1;
		if (qpls->numb[i] > -1) np++;
		npix++;
		if (ist < 0) ist = i;
	}
	if (np > 1) {
		if (qpls->overlaps == NULL) {
			qpls->overlaps = (QPlot *) malloc (sizeof(QPlot));
			if (qpls->overlaps == NULL) {
				fprintf (stderr, "qpbin: Malloc error.\n");
				return (NULL);
			}
			qpls->noverlaps = 1;
			qov = qpls->overlaps;
		} else {
			(qpls->noverlaps)++;
			qpls->overlaps = (QPlot *) realloc (qpls->overlaps, qpls->noverlaps*sizeof(QPlot));
			if (qpls->overlaps == NULL) {
				fprintf (stderr, "qpbin: Realloc error.\n");
				qpls->noverlaps = 0;
				return (NULL);
			}
			qov = &(qpls->overlaps[qpls->noverlaps-1]);
		}
		qov->npixels = npix;
		qov->np = 0;
		qov->istart = ist;
		qov->min = (float *) malloc (npix*sizeof(float));
		qov->max = (float *) malloc (npix*sizeof(float));
		qov->in = (float *) malloc (npix*sizeof(float));
		qov->out = (float *) malloc (npix*sizeof(float));
		qov->numb = (int *) malloc (npix*sizeof(int));
		if (qov->min == NULL || qov->max == NULL || qov->in == NULL || qov->out == NULL
								|| qov->numb == NULL) {
			fprintf (stderr, "qpbin: Malloc error.\n");
			qpfreeoverlaps (qpls);
			return (NULL);
		}
		qov->overlap = 1;
		qov->noverlaps = 0;
		qov->overlaps = NULL;
		for (i=0; i<npix; i++) {
			qov->min[i] = 0.0;
			qov->max[i] = 0.0;
			qov->in[i] = 0.0;
			qov->out[i] = 0.0;
			qov->numb[i] = -1;
		}
		samp = (tmin-ts) / dt;
		dt2 = pdt / dt;
		samp -= dt2;
		j0 = samp;
		if (samp < 0.0) j0--;
		j0++;
		samp += dt2;
		j1 = samp;
		np = 0;
		for (i=0; i<npixels; i++, samp += dt2, j0 = j1 + 1, j1 = samp) {
			if (samp < 0.0) j1--;
			if (j1 < 0) continue;
			if (j0 < 0) j0 = 0;
			if (j0 > nsamp-1) break;
			if (j1 > nsamp-1) j1 = nsamp - 1;
			i2 = i - ist;
			if (j1 >= j0) {
				qov->numb[i2] = 1;
				qov->min[i2] = data[j0];
				qov->max[i2] = data[j0];
				qov->in[i2] = data[j0];
				qov->out[i2] = data[j1];
				np++;
			} else {
				if (i == 0 || i == npixels-1) {
					fsamp = samp - ((int)samp);
					dat = data[j1] + (data[j0]-data[j1])*fsamp;
					qov->numb[i2] = 1;
					qov->min[i2] = dat;
					qov->max[i2] = dat;
					qov->in[i2] = dat;
					qov->out[i2] = dat;
					np++;
				} else {
					qov->numb[i2] = 0;
					qov->min[i2] = 0.0;
					qov->max[i2] = 0.0;
					qov->in[i2] = 0.0;
					qov->out[i2] = 0.0;
				}
			}
			for (j=j0+1; j<=j1; j++) {
				if (data[j] > qov->max[i2]) qov->max[i2] = data[j];
				if (data[j] < qov->min[i2]) qov->min[i2] = data[j];
				qov->numb[i2]++;
				np++;
			}
		}
		qov->np += np;
		return (qpls);
	}

	/* Now bin out segment. */

	samp = (tmin-ts) / dt;
	dt2 = pdt / dt;
	samp -= dt2;
	j0 = samp;
	if (samp < 0.0) j0--;
	j0++;
	samp += dt2;
	j1 = samp;
	np = 0;
	for (i=0; i<npixels; i++, samp += dt2, j0 = j1 + 1, j1 = samp) {
		if (samp < 0.0) j1--;
		if (j1 < 0) continue;
		if (j0 < 0) j0 = 0;
		if (j0 > nsamp-1) break;
		if (j1 > nsamp-1) j1 = nsamp - 1;
		if (j1 >= j0) {
			qpls->numb[i] = 1;
			qpls->min[i] = data[j0];
			qpls->max[i] = data[j0];
			qpls->in[i] = data[j0];
			qpls->out[i] = data[j1];
			np++;
		} else {
			if (i == 0 || i == npixels-1) {
				fsamp = samp - ((int)samp);
				dat = data[j1] + (data[j0]-data[j1])*fsamp;
				qpls->numb[i] = 1;
				qpls->min[i] = dat;
				qpls->max[i] = dat;
				qpls->in[i] = dat;
				qpls->out[i] = dat;
				np++;
			} else {
				qpls->numb[i] = 0;
				qpls->min[i] = 0.0;
				qpls->max[i] = 0.0;
				qpls->in[i] = 0.0;
				qpls->out[i] = 0.0;
			}
		}
		for (j=j0+1; j<=j1; j++) {
			if (data[j] > qpls->max[i]) qpls->max[i] = data[j];
			if (data[j] < qpls->min[i]) qpls->min[i] = data[j];
			qpls->numb[i]++;
			np++;
		}
	}
	qpls->np += np;
	return (qpls);
}

int
qpfree (qpl)

QPlot *         qpl;

{
	if (!qpl) return;
	qpfreeoverlaps (qpl);
	if (qpl->min) free (qpl->min);
	if (qpl->max) free (qpl->max);
	if (qpl->in) free (qpl->in);
	if (qpl->out) free (qpl->out);
	if (qpl->numb) free (qpl->numb);
	free (qpl);
}

int
qpfreeoverlaps (qpl)

QPlot *         qpl;

{
	int i;

	if (!qpl) return;
	if (!qpl->overlaps) return;
	for (i=0; i<qpl->noverlaps; i++) {
		if (qpl->overlaps[i].min) free (qpl->overlaps[i].min);
		if (qpl->overlaps[i].max) free (qpl->overlaps[i].max);
		if (qpl->overlaps[i].in) free (qpl->overlaps[i].in);
		if (qpl->overlaps[i].out) free (qpl->overlaps[i].out);
		if (qpl->overlaps[i].numb) free (qpl->overlaps[i].numb);
	}
	free (qpl->overlaps);
	qpl->overlaps = NULL;
	qpl->noverlaps = 0;
}

int
qplot (ts, dt, nsamp, data, tmin, twin, npixels, x, y, h, ybot, ytop, 
       scale_type, ygain, xp)

double ts;
double     dt;
int            nsamp;
float *               data;
double                      tmin;
double                            twin;
int                                     npixels;
int                                              x;
int                                                 y;
int                                                    h;
float *                                                   ybot;
float *                                                         ytop;
int    scale_type;
float              ygain;
XPoint **                 xp;

{
	float *min, *max, *in, *out;
	int *numb;
	QPlot *qpl;
	int i, n;
	int dimintot, dimaxtot;
	short dsmintot, dsmaxtot;
	float dfmintot, dfmaxtot;
	double ddmintot, ddmaxtot;
	float center, ytp;
	float scale;

	QPlot *qpbin();

	qpl = NULL;
	qpl = qpbin (ts, dt, nsamp, data, tmin, twin, npixels, qpl);
	if (qpl == NULL) return (0);
	n = qpl->np;
	if (n < 1) {
		qpfree (qpl);
		return (0);
	}
	min = qpl->min;
	max = qpl->max;
	in = qpl->in;
	out = qpl->out;
	numb = qpl->numb;
	switch (scale_type) {
	case QPL_SCALE_FIXED:
		for (i = 0, n = 0; i<npixels; i++) {
			if (numb[i] == 1) {
				n++;
			} else if (numb[i] > 1) {
				n += 4;
			}
		}
		if (n == 0) {
			qpfree (qpl);
			return (0);
		}
		scale = (float) h / (*ybot - *ytop);
		break;
	case QPL_SCALE_AUTO:
	case QPL_SCALE_AUTO0:
		for (i = 0, n = 0; i<npixels; i++) {
			if (numb[i] == 1) {
				if (n == 0) {
					dfmintot = min[i];
					dfmaxtot = max[i];
				} else {
					if (min[i] < dfmintot) 
						dfmintot = min[i];
					if (max[i] > dfmaxtot) 
						dfmaxtot = max[i];
				}
				n++;
			} else if (numb[i] > 1) {
				if (n == 0) {
					dfmintot = min[i];
					dfmaxtot = max[i];
				} else {
					if (min[i] < dfmintot) 
						dfmintot = min[i];
					if (max[i] > dfmaxtot) 
						dfmaxtot = max[i];
				}
				n += 4;
			}
		}
		if (n == 0) {
			qpfree (qpl);
			return (0);
		}
		if (scale_type == QPL_SCALE_AUTO0) {
			if (dfmaxtot < 0) dfmaxtot = -dfmaxtot;
			if (dfmintot < 0) dfmintot = -dfmintot;
			if (dfmintot > dfmaxtot) dfmaxtot = dfmintot;
			*ytop = dfmaxtot;
			*ybot = -dfmaxtot;
		} else {
			*ytop = dfmaxtot;
			*ybot = dfmintot;
		}
		if (*ybot == *ytop) {
			*ytop += 0.5;
			*ybot -= 0.5;
		}
		scale = (float) h / (*ybot - *ytop);
		break;
	default:
		qpfree (qpl);
		return (0);
	}
	if (n > MAX_POINTS) {
		fprintf (stderr, "qplot: Attempt to exceed MAX_POINTS.\n");
		qpfree (qpl);
		return (0);
	}
	n = 0;
	center = (*ybot + *ytop) * 0.5;
	ytp = center - (center - *ytop) / ygain;
	scale *= ygain;
	for (i=0; i<npixels; i++) {
		if (numb[i] == 1) {
			xps[n].x = (short) (i + x);
			xps[n].y = (short) (scale*(in[i]-ytp) + y);
			n++;
		} else if (numb[i] > 1) {
			xps[n].x = (short) (i + x);
			xps[n].y = (short) (scale*(in[i]-ytp) + y);
			n++;
			xps[n].x = xps[n-1].x;
			xps[n].y = (short) (scale*(min[i]-ytp) + y);
			if (xps[n].y != xps[n-1].y) n++;
			xps[n].x = xps[n-1].x;
			xps[n].y = (short) (scale*(max[i]-ytp) + y);
			if (xps[n].y != xps[n-1].y) n++;
			xps[n].x = xps[n-1].x;
			xps[n].y = (short) (scale*(out[i]-ytp) + y);
			if (xps[n].y != xps[n-1].y) n++;
		}
	}
	*xp = xps;
	qpfree (qpl);
	return (n);
}

int
qplotsegs (qpl, x, y, h, ybot, ytop, scale_type, ygain, display, drawable, gc, gcov)

QPlot *    qpl;
int             x;
int                y;
int                   h;
float *                  ybot;
float *                        ytop;
int                                  scale_type;
float                                            ygain;
Display *                                               display;
Drawable                                                         drawable;
GC *                                                                       gc;
GC *                                                                           gcov;

{
	float *min, *max, *in, *out;
	int *numb;
	int npixels;
	int i, j, n, ntot;
	int dimintot, dimaxtot;
	short dsmintot, dsmaxtot;
	float dfmintot, dfmaxtot;
	double ddmintot, ddmaxtot;
	float center, ytp;
	float scale;
	int iepoch, iepoch0;
	GC *gcl;

	if (qpl == NULL) return (0);
	n = qpl->np;
	if (n < 1) return (0);
	npixels = qpl->npixels;
	if (npixels < 2) return (0);
	if (!qpl->overlap) {
		gcl = gc;
	} else {
		gcl = gcov;
	}
	min = qpl->min;
	max = qpl->max;
	in = qpl->in;
	out = qpl->out;
	numb = qpl->numb;
	switch (scale_type) {
	case QPL_SCALE_FIXED:
		for (i = 0, n = 0; i<npixels; i++) {
			if (numb[i] == 1) {
				n++;
			} else if (numb[i] > 1) {
				n += 4;
			}
		}
		if (n == 0) return (0);
		scale = (float) h / (*ybot - *ytop);
		break;
	case QPL_SCALE_AUTO:
	case QPL_SCALE_AUTO0:
		for (i = 0,n = 0; i<npixels; i++) {
			if (numb[i] == 1) {
				if (n == 0) {
					dfmintot = min[i];
					dfmaxtot = max[i];
				} else {
					if (min[i] < dfmintot) 
						dfmintot = min[i];
					if (max[i] > dfmaxtot) 
						dfmaxtot = max[i];
				}
				n++;
			} else if (numb[i] > 1) {
				if (n == 0) {
					dfmintot = min[i];
					dfmaxtot = max[i];
				} else {
					if (min[i] < dfmintot) 
						dfmintot = min[i];
					if (max[i] > dfmaxtot) 
						dfmaxtot = max[i];
				}
				n += 4;
			}
		}
		for (j=0; j<qpl->noverlaps; j++) {
			for (i = 0; i<qpl->overlaps[j].npixels; i++) {
				if (qpl->overlaps[j].min[i] < dfmintot) 
					dfmintot = qpl->overlaps[j].min[i];
				if (qpl->overlaps[j].max[i] > dfmaxtot) 
					dfmaxtot = qpl->overlaps[j].max[i];
			}
		}
		if (n == 0) return 0;
		if (scale_type == QPL_SCALE_AUTO0) {
			if (dfmaxtot < 0) dfmaxtot = -dfmaxtot;
			if (dfmintot < 0) dfmintot = -dfmintot;
			if (dfmintot > dfmaxtot) dfmaxtot = dfmintot;
			*ytop = dfmaxtot;
			*ybot = -dfmaxtot;
		} else {
			*ytop = dfmaxtot;
			*ybot = dfmintot;
		}
		if (*ybot == *ytop) {
			*ytop += 0.5;
			*ybot -= 0.5;
		}
		scale = (float) h / (*ybot - *ytop);
		break;
	default:
		return (0);
	}
	if (n > MAX_POINTS) {
		fprintf (stderr, "qplotsegs: Attempt to exceed MAX_POINTS.\n");
		return (0);
	}
	n = 0;
	ntot = 0;
	center = (*ybot + *ytop) * 0.5;
	ytp = center - (center - *ytop) / ygain;
	scale *= ygain;
	x += qpl->istart;
	iepoch0 = time(NULL);
	for (i=0; i<npixels; i++) {
		if (numb[i] == 1) {
			xps[n].x = (short) (i + x);
			xps[n].y = (short) (scale*(in[i]-ytp) + y);
			n++;
		} else if (numb[i] > 1) {
			xps[n].x = (short) (i + x);
			xps[n].y = (short) (scale*(in[i]-ytp) + y);
			n++;
			xps[n].x = xps[n-1].x;
			xps[n].y = (short) (scale*(min[i]-ytp) + y);
			if (xps[n].y != xps[n-1].y) n++;
			xps[n].x = xps[n-1].x;
			xps[n].y = (short) (scale*(max[i]-ytp) + y);
			if (xps[n].y != xps[n-1].y) n++;
			xps[n].x = xps[n-1].x;
			xps[n].y = (short) (scale*(out[i]-ytp) + y);
			if (xps[n].y != xps[n-1].y) n++;
		} else if (numb[i] < 0) {
			if (n > 0) {
				ntot += n;
				if (n > 1) XDrawLines (display, drawable, gcl, 
							xps, n,CoordModeOrigin);
				n = 0;
			}
		}
		iepoch = time(NULL);
	}
	if (n > 0) {
		ntot += n;
		if (n > 1) XDrawLines (display, drawable, gcl, xps, 
							n, CoordModeOrigin);
	}
	for (i=0; i<qpl->noverlaps; i++) {
		ntot += qplotsegs (&(qpl->overlaps[i]), x, y, h, ybot, ytop, QPL_SCALE_FIXED, 
							ygain, display, drawable, gc, gcov);
	}
	return (ntot);
}

int
nqplotsegs (qpl, xdim, ydim, xlow, ylow, ybot, ytop, scale_type, iclip, ygain)

QPlot *    qpl;
float            xdim;
float                  ydim;
float                        xlow;
float                              ylow;
float *                                  ybot;
float *                                        ytop;
int                                                  scale_type;
int                                                              iclip;
float                                                                   ygain;

{
	float *min, *max, *in, *out;
	int *numb;
	int npixels;
	int i, j, n, ntot;
	int dimintot, dimaxtot;
	short dsmintot, dsmaxtot;
	float dfmintot, dfmaxtot;
	double ddmintot, ddmaxtot;
	float center;
	int igraf = 0;
	int ithick = 0;
	float thick = 0.0;
	static char asymb[] = " ";
	static float xmin, xmax, ymin, ymax;
	static float xdm, ydm, xlw, ylw;
	static int nnpixels;

	if (qpl == NULL) return (0);
	n = qpl->np;
	if (n < 1) return (0);
	npixels = qpl->npixels;
	if (npixels < 2) return (0);
	if (!qpl->overlap) nnpixels = npixels;
	min = qpl->min;
	max = qpl->max;
	in = qpl->in;
	out = qpl->out;
	numb = qpl->numb;
	switch (scale_type) {
	case QPL_SCALE_FIXED:
		for (i = 0, n = 0; i<npixels; i++) {
			if (numb[i] == 1) {
				n++;
			} else if (numb[i] > 1) {
				n += 4;
			}
		}
		if (n == 0) return (0);
		break;
	case QPL_SCALE_AUTO:
	case QPL_SCALE_AUTO0:
		for (i = 0,n = 0; i<npixels; i++) {
			if (numb[i] == 1) {
				if (n == 0) {
					dfmintot = min[i];
					dfmaxtot = max[i];
				} else {
					if (min[i] < dfmintot) 
						dfmintot = min[i];
					if (max[i] > dfmaxtot) 
						dfmaxtot = max[i];
				}
				n++;
			} else if (numb[i] > 1) {
				if (n == 0) {
					dfmintot = min[i];
					dfmaxtot = max[i];
				} else {
					if (min[i] < dfmintot) 
						dfmintot = min[i];
					if (max[i] > dfmaxtot) 
						dfmaxtot = max[i];
				}
				n += 4;
			}
		}
		for (j=0; j<qpl->noverlaps; j++) {
			for (i = 0; i<qpl->overlaps[j].npixels; i++) {
				if (qpl->overlaps[j].min[i] < dfmintot) 
					dfmintot = qpl->overlaps[j].min[i];
				if (qpl->overlaps[j].max[i] > dfmaxtot) 
					dfmaxtot = qpl->overlaps[j].max[i];
			}
		}
		if (n == 0) return 0;
		if (scale_type == QPL_SCALE_AUTO0) {
			if (dfmaxtot < 0) dfmaxtot = -dfmaxtot;
			if (dfmintot < 0) dfmintot = -dfmintot;
			if (dfmintot > dfmaxtot) dfmaxtot = dfmintot;
			*ytop = dfmaxtot;
			*ybot = -dfmaxtot;
		} else {
			*ytop = dfmaxtot;
			*ybot = dfmintot;
		}
		if (*ybot == *ytop) {
			*ytop += 0.5;
			*ybot -= 0.5;
		}
		break;
	default:
		return (0);
	}
	if (n > MAX_POINTS) {
		fprintf (stderr, "qplotsegs: Attempt to exceed MAX_POINTS.\n");
		return (0);
	}
	n = 0;
	ntot = 0;
	center = (*ybot + *ytop) * 0.5;
	ymax = center + (*ytop - center) / ygain;
	ymin = center - (center - *ybot) / ygain;
	xmin = 0.0;
	xmax = nnpixels-1;
	xdm = xdim;
	ydm = ydim;
	xlw = xlow;
	ylw = ylow;
	setdim_ (&xdm, &ydm, &xlw, &ylw);
	setscl_ (&xmin, &xmax, &ymin, &ymax);
	for (i=0; i<npixels; i++) {
		if (numb[i] == 1) {
			xpl[n] = (i + qpl->istart);
			ypl[n] = in[i];
			n++;
		} else if (numb[i] > 1) {
			xpl[n] = (i + qpl->istart);
			ypl[n] = in[i];
			n++;
			xpl[n] = xpl[n-1];
			ypl[n] = min[i];
			if (ypl[n] != ypl[n-1]) n++;
			xpl[n] = xpl[n-1];
			ypl[n] = max[i];
			if (ypl[n] != ypl[n-1]) n++;
			xpl[n] = xpl[n-1];
			ypl[n] = out[i];
			if (ypl[n] != ypl[n-1]) n++;
		} else if (numb[i] < 0) {
			if (n > 0) {
				ntot += n;
				if (n > 1) {
					nplot_ (&n, xpl, ypl, &igraf, &iclip, &thick, &ithick,
								asymb, strlen(asymb));
				}
				n = 0;
			}
		}
	}
	if (n > 0) {
		ntot += n;
		if (n > 1) {
			nplot_ (&n, xpl, ypl, &igraf, &iclip, &thick, &ithick,
								asymb, strlen(asymb));
		}
	}
	for (i=0; i<qpl->noverlaps; i++) {
		ntot +=  nqplotsegs (&(qpl->overlaps[i]), xdim, ydim, xlow, ylow, ybot, ytop, QPL_SCALE_FIXED, iclip, ygain);
	}
	return (ntot);
}

/* $Id$ */
