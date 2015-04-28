#include	<stdio.h>
/*
#include	"../../h/chars.h"
*/
#include	"chardata.h"

#define F (float)

/*
 * The characters in the Hershey font set have arbitrary width,
 * but the maximum height is 25 and the maximum depth is -7 for
 * a total height of 32.  Of this, a typical capital letter has
 * a height of 21.  Therefore, vertical spacing should always
 * be 32 in font units.  For other data: m is 30 wide, n is 19 wide.
 */

#define VSPACE 32
#define FONT_HGT 21
#define M_WID 30
#define N_WID 19
#define TabStop 8	/* tab is 8 spaces */

extern int textfont, textcenter;
extern float textsize, pixinch;
extern float textcosang, textsinang, xscale, yscale;

struct xypair { char x, y; } *xy_ptr;

/* #define DROPBIT 02000 */

struct IglFont *get_font();

text(xcur,ycur,str)
int xcur,ycur;
char str[];
/*
 * interpret characters into vectors
 */
{
	int add, drop;
	char c, xyw, *pstr;
	float size, hstepx, hstepy, vstepx, vstepy, xbase, ybase;
	float tabstepx, tabstepy;
	float wid, hgt, stroke_unit;
	int hshift, vshift, h, v, txorig, tyorig, xp, yp;
	int n, k;
	int sym_ch;
	short *sym_data, *sym_ptr;
	struct charglyph *sym_cg;
	struct IglFont *current_font;

	current_font = get_font(textfont);
	sym_cg   = current_font->cg;
	sym_data = current_font->data;
	if (textsize > 5.0) textsize /= 72.0; /* 72 points / inch */
	size = textsize * pixinch;
	stroke_unit = size / F FONT_HGT;

	if (textcenter) {
		wid = F 0.0;
		pstr = str;
		while ((sym_ch = *pstr++) != '\0')
			wid += F sym_cg[sym_ch - 040].width;
		hgt = size;
		hshift = F (textcenter & 0x3);
		xcur  -= F 0.5 * hshift * textcosang * wid * stroke_unit;
		ycur  -= F 0.5 * hshift * textsinang * wid * stroke_unit;
		vshift = F ((textcenter >>2) % 3);
		xcur  += F 0.5 * vshift * textsinang * size; 
		ycur  -= F 0.5 * vshift * textcosang * size;
	}


	txorig = xcur;
	tyorig = ycur;
	xbase  = (float) xcur;
	ybase  = (float) ycur;

	hstepx = N_WID * stroke_unit * textcosang;
	hstepy = N_WID * stroke_unit * textsinang;
	tabstepx = hstepx * TabStop;
	tabstepy = hstepy * TabStop;
	vstepx = VSPACE * stroke_unit * textsinang;
	vstepy = VSPACE * stroke_unit * textcosang;

	while ((sym_ch = *str++) != '\0') {
		if (sym_ch < 040) {
			switch (sym_ch) {	/* standard carriage controls */
				case '\b': /* back space */
					xcur -= hstepx;
					ycur -= hstepy;
					xbase -= hstepx;
					ybase -= hstepy;
					break;
				case '\t': /* tab */
					xbase = F ((int) (xcur / tabstepx + 1))
						* tabstepx;
					ybase = F ((int) (ycur / tabstepy + 1))
						* tabstepy;
					xcur  = (int) xbase;
					ycur  = (int) ybase;
					break;
				case '\r': /* carriage return */
				case '\n': /* newline */
					txorig += vstepx;
					tyorig += vstepy;
					xcur    = txorig;
					ycur    = tyorig;
					xbase   = (float) xcur;
					ybase   = (float) ycur;
					break;
				default: /* map to blank */
					c = ' ';
					break;
			}
			continue;
		}
		if (sym_ch == ' ') {
			hstepx = N_WID * stroke_unit * textcosang;
			hstepy = N_WID * stroke_unit * textsinang;
		}
		else {
			sym_ch -= 040;
			add = sym_cg[sym_ch].index;
			sym_ptr = sym_data + add;
			/*
			 * I'm commenting this out 'cause I don't know
			 * what it is.
			 * drop = (add & DROPBIT ? 2 : 0);
			 */
			/* Force first point to be a move */
			xy_ptr = (struct xypair *) sym_ptr++;
			h = (xy_ptr->x  >> 1) * stroke_unit; 
			v = xy_ptr->y * stroke_unit;
			xcur= (int) (xbase + h*textcosang - v*textsinang);
			ycur= (int) (ybase + h*textsinang + v*textcosang);
			for (k = 1; k < sym_cg[sym_ch].npoints; k++) {
				xy_ptr = (struct xypair *) sym_ptr++;
				h = (xy_ptr->x  >> 1) * stroke_unit; 
				v = xy_ptr->y * stroke_unit;
				/* v = (((xyw&07) - drop)*stroke_unit); */
				xp= (int) (xbase + h*textcosang - v*textsinang);
				yp= (int) (ybase + h*textsinang + v*textcosang);
				if ( !(xy_ptr->x & 0x1))
					do_line(xcur,ycur,xp,yp);
				xcur = xp; ycur = yp; 
			}
			hstepx = sym_cg[sym_ch].width * stroke_unit * textcosang;
			hstepy = sym_cg[sym_ch].width * stroke_unit * textsinang;
		}
		xbase += hstepx; /* move to starting point of next char */
		ybase += hstepy; 
		xcur= (int) xbase; 
		ycur= (int) ybase;
	}
}

struct IglFont *
get_font(font_id)
int font_id;
{
	static struct IglFont ret_font;

	/* default case is same as font_id=0 */
	if (Rs_cg == NULL || Rs_data == NULL) exit(-1);
	ret_font.cg   = Rs_cg;
	ret_font.data = Rs_data;

	switch (font_id) {

	case 1:
		if (Is_cg == NULL || Is_data == NULL) break;
		ret_font.cg   = Is_cg;
		ret_font.data = Is_data;
		break;

	case 2:
		if (Bs_cg == NULL || Bs_data == NULL) break;
		ret_font.cg   = Bs_cg;
		ret_font.data = Bs_data;
		break;

	case 3:
		if (Ss_cg == NULL || Ss_data == NULL) break;
		ret_font.cg   = Ss_cg;
		ret_font.data = Ss_data;
		break;

	case 4:
		if (Rs_cg == NULL || Rs_data == NULL) break;
		ret_font.cg   = Rc_cg;
		ret_font.data = Rc_data;
		break;

	case 5:
		if (Is_cg == NULL || Is_data == NULL) break;
		ret_font.cg   = Ic_cg;
		ret_font.data = Ic_data;
		break;

	case 6:
		if (Bs_cg == NULL || Bs_data == NULL) break;
		ret_font.cg   = Bc_cg;
		ret_font.data = Bc_data;
		break;

	case 7:
		if (Ss_cg == NULL || Ss_data == NULL) break;
		ret_font.cg   = Sc_cg;
		ret_font.data = Sc_data;
		break;

	case 8:
		if (Rs_cg == NULL || Rs_data == NULL) break;
		ret_font.cg   = Rx_cg;
		ret_font.data = Rx_data;
		break;

	case 9:
		if (Is_cg == NULL || Is_data == NULL) break;
		ret_font.cg   = Ix_cg;
		ret_font.data = Ix_data;
		break;

	case 10:
		if (Bs_cg == NULL || Bs_data == NULL) break;
		ret_font.cg   = Bx_cg;
		ret_font.data = Bx_data;
		break;

	case 11:
		if (Ss_cg == NULL || Ss_data == NULL) break;
		ret_font.cg   = Sx_cg;
		ret_font.data = Sx_data;
		break;
	}

	return(&ret_font);
}
