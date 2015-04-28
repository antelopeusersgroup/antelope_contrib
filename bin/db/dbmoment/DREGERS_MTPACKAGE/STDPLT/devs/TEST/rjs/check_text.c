#include <stdio.h>

main()
{
	int i;

	settextsize(0.15);
	settextcenter(0,0);
	for (i=0; i<12; i++) {
	settextfont(i);
	text(0.2, 1.0 + 0.5 * (float) i,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%%^&*()-_=+`~[{]}\|;:,<.>/?'\"");
	}
}
