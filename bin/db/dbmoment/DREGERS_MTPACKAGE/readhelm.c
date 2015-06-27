/* Routine to read Helmberger Format Seismograms and to return */
/* information about number of traces, number of time points,  */
/* dt, and the data vector                                     */
#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <stdlib.h>

#define FORM1a "%8d\n"
#define FORM1b "%8d      %s\n"
#define FORM2  "%s\n"
#define FORM3  "    %11.4e    %11.4e      0  0  0.00\n"
#define FORM4a "%8d  %8.5f %11.4e\n"
#define TRUE 1
#define FALSE 0

char form[32]     = "(6e12.5)";
FILE *fopen(),*inf,*of;
int fclose();

readhelm(in,N,NT,DT,vec1)
char *in;
int *N, *NT;
float *DT;
float *vec1;
{
   int nt, orignt, nx,nxx, perline, i, j, cnt, left, f_width, ss;
   int nn, ntemp, test=0;
   float BA, dt, mul=0.0, *pv, *vec, *p, dd, tt, mxtt=0.0;
   float pi=3.14159265;
   char c_form[512],c_form1[128],c_form2[128];
   char line[128];
   char out[50];

   inf=fopen(in,"r");

   /* read initial header */
   fgets(line,100,inf);
   sscanf(line,"%d",&nxx);
   *N=nxx; /*send number of traces*/
   fgets(form,100,inf);
   perline = chkform(form,c_form1,c_form2,&f_width);
   /*fprintf(stderr,"perline=%d  form=%s\n",perline,form);*/

   nx=nxx;
   while (nx) {
   fgets(line,100,inf);
   sscanf(line,"%f %f",&dd,&tt);
   fgets(line,100,inf);
   sscanf(line,"%d %f",&nt,&dt);
   *NT++ = nt;
   *DT++ = dt;
/*   fprintf(stderr,"nt=%d dt=%f\n",nt,dt);*/

   if(nx == nxx)
     {
     orignt = nt;
     vec = (float *) malloc(4*nt);
     p=vec1;
     }
     /* Zero out so variable length and dt's are read
   else
     {
     if (nt != orignt) {
	fprintf (stderr, "nt is not the same throughout\n");
	exit(1);
     }
     }
     */
   cnt  = nt / perline;
   left = nt % perline;
   pv = vec;
   for (i=0;i<cnt;i++)
      {
      for (j=0;j<perline;j++)
	ss=fscanf(inf,c_form2,pv++);

      fgets(line,100,inf);
      }

if ( left != 0)
   {
   for (j=0;j<left;j++)
     fscanf(inf,c_form2,pv++);
     fgets(line,100,inf);
   }

   pv = vec;
   for (j=0;j < nt;j++)
     *p++=*pv++;

   nx--;

   } /*end while */
   fclose(inf);
/*   fprintf(stderr,"Finished Read\n");*/
   }


   /*Subroutines*/
char x_form[64];

int
chkform(f_form,c_form1,c_form2,f_width)
char *f_form, *c_form1, *c_form2;
int *f_width;
{
   int i, cnt, not_done, found_num, num, con1, con2;
   char *px_form, *pf_form, *pn;

   px_form = x_form;
   *f_width = 0;

   for(i=0;i<32;i++) if (f_form[i] == '(') break;
   if (i == 32) {
      fprintf(stderr,"readhelm:  Bad format, no '(', continuing...\n");
      *px_form++ = '(';
      i = 0;
   }
   else {
      *px_form++ = f_form[i];
      i++;
   }
   pf_form = &(f_form[i]);
   cnt = 32 - i;
   not_done = TRUE;
   for (i=0;i<cnt && not_done;i++) {
      
      if (isspace(*pf_form)) {
         pf_form++;
         fprintf(stderr,"readhelm:  Avoid blanks in format\n");
      }

      else if (isdigit(*pf_form)) {
         pn = px_form;
         *px_form++ = *pf_form++;
         while(isdigit(*pf_form)) {
            *px_form++ = *pf_form++;
            i++;
         }
         *px_form = '\0';
         if ((found_num = sscanf(pn,"%d",&num)) != 1) {
            fprintf(stderr,"readhelm:  unknown format error\n");
            exit(-1);
         }
      }

      else switch (*pf_form) {
         case 'e':
         case 'E':
            *px_form++ = *pf_form++;
            if ((sscanf(pf_form,"%d.%d",&con1,&con2)) != 2)
            {
               fprintf(stderr,"readhelm:  e-format error\n");
               exit(-1);
            }
            if (con1 < con2 + 6)
            {
               fprintf(stderr,"readhelm:  e-format error\n");
               exit(-1);
            }
            if (con1 == con2 + 6)
            {
               fprintf(stderr,"readhelm:  e-format may be unreadable, continuing...\n");
            }
            sprintf(c_form1,"%%%d.%de\0",con1,con2);
            sprintf(c_form2,"%%%df\0",con1);
            sprintf(px_form,"%d.%d)\0",con1,con2);
            not_done = FALSE;
            break;
         
         case 'f':
         case 'F':
            *px_form++ = *pf_form++;
            if ((sscanf(pf_form,"%d.%d",&con1,&con2)) != 2)
            {
               fprintf(stderr,"readhelm:  f-format error\n");
               exit(-1);
            }
            if (con1 < con2 + 2)
            {
               fprintf(stderr,"readhelm:  f-format error\n");
               exit(-1);
            }
            if (con1 == con2 + 2)
            {
               fprintf(stderr,"readhelm:  f-format may be unreadable, continuing...\n");
            }
            sprintf(c_form1,"%%%d.%df\0",con1,con2);
            sprintf(c_form2,"%%%df\0",con1);
            sprintf(px_form,"%d.%d)\0",con1,con2);
	    *f_width = con1;
            not_done = FALSE;
            break;

         case 'g':
         case 'G':
            *px_form++ = *pf_form++;
            if ((sscanf(pf_form,"%d.%d",&con1,&con2)) != 2)
            {
               fprintf(stderr,"readhelm:  g-format error\n");
               exit(-1);
            }
            if (con1 < con2 + 2)
            {
               fprintf(stderr,"readhelm:  g-format error\n");
               exit(-1);
            }
            if (con1 == con2 + 2)
            {
               fprintf(stderr,"readhelm:  g-format may be unreadable, continuing...\n");
            }
            sprintf(c_form1,"%%%d.%dg\0",con1,con2);
            sprintf(c_form2,"%%%df\0",con1);
            sprintf(px_form,"%d.%d)\0",con1,con2);
            not_done = FALSE;
            break;
         
         default:
            if (found_num) {
               fprintf(stderr,"readhelm:  unknown format error\n");
               exit(-1);
            }
            fprintf(stderr,"readhelm:  Bad characters in format continuing...\n");
            pf_form++;
            i++;
            break;
         }
   }
   not_done = FALSE;
   for(i=0;i<32;i++) {
      if (f_form[i] == ')') break;
      if (f_form[i] == '\0') not_done = TRUE;
   }
   if (i == 32 || not_done)
      fprintf(stderr,"readhelm:  Bad format, no ')', continuing...\n");
   for (i=0;i<32;i++) f_form[i] = x_form[i];
   if (found_num) return(num);
   return(1);
}
