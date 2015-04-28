#include <stdio.h>
#include <ctype.h>

#define TRUE 1
#define FALSE 0


main(ac,av)
int ac;
char **av;
{
   int nt, nx, perline, nto, i, j, cnt, left, f_width, ss, nmalloc;
   int nn, ntemp, test=0;
   int NTT=0,NT=0;            /*If NT>0 NT-nt zeros added at end of file*/
   float *ZERO;
   float fbase=0.02;
   float dt, mul, *pv, *vec, *p, *vec0, dd, tt, mxtt=0.0;
   char c_form1[128],c_form2[128];
   char line[128];
   char form[32]     = "(6e12.5)";

   setpar(ac,av);
   getpar("NT","d",&NT);
   endpar();
   fprintf(stderr,"NT=%d\n",NT);

   if(NT)
     {
     ZERO= (float *) malloc(4*NT);
     for(i=0 ; i < NT ; i++)
       ZERO[i]=0.0;
       }

   /* read initial header */
   fgets(line,128,stdin);
   sscanf(line,"%d",&nx);
   fgets(form,38,stdin);
   perline = chkform(form,c_form1,c_form2,&f_width);
   fprintf(stderr,"nx=%d perline=%d\n",nx,perline);

   while (nx--) {
   fgets(line,128,stdin);
   sscanf(line,"%f %f",&dd,&tt);
fprintf(stderr,"%f %f\n",dd,tt);
   fgets(line,128,stdin);
   sscanf(line,"%d %f",&nt,&dt);
fprintf(stderr,"%d %f\n",nt,dt);

   nmalloc = (nt > nto ? nt : nto);
   nmalloc += (int)(tt / dt);
   vec = (float *) malloc(4*nmalloc);




   cnt = nt / perline;
   left = nt % perline;
   pv = vec;
   for (i=0;i<cnt;i++) {
      for (j=0;j<perline;j++)
	ss=fscanf(stdin,c_form2,pv++);

	/*fixbase(vec,nt,fbase); To remove mean noise*/
      fgets(line,128,stdin);

   }
   switch(left)
    {
    case 0:
    {
    for (j=0;j<left;j++)
     {
     fscanf(stdin,c_form2,pv++);
     fgets(line,128,stdin);
     }
     break;
    }
    default: fgets(line,128,stdin);
    }

   nn = (int)(tt / dt);
fprintf(stderr,"nn=%d\n",nn);
   vec0 = (float *) malloc(4*nn);

   p=vec0;
   for (j=0 ; j < nn ; j++)
       *p++ = 0.0;

   /* write data */
   nto = 0;
   if (ac > 1) nto = atoi(av[1]);
   if (nto <= 0) nto = nt;
if(NT==0)
   {
   test=write(1,vec0,4*nn);
   fprintf(stderr,"%d bytes written\n",test);
   test=write(1,vec,4*(nto-nn));
   fprintf(stderr,"%d bytes written\n",test);
   }
if(NT < 0)
   {
   test=write(1,vec,4*(nto));
   fprintf(stderr,"%d bytes written\n",test);
   }
if(NT > 0)
   {
   test=write(1,vec0,4*nn);
   fprintf(stderr,"%d bytes written(LeadZeros)\n",test);
   if((NT-nto-nn) > 0)
      {
      test=write(1,vec,4*nto);
      fprintf(stderr,"%d bytes written(data)\n",test);
      test=write(1,ZERO,4*(NT-nto-nn));
      fprintf(stderr,"%d bytes written(PostZeros)\n",test);
      }
   if((NT-nto-nn) <= 0)
      {
      NTT=NT-nn;
      test=write(1,vec,4*NTT);
      fprintf(stderr,"%d bytes written(data)\n",test);
      }
    }
   free(vec);
   free(vec0);
   }
}

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
      fprintf(stderr,"mkHelm:  Bad format, no '(', continuing...\n");
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
         fprintf(stderr,"mkHelm:  Avoid blanks in format\n");
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
            fprintf(stderr,"mkHelm:  unknown format error\n");
            exit(-1);
         }
      }

      else switch (*pf_form) {
         case 'e':
         case 'E':
            *px_form++ = *pf_form++;
            if ((sscanf(pf_form,"%d.%d",&con1,&con2)) != 2)
            {
               fprintf(stderr,"mkHelm:  e-format error\n");
               exit(-1);
            }
            if (con1 < con2 + 6)
            {
               fprintf(stderr,"mkHelm:  e-format error\n");
               exit(-1);
            }
            if (con1 == con2 + 6)
            {
               fprintf(stderr,"mkHelm:  e-format may be unreadable, continuing...\n");
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
               fprintf(stderr,"mkHelm:  f-format error\n");
               exit(-1);
            }
            if (con1 < con2 + 2)
            {
               fprintf(stderr,"mkHelm:  f-format error\n");
               exit(-1);
            }
            if (con1 == con2 + 2)
            {
               fprintf(stderr,"mkHelm:  f-format may be unreadable, continuing...\n");
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
               fprintf(stderr,"mkHelm:  g-format error\n");
               exit(-1);
            }
            if (con1 < con2 + 2)
            {
               fprintf(stderr,"mkHelm:  g-format error\n");
               exit(-1);
            }
            if (con1 == con2 + 2)
            {
               fprintf(stderr,"mkHelm:  g-format may be unreadable, continuing...\n");
            }
            sprintf(c_form1,"%%%d.%dg\0",con1,con2);
            sprintf(c_form2,"%%%df\0",con1);
            sprintf(px_form,"%d.%d)\0",con1,con2);
            not_done = FALSE;
            break;
         
         default:
            if (found_num) {
               fprintf(stderr,"mkHelm:  unknown format error\n");
               exit(-1);
            }
            fprintf(stderr,"mkHelm:  Bad characters in format continuing...\n");
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
      fprintf(stderr,"mkHelm:  Bad format, no ')', continuing...\n");
   for (i=0;i<32;i++) f_form[i] = x_form[i];
   if (found_num) return(num);
   return(1);
}
