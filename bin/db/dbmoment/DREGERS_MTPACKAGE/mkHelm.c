#include <stdio.h>
#include <ctype.h>

#define FORM1a "%8d\n"
#define FORM1b "%8d      %s\n"
#define FORM2  "%s\n"
#define FORM3  "    %11.4e    %11.4e      0  0  0.00\n"
#define FORM4a "%8d  %8.5f %11.4e\n"
#define TRUE 1
#define FALSE 0

char form[32]     = "(7e14.5)";
char comment1[80];

main(ac,av)
int ac; char **av;
{
   int nt, nx, ntr, comm1, perline, nread, i, j, l, cnt, left, f_width;
   float offset=0.0, t0=0.0, mul=0.0;
   float dt, *pv, *vec;
   char c_form[512];

   setpar(ac,av);
   mstpar("nt","d",&nt);
   mstpar("dt","f",&dt);
   if (getpar("ntr","d",&nx) == 0)
      getpar("nx","d",&nx);
   getpar("mul","f",&mul);
   if (getpar("format","s",form)) {
      fprintf(stderr,"mkHelm:  input format: %s\n",form);
      perline = chkform(form,c_form,&f_width);
      fprintf(stderr,"          format used: %s\n",form);
   }
   else perline = chkform(form,c_form,&f_width);
   comm1 = getpar("comment1","s",comment1);
   getpar("offset","f",&offset);
   getpar("starttime","f",&t0);
   endpar();
   ntr=nx;
   fprintf(stderr,"ntr=%d\n",ntr);

   vec = (float *) malloc(4*nt);


   /* write initial header */
   if (comm1) fprintf(stdout,FORM1b,nx,comment1);
   else       fprintf(stdout,FORM1a,nx);
   fprintf(stdout,FORM2,form);
   fprintf(stdout,FORM3,offset,t0);
   fprintf(stdout,FORM4a,nt,dt,mul);

   /* write data */
   for (l=0 ; l < ntr ; l++)
    {
    if ((nread = read(0,vec,4*nt)) != 4*nt)
      {
      fprintf(stderr,"mkHelm:  Read error, only %d bytes\n",nread);
      exit(-1);
      }
    cnt = nt / perline;
    left = nt % perline;
    pv = vec;
    for (i=0;i<cnt;i++)
      {
      for (j=0;j<perline;j++) fprintf(stdout,c_form,*pv++);
      putc('\n',stdout);
      }
    if(left)
    {
    for (j=0;j<left;j++) fprintf(stdout,c_form,*pv++);
    putc('\n',stdout);
    }

    if ( l != (ntr - 1))
      {
      fprintf(stdout,FORM3,offset,t0);
      fprintf(stdout,FORM4a,nt,dt,mul);
      }
    }
 
}

char x_form[64];

int
chkform(f_form,c_form,f_width)
char *f_form, *c_form;
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
            if (con1 < con2 + 7)
            {
               fprintf(stderr,"mkHelm:  e-format error\n");
               exit(-1);
            }
            sprintf(c_form,"%%%d.%de\0",con1,con2);
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
            if (con1 < con2 + 3)
            {
               fprintf(stderr,"mkHelm:  f-format error\n");
               exit(-1);
            }
            sprintf(c_form,"%%%d.%df\0",con1,con2);
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
            if (con1 < con2 + 3)
            {
               fprintf(stderr,"mkHelm:  g-format error\n");
               exit(-1);
            }
            sprintf(c_form,"%%%d.%dg\0",con1,con2);
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
