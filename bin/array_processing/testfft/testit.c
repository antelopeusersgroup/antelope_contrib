/*This program tests calling the C routine fft_sp.c or fft_dp.c by using it 
  to compute spectra and Hilbert transforms.

  Usage testit filename

  where filename specifies a file of ASCII data points, one per line.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MXPTS 1024

int main (int argc, char *argv[])
{
  FILE      *fp;
  float     *s,anum,amp,sig,env;
/*Test the single-precision fucntion or double-precision one by changing the
  comment line between the two below.  Change the Make file also.*/
/*  float     dum[MXPTS],dum1[MXPTS],dum2[MXPTS];*/
  double    dum[MXPTS],dum1[MXPTS],dum2[MXPTS];
  int       i,n,spts,len,isign;
  char      *filename;

  filename = malloc(20);
  s = malloc(MXPTS*sizeof(float));

  if (argc < 2)
  {
    printf("Usage: testit filename\n");
    return 1;
  }
  filename=argv[1];

  if ( (fp = fopen(filename,"r")) == NULL)
  {
    printf("could not open data file\n");
  }
  printf("opened data file\n");

  spts = 0;
  while(fscanf(fp,"%f",&anum) != EOF)
  {
    s[spts] = anum;
    spts++;
  }
  printf("# of time points = %d\n",spts);
  fclose(fp);

/*If series is not a power of two, determine next highest power of two.*/
  if((n=check2(spts))==0)
  {
    n=(int)(log10((double)spts)/log10(2.0))+1;
  }

/*Fill with zeros to power of two and then interlace zeroes to make it
  appear as a complex array.*/
  len = (int)pow(2.0,n);
  printf("len = %d\n",len);
  for(i=spts;i<len;i++)
    s[spts]=0.0;
  for(i=0;i<len;i++)
  {
    dum[i*2]  = s[i];
    dum[i*2+1]= 0.0;
    dum1[i] = dum[i*2];
  }

  isign = -1;
  fft(dum,len,isign);
/*Fourier amplitude is in the first half of array + 2 elements.*/
  for(i=0;i<=len;i++)
  {
    amp = sqrt(dum[i*2]*dum[i*2]+dum[i*2+1]*dum[i*2+1]);
    printf("i,amp = %4d %8.4f\n",i,amp);
  }

/*Now do inverse transform -- data should be nearly exactly the same as the
  original.*/

  isign = +1;
  fft(dum,len,isign);
  for(i=0;i<len;i++)
  {
    dum2[i] = dum[i*2];
  }
  for(i=0;i<len;i++)
  {
    printf("i,dum1,dum2 = %4d %8.4f %8.4f\n",i,dum1[i],dum2[i]);
  }

/*Now do a hilbert transform.*/

  for(i=0;i<len;i++)
  {
    dum[i*2]  =s[i];
    dum[i*2+1]=0.0;
  }

  isign = -1.0;
  fft(dum,len,isign);

/*Double all values of positive frequency part of transform (except 0 freq)*/
  for(i=2;i<=len+1;i++)
    dum[i]=2*dum[i];

/*Set all values of negative frequency part of transform to zero.*/
  for(i=len+2;i<len*2;i++)
    dum[i]=0.0;

  isign = +1.0;
  fft(dum,len,isign);

/*The Hilbert envelope is the modulus of the real and imag parts.*/
  for(i=0;i<spts;i++)
  {
    env=sqrt(dum[2*i]*dum[2*i]+dum[2*i+1]*dum[2*i+1]);
    printf("i,env = %4d %8.4f\n",i,env);
  }

  return 0;
}

/*Returns 0 if not power of two, otherwise log2*/
int check2(int n)
{
        int val,cnt=0;

        if(n<1){
            return(0);
        }

        val=n;
        while((val%2)==0){
            val/=2;
            if(val==1){
                return(cnt+1);
            }
            cnt++;
        }
        return(0);
}
