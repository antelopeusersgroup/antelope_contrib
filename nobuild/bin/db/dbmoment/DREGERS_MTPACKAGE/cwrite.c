#include<sys/file.h>
#include<fcntl.h>
#include<stdio.h>

void cwrite_(float *data, int *n, int *nd, int *l)
  {
  FILE *fd;
  int bytes;
  bytes=*n;
  if(*nd==1 && *l==1)
    fd=fopen("vec","w");
  else
    fd=fopen("vec","a");
  //write(fd,data,bytes*sizeof(float));
  fwrite(data, sizeof(float), bytes, fd);
  fclose(fd);
  }
