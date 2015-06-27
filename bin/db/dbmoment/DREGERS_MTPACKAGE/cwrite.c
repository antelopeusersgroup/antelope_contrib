#include<sys/file.h>
#include<fcntl.h>
#include<stdio.h>

cwrite_(data,n,nd,l)
  float *data;
  int *n,*nd,*l;
  {
  int bytes,fd;
  bytes=*n;
  if(*nd==1 && *l==1)
    fd=open("vec",O_WRONLY | O_CREAT | O_TRUNC,0644);
  else
    fd=open("vec",O_WRONLY | O_CREAT | O_APPEND,0644);
  write(fd,data,bytes*sizeof(float));
  close(fd);
  }
