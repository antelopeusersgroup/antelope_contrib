#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>

#define f char*

void killerror(char error_text[])
{
    fprintf(stderr,"run-time error %s\n",error_text);
    exit(1);
}

float *vec(long nl, long nh)
{
    float *v;
    v=(float *)malloc((size_t) ((nh-nl+2)*sizeof(float)));
    if (!v) killerror("cannot allocate vector");
    return v-nl+1;
}

int *ivec(long nl, long nh)
{
    int *v;
    v=(int *)malloc((size_t) ((nh-nl+2)*sizeof(int)));
    if (!v) killerror("cannot allocate ivector");
    return v-nl+1;
}


void f_vec(float *v, long nl, long nh)
{
    free((f) (v+nl-1));
}

void f_ivec(int *v, long nl, long nh)
{
    free((f) (v+nl-1));
}
