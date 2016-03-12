#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>

#define END 1
#define FA char*

void nrerror(char error_text[])
{
    fprintf(stderr,"run-time error\n");
    fprintf(stderr,"%s\n",error_text);
    fprintf(stderr,"exit system\n");
    exit(1);
}

float *vector(long nl, long nh)
{
    float *v;

    v=(float *)malloc((size_t) ((nh-nl+1+END)*sizeof(float)));
    if (!v) nrerror("failure in vector()");
    return v-nl+END;
}

int *ivector(long nl, long nh)
{
    int *v;

    v=(int *)malloc((size_t) ((nh-nl+1+END)*sizeof(int)));
    if (!v) nrerror("failure in ivector()");
    return v-nl+END;
}


void free_vector(float *v, long nl, long nh)
{
    free((FA) (v+nl-END));
}

void free_ivector(int *v, long nl, long nh)
{
    free((FA) (v+nl-END));
}
