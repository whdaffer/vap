#include <stdio.h>


void main (int argc, char ** argv){
    FILE *fp;
    char *file='ascdata.dat';
    integer nvec;
    float *u, *v, *lon, *lat;
    float lonpar[3]={0,359,1};
    float latpar[3]={-90,90,1};
    float rainf[3]={12,4,2};
    float ermax[3]={40,40,40};
    float trainf=1;
    float ug[360][181],vg[360][181];
    
    if ((fp=fopen(file,'rb')) == NULL ) {
        printf("error opening %s",file);
        exit(1);
    }
    nn=read(fp,&nvec,4);
    
    u=(float*) calloc(
}
