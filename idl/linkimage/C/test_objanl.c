#include <stdio.h>
#include <stdlib.h>
extern int objanl();

int main(int argc, char **argv ){
    char *file="ascat_data.dat";
    FILE *fp;
    long nvec, nels,nlons=360,nlats=181,npasses=3;
    float *u, *v, *lon, *lat,*time;
    float lonpar[3]={0,359,1};
    float latpar[3]={-90,90,1};
    float rainf[3]={12,4,2};
    float ermax[3]={40,40,40};
    float trainf=1;
    float ug[360][181],vg[360][181];
    long i,j,k,ngrid=360*181;


    if ((fp=fopen(file,"rb"))==NULL){
        printf("error opening %s\n",file);
        exit(1);
    }
    
    nels=fread(&nvec,sizeof(nels),1,fp);
    if (nels != 1 ){
        printf("error reading nvec, expected 4, got %d\n",nels);
        exit(2);
    }

    u= (float *) calloc( nvec, sizeof(float));
    if (u == NULL) {
        printf("calloc: error on u\n");
        exit(3);
    }
    v= (float *) calloc( nvec, sizeof(float));
    if (v == NULL) {
        printf("calloc: error on v\n");
        free(u);
        exit(3);
    }
    lon= (float *) calloc( nvec, sizeof(float));
    if (lon == NULL) {
        printf("calloc: error on lon\n");
        free(u);
        free(v);
        exit(3);
    }

    lat= (float *) calloc( nvec, sizeof(float));
    if (lat== NULL) {
        printf("calloc: error on lat\n");
        free(u);
        free(v);
        free(lon);
        exit(3);
    }

    time= (float *) calloc( nvec, sizeof(float));
    if (time== NULL) {
        printf("calloc: error on lat\n");
        free(u);
        free(v);
        free(lon);
        free(lat);
        exit(3);
    }
    for (i=0; i<nvec; i++){
        *(time+i)=1;
    }

    nels=fread(u,sizeof(float),nvec,fp);
    if (nels != nvec ){
        printf("read:u, got %d, instead of %d\n",
               nels/sizeof(float), nvec);
        exit(4);
    }

    nels=fread(v,sizeof(float),nvec,fp);
    if (nels != nvec ){
        printf("read:v, got %d, instead of %d\n",
               nels, nvec);
        exit(4);
    }
    nels=fread(lon,sizeof(float),nvec,fp);
    if (nels != nvec ){
        printf("read:lon, got %d, instead of %d\n",
               nels, nvec);
        exit(4);
    }

    /*
    nels=fread(time,sizeof(float),nvec,fp);
    if (nels != nvec ){
        printf("read:time, got %d, instead of %d\n",
               nels, nvec);
        exit(4);
    }
    */

    nels=fread(lat,sizeof(float),nvec,fp);
    if (nels != nvec ){
        printf("read:lat, got %d, instead of %d\n",
               nels, nvec);
        exit(4);
    }

    nels=fread(ug,sizeof(float),ngrid,fp);
    if (nels != ngrid ){
        printf("read:ug, got %d, instead of %d\n",
               nels, nlons*nlats);
        exit(5);
    }


    nels=fread(vg,sizeof(float),ngrid,fp);
    if (nels != ngrid ){
        printf("read:vg, got %d, instead of %d\n",
               nels, nlons*nlats);
        exit(5);
    }

    fclose(fp);
    
    if ((fp=fopen("objanl.out","wb")) == NULL ){
        printf("Error opening output\n");
        free(u);
        free(v);
        free(lon);
        free(lat);
        free(time);
        exit(6);
    }


    nels = objanl(u, 
                  v, 
                  lon, 
                  lat,
                  time, 
                  &lonpar, 
                  &latpar, 
                  &rainf,
                  &ermax,
                  &trainf, 
                  &ug,
                  &vg,
                  nvec,
                  npasses);



    fwrite(&ug,sizeof(float),nlons*nlats,fp);
    fwrite(&vg,sizeof(float),nlons*nlats,fp);
    fclose(fp);
    free(u);
    free(v);
    free(lon);
    free(lat);
    free(time);
    
    exit(1);
    
}
