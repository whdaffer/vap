/*
 * Adapted from 'subobj.f' which I got from Victor Zlotniki.
 * This subroutine is based (loosely) on the paper 
 *
 *  "Low-Level Flows of the GATE area during Summer 1972"
 *   Gregory j. Tripoli and T.N.Krishnamurti
 *   Dept of Meteorology, Florida State University, Tallahassee Fla
 *   
 *  Monthly Weather Review, Volume 103
 *  pp 197-216 
 *  March 1975.
 * 
 * $Id$
 *
 * Modification Log
 *
 * $Log$
 * Revision 1.1  2002/08/21 18:28:54  vapdev
 * initial revision
 *
 *
 * Usage: status=objanl(sp,rinfo,ug,vg,l,m,nvect,nreps,time,
 *                      sumx,sumy,a,store,latpar,lonpar,nvesc,trainf,
 *                      gamma,denom,ermax,rainf,udev,vdev,alat,work)
 *
 * I   sp:special (bad) value if(rinfo(5,*),(6,*) = sp) vector not used.
 * I,O rinfo(1,kk)=1.
 * I,O rinfo(2,kk)=latitude    (O: y grid coordinate)
 * I,O rinfo(3,kk)=longitude   (O: x grid coordinate)
 * I,O rinfo(4,kk)=time        (O: time weight)
 * I,O rinfo(5,kk)=1st dimension of data.
 * I,O rinfo(6,kk)=2nd dimension of data.  (if (nvesc.eq.1))
 * I,O ug, vg: Guess fields.
 * I,O l,m: 
 * I,O nvect: number of points in rinfo.  (O: n.retained vectors).
 * I   nreps: number of iterations throught the *big* loop.
 * I,O time
 * I,O sumx, sumy
 * I,O a
 * I,O store
 * I,O rltmn,rlnmn
 * I,O rltinc,rlninc
 * I,O nvesc : Flag, if data is 2-dimensional, rinfo(6,*) has the second dimension.
 * I,O trainf
 * I,O pres
 * I,O gamma
 * I,O denom
 * I,O ermx:  Error for each iteration of big loop. Data larger than
 *            this is rejected.
 * I,O rainf: spatial search radii, one per iteration, in gridstep units.
 * I,O udev, vdev
 * I,O alat
 * I,O lt,mt
 *
 *
 *
 */

int objanl(float *u, 
           float *v, 
           float *lon, 
           float *lat,
           float *time, 
           long  *nvec,
           float *lonpar, 
           float *latpar, 
           float *rainf,
           float *ermax,
           float *ug,
           float *vg,
           float rainfT, 
           long  npasses) {

    long nlon,nlat,loninc, latinc;
    float lon0,lon1,lat0,lat1;
    float r,r2,dist,dist2,xdist,ydist,alpha;
    float denom[3] = {0.25, -0.5, 1};
    float gamma[9] = {1,1,1,1,1,1,1,1,1};
    float *udev, *vdev, *sumx, *sumy, *a;
    long *num;
    float a1,a2,a3;

    long i,j,k,ii,jj,kk,i1,i2,j1,j2,ii1,ii2,jj1,jj2,pass,vec;
    
    /* Find dimensions, create work arrays */

    lon0=*lonpar;
    lon1=(*lonpar+1);
    loninc=*lonpar+2;
    lat0=*latpar;
    lat1=(*latpar+1);
    latinc=*latpar+2;
    nlon = (lon1-lon0)/loninc+1;
    nlat = (lat1-lat0)/latinc+1;

    
    if (objanl_allocmem(nlon,nlat,nvec,
                        udev,vdev,sumx,sumy,a,num,flag)==0){
        printf (" objanl_allocmem failure!\n");
        return (0);
    }

    /* initialize flag to 1
    /* Go through the data and mark all those points that fall within
       the grid for the first Radius of influence (rainf) */
        dout= (*rainf)*loninc;

    for (vec=0;vec<nvec;vec++){
        *flag+vec = 1;
        
            
    }
    

        


    /* Wrap up, deallocate memory */
}

int objanl_allocmam(long nlon,
                    long nlat,
                    long nvec, 
                    float *udev,
                    float *vdev, 
                    float *sumx, 
                    float *sumy,
                    float *a, 
                    float *num, 
                    char *flag)
{
#ifdef IDL_DLM
#else
    udev=(float *) calloc( (size_t) (nlon*nlat),   (size_t) (float));
    if (udev==NULL) {
        printf("objanl : Error allocating udev\n");
        return (0);
    }
    
    vdev=(float *) calloc( (size_t) (nlon*nlat),   (size_t) (float));
    if (vdev==NULL) {
        printf("objanl : Error allocating vdev\n");
        free(udev);
        return (0);
    }
    
    sumx=(float *) calloc( (size_t) (nlon*nlat),   (size_t) (float));
    if (sumx==NULL) {
        printf("objanl : Error allocating sumx\n");
        free(udev);
        free(vdev);
        return (0);
    }
    
    sumy=(float *) calloc( (size_t) (nlon*nlat),   (size_t) (float));
    if (sumy==NULL) {
        printf("objanl : Error allocating sumy\n");
        free(udev);
        free(vdev);
        free(sumx);
        return (0);
    }
    
    a   =(float *) calloc( (size_t) (nlon*nlat*3), (size_t) (float));
    if (a==NULL) {
        printf("objanl : Error allocating a\n");
        free(udev);
        free(vdev);
        free(sumx);
        free(sumy);
        return (0);
    }
    
    num =(long  *) calloc( (size_t) (nlon*nlat),   (size_t) (long));
    if (num==NULL) {
        printf("objanl : Error allocating num\n");
        free(udev);
        free(vdev);
        free(sumx);
        free(sumy);
        free(a);
        return (0);
    }
    flag=(char *) calloc( (size_t) (nvec), (size_t) (char));
    if (flag==NULL) {
        printf("objanl : Error allocating flag\n");
        free(udev);
        free(vdev);
        free(sumx);
        free(sumy);
        free(a);
        return (0);
    }
    return(1)
#endif 
}
                    
