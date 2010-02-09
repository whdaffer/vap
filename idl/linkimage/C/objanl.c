#include <stdio.h>
#include <math.h>
#include <stdlib.h>
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
 * Revision 1.2  2010/01/21 18:06:42  whdaffer
 * continuing work
 *
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
           float *lonpar, 
           float *latpar, 
           float *rainf,
           float *ermx,
           float *rainfT, 
           float *ug,
           float *vg,
           long  nvec,
           long  npasses) {

    long nlons,nlats,loninc, latinc;
    float lon0,lon1,lat0,lat1;
    float r,r2,dist,dist2,xdist,ydist,alpha,ermax;
    float x,y,xp,xm,yp,ym,dx,dy,yk1,yk2,yk3,xk1,xk2,xk3;
    float g1,g2,g3,g4,g5,g6,g7,g8,g9,uval,vval, p1, p2, aa,bb;
    float xx,yy,p1dp2, cgmax, weight,cc, xc, yc, sum;
    float denom[3] = {0.25, -0.5, 1};
    float gamma[9] = {1,1,1,1,1,1,1,1,1};
    float *udevp, *vdevp, *sumxp, *sumyp, *ap,*coscorp;
    long *nelp;
#ifdef IDL_DLM
    IDL_VPTR udev, vdev, sumx, sumy, a, num, flag, coscor,nel;
# else 
    float udev, vdev, sumx, sumy, a, coscor;
    long num, nel;
    char flag;
#endif
    long *nump;
    float a1,a2,a3;
    char *flagp;
    float DtoR,dout;

    long i,j,k,ii,jj,kk,i1,i2,j1,j2,ii1,ii2,jj1,jj2,pass,vec;
    long num_cressman,ix,iy,ixm,ixp,iym,iyp,id;
    
    DtoR=180.0/3.14159;


    /* *( Find dimensions, create work arrays */

    lon0   = *lonpar;
    lon1   = *(lonpar+1);
    loninc = *(lonpar+2);
    lat0   = *latpar;
    lat1   = *(latpar+1);
    latinc = *(latpar+2);
    nlons   = (lon1-lon0)/loninc+1;
    nlats   = (lat1-lat0)/latinc+1;


/* Have to do this in line, because of IDL's memory managament. */


#ifdef IDL_DLM

    /* Being built as a DLM, use this sort of allocation */

    //void *IDL_MemAlloc(IDL_MEMINT n, char *err_str, int action) 

    dim[0]=nvec;
    
    udevp= = (float *) IDL_MakeTempArray(IDL_TYP_FLOAT,1,dim,
                                         IDL_ARR_INI_ZERO,
                                         &udev);  
    vdevp= = (float *) IDL_MakeTempArray(IDL_TYP_FLOAT,1,dim,
                                         IDL_ARR_INI_ZERO,
                                         &vdev);  
    dim[0]=nlons;
    dim[1]=nlats;
    sumxp= = (float *) IDL_MakeTempArray(IDL_TYP_FLOAT,2,dim,
                                         IDL_ARR_INI_ZERO,
                                         &sumx);  
    sumyp= = (float *) IDL_MakeTempArray(IDL_TYP_FLOAT,2,dim,
                                         IDL_ARR_INI_ZERO,
                                         &sumy);  
    
    nump= = (long *) IDL_MakeTempArray(IDL_TYP_LONG,2,dim,
                                         IDL_ARR_INI_ZERO,
                                         &num);  
    dim[2]=3
    ap = (float *) IDL_MakeTempArray(IDL_TYP_FLOAT,3,dim,
                                         IDL_ARR_INI_ZERO,
                                         &sumy);  
    dim[0]=nvec
    flagp = (char *) IDL_MakeTempArray(IDL_TYP_BYTE,1,dim,
                                         IDL_ARR_INI_ZERO,
                                         &flag);  
    dim[0]=npasses;
    nelp = (long *) IDL_MakeTempArray(IDL_TYP_LONG,1,dim,
                                         IDL_ARR_INI_ZERO,
                                         &nel);  
#else

    /* Being built as a regular C routine */


    udevp=(float *) calloc( (size_t) (nvec),   sizeof(float));
    if (udevp==NULL) {
      printf("objanl : Error allocating udev\n");
      exit(1);
    }
    
    vdevp=(float *) calloc( (size_t) (nvec),   sizeof(float));
    if (vdevp==NULL) {
      printf("objanl : Error allocating vdev\n");
      free(udevp);
      exit(1);
    }
    
    sumxp=(float *) calloc( (size_t) (nlons*nlats),   sizeof(float));
    if (sumxp==NULL) {
      printf("objanl : Error allocating sumxp\n");
      free(udevp);
      free(vdevp);
      exit(1);
    }
    
    sumyp=(float *) calloc( (size_t) (nlons*nlats),   sizeof(float));
    if (sumyp==NULL) {
      printf("objanl : Error allocating sumyp\n");
      free(udevp);
      free(vdevp);
      free(sumxp);
      exit(1);
    }
    
    ap   =(float *) calloc( (size_t) (nlons*nlats*3), sizeof(float));
    if (ap==NULL) {
      printf("objanl : Error allocating a\n");
      free(udevp);
      free(vdevp);
      free(sumxp);
      free(sumyp);
      exit(1);
    }
    
    nump =(long  *) calloc( (size_t) (nlons*nlats),   sizeof(long));
    if (nump==NULL) {
      printf("objanl : Error allocating nump\n");
      free(udevp);
      free(vdevp);
      free(sumxp);
      free(sumyp);
      free(ap);
      exit(1);
    }
    flagp=(char *) calloc( (size_t) (nvec), sizeof(char));
    if (flagp==NULL) {
      printf("objanl : Error allocating flagp\n");
      free(udevp);
      free(vdevp);
      free(sumxp);
      free(sumyp);
      free(ap);
      exit(1);
    }
    coscorp=(float *) calloc( (size_t) (nvec), sizeof(float));
    if (coscorp==NULL) {
      printf("objanl : Error allocating coscor\n");
      free(udevp);
      free(vdevp);
      free(sumxp);
      free(sumyp);
      free(ap);
      free(flagp);
      exit(1);
    }
    nelp=(long *) calloc( (size_t) (npasses), sizeof(long));
    if (nelp==NULL) {
      printf("objanl : Error allocating nelp\n");
      free(udevp);
      free(vdevp);
      free(sumxp);
      free(sumyp);
      free(ap);
      free(flagp);
      free(coscorp);
      exit(1);
    }
    
#endif 
    
    
    
    /* Largest allowable deviation from guess grid */
    dout= (*rainf)*loninc;
    
    
    /* 
     *  Go through the data and mark all those points that fall within
     *  the grid for the first Radius of influence (rainf) and calculate some 
     *  preliminary quantities.
     *
     */
    
    for (vec=0;vec<nvec;vec++){
      *(flagp+vec) = 1;     /* assume it's inside the grid*/
      if (  fabs(lon0 - *(lon+vec)) > dout ||
            fabs(lon1 - *(lon+vec)) > dout ||
            fabs(lat0 - *(lat+vec)) > dout ||
            fabs(lat1 - *(lat+vec)) > dout ) 
      {
        *(flagp+vec)=0; /* more than max(rainf) outside grid */
      }
      if ((*flagp+vec) != 0){
        if (*(lon+vec)<lon0 || 
            *(lon+vec)>lon1 || 
            *(lat+vec)<lat0 || 
            *(lat+vec)>lat1)
          *(flagp+vec) *= -1;/*inside max(rainf) of grid, but outside */
        
      }
      
      /* put lat/lon into grid units */
      *(coscorp+vec) = cos(*(lat+vec)*DtoR);
      *(lon+vec)    = (*(lon+vec) - lon0)/loninc;
      *(lat+vec)    = (*(lat+vec) - lat0)/latinc;
    }
    for (pass=0 ; pass<npasses ; pass++){
      r=rainf[pass];
      r2=r*r;
      num_cressman=0;
      ermax= *(ermx+pass);
      for (vec=0;vec<nvec;vec++){
        if ((*flagp+vec) == 0) 
          break; /* point is too far outside grid */
        
        *(udevp+vec)=0;
        *(vdevp+vec)=0;
        x = *(lon+vec);
        y = *(lat+vec);

        
        if ((*flagp+vec) >= 1){
          /* Inside grid, use 9-point Lagrangian interpolation */
          
          ix=(long) (x+0.5);
          iy=(long) (y+0.5);
          xp=x-ix;
          yp=y-iy;
          dx=1.0;
          dy=1.0;

          yk1=yp*(yp-dy);
          yk2=(yp-dy)*(yp+dy);
          yk3=(yp+dy)*yp;

          xk1=xp*(xp-dx);
          xk2=(xp-dx)*(xp+dx);
          xk3=(xp+dx)*xp;


          
          g1=xk1*yk1*denom[0];
          g2=xk2*yk1*denom[1];
          g3=xk3*yk1*denom[0];
          g4=xk1*yk2*denom[1];
          g5=xk2*yk2*denom[2];
          g6=xk3*yk2*denom[1];
          g7=xk1*yk3*denom[0];
          g8=xk2*yk3*denom[1];
          g9=xk3*yk3*denom[0];

          ixm=ix-1;
          ixp=ix+1;
          iym=iy-1;
          iyp=iy+1;
        
          uval=
            g1* *(ug + ixm + nlons*iym) +
            g2* *(ug + ix  + nlons*iym) +
            g3* *(ug + ixp + nlons*iym) +
            g4* *(ug + ixm + nlons*iy) +
            g5* *(ug + ix  + nlons*iy) +
            g6* *(ug + ixp + nlons*iy) +
            g7* *(ug + ixm + nlons*iyp) +
            g8* *(ug + ix  + nlons*iyp) +
            g9* *(ug + ixp + nlons*iyp);

          vval=
            g1* *(vg + ixm + nlons*iym) +
            g2* *(vg + ix  + nlons*iym) +
            g3* *(vg + ixp + nlons*iym) +
            g4* *(vg + ixm + nlons*iy) +
            g5* *(vg + ix  + nlons*iy) +
            g6* *(vg + ixp + nlons*iy) +
            g7* *(vg + ixm + nlons*iyp) +
            g8* *(vg + ix  + nlons*iyp) +
            g9* *(vg + ixp + nlons*iyp);

          *(udevp+vec) = *(u+vec)-uval;
          *(vdevp+vec) = *(v+vec)-vval;
            
          if (fabs( *(udevp+vec) ) > ermax) {
            *(udevp+vec) =-999.0;
          }
          if (fabs( *(vdevp+vec) ) > ermax) {
            *(vdevp+vec) =-999.0;
          }
          if ( fabs(*(udevp+vec)+999.0) > 1 &&
               fabs(*(vdevp+vec)+999.0) > 1 ){
            *(nelp+pass) += 1;
          }
        } else {
          /* Outside grid, Cressman weighting */
          a1=a2=a3=0.0;

          ii=(long) x;
          i1=ii-(long) r-1;
          i1=ii+(long) r+1;
          
          if (i1 <0 ) i1=0;
          if (i2> (nlons-1)) i2=nlons-1;

          jj=(long) y;
          j1=jj-(long) r-1;
          j1=jj+(long) r+1;

          if (j1 <0 ) j1=0;
          if (j2>nlats-1) j2=nlats-1;
          
          for (i=i1;i<=i2;i++){
            xdist=fabs(x-i) * (*(coscorp+vec));
            for (j=j1;j<=j2;j++){
              ydist=fabs(y-j);
              dist2=xdist*xdist+ydist*ydist;
              p1=r2-dist2;
              p2=r2+dist2;
              weight=(p2>1.e-10)? p1/p2 : 0.0;
              a1 += weight* (*(ug+nlons*j+i));
              a2 += weight* (*(vg+nlons*j+i));
              a3 += weight;
            }
          }
          (*(udevp + vec)) = *(u+vec) - a1/a3;
          (*(vdevp + vec)) = *(v+vec) - a2/a3;
          if (fabs( *(udevp+vec) ) > ermax) {
            *(udevp+vec) =-999.0;
          }
          if (fabs( *(vdevp+vec) ) > ermax) {
            *(vdevp+vec) =-999.0;
          }
          if ( fabs(*(udevp+vec)+999.0) > 1 &&
               fabs(*(vdevp+vec)+999.0) > 1 ){
            *(nelp + pass) += 1;
          }
        }
      }

      /* initialize */
      cgmax=0.6*r;
      for (i=0 ; i<nlons ; i++){
        for (j=0 ; j<nlats ; j++){
          for (k=0 ; k<2 ; k++){
            *( ap + i+nlons*(j+k*nlats)) = 0.0;
          }
          *(sumxp+i+j*nlons) = 
            *(sumyp+i+j*nlons) = 0.0;
          *(nump+i+j*nlons) = 0;
        }
      }
      
      for (vec=0;vec<nvec;vec++){
        if ((*flagp+vec) == 0) 
          break; /* point is too far outside grid */

        id=(long) fabs( *(time+vec)+0.01);

        x = *(lon+vec);
        y = *(lat+vec);
        udev = *(udevp+vec);
        vdev = *(vdevp+vec);
        
        ii=(long) x;
        i1=ii-(long) r-1;
        i1=ii+(long) r+1;


        jj=(long) y;
        j1=jj-(long) r-1;
        j1=jj+(long) r+1;

        cc=*(coscorp+vec) ;

        i1 = ( cc > 1.e-7 ) ? ii - ((long) r/cc)-1 : ii-(long)r-1;
        i2 = ( cc > 1.e-7 ) ? ii + ((long) r/cc)+1 : ii+(long)r+1;
        if (i1 <0 ) i1=0;
        if (i2>nlons-1) i2=nlons-1;

        j1 = ( cc > 1.e-7 ) ? jj - ((long) r/cc)-1 : jj-(long) r-1;
        j2 = ( cc > 1.e-7 ) ? jj + ((long) r/cc)+1 : jj+(long) r+1;

        if (j1 <0 ) j1=0;
        if (j2>nlats-1) j2=nlats-1;

        for (i=i1 ; i<i2 ; i++){
          
          xdist=fabs(x-i) *cc;
          for (j=j1 ; j<=j2 ; j++){
            ydist=fabs(y-j);
            dist2=xdist*xdist+ydist*ydist;
            p1=r2-dist2;
            p2=r2+dist2;
            weight = (*(time+vec))  * *(gamma+id);
            if (p2>1.e-7){
              weight *= p1/p2;
            } 
            *(sumxp+j*nlons + i) += (x-xx);
            *(sumyp+j*nlons + i) += (y-yy);
            *(nump +j*nlons + i) += 1;
            /* a is a nlons by nlats by 3 array */
            *(ap + i+nlons*j) += *(udevp+vec)*weight;
            *(ap + i+nlons*(j+nlats)) += *(vdevp+vec)*weight;
            *(ap + i+nlons*(j+2*nlats)) += weight;
          }
        } /* loop over vectors */
        for (i=0 ; i<nlons; i++){
          for (j=0 ; j<nlats ; j++) {
            ii=+ i +j*nlons;
            xc= (*(sumxp + ii) ) / ( *(nump+ii));
            yc= (*(sumyp + ii) ) / ( *(nump+ii));
            sum=sqrt(xc*xc+yc*yc);
            p1dp2=1.0;
            if (sum<cgmax) break;
            aa=(0.4*r)*(0.4*r);
            bb=(r-fabs(sum));
            bb *= bb;
            p1dp2=(aa-bb)/(aa+bb);
            /* In objanl2.f, the following code occurred after the
             * above, replacing the result, but the paper really
             * points to the above code.
             * aa=(r2- sumx(i,j))
             * if (aa .lt.0) aa=0.
             * bb=(r2+sumx(i,j))
             * p1dp2=aa/bb
             */

            if (*(ap + i+nlons*(j+2*nlats)) <= 0) break;

            /* Finally!*/
            *(ug+ii) += 
              *(ap + ii)/( *(ap + i+nlons*(j+2*nlats)) )*p1dp2;
            *(vg+ii) += 
              *(ap + i+nlons*(j+nlats))/(*(ap + i+nlons*(j+2*nlats)))*p1dp2;
          }
        }
        
      }
    }
    
    /* Wrap up, deallocate memory */
    
#ifdef IDL_DLM
    
    IDL_DELTMP(udev);
    IDL_DELTMP(vdev);
    IDL_DELTMP(sumx);
    IDL_DELTMP(sumy);
    IDL_DELTMP(num);
    IDL_DELTMP(a);
    IDL_DELTMP(flag);
    IDL_DELTMP(coscor);
    IDL_DELTMP(nel);

#else

        printf("objanl : Freeing malloc'd data\n");
        free(coscorp);
        free(flagp);
        free(ap);
        free(sumyp);
        free(sumxp);
        free(vdevp);
        free(udevp);
#endif
        

        return(1);

}
                    
