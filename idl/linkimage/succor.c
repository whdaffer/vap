#include <stdio.h>
#include <stdlib.h>
#include "/usr/local/rsi/idl/external/export.h"
#define NVEC 20000

void succor( int argc, IDL_VPTR argv[], char * argk )
/* succor.c - IDL linkimage routine.
 * whd, 8-jan-1996, original version.
 *
 * This routine calls the fortran subroutine subobj.f which uses the
 * method of successive corrections to interpolate the input data to
 * the input grid.  See "Statistical interpolation by means of
 * successive corrections", Arne M. Bratseth, Tellus 38A, page 439-447
 * and Tripoli, G.J. and Krishnamuyrti,T.N., "Low-Level Flows of the
 * GATE Area during Summer,1972" Monthly Weather Review, 103: 197-216
 * for the method. At the moment, this routine serves as a wrapper for
 * subobj.f, which I've taken over in it's entirety from Victor
 * Zlotnicki, with all of its attendant oddities. These oddities
 * force some on me, like the use of the rinfo and zstd arrays.
 * I'm not going to change this right now. I don't have the
 * time. 
 *
 * Just be advised that the variables sp, time, zstd, lev and trainf are
 * required only because they are required in the code of subobj.f and
 * not because they are implicit to the problem at hand.
 *
 * Please note that the routine currently makes no attempt to use information 
 * about the time of an observation, although the routine subobj.f can make
 * use of this information in the interpolations. This may change in the future.
 *
 *
 * Parameters: All parameters must be present, correctly dimensioned and 
 *             of the correct type or this program will FAIL!!!
 *
 *   lon - vector of longitudes
 *   lat - vector of latitudes
 *   u   - vector of u component of winds
 *   v   - vector of v component of winds
 *   ui  - the NxM return array of interpolated u values
 *   vi  - the NxM return array of interpolated v values
 *   lonpar - a 3 elements vector, lonpar(0) = min longitude of grid
 *                                 lonpar(1) = max longitude of grid
 *                                 lonpar(2) = grid spacing in longitude
 *   latpar - same a lonpar, but for lattitude
 *   ermax  - 4 element vector, one for each 'redius of influence'. 
 *            The maximum 'error' value. Data which deviates more than this 
 *            amount from the interpolated values is dropped.
 *   rainf  - the 4 element vector of decreasing 'radius of influences' 
 *            to be  used in the successive corrections.
 *
 * $Log$
 *
 *
 *
 * $Id$
 *
 */


{
  IDL_VPTR lon, lat, u, v, ui,vi, lonpar, latpar, ermax, rainf;
  IDL_VPTR  rinfo_vptr, udev_vptr, vdev_vptr, alat_vptr,a_vptr,
    sumx_vptr, sumy_vptr,store_vptr, work_vptr; /* temp variables */
  float *lond, *latd, *ud, *vd, *uid, *vid, *lonpd, *latpd, 
    *ermaxd, *rainfd;
  static char succor_rcs_id[]="$Id$";
  float time=1.;
  float zstd[3]={1000., 750., 1250.};
  float trainf=1.,lonmin, lonmax, loninc, latmin, latmax, latinc;
  float denom[3] = {0.25, -0.5, 1};
  float gamma[9] = {1,1,1,1,1,1,1,1,1};
  float *rinfo, *udev,*vdev, *sumx, *sumy, *a, *alat, *work; 
  int *store, nop=4;
  int i,j, k, lev=1, sp=32767, l, m, l1,m1, lmax=720, mmax = 290, 
    nvect, nvesc=1;
  extern void subobj_();

  if ( argc < 10 ) {
    IDL_Message( IDL_M_GENERIC, IDL_MSG_LONGJMP | IDL_MSG_ATTR_SYS,"usage: succor, lon, lat, u, v, ui, vi, lonpar, latpar, ermax, rainf" );
  }
  lon    = argv[0];
  lat    = argv[1];
  u      = argv[2];
  v      = argv[3];
  ui     = argv[4];
  vi     = argv[5];
  lonpar = argv[6];
  latpar = argv[7];
  ermax  = argv[8];
  rainf  = argv[9];
  IDL_ENSURE_SIMPLE( lon );
  IDL_ENSURE_SIMPLE( lat );
  IDL_ENSURE_SIMPLE( u );
  IDL_ENSURE_SIMPLE( v );
  IDL_ENSURE_SIMPLE( ui );
  IDL_ENSURE_SIMPLE( vi );
  IDL_ENSURE_SIMPLE( lonpar );
  IDL_ENSURE_SIMPLE( latpar );
  IDL_ENSURE_SIMPLE( ermax );
  IDL_ENSURE_SIMPLE( rainf );

/*  
  if (lon->type  != IDL_TYP_FLOAT) lon  = IDL_CvtFlt(1,argv);
  if (lat->type  != IDL_TYP_FLOAT) lat  = IDL_CvtFlt(1,argv);
  if (u  ->type  != IDL_TYP_FLOAT) u    = IDL_CvtFlt(1,argv);
  if (v  ->type  != IDL_TYP_FLOAT) v    = IDL_CvtFlt(1,argv);
  if (ui ->type  != IDL_TYP_FLOAT) ui   = IDL_CvtFlt(1,argv);
  if (vi ->type  != IDL_TYP_FLOAT) vi   = IDL_CvtFlt(1,argv);
*/
  
  latd   = (float *) lat->value.arr->data;    
  lond   = (float *) lon->value.arr->data;    
  ud     = (float *) u->value.arr->data;      
  vd     = (float *) v->value.arr->data;      
  uid    = (float *) ui->value.arr->data;     
  vid    = (float *) vi->value.arr->data;     
  lonpd  = (float *) lonpar->value.arr->data;
  latpd  = (float *) latpar->value.arr->data;
  ermaxd = (float *) ermax->value.arr->data;
  rainfd = (float *) rainf->value.arr->data;

#ifdef DEBUG
  for (i=0;i<ermax->value.arr->n_elts;i++) 
    printf(" i,ermax[i] = %d,%f\n",i,*(ermaxd+i) );
  for (i=0;i<rainf->value.arr->n_elts;i++) 
    printf(" i,rainf[i] = %d,%f\n",i,*(rainfd+i) );
#endif 
  lonmin = *lonpd;
  lonmax = *(lonpd+1);
  loninc = *(lonpd+2);

  latmin = *latpd;
  latmax = *(latpd+1);
  latinc = *(latpd+2);

#ifdef DEBUG
  printf("succor: lonmin, lonmax, loninc = %f,%f,%f\n", lonmin, lonmax, loninc );
#endif

  l=(int) (((lonmax-lonmin+0.00001)/loninc)+1);
  m=(int) (((latmax-latmin+0.00001)/latinc)+1);
  l = ui->value.arr->dim[0];
  m = ui->value.arr->dim[1];
  l1 = vi->value.arr->dim[0];
  m1 = vi->value.arr->dim[1];
  nvect = lat->value.arr->n_elts;

#ifdef DEBUG 
  printf(" succor.c: l,m,l1,m1,nvect = %ld,%ld,%ld,%ld,%ld\n",
	 l,m,l1,m1,nvect );
#endif
/*
  printf(" succor : \n");
  for (i=0; i<5; i++ ){
   printf("i = %d : ",i);
   for (k=0;k<5;k++){
     for (j=0;j<8;j++) {
       printf(" %8.4f ", *(uid + i*l + k*8 + j) );
     }
     printf("\n");
   }
 }
*/
#ifdef DEBUG
 printf(" getting memory for rinfo \n");
#endif 
  if ( ( rinfo = (float*) IDL_GetScratch( &rinfo_vptr, nvect*7, sizeof(float) ) ) != NULL ) {
    for (i=0;i<nvect; i++) {
      *(rinfo + i*7    )   = 1;
      *(rinfo + i*7 + 1)  = *(latd+i);
      *(rinfo + i*7 + 2)  = *(lond+i);
      *(rinfo + i*7 + 3)  = zstd[lev-1];
      *(rinfo + i*7 + 4)  = time;
      *(rinfo + i*7 + 5)  = *(ud+i);
      *(rinfo + i*7 + 6)  = *(vd+i);
    } 

    udev = (float*) IDL_GetScratch( &udev_vptr,  nvect,4 );
    vdev = (float*) IDL_GetScratch( &vdev_vptr,  nvect,4 );
    alat = (float*) IDL_GetScratch( &alat_vptr,  nvect,4 );
    a    = (float*) IDL_GetScratch( &a_vptr,     l*m*3,  4 );
    sumx = (float*) IDL_GetScratch( &sumx_vptr,  l*m,  4 );
    sumy = (float*) IDL_GetScratch( &sumy_vptr,  l*m,  4 );
    store = (int*)  IDL_GetScratch( &store_vptr, l*m,  4 );
    work = (float*) IDL_GetScratch( &work_vptr,  l*m,  4 );

#ifdef DEBUG

    printf(" sumy_vptr->type = %d\n", sumy_vptr->type );
    printf(" sumy_vptr->flags = %d\n", sumy_vptr->flags );
    printf(" sumy_vptr->value.arr->arr_len = %d\n", sumy_vptr->value.arr->arr_len );
    printf(" sumy_vptr->value.arr->elt_len = %d\n", sumy_vptr->value.arr->elt_len );
    printf(" sumy_vptr->value.arr->n_elts = %d\n", sumy_vptr->value.arr->n_elts );
    printf(" sumy_vptr->value.arr->n_dim = %d\n", sumy_vptr->value.arr->n_dim );
    printf(" sumy_vptr->value.arr->flags = %d\n", sumy_vptr->value.arr->flags );
    
    printf(" sumx_vptr->type = %d\n", sumx_vptr->type );
    printf(" sumx_vptr->flags = %d\n", sumx_vptr->flags );
    printf(" sumx_vptr->value.arr->arr_len = %d\n", sumx_vptr->value.arr->arr_len );
    printf(" sumx_vptr->value.arr->elt_len = %d\n", sumx_vptr->value.arr->elt_len );
    printf(" sumx_vptr->value.arr->n_elts = %d\n", sumx_vptr->value.arr->n_elts );
    printf(" sumx_vptr->value.arr->n_dim = %d\n", sumx_vptr->value.arr->n_dim );
    printf(" sumx_vptr->value.arr->flags = %d\n", sumx_vptr->value.arr->flags );
    
#endif
    
    if (udev == NULL || vdev == NULL || alat == NULL || 
        a == NULL || sumx == NULL || sumy == NULL || store == NULL ) {
      IDL_Message( IDL_M_GENERIC, IDL_MSG_LONGJMP, "succor: u/vdev malloc failure");
      
    }
#ifdef DEBUG
    printf(" &rinfo = %d \n", &rinfo );
    printf(" &ui = %d \n", &ui );
    printf(" &vi = %d \n", &vi );
    printf(" &udev = %d \n", &udev );
    printf(" &vdev = %d \n", &vdev );
    printf(" &alat = %d \n", &alat );
    printf(" &a = %d \n", &a );
    printf(" &sumx = %d \n", &sumx );
    printf(" &sumy = %d \n", &sumy );
    printf(" &store = %d \n", &store ); 

    printf(" rinfo = %p \n", rinfo );
    printf(" uid = %p \n", uid );
    printf(" uid[1-2] = %p,%p \n", uid+1, uid+2 );
    printf(" vid = %p \n", vid );
    printf(" udev = %p \n", udev );
    printf(" vdev = %p \n", vdev );
    printf(" alat = %p \n", alat );
    printf(" a = %p \n", a );
    printf(" sumx = %p \n", sumx );
    printf(" sumy = %p \n", sumy );
    printf(" store = %p \n", store );

    printf(" rinfo_vptr = %p \n", rinfo_vptr );
    printf(" ui_vptr = %p \n", ui );
    printf(" vi_vptr = %p \n", vi );
    printf(" udev_vptr = %p \n", udev_vptr );
    printf(" vdev_vptr = %p \n", vdev_vptr );
    printf(" alat_vptr = %p \n", alat_vptr );
    printf(" a_vptr = %p \n", a_vptr );
    printf(" sumx_vptr = %p \n", sumx_vptr );
    printf(" sumy_vptr = %p \n", sumy_vptr );
    printf(" store_vptr = %p \n", store_vptr );


    printf(" succor: lonmin, loninc, latmin, latinc = %f,%f,%f,%f\n",
	   lonmin,loninc,latmin,latinc);
    printf(" succor: ermax = %f,%f,%f,%f\n",*ermaxd,*(ermaxd+1),*(ermaxd+2),*(ermaxd+3));

#endif

    /*    IDL_Message( IDL_M_GENERIC, IDL_MSG_LONGJMP, "succor.c: exiting ");*/

    (void) subobj_( &sp, rinfo, uid, vid, &l, &m, &nvect, &time, &lev, sumx, sumy, 
		    a, store, &latmin, &lonmin, &latinc, &loninc, zstd, &nvesc, 
		    &trainf, &nop, gamma, denom, ermaxd, rainfd, udev, vdev, alat, work );
  } else {
    IDL_Message( IDL_M_GENERIC, IDL_MSG_LONGJMP, "succor: rinfo malloc failure");
  }

#ifdef DEBUG

  printf(" a_vptr->type = %d\n", a_vptr->type );
  printf(" a_vptr->flags = %d\n", a_vptr->flags );
  printf(" a_vptr->value.arr->arr_len = %d\n", a_vptr->value.arr->arr_len );
  printf(" a_vptr->value.arr->elt_len = %d\n", a_vptr->value.arr->elt_len );
  printf(" a_vptr->value.arr->n_elts = %d\n", a_vptr->value.arr->n_elts );
  printf(" a_vptr->value.arr->n_dim = %d\n", a_vptr->value.arr->n_dim );
  printf(" a_vptr->value.arr->flags = %d\n", a_vptr->value.arr->flags );
  printf(" a_vptr->value.arr->data = %p\n", a_vptr->value.arr->data );

  printf(" udev_vptr->type = %d\n", udev_vptr->type );
  printf(" udev_vptr->flags = %d\n", udev_vptr->flags );
  printf(" udev_vptr->value.arr->arr_len = %d\n", udev_vptr->value.arr->arr_len );
  printf(" udev_vptr->value.arr->elt_len = %d\n", udev_vptr->value.arr->elt_len );
  printf(" udev_vptr->value.arr->n_elts = %d\n", udev_vptr->value.arr->n_elts );
  printf(" udev_vptr->value.arr->n_dim = %d\n", udev_vptr->value.arr->n_dim );
  printf(" udev_vptr->value.arr->flags = %d\n", udev_vptr->value.arr->flags );
  printf(" udev_vptr->value.arr->data = %p\n", udev_vptr->value.arr->data );

  printf(" alat_vptr->type = %d\n", alat_vptr->type );
  printf(" alat_vptr->flags = %d\n", alat_vptr->flags );
  printf(" alat_vptr->value.arr->arr_len = %d\n", alat_vptr->value.arr->arr_len );
  printf(" alat_vptr->value.arr->elt_len = %d\n", alat_vptr->value.arr->elt_len );
  printf(" alat_vptr->value.arr->n_elts = %d\n", alat_vptr->value.arr->n_elts );
  printf(" alat_vptr->value.arr->n_dim = %d\n", alat_vptr->value.arr->n_dim );
  printf(" alat_vptr->value.arr->flags = %d\n", alat_vptr->value.arr->flags );
  printf(" alat_vptr->value.arr->data = %p\n", alat_vptr->value.arr->data );

  printf(" sumy_vptr->type = %d\n", sumy_vptr->type );
  printf(" sumy_vptr->flags = %d\n", sumy_vptr->flags );
  printf(" sumy_vptr->value.arr->arr_len = %d\n", sumy_vptr->value.arr->arr_len );
  printf(" sumy_vptr->value.arr->elt_len = %d\n", sumy_vptr->value.arr->elt_len );
  printf(" sumy_vptr->value.arr->n_elts = %d\n", sumy_vptr->value.arr->n_elts );
  printf(" sumy_vptr->value.arr->n_dim = %d\n", sumy_vptr->value.arr->n_dim );
  printf(" sumy_vptr->value.arr->flags = %d\n", sumy_vptr->value.arr->flags );
  printf(" sumy_vptr->value.arr->data = %p\n", sumy_vptr->value.arr->data );

  printf(" sumx_vptr->type = %d\n", sumx_vptr->type );
  printf(" sumx_vptr->flags = %d\n", sumx_vptr->flags );
  printf(" sumx_vptr->value.arr->arr_len = %d\n", sumx_vptr->value.arr->arr_len );
  printf(" sumx_vptr->value.arr->elt_len = %d\n", sumx_vptr->value.arr->elt_len );
  printf(" sumx_vptr->value.arr->n_elts = %d\n", sumx_vptr->value.arr->n_elts );
  printf(" sumx_vptr->value.arr->n_dim = %d\n", sumx_vptr->value.arr->n_dim );
  printf(" sumx_vptr->value.arr->flags = %d\n", sumx_vptr->value.arr->flags );
  printf(" sumx_vptr->value.arr->data = %p\n", sumx_vptr->value.arr->data );
  

  printf(" about to deltemp rinfo_vptr\n");
#endif 
  IDL_Deltmp(rinfo_vptr);
/*  printf(" about to deltemp udev_vptr\n");*/
  IDL_Deltmp(udev_vptr);
/*  printf(" about to deltemp vdev_vptr\n");*/
  IDL_Deltmp(vdev_vptr);
/*  printf(" about to deltemp store_vptr\n");*/
  IDL_Deltmp(alat_vptr); /*alright, with some tmp variables still to clean up */
  IDL_Deltmp(a_vptr); /*no  */
/*  printf(" about to deltemp sumx_vptr\n");*/
  IDL_Deltmp(sumx_vptr);/* no */ 
/*  printf(" about to deltemp sumy_vptr\n");*/
  IDL_Deltmp(sumy_vptr);/*no */
/*  printf(" about to deltemp a_vptr\n");*/
  IDL_Deltmp(store_vptr); /*alright, with some tmp variables still to clean up */
/*  printf(" about to deltemp alat_vptr\n");*/
  IDL_Deltmp(work_vptr); /*alright, with some tmp variables still to clean up */
/*  printf(" about to deltemp alat_vptr\n");*/

#ifdef DEBUG  
  printf(" After all deltmps\n");
#endif 
  return;
}


