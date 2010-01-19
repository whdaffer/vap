#include <stdio.h>
#include <stdlib.h>
#include "/usr/local/rsi/idl/external/export.h"
#define NVEC 200000

void succor( int argc, IDL_VPTR argv[], char * argk )
/* succor2.c - IDL linkimage routine.
 * 
 *
 * call: IDL> succor2, lon,lat,u,v,ui,vi,lonpar,latpar,ermax,rainf
 *
 * Parameters: All parameters must be present, correctly dimensioned and 
 *             of the correct type or this program will FAIL HORRIBLY!
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

 *   rainf - A scalar or vector or reals. If vector, it is a
 *           monotonically decreasing function, representing the 
 *          'radii of influence' for each iteration through the 
 *           objective analysis routine 'objanl.f'
 *   ermax  - The error to be used for each 'rainf'. 
 *            Data which deviates more than this amount from the 
 *            input guess field, or the current guess field, if 
 *            not the initial iteration, is dropped.
 * $Log$
 * Revision 1.3  1999/09/21 18:32:34  vapuser
 * changed succor2 to succor
 *
 * Revision 1.2  1999/09/21 18:31:30  vapuser
 * Rewrote to handle variable rainf/ermax.
 *
 * $Id$
 * */


{
  IDL_VPTR lon, lat, u, v, ui,vi, lonpar, latpar, ermax, rainf;
  IDL_VPTR  rinfo_vptr, udev_vptr, vdev_vptr, alat_vptr,a_vptr,
    sumx_vptr, sumy_vptr,store_vptr, work_vptr; /* temp variables */
  IDL_VPTR tmp;
  float *lond, *latd, *ud, *vd, *uid, *vid, *lonpd, *latpd, 
    *ermaxd, *rainfd;
  static char succor_rcs_id[]="$Id$";
  float time=1.;
  float trainf=1.,lonmin, lonmax, loninc, latmin, latmax, latinc;
  float denom[3] = {0.25, -0.5, 1};
  float gamma[9] = {1,1,1,1,1,1,1,1,1};
  float *rinfo, *udev,*vdev, *sumx, *sumy, *a, *alat, *work; 
  int *store, nop=4;
  int i,j, k, lev=1, sp=32767, l, m, l1,m1, lmax=720, mmax = 290, 
    nvect, nvesc=1, nreps;
  long *nr;
  extern void objanl2_();

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

  
  if (lon->type  != IDL_TYP_FLOAT) {
    tmp  = IDL_CvtFlt(1,&lon);
    IDL_VarCopy( tmp, lon );
  }
  if (lat->type  != IDL_TYP_FLOAT) {
    tmp = IDL_CvtFlt(1,&lat);
    IDL_VarCopy(tmp, lat);
  }
  if (u  ->type  != IDL_TYP_FLOAT) {
    tmp = IDL_CvtFlt(1,&u);
    IDL_VarCopy(tmp, u);
  }
  if (v  ->type  != IDL_TYP_FLOAT) {
    tmp = IDL_CvtFlt(1,&v);
    IDL_VarCopy(tmp, v);
  }    
  if (ui ->type  != IDL_TYP_FLOAT) {
    tmp = IDL_CvtFlt(1,&ui);
    IDL_VarCopy(tmp, ui);
  }
  if (vi ->type  != IDL_TYP_FLOAT) {
    tmp = IDL_CvtFlt(1,&vi);
    IDL_VarCopy(tmp, vi);
  }

  if (rainf ->type  != IDL_TYP_FLOAT) {
    tmp = IDL_CvtFlt(1,&rainf);
    IDL_VarCopy(tmp, rainf);
  }

  if (ermax ->type  != IDL_TYP_FLOAT) {
    tmp = IDL_CvtFlt(1,&ermax);
    IDL_VarCopy(tmp, ermax);
  }

  
  latd   = (float *) lat->value.arr->data;    
  lond   = (float *) lon->value.arr->data;    
  ud     = (float *) u->value.arr->data;      
  vd     = (float *) v->value.arr->data;      
  uid    = (float *) ui->value.arr->data;     
  vid    = (float *) vi->value.arr->data;     
  lonpd  = (float *) lonpar->value.arr->data;
  latpd  = (float *) latpar->value.arr->data;

  if (rainf->flags & IDL_V_ARR) {
    nreps = rainf->value.arr->n_elts;
    rainfd = (float *) rainf->value.arr->data;
  } else {
    nreps=1;
    rainfd = &(rainf->value.f);
  }
  if (ermax->flags & IDL_V_ARR) {
    
    if (nreps != ermax->value.arr->n_elts) {
      IDL_Message( IDL_M_GENERIC, IDL_MSG_LONGJMP, 
	  "succor: RAINF and ERMAX dim mismatch!");
    }
    ermaxd = (float *) ermax->value.arr->data;
  } else {
    if (nreps > 1) {
      IDL_Message( IDL_M_GENERIC, IDL_MSG_LONGJMP, 
	 "succor: RAINF and ERMAX dim mismatch!");
    }
    ermaxd = &(ermax->value.f);
  }


#ifdef DEBUG
  if (ermax->flags & IDL_V_ARR) {
    for (i=0;i<ermax->value.arr->n_elts;i++) 
      printf(" i,ermax[i] = %d,%f\n",i,*(ermaxd+i) );
    for (i=0;i<rainf->value.arr->n_elts;i++) 
      printf(" i,rainf[i] = %d,%f\n",i,*(rainfd+i) );
  } else {
    printf(" ermax=%f\n",*ermaxd);
    printf(" rainf=%f\n",*rainfd);
  }
#endif 

  lonmin = *lonpd;
  lonmax = *(lonpd+1);
  loninc = *(lonpd+2);

  latmin = *latpd;
  latmax = *(latpd+1);
  latinc = *(latpd+2);

#ifdef DEBUG
  printf("succor2: lonmin, lonmax, loninc = %f,%f,%f\n", lonmin, lonmax, loninc );
#endif

  l=(int) (((lonmax-lonmin+0.00001)/loninc)+1);
  m=(int) (((latmax-latmin+0.00001)/latinc)+1);
  l = ui->value.arr->dim[0];
  m = ui->value.arr->dim[1];
  l1 = vi->value.arr->dim[0];
  m1 = vi->value.arr->dim[1];
  nvect = lat->value.arr->n_elts;

#ifdef DEBUG 
  printf(" succor2.c: l,m,l1,m1,nvect = %ld,%ld,%ld,%ld,%ld\n",
	 l,m,l1,m1,nvect );
#endif

#ifdef DEBUG
 printf(" getting memory for rinfo \n");
#endif 

  if ((rinfo = (float*) 
       IDL_GetScratch( &rinfo_vptr, 
		  nvect*6,sizeof(float))) != NULL) {

    for (i=0;i<nvect; i++) {
      *(rinfo + i*6    )   = 1;
      *(rinfo + i*6 + 1)  = *(latd+i);
      *(rinfo + i*6 + 2)  = *(lond+i);
      *(rinfo + i*6 + 3)  = time;
      *(rinfo + i*6 + 4)  = *(ud+i);
      *(rinfo + i*6 + 5)  = *(vd+i);
    } 

    udev = (float*) IDL_GetScratch( &udev_vptr,  nvect,4 );
    vdev = (float*) IDL_GetScratch( &vdev_vptr,  nvect,4 );
    alat = (float*) IDL_GetScratch( &alat_vptr,  nvect,4 );
    a    = (float*) IDL_GetScratch( &a_vptr,     l*m*3,  4 );
    sumx = (float*) IDL_GetScratch( &sumx_vptr,  l*m,  4 );
    sumy = (float*) IDL_GetScratch( &sumy_vptr,  l*m,  4 );
    store = (int*)  IDL_GetScratch( &store_vptr, l*m,  4 );
    work = (float*) IDL_GetScratch( &work_vptr,  l*m,  4 );

    
    if (udev  == NULL || 
	vdev  == NULL || 
	alat  == NULL || 
        a     == NULL || 
	sumx  == NULL || 
	sumy  == NULL || 
	store == NULL ) {
      IDL_Message( IDL_M_GENERIC, IDL_MSG_LONGJMP, 
		   "succor: u/vdev malloc failure");

#if 0
    printf(" sumy_vptr->type = %d\n", sumy_vptr->type );
    printf(" sumy_vptr->flags = %d\n", sumy_vptr->flags );
    printf(" sumy_vptr->value.arr->arr_len = %d\n", 
	   sumy_vptr->value.arr->arr_len );
    printf(" sumy_vptr->value.arr->elt_len = %d\n", 
	   sumy_vptr->value.arr->elt_len );
    printf(" sumy_vptr->value.arr->n_elts = %d\n", 
	   sumy_vptr->value.arr->n_elts );
    printf(" sumy_vptr->value.arr->n_dim = %d\n", 
	   sumy_vptr->value.arr->n_dim );
    printf(" sumy_vptr->value.arr->flags = %d\n", 
	   sumy_vptr->value.arr->flags );
    printf(" sumx_vptr->type = %d\n", sumx_vptr->type );
    printf(" sumx_vptr->flags = %d\n", sumx_vptr->flags );
    printf(" sumx_vptr->value.arr->arr_len = %d\n", 
	   sumx_vptr->value.arr->arr_len );
    printf(" sumx_vptr->value.arr->elt_len = %d\n", 
	   sumx_vptr->value.arr->elt_len );
    printf(" sumx_vptr->value.arr->n_elts = %d\n", 
	   sumx_vptr->value.arr->n_elts );
    printf(" sumx_vptr->value.arr->n_dim = %d\n",
	   sumx_vptr->value.arr->n_dim );
    printf(" sumx_vptr->value.arr->flags = %d\n", 
	   sumx_vptr->value.arr->flags );
#endif
      
    }
#if 0
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
    printf("succor2: nreps = %d\n",nreps );

#endif 
#ifdef DEBUG
    /*
     *(uid + 5) = 1;
     printf("Setting uid[0][5] = %f\n",*(uid+5) );
     *(vid + 6 ) = 2;
     printf("Setting vid[0][6]=%f\n",*(vid+6) );
    */
#endif

    (void) objanl2_( &sp, rinfo, uid, vid, &l, &m, 
		    &nvect, &nreps, &time, sumx, sumy, 
		    a, store, &latmin, &lonmin, 
		    &latinc, &loninc, &nvesc, 
		    &trainf, gamma, denom, 
		    ermaxd, rainfd, udev, vdev, alat, work );
  } else {
    IDL_Message( IDL_M_GENERIC, IDL_MSG_LONGJMP, 
		 "succor: rinfo malloc failure");
  }

#if 0

  printf(" a_vptr->type = %d\n", a_vptr->type );
  printf(" a_vptr->flags = %d\n", a_vptr->flags );
  printf(" a_vptr->value.arr->arr_len = %d\n", 
	 a_vptr->value.arr->arr_len );
  printf(" a_vptr->value.arr->elt_len = %d\n", 
	 a_vptr->value.arr->elt_len );
  printf(" a_vptr->value.arr->n_elts = %d\n", 
	 a_vptr->value.arr->n_elts );
  printf(" a_vptr->value.arr->n_dim = %d\n", 
	 a_vptr->value.arr->n_dim );
  printf(" a_vptr->value.arr->flags = %d\n", 
	 a_vptr->value.arr->flags );
  printf(" a_vptr->value.arr->data = %p\n", 
	 a_vptr->value.arr->data );

  printf(" udev_vptr->type = %d\n", 
	 udev_vptr->type );
  printf(" udev_vptr->flags = %d\n", 
	 udev_vptr->flags );
  printf(" udev_vptr->value.arr->arr_len = %d\n", 
	 udev_vptr->value.arr->arr_len );
  printf(" udev_vptr->value.arr->elt_len = %d\n", 
	 udev_vptr->value.arr->elt_len );
  printf(" udev_vptr->value.arr->n_elts = %d\n", 
	 udev_vptr->value.arr->n_elts );
  printf(" udev_vptr->value.arr->n_dim = %d\n", 
	 udev_vptr->value.arr->n_dim );
  printf(" udev_vptr->value.arr->flags = %d\n", 
	 udev_vptr->value.arr->flags );
  printf(" udev_vptr->value.arr->data = %p\n", 
	 udev_vptr->value.arr->data );

  printf(" alat_vptr->type = %d\n", alat_vptr->type );
  printf(" alat_vptr->flags = %d\n", alat_vptr->flags );
  printf(" alat_vptr->value.arr->arr_len = %d\n", 
	 alat_vptr->value.arr->arr_len );
  printf(" alat_vptr->value.arr->elt_len = %d\n", 
	 alat_vptr->value.arr->elt_len );
  printf(" alat_vptr->value.arr->n_elts = %d\n", 
	 alat_vptr->value.arr->n_elts );
  printf(" alat_vptr->value.arr->n_dim = %d\n", 
	 alat_vptr->value.arr->n_dim );
  printf(" alat_vptr->value.arr->flags = %d\n", 
	 alat_vptr->value.arr->flags );
  printf(" alat_vptr->value.arr->data = %p\n", 
	 alat_vptr->value.arr->data );

  printf(" sumy_vptr->type = %d\n", sumy_vptr->type );
  printf(" sumy_vptr->flags = %d\n", sumy_vptr->flags );
  printf(" sumy_vptr->value.arr->arr_len = %d\n", 
	 sumy_vptr->value.arr->arr_len );
  printf(" sumy_vptr->value.arr->elt_len = %d\n", 
	 sumy_vptr->value.arr->elt_len );
  printf(" sumy_vptr->value.arr->n_elts = %d\n", 
	 sumy_vptr->value.arr->n_elts );
  printf(" sumy_vptr->value.arr->n_dim = %d\n", 
	 sumy_vptr->value.arr->n_dim );
  printf(" sumy_vptr->value.arr->flags = %d\n", 
	 sumy_vptr->value.arr->flags );
  printf(" sumy_vptr->value.arr->data = %p\n", 
	 sumy_vptr->value.arr->data );

  printf(" sumx_vptr->type = %d\n", sumx_vptr->type );
  printf(" sumx_vptr->flags = %d\n", sumx_vptr->flags );
  printf(" sumx_vptr->value.arr->arr_len = %d\n", 
	 sumx_vptr->value.arr->arr_len );
  printf(" sumx_vptr->value.arr->elt_len = %d\n", 
	 sumx_vptr->value.arr->elt_len );
  printf(" sumx_vptr->value.arr->n_elts = %d\n", 
	 sumx_vptr->value.arr->n_elts );
  printf(" sumx_vptr->value.arr->n_dim = %d\n", 
	 sumx_vptr->value.arr->n_dim );
  printf(" sumx_vptr->value.arr->flags = %d\n", 
	 sumx_vptr->value.arr->flags );
  printf(" sumx_vptr->value.arr->data = %p\n", 
	 sumx_vptr->value.arr->data );

#endif 
#if 0
  printf(" about to deltemp rinfo_vptr\n");
#endif 
  IDL_Deltmp(rinfo_vptr);

#if 0 
   printf(" about to deltemp udev_vptr\n");
#endif 
  IDL_Deltmp(udev_vptr);

#if 0 
  printf(" about to deltemp vdev_vptr\n");
#endif 
  IDL_Deltmp(vdev_vptr);

#if 0
  printf(" about to deltemp alat_vptr\n");
#endif
  IDL_Deltmp(alat_vptr); 

#if 0
  printf(" about to deltemp a_vptr\n");
#endif
  IDL_Deltmp(a_vptr); 

#if 0
  printf(" about to deltemp sumx_vptr\n");
#endif
  IDL_Deltmp(sumx_vptr);

#if 0
  printf(" about to deltemp sumy_vptr\n");
#endif
  IDL_Deltmp(sumy_vptr);

#if 0
  printf(" about to deltemp store_vptr\n");
#endif
  IDL_Deltmp(store_vptr); 

#if 0
  printf(" about to deltemp work_vptr\n");
#endif
  IDL_Deltmp(work_vptr); 

#if 0  
  printf(" After all deltmps\n");
#endif 
  return;
}


