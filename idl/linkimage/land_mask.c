#include <stdio.h>
/*#include "/usr/local/rsi/idl_5.1/external/export.h"*/
#include "/usr/local/rsi/idl/external/export.h"


/* Land_mask.c This routine takes a 2 vectors, one of latitudes and
*  the other of longitude and it returns a vector of the same size
*  containing a '1' where the lat/lon combination is on land and a 0
*  where it's one water. This routine calls lndmsk.c which in turns
*  queries a database designed by Jan Depner and Jim Hammack, both of
*  Stennis Space Flight Center, Louisiana. All the glory should go to
*  them, since they designed the database and wrote lndmsk.c
*
* COMPILING AND LINKING THIS ROUTINE -----------------
* (on HP-UX 9.01)
* cc -Aa +e -c +z land_mask.c lndmsk.c
* ld -b -o land_mask.so land_mask.o lndmsk.o -lc
*
* (on an SGI running IRIX 6.2)
*  cc -KPIC -c land_mask.c  lndmsk.c
* ld -shared -o land_mask.so land_mask.o  lndmsk.o -lc 
*
*  LINKING THE PROGRAM INTO IDL ---------------------
* 
*  This program is meant to be linked into a running instance of IDL using the 
*  routine 'linkimage' using the following IDL command.
*
*  IDL> linkimage,'land_mask','path/land_mask.sl'
*
* INVOCATION -----------------
*
* From the IDL prompt:
* IDL> land_mask, lon, lat, mask
*
*
* RETURNS: The variable argv[2] (mask) contains the land/water flags, 0=water, 1=land
* 
*
* Author: William Daffer (daffer@rainy.jpl.nasa.gov)
*         m/s 300-319
*         818-354-0161 (voice)
*
* Modification Log:
*
* $Log$
* Revision 1.1  1999/04/09 22:55:49  vapuser
* Initial revision
*
*
* $Id$
*
*/


IDL_VPTR land_mask( int argc, IDL_VPTR argv[], char *argk )
{
  IDL_VPTR lat, lon, mask,tmp;
  float *lat_d, *lon_d, latd,lond,latm=0.0, lonm = 0.0;
  long *mask_d;
  int first=1, status=0;
  int i,j,n;
  long mask01(), m;
  static char land_mask_rcsid[]="$Id$";
  if ( argc < 3 ) 
    IDL_Message( IDL_M_GENERIC, IDL_MSG_LONGJMP, 
		 "usage: land_mask, lon, lat, mask" );

  lon = argv[0];
  lat = argv[1];
  mask = argv[2];
  IDL_ENSURE_SIMPLE( lon );
  IDL_ENSURE_SIMPLE( lat );
  IDL_ENSURE_SIMPLE( mask );
  if (lon->type  != IDL_TYP_FLOAT) 
    {
      tmp  = IDL_CvtFlt(1,&lon);
      IDL_VarCopy( tmp, lon );
    } 
  if (lat->type  != IDL_TYP_FLOAT) 
    {
      tmp  = IDL_CvtFlt(1,&lat);
      IDL_VarCopy( tmp, lat );
    }
  if (mask->type != IDL_TYP_LONG ) 
    {
      tmp = IDL_CvtLng(1,&mask);
      IDL_VarCopy( tmp, mask );
    }
	
  if (lon->flags  & IDL_V_TEMP) printf("lon is temporary\n");
  if (lat->flags  & IDL_V_TEMP) printf("lat is temporary\n");
  if (mask->flags & IDL_V_TEMP) printf("mask is temporary\n");

  lat_d = (float *) lat->value.arr->data;
  lon_d = (float *) lon->value.arr->data;
  mask_d = (long *) mask->value.arr->data;


#ifdef DEBUG
  printf(" n_elts = %d\n", lat->value.arr->n_elts);
#endif


  for (i=0;i<lat->value.arr->n_elts; i++) 
    {
      latd = *(lat_d+i);
      lond = *(lon_d+i);


#ifdef DEBUG
      printf(" latd, lond = %f,%f\n", latd, lond );
#endif


      m =  mask01( &latd, &latm, &lond, &lonm, &first, &status );
      if (status != 1 || m<0 ) 
	{
	  IDL_Message( IDL_M_NAMED_GENERIC, 
		       IDL_MSG_LONGJMP, 
		       " Bad return from mask01, status == %d, m=%d: at i=%d\n",
		       status, m, i);
	}


#ifdef DEBUG
      printf(" m = %d\n", m );
#endif

      first=0;
      mask_d[i] = m;
    }
} /* end land_mask */




