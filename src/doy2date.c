/*
 * doy2date.c
 * $Id$:
 * Modification History:
 * 
 * $Log$
 * Revision 1.1  1998/10/22 21:43:50  vapuser
 * Initial revision
 *
 *
 *
 */
#include <stdio.h>
int doy2date( int year, int doy, int *month, int *day) {

  static const char doy2date_c_rcsid[]=
    "$Id$";
  int dpm[2][12]   =  { 31, 28, 31, 30,  31,  30,  31,  31,  30,  31,  30,  31,
                       31, 29, 31, 30,  31,  30,  31,  31,  30,  31,  30,  31 }; 
  int mend[2][12]  =  {  31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365, 
                        31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 };
  char mnum[12][3] = { "01", "02", "03", "04", "05","06","07","08","09",
                    "10","11","12" };
  int    i, found=0, leap=-1;

  /* Begin program */

  leap = ( (( year % 100 != 0) && (year % 4 == 0)) ^ (year % 400 == 0) );

  if ( doy <= 31) {
    found=1;
    *month = 1;
    *day = doy;
  } else {
    i=1;
    while ( i < 12 ) {
      if ( doy > mend[leap][i-1] && doy <= mend[leap][i] ) {
	*day = doy-mend[leap][i-1];
	*month=i+1;
	found=1;
	break;
      } else {
        i++;
      }
    }
  }
  if (!found) {
    printf("doy2date: couldnt find month - exiting\n" );
    return (0);
  }
  return ( 1 );
}
