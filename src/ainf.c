/* ainf.c 
 *
 * $Id$
 *
 * This routine reads the header block of a McIdas AREA file and writes 
 * some of the  information therein to STDOUT.
 *
 * William Daffer
 * Aug 28 1996
 *
 * Example output line
 *
 * AREA9438: Data Taken on 09/22/1996, at 1345  Hours: Long Res = 4, Lat Res = 4, DOY = 266
 *
 * $Log$
 *
 *
 */
#include <stdio.h>
#include <stdlib.h>


int doy2date( int year, int doy, int *month, int *day) {


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

/* ======================== ainf.c ==================================== */

main ( int argc, char * argv[] )
     /* Reads input AREA file and output time info from header */
{
  static char *ainf_rcsid="$Id$";

  long hdr[64], yearday, doy, year, time, lres, eres, n;
  int month, day;
  long hour, min, ret;
  FILE *fd;
  char date[6], date_str[11], *cptr = date_str;
  date[5]=NULL;
  date_str[10]=NULL;
  

  if (argc < 2) {
    printf( "usage: ainf filename [filename ...]\n");
    exit (1);
  }
  /*  printf ("argc = %d \n",argc);*/
  while( *(++argv) != NULL ) {
    /*    printf("ii,argv[ii] = %d,%s\n",ii,argv[ii]);*/
    if ( (fd=fopen(*argv,"rb")) == NULL ) {
      printf( "Error opening file %s\n", argv[0] );
      exit (1);
    }
    
    if ( (n=fread( hdr, sizeof( long ), 64, fd )) != 64 ) {
      printf( "Error reading the 64 longwords from file %s\n", *argv );
      exit (1);
    }
    fclose( fd );
    
    yearday  = hdr[3];
    time     = hdr[4]; 
    lres     = hdr[11];
    eres     = hdr[12];
    hour     = ( long ) (time/10000);
    min      = ( (time-hour*10000)/100);
    year     = (long) yearday/1000.;
    doy      = yearday - year*1000.; 
    year     = year + 1900L;
    /*  printf(" time = %d\n", time );
	printf(" hour = %d\n",hour);  
	printf(" hour10 = %d\n",hour10);
	printf(" hour1 = %d\n",hour1);
	printf(" min = %d\n",min);  
	printf(" min10 = %d\n",min10);
	printf(" min1 = %d\n",min1); */
    ret=doy2date( year, doy, &month, &day );
    /*    " filename: mm/dd/yyyy, time, eres, lres, doy " */
    if (ret==1) {
      printf( "%s: Data Taken on %02d/%02d/%4d, at %02d%02d Hours: Long Res = %d, Lat Res = %d, DOY = %d\n", 
	    *argv, month,day, year,hour,min, eres, lres, doy );
    } else {
      fprintf(stderr,"Error getting date on file %s\n",*argv);
    }  
  }
  exit (0);

}
