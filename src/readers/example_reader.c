/*
 * example_reader.c
 *
 * An example of how to read a RMGDR Vap file.
 * William Daffer
 * William.Daffer@jpl.nasa.gov
 * 818-354-0161
 * 
 * Usage: example_reader SeaNRTfile switch
 *
 * The second argument tells the program whether to write out 
 * lon,lat,speed,direction (switch ==1 ) or 
 * lon,lat,u,v (switch==0)
 *
 * The program defaults to switch==1!
 *
 *
 * $Id$
 * 
 * $Log$
 * Revision 1.3  2002/06/11 15:19:22  vapuser
 * Added rfsw to arguments so the user can excise rainflagged data. To
 * this end, extract rainflag. Extract row_time and print it out.
 *
 * Revision 1.2  2001/05/17 17:44:42  vapuser
 * Added code for byteswapping. Cleaned up header a bit
 *
 * Revision 1.1  2001/02/23 18:27:25  vapuser
 * Initial revision
 *
 * */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "qscat_rec.h"
#define   DTOR  0.0174533
#define BIG_E      0
#define LITTLE_E   1
#define SWAP_S(A)        ((short)( ((A & 0x00ff)<<8) | ((A & 0xff00)>>8) ))
#define SWAP_US(A)       ( ((A & 0x00ff)<<8) | ((A & 0xff00)>>8) )


char qscathdr[QSCAT_REC_LEN];

int main (int argc, char *argv[]) {

  QREC qrec;
  FILE *fp;
  int n,i,j,s,sw=1,r=0, rfsw=0;
  float lat,lon,dir,speed,u,v;
  short int lati,speedi;
  unsigned short int loni,diri;
  int rf;
  char time[25];



  if (argc<2) {
    fprintf(stderr,
	    "Usage: %s RMGDR-file [use-rainflag-flag u/v-flag] \n", 
	    argv[0]);
    exit (1);
  }
  
  if (argc>2)
    rfsw=atoi(argv[2]);

  if (argc>3)
    sw=atoi(argv[3]);

  if ( (fp=fopen(argv[1],"rb")) == NULL) {
    fprintf(stderr,"%s: Error opening %s\n", argv[0], argv[1]);
    exit(1);
  }
  
  if ( (n=fread(qscathdr,sizeof(qscathdr),1,fp)) != 1) {
    fprintf(stderr,"%s: Error reading header of %s\n",
	    argv[0], argv[1]);
    exit(1);
  }
  fprintf(stdout,"%% Output from file %s\n", argv[1]);

  if (sw==1) {
    fprintf(stdout,"%% Lon   Lat   Speed    Direction\n");
  } else {
    fprintf(stdout,"%% Lon   Lat   U        V\n");
  }

  while (!feof(fp)) {
    n=qread_rmgdr( fp, &qrec, QSCAT_REC_LEN);
    if (!n) {
      if (feof(fp)) exit(0);
      fprintf(stderr,"%s: Terminal error reading %s\n",
	      argv[0], argv[1]);
      exit(1);
    }
    time[24] = '\0';
    for (i=0;i<24;i++)
      time[i] = qrec.row_time[i];
    printf("---------- time: %s ----------------- \n", time);
    

    for (i=0;i<NCELLS;i++) {
      if (qrec.nambig[i]>0 && qrec.wvc_sel[i]>0){
	if (rfsw==1) {
	  rf= ( ((qrec.wvcqual_flag[i]>>12) & 3) == 2);
	  if (rf == 1)
	    continue;
	}
	s=qrec.wvc_sel[i]-1;
	lati=qrec.wvc_lat[i];
	loni=qrec.wvc_lon[i];
	speedi=qrec.windspd[i][s];
	diri=qrec.winddir[i][s];

	if (little_endian()) {
	  lati   = SWAP_S(lati) ;
	  loni   = SWAP_US(loni);
          speedi = SWAP_S(speedi);
          diri   = SWAP_US(diri);
	} 
	
	lat   = lati*0.01;
	lon   = loni*0.01;
        speed = speedi*0.01;
        dir   = diri*0.01;

	if (sw==1) {
	  printf("%d, %d, %d, %f, %f, %f, %f\n",r,i,s,lon,lat,speed,dir);
	} else {
	  u =  speed*sin(dir*DTOR);
	  v =  speed*cos(dir*DTOR);
	  printf("%d, %d, %d, %f, %f, %f, %f\n",r,i,s,lon,lat,u,v);
	}
      }
    }
    r+=1;
  }
}

int little_endian()
{
  short int word = 0x0001;
  char *byte = (char *) &word;
  return(byte[0] ? LITTLE_E : BIG_E);
}

