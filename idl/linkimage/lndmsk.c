#include <stdio.h>
#include <math.h>

#define PHYREC 1024
#define LPERP PHYREC/32
#define FILENAME "LANDMASK.DAT"
#define SIGN_OF(x) ((x)<0.0 ? -1 : 1)

FILE *lunfil;

/*
     FUNCTION MASK01 : RETURNS A LOGICAL FLAG INDICATING THAT THE
     ONE MINUTE GRID NEAREST TO THE LATITUDE AND LONGITUDE POSITION
     PASSED TO THE SUBROUTINE IS EITHER ON LAND OR WATER.  THE POINT
     MAY BE DEFINED AS DEGREES DECIMAL OR DEGREES AND MINUTES DECIMAL.
     THE FLAG RETURNED IS SET TO .TRUE. FOR LAND OR .FALSE. FOR WATER.
     THIS ROUTINE CALLS MASK60 TO GET THE DEGREE SQUARE MASK.

     If there's a problem in mask60 will set status=0 and return.
     This routine will return a -1 to the land_mask.c (which is an
     IDL linkimage routine). At the moment (Tue Apr 30 15:51:39 2002)
     the only possible circumstances under which this would occur is
     either the user doesn't have the required environmental variable
     VAP_LIBRARY defined or there was an an open failure on the data
     file $VAP_LIBRARY/LANDMASK.DAT.

     The caller of this routine must check the return status both of
     'status' and the returned value to see if there's been an error.
   

		THIS PROGRAM USES A DATA BASE DESIGNED BY:

	    JAN C. DEPNER                      JAMES A. HAMMACK
	    NAVAL OCEANOGRAPHIC OFFICE         NAVAL OCEAN R&D ACTIVITY
	    SYSTEMS TECHNOLOGY BRANCH          CODE 117
	    STENNIS SPACE CENTER, MS           STENNIS SPACE CENTER, MS
	    39522-5001                         39529-

	    AUTHOR : JAN C. DEPNER, 04/03/89

     ARGUMENTS :

	latd     -  (F) LATITUDE DEGREES OF THE POINT (SOUTH NEGATIVE)
	latm     -  (F) LATITUDE MINUTES OF THE POINT (UNSIGNED UNLESS
		    LATD IS 0.0)
	lond     -  (F) LONGITUDE DEGREES OF THE POINT (WEST NEGATIVE OR
		    0 TO 360)
	lonm     -  (F) LONGITUDE MINUTES OF THE POINT (UNSIGNED UNLESS
		    LOND IS 0.0)

     VARIABLES :

	mask     -  (C) ARRAY CONTAINING THE ONE MINUTE GRID OF LAND
		    MASK POINTS FOR THE ONE DEGREE CELL THAT CONTAINS
		    THE LATITUDE AND LONGITUDE POINT REQUESTED
	latp     -  (I) LATITUDE INDEX INTO MASK ARRAY
	lonp     -  (I) LONGITUDE INDEX INTO MASK ARRAY
	lat      -  (F) LATITUDE IN DECIMAL DEGREES
	lon      -  (F) LONGITUDE IN DECIMAL DEGREES
*/

/*
 * Modification Log:
 *
 * $Log$
 * Revision 1.2  1999/04/09 23:05:38  vapuser
 * Changed location of LANDMASK.DAT file
 *
 * Revision 1.1  1999/04/09 22:56:57  vapuser
 * Initial revision
 *:
 *
 *
 * $Id$
 */

int mask01( float *latd, float *latm, float *lond, float *lonm, 
	    int *first, int *status)
{
  unsigned static char mask[3600];
  signed static char alflag;
  signed static char flag60;
  float lat, lon,t;
  double dummy;
  int dlat, dlon, latp, lonp;
  static int plat = -1, plon = -1;
  static char lndmsk_rcsid[]="$Id$";
  
  void mask60();
  
  /* CONVERT LAT AND LON TO DEGREES DECIMAL, ADJUST TO NEAREST MINUTE  */
  
#ifdef DEBUG
  printf("In lndmsk\n");
  printf("lndmsk: latd,latm,lond,lonm = %f,%f,%f,%f\n",
	 *latd,*latm,*lond,*lonm);
#endif


  if (*first) flag60=127;
  
  lat = *latd + (*latm/60.0) * SIGN_OF(*latd) + 90.0;
  lon = *lond + (*lonm/60.0) * SIGN_OF(*lond);


#ifdef DEBUG
  printf("lat,lon = %f, %f\n", lat, lon);
#endif
  
  if (lon<0.0) lon += 360.0;
  lat += .00833333333333;
  lon += .00833333333333;


  /*
   * IF THIS IS IN THE SAME ONE DEGREE SQUARE AS THE LAST ACCESS,
   * DO NOT READ A NEW MASK
   */


  dlat = (int)lat;
  dlon = (int)lon;
  if (dlon!=plon || dlat!=plat) 
    {
      
      
      /* GET THE ONE DEGREE MASK   */
      
      t=lat-90.0;
      mask60( mask, &t, &lon, &status, &flag60 );
      if (*status != 1)
	return (-1);
    }
  
#ifdef DEBUG
  printf("after mask60: flag60 = %ld\n", flag60 );
#endif
  
  /*
   * IF THE ALL LAND OR WATER FLAG IS NOT SET, GET THE SINGLE POINT
   * FROM THE MASK
   */


  if (flag60<0) 
    {
      latp = modf((double)lat,&dummy)*60.0;
      lonp = modf((double)lon,&dummy)*60.0;
      alflag = mask[(int)latp*60+(int)lonp];
    }
  else 
    {
      alflag = flag60;
    }



  /* SAVE THE PREVIOUS LAT AND LON DEGREES */


  plat = dlat;
  plon = dlon;


  /* SET THE LOGICAL VALUE BASED ON THE SETTING OF ALFLAG */


  
#ifdef DEBUG
  printf("mask01: returning 0\n");
#endif
  
  if (alflag) 
    return (1);
  else 
    return (0);
  
}

/*
      FUNCTION MASK60 : GIVEN A REAL LATITUDE AND LONGITUDE IN DEGREES
      (WEST LONGITUDE NEGATIVE OR 0 TO 360 LONGITUDE AND SOUTH LATITUDE
      NEGATIVE) THIS SUBROUTINE RETURNS A CHARACTER ARRAY CONTAINING A
      ONE MINUTE LAND MASK FOR THE ONE DEGREE CELL THAT THE LATITUDE
      AND LONGITUDE POINT IS IN.  THE CHARACTER ARRAY CONTAINS 3600
      CHARACTERS AND IS SET UP AS 'UNSIGNED CHAR[60][60]'.  EACH
      CHARACTER IS SET TO EITHER '1' OR '0'.  IF ALL THE POINTS ARE
      LAND OR ALL ARE WATER, THE 1 CHARACTER FLAG 'ALFLAG' WILL BE SET
      TO THE CORRESPONDING VALUE.  IF BOTH LAND AND WATER POINTS ARE
      PRESENT, 'ALFLAG' WILL BE SET TO '-1'.  THE MASK IS SET UP WITH
      POINT 'MASK[0][0]' BEING THE SOUTHWEST CORNER OF THE CELL AND POINT
      'MASK[59][59]' THE NORTHEAST CORNER.  THE FIRST SUBSCRIPT REPRESENTS
      THE LONGITUDE MINUTES, THE SECOND IS LATITUDE MINUTES (WITH THE
      COORDINATE SYSTEM SET UP AS 0 - 360 LONGITUDE AND 0 - 180 LATITUDE).

      If there's a failure in opening the database file
      (LANDMASK.DAT), this routine will set status=0 and return. This
      will occur in only two circumstances: 1. the user doesn't have
      the environmental variable VAP_LIBRARY defined or 2, there was a
      failure in the attempt to open $VAP_LIBRARY/LANDMASK.DAT'


		 THIS PROGRAM USES A DATA BASE DESIGNED BY:

	     JAN C. DEPNER                      JAMES A. HAMMACK
	     NAVAL OCEANOGRAPHIC OFFICE         NAVAL OCEAN R&D ACTIVITY
	     SYSTEMS TECHNOLOGY BRANCH          CODE 117
	     STENNIS S6ACE CENTER, MS           STENNIS SPACE CENTER, MS
	     39522-5001                         39529-

	     AUTHOR : JAN C. DEPNER, 04/03/89
      ARGUMENTS :

	 mask     -  (C) ARRAY CONTAINING THE ONE MINUTE GRID OF LAND
		     MASK POINTS FOR THE ONE DEGREE CELL THAT CONTAINS
		     THE LATITUDE AND LONGITUDE POINT REQUESTED
	 lat      -  (F) LATITUDE DEGREES OF THE POINT (0 - 180)
	 lon      -  (F) LONGITUDE DEGREES OF THE POINT (0 - 360)
	 status   -  (I) =1 if success, 0 otherwise. Used by caller 
                      to determine whether mask01 should return
                      immediately to IDL.
	 alflag   -  (C) ALL LAND/WATER FLAG RETURNED 
	 
      VARIABLES :

	 PHYREC   -  (I) CONSTANT SET TO NUMBER OF BYTES IN A PHYSICAL
		     RECORD IN THE DATA BASE.
	 irec10   -  (C) ARRAY CONTAINING AN INPUT RECORD FROM THE
		     DATA BASE.
	 LPERP    -  (I) LOGICAL RECORDS PER PHYSICAL RECORD.
	 rec00    -  (I) LOGICAL ADDRESS OF THE ONE DEGREE DATA RECORD
		     (NOT USED IN THIS ROUTINE).
	 rec01    -  (I) LOGICAL ADDRESS OF THE ONE DEGREE CELL MAP.
	 paddr    -  (I) PREVIOUS PHYSICAL RECORD ADDRESS.
*/

void mask60(char mask[], float *lat, float *lon, int* status, signed char *flag )
{

  static long rec00, rec01;
  static int first = 1, plat = -1, plon = -1;
  int clat, clon;
  char *dir;
  int l=len(dir);
  char filename[256];

  void rdten(), rdone(), allmsk(), bldmsk();
  
  status=1; /* hope for the best */

  /* IF FIRST TIME THROUGH, OPEN FILE AND SET LPERP */


#ifdef DEBUG
  printf("mask60, lat, lon, flag = %f,%f,%d\n", *lat, *lon, *flag);
#endif

  if (first) 
    {
      
      dir = getenv("VAP_LIBRARY");
      if (dir == NULL)
	{
	  *status=0;
	  return
	}
      memset(filename,'\0',256);
      strncpy(filename,dir,l);
      strncat(filename,"/",1);
      strncat(filename,"LANDMASK.DAT",256-l-2);
      lunfil = fopen(filename,"r");
      if (lunfil == NULL)
	{
	  *status=0;
	  return;
	}
      first = 0;
    }
  /*
    IF LAT >= 90 SET TO ALL WATER AND RETURN
  */
  
  if (*lat >= 90.0) 
    {
      *flag = 0;
#ifdef DEBUG
      printf("mask60: going into allmsk when lat > 90 n");
#endif
      
      allmsk(flag, mask); 
      return;
    }


  /*
    CALCULATE SOUTHWEST CORNER OF CELL
  */


  clat = (int)(*lat+90.0);
  if (*lon<0.0) 
    {
      clon = (int)(*lon+360.0);
    }
  else 
    {
      clon = ((int) *lon)%360;
    }


  /*
   * IF THE LAT AND LON ARE NOT IN THE SAME ONE DEGREE SQUARE AS THE
   * PREVIOUS LAT AND LON, OR ALFLAG = 127 (FIRST CALL FROM MASK01),
   * PROCESS THE CELL
  */


  if (clon!=plon || clat!=plat || *flag==127) 
    {
      *flag = ' ';

      /* READ THE TEN DEGREE SQUARE MAP AND CHECK FOR ALL LAND OR WATER     */
      
      rdten(lunfil,&rec01,clat,clon,flag);
      
#ifdef DEBUG
      printf(
	     "mask60: Just back from rdten,  &flag, *flag (int) *flag= %ld, %ld ,%ld\n", 
	     flag, *flag, (int) *flag);
#endif
      
      
      if (*flag>=0) 
	{
	  
	  
#ifdef DEBUG
	  printf("mask60: going into allmsk after rdten\n");
#endif
	  
	  allmsk(flag, mask); 
	}
      else 
	{
	  /*
	    READ THE ONE DEGREE CELL MAP AND CHECK FOR ALL LAND OR WATER
	  */
	  
	  
#ifdef DEBUG
	  printf("mask60: going into rdone: \n");
#endif
	  
	  
	  rdone(lunfil,rec01,&rec00,clat,clon, flag);
	  
#ifdef DEBUG
	  printf(
		 "mask60: just back from rdone, &flag,*flag, (int) flag= %ld, %ld %ld\n", 
		 flag, *flag, (int) *flag );
#endif
	  
	  if ( (int) *flag>=0) 
	    {
	      
#ifdef DEBUG
	      printf("mask60: going into allmsk: after rdone \n");
#endif
	      
	      allmsk(flag, mask); 
	    }
	  else 
	    {
	      
#ifdef DEBUG
	      printf("mask60: going into bldmask: \n");
#endif
	      
	      /*
		READ THE RUN LENGTH DATA FOR THE ONE DEGREE CELL AND BUILD
		THE MASK
	      */
	      bldmsk(lunfil,rec00,rec01,mask);
	    }
	}
      
      /*
	SAVE THE LAT AND LON DEGREES
      */
      
      plat = clat;
      plon = clon;
    }
}

/*
      FUNCTION RDTEN : READS THE TEN DEGREE SQUARE MAP AT THE
      BEGINNING OF THE DATA BASE.  RETURNS A FLAG INDICATING IF THE
      ENTIRE SQUARE WAS EITHER LAND OR WATER.  IF NOT, THE VARIABLE
      'REC01' IS SET TO THE LOGICAL ADDRESS OF THE ONE DEGREE CELL MAP.
      THE FLAG RETURNED IS SET TO '1' FOR LAND OR '0' FOR WATER.

	     AUTHOR : JAN C. DEPNER, 04/03/89

      ARGUMENTS :

	 lunfil   -  (I) INPUT FILE HANDLE
	 rec01    -  (L) RECORD NUMBER OF ONE DEGREE CELL MAP
	 lat      -  (F) LATITUDE DEGREES OF THE POINT (0 - 180)
	 lon      -  (F) LONGITUDE DEGREES OF THE POINT (0 - 360)
	 tenflg   -  (C) ALL LAND/WATER FLAG RETURNED
	 
      VARIABLES :

	 PHYREC   -  (I) CONSTANT SET TO NUMBER OF BYTES IN A PHYSICAL
		     RECORD IN THE DATA BASE.
	 irec10   -  (C) ARRAY CONTAINING AN INPUT RECORD FROM THE
		     DATA BASE.
	 LPERP    -  (I) LOGICAL RECORDS PER PHYSICAL RECORD.
	 rec00    -  (I) LOGICAL ADDRESS OF THE ONE DEGREE DATA RECORD
		     (NOT USED IN THIS ROUTINE).
	 rec01    -  (I) LOGICAL ADDRESS OF THE ONE DEGREE CELL MAP.
	 paddr    -  (I) PREVIOUS PHYSICAL RECORD ADDRESS.
*/

void rdten(FILE* lunfil, long *rec01, int lat, int lon, signed char *tenflg)
{

	static long paddr = -1, pword = -1;
	static unsigned char irec10[PHYREC];
	long addr, word, lrec, pnt, lstat, *tmp_ptr;

/*
        COMPUTE THE WORD (4 BYTE) POSITION WITHIN THE 648 WORD TEN
        DEGREE SQUARE MAP, MAP STARTS IN THE 2ND LOGICAL RECORD
*/
#ifdef DEBUG
	printf("rdten: \n");
#endif

	word = (lat/10)*36+lon/10;
#ifdef DEBUG
	printf("pword, word = %ld, %ld\n", pword, word);
#endif

/*
        IF THE WORD POSITION HAS CHANGED GET THE NEW ONE DEGREE CELL
        LOGICAL ADDRESS
*/
	if (word!=pword) {
/*
	   COMPUTE THE LOGICAL AND PHYSICAL ADDRESS OF THE ONE DEGREE
	   CELL POINTER WITHIN THE TEN DEGREE MAP
*/
	   lrec = (word*4)/32+1;
	   addr = (lrec/(long)(LPERP))*PHYREC;
#ifdef DEBUG
	   printf("lrec, addr = %ld, %ld\n", lrec, addr );
#endif

/*
	   IF THE PHYSICAL ADDRESS HAS CHANGED, READ A NEW RECORD
*/
	   if (addr!=paddr) {
	      lstat = fseek(lunfil,addr,0);
	      lstat = fread(irec10,PHYREC,1,lunfil);
	      paddr = addr;
	   }
/*
	   COMPUTE THE BYTE POSITION OF THE ONE DEGREE CELL LOGICAL
	   ADDRESS WITHIN THE PHYSICAL RECORD
*/
	   pnt = ((word+8)*4)%PHYREC;
#ifdef DEBUG
	   printf("pnt = %ld\n", pnt);
#endif


/*
	   BUILD THE LOGICAL ADDRESS FROM THE 4 BYTES AT 'PNT'
*/
	   /* old method
	   *rec01 = irec10[pnt]*16777216+irec10[pnt+1]*65536+
	   irec10[pnt+2]*256+irec10[pnt+3];
	   */
	   *rec01 = *( (long * ) (irec10 + pnt) );
#ifdef DEBUG
	   printf("rec01 = %ld\n", *rec01 );
#endif


	   pword = word;
	}
/*
        IF THE TEN DEGREE SQUARE IS ALL LAND OR WATER, SET THE FLAG
*/
	if (*rec01<3) {
	   *tenflg = (char) *rec01%2;
	}
	else {
	   *tenflg = -1;
	}
#ifdef DEBUG
	printf(
	 "returning from rdten: rec01, tenflag, rec01%2 = %ld, %ld,%ld\n", 
	       *rec01, *tenflg, *rec01%2 );
#endif

	return ;
}

/*
      FUNCTION RDONE : READS THE ONE DEGREE CELL MAP.  RETURNS A
      FLAG THAT INDICATES IF THE ENTIRE SQUARE IS EITHER LAND OR
      WATER.  IF NOT, THE VARIABLE 'REC00' IS SET TO THE LOGICAL
      ADDRESS OF THE ONE DEGREE DATA RECORD.  THE FLAG RETURNED IS
      SET TO 1 FOR LAND OR 0 FOR WATER.

	     AUTHOR : JAN C. DEPNER, 04/03/89

      ARGUMENTS :

	 lat      -  (R) LATITUDE DEGREES OF THE POINT (0 - 180)
	 lon      -  (R) LONGITUDE DEGREES OF THE POINT (0 - 360)
	 oneflg   -  (C) ALL LAND/WATER FLAG RETURNED

      VARIABLES :

	 PHYREC   -  (I) CONSTANT SET TO NUMBER OF BYTES IN A PHYSICAL
		     RECORD IN THE DATA BASE.
	 celmsk   -  (I) ARRAY CONTAINING THE LOGICAL ADDRESSES OF
		     THE ONE DEGREE CELL DATA RECORDS.
	 irec01   -  (C) ARRAY CONTAINING AN INPUT RECORD FROM THE
		     DATA BASE.
	 LPERP    -  (I) LOGICAL RECORDS PER PHYSICAL RECORD.
	 rec00    -  (I) LOGICAL ADDRESS OF THE ONE DEGREE DATA RECORD
		     (NOT USED IN THIS ROUTINE).
	 rec01    -  (I) LOGICAL ADDRESS OF THE ONE DEGREE CELL MAP.
	 paddr    -  (I) PREVIOUS PHYSICAL RECORD ADDRESS.
*/

void rdone( FILE * lunfil, long rec01,long * rec00, int lat, int lon, 
	    signed  char * oneflg)
{

	static long celmsk[10][10], paddr = -1, prec01 = -1;
	static unsigned char irec01[PHYREC];
	static int power[] = {1,2,4,8,16,32,64,128};
	long addr, cell, lstat;
	int sum, bpos, byt, bit, set, i, j, latp, lonp;

/*
        COMPUTE THE CELL POSITION WITHIN THE PHYSICAL RECORD AND THE
        PHYSICAL RECORD ADDRESS
*/
#ifdef DEBUG
	printf("rdone: rec01, rec00, lat, lon, ondflg = %ld,%ld,%ld,%ld,%ld\n",
	       rec01, *rec00, lat, lon, *oneflg );
#endif

	cell = (rec01%(long)(LPERP))*32;
	addr = (rec01/(long)(LPERP))*PHYREC;
/*
        IF LOGICAL RECORD HAS CHANGED, BUILD A NEW CELL MASK
*/
	if (rec01!=prec01) {
/*
 	   IF THE ADDRESS HAS CHANGED, READ A NEW RECORD
*/
	   if (addr!=paddr) {
/*printf ("addr = %d, paddr = %d\n",addr,paddr); */
	      lstat = fseek(lunfil,addr,0);
	      lstat = fread(irec01,PHYREC,1,lunfil);
	      paddr = addr;
	   }
/*
	   BUILD THE CELL MASK FROM THE 2 BIT RECORDS WITHIN THE LOGICAL
	   RECORD; 00 - BOTH LAND AND WATER, 01 - LAND, 10 - WATER.  IF
	   THE 2 BIT RECORD IS 00, SET THE CELL MASK VALUE TO THE LOGICAL
	   ADDRESS OF THE ONE DEGREE DATA RECORD
*/
	   sum = 0;
	   for (i=0; i<=9; i++) {
	      for (j=0; j<=9; j++) {
/*
	         COMPUTE THE BIT POSITION WITHIN THE PHYSICAL RECORD, THE
	         BYTE POSITION WITHIN THE PHYSICAL RECORD, AND THE BIT
	         POSITION WITHIN THE BYTE
*/
		 bpos = (i*10+j)*2;
		 byt = bpos/8;
		 bit = bpos%8;
		 set = (int) (( ( (int) irec01[cell+byt])/power[bit])%4);
		 if (!set) {
		    sum++;
		    celmsk[j][i] = rec01+sum;
		 }
		 else {
		    celmsk[j][i] = set;
		 }
	      }
	   }
	   prec01 = rec01;
	}
/*
        GET THE ADDRESS, OR GET THE FLAG IF ALL LAND OR WATER
*/
	lonp = lon%10;
	latp = lat%10;
	if (celmsk[lonp][latp]>2) {
	   *oneflg = -1;
	   *rec00 = celmsk[lonp][latp];
	}
	else {
	   *oneflg = (char) celmsk[lonp][latp]%2;
#ifdef DEBUG 
	   printf("rdone: oneflg, celmsk = %ld,%ld\n", 
		  *oneflg, celmsk[lonp][lat]%2);
#endif 
	}
	return;
}

/*
      FUNCTION ALLMSK : SET THE ENTIRE MASK TO EITHER 'L' OR 'W'.

	     AUTHOR : JAN C. DEPNER, 04/03/89

      ARGUMENTS :

	 mask     -  (C) ARRAY CONTAINING THE ONE MINUTE GRID OF LAND
		     MASK POINTS FOR THE ONE DEGREE CELL THAT CONTAINS
		     THE LATITUDE AND LONGITUDE POINT REQUESTED
	 alflag   -  (C) ALL LAND/WATER FLAG
*/

void allmsk(char *flag, char mask[])
{

	int i, j;

#ifdef DEBUG
	printf("allmsk: \n");
	printf("flag = %ld \n", *flag);

#endif


	for (i=0; i<=59; i++) {
	   for (j=0; j<=59; j++) {
#ifdef DEBUG
/*           printf("i,j, flag = %ld,%ld,%ld\n",i,j, *flag);
	     printf("mask[i*60+j] = %ld\n",mask[i*60+j] ); */
#endif
	      mask[i*60+j] = *flag;
	   }
	}
#ifdef DEBUG
	printf("allmsk: returning \n");
#endif
	return;
}

/*
      FUNCTION BLDMSK : BUILDS THE MASK FROM THE ONE DEGREE DATA
      RECORDS IN THE DATA BASE.

	     AUTHOR : JAN C. DEPNER, 04/03/89

      ARGUMENTS :

	 mask     -  (C) ARRAY CONTAINING THE ONE MINUTE GRID OF LAND
		     MASK POINTS FOR THE ONE DEGREE CELL THAT CONTAINS
		     THE LATITUDE AND LONGITUDE POINT REQUESTED

      VARIABLES :

	 PHYREC   -  (I) CONSTANT SET TO NUMBER OF BYTES IN A PHYSICAL
		     RECORD IN THE DATA BASE.
	 irec00   -  (C) ARRAY CONTAINING AN INPUT RECORD FROM THE
		     DATA BASE.
	 LPERP    -  (I) LOGICAL RECORDS PER PHYSICAL RECORD.
	 rec00    -  (I) LOGICAL ADDRESS OF THE ONE DEGREE DATA RECORD
		     (NOT USED IN THIS ROUTINE).
	 rec01    -  (I) LOGICAL ADDRESS OF THE ONE DEGREE CELL MAP.
	 paddr    -  (I) PREVIOUS PHYSICAL RECORD ADDRESS.
*/

void bldmsk( FILE *lunfil, long rec00, long rec01, char mask[])
{

	static long paddr = -1;
	static unsigned char irec00[PHYREC];
	int start, type, count, finish, j;
	long addr, pos, lstat;
	unsigned char mbyte, dflag;

	void movpos();

	start = 0;
/*
        COMPUTE THE FIRST BYTE POSITION WITHIN THE PHYSICAL RECORD, AND THE
        PHYSICAL RECORD ADDRESS
*/
#ifdef DEBUG
	printf("Just entered bldmsk:\n");
#endif

	pos = (rec00%(long)(LPERP))*32;
	addr = (rec00/(long)(LPERP))*PHYREC;
/*
        IF THE ADDRESS HAS CHANGED, READ A NEW RECORD
*/
	if (addr!=paddr) {
/*printf ("paddr = %d, addr = %d rat farts\n",paddr,addr);*/
	   lstat = fseek(lunfil,addr,0);
	   lstat = fread(irec00,PHYREC,1,lunfil);
	   paddr = addr;
	}
/*
        MAIN LOOP, GETS TYPE AND COUNT FROM BYTES.  FIRST TWO BITS ARE TYPE
        FLAGS; 00 - 1 BYTE WATER, 01 - 1 BYTE LAND, 10 - 2 BYTE WATER,
        11 - 2 BYTE LAND.  NEXT 6 OR 14 BITS ARE COUNT
*/
	mbyte = irec00[pos];
	while (mbyte) {
/*
	   IF THIS IS A TWO BYTE RECORD, SET DFLAG
*/
	   if (mbyte>=128) {
	      mbyte = mbyte-128;
	      dflag = 1;
	   }
	   else {
	      dflag = 0;
	   }
/*
	   GET THE TYPE AND COUNT
*/
	   type = (int) mbyte/64;
	   count = (int) mbyte-type*64;
/*
	   IF THIS IS A TWO BYTE RECORD, GET THE REST OF THE COUNT FROM
	   THE NEXT BYTE
*/
	   if (dflag) {
	      movpos(lunfil,&pos,irec00,&paddr,rec01);
	      count = count*256+irec00[pos];
	   }
/*
	   LOOP THROUGH THE MASK AND SET THE CHARACTERS
*/
	   finish = start+count-1;
	   for (j=start; j<=finish; j++) {
	      mask[j] = type;
	   }
/*
	   MOVE THE BYTE POSITION AND GO TO TOP OF LOOP
*/
	   movpos(lunfil,&pos,irec00,&paddr,rec01);
	   start = finish+1;
	   mbyte = irec00[pos];
	}
/*
        FLIP THE TYPE FLAG AND SET THE REMAINDER OF THE MASK
*/
	type = 1-type;
	for (j=start; j<=3599; j++) {
	   mask[j] = type;
	}
	return;
}

/*
      FUNCTION MOVPOS : INCREMENTS THE BYTE POSITION AND CHANGES
      TO THE NEXT OVERFLOW RECORD IN THE CHAIN IF NECCESSARY.

	     AUTHOR : JAN C. DEPNER, 04/03/89

      ARGUMENTS :

	 pos      -  (I) BYTE POSITION WITHIN THE PHYSICAL RECORD.
	 irec00   -  (C) ARRAY CONTAINING AN INPUT RECORD FROM THE
		     DATA BASE.
	 paddr    -  (I) PREVIOUS PHYSICAL RECORD ADDRESS.

      VARIABLES :

	 PHYREC   -  (I) CONSTANT SET TO NUMBER OF BYTES IN A PHYSICAL
		     RECORD IN THE DATA BASE.
	 LPERP    -  (I) LOGICAL RECORDS PER PHYSICAL RECORD.
	 rec00    -  (I) LOGICAL ADDRESS OF THE ONE DEGREE DATA RECORD
		     (NOT USED IN THIS ROUTINE).
	 rec01    -  (I) LOGICAL ADDRESS OF THE ONE DEGREE CELL MAP.
*/

void movpos(FILE* lunfil,long *pos, unsigned char irec00[], long *paddr, long rec01)
{

	long lrec, addr, lstat;

	(*pos)++;
/*
        IF THIS IS THE END OF THE LOGICAL RECORD, GET THE OVERFLOW POINTER
        FROM THE LAST 2 BYTES OF THE RECORD
*/
	if ((*pos+2)%32==0) {
	   lrec = irec00[*pos]*256+irec00[*pos+1]+rec01;
/*
	   IF LREC = REC01, THERE IS NO OVERFLOW POINTER AND THIS IS THE
	   END OF THE CHAIN
*/
	   if (lrec!=rec01) {
/*
	      COMPUTE THE PHYSICAL ADDRESS, IF IT HAS CHANGED, READ A NEW
	      RECORD
*/
	      addr = (lrec/(long)(LPERP))*PHYREC;
	      if (addr!=*paddr) {
		 lstat = fseek(lunfil,addr,0);
		 lstat = fread(irec00,PHYREC,1,lunfil);
		 *paddr = addr;
	      }
/*
	      COMPUTE THE NEW BYTE POSITION WITHIN THE PHYSICAL RECORD
*/
	      *pos = (lrec%(long)(LPERP))*32;
	   }
	}
	return;
}
