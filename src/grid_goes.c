/* Grid_Goes
 *
 * $Id$  
 *
 *    Modification Log: (After RCS)
 *
 *    $Log$
 *    Revision 1.3  2002/05/14 15:54:50  vapuser
 *    General cleanup
 *
 *    Revision 1.2  1998/10/22 21:46:09  vapuser
 *    Corrected rcsid flub
 *
 *    Revision 1.1  1998/10/22 21:43:58  vapuser
 *    Initial revision
 *:
 *
 *
 *    7/29/98  Added getopt processing (whd)
 *    12/11/97 make 8-bit (no shift) image data work  (mostly 16-bit, shift 5-bit)
 *   
 *    1/2/96  get year from system (careful about jday-1) 
 *    9/16/96 if fix hh, search closest one within 10 hh, or yesterday's  
 *
 *    9/12/96 add day tag (+n -n of current day) if just fix hour and 
 *            looking for the closest time 
 *
 *      handle whole/sub and goes8/9, paras[17] reorder 
 *     .Get the named file (month/day/hour) or the latest one  
 *     .Precision 0.02 (lat range <= 50) or 0.05 deg  
 *           (0.02 the best, 100km-1deg, 2km resolution for GOES)
 *     .Input: GOES type (8/9) and sub-area: minlat maxlat minlon maxlon
 *               or 0 0 0 0  for whole coverage 
 *     .Take care when minlon>maxlon and minlon*maxlon<0 ( crossing -180)
 *     .For sub-area, pick lin +- 200 and ele +- 1000 boundaries to cover the 
 *       required lat/lon subarea 
 *
 *     .GOES area data take 16-bit short intergers enough! (corresponding
 *        integer*2 in FORTRAN and intarr in IDL)
 *         ** if integer in C then integer in FORTRAN and lonarr in IDL !! 
 *     .pair_goes_() return 1 (nret) if initial navigation failed 
 *
 * whole coverage:     GOES8: lon (-140,-10)
 *                     GOES9: lon (160,-70) 
 * -------------------------------------------------------------------
 *  output: GOESy_xxxxxxx.dat    y: 8/9   xxxxxxx: jday,hhmm
 *  output content:
 *     1.Head line:goes_type, array lin/ele sizes, jday, time, minlat,minlon,
 *                maxlat,maxlon, and lat/lon precision
 *        -- 8 integers and 2 real numbers 
 *
 *     2.image data (ima_e,ima_l) 
 *        -- 16-bit integers
 *
 *Package: main_goes.c navig_goes.f pair_goes.f mcidas.h goes_gs map_goes.pro  
 *.....................................................................
 *note: GOES-8 area observed data in 2 bytes -- short int!! 
 *note: array (row,col)  while Fortran, IDL (col,row) !
 *note:  array start from 1 in Fortran, 0 in C and IDL !

 *
 *GOES raw data file structure: head block (64 4-byte), 
 *                              NAV block, 640*4 byte
 *                              CAL block, 512 byte
 *                              DATA block
 *---------------------------------------------------------------------*/

#include "mcidas.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <time.h>
#include <getopt.h>
#include <sys/types.h>
#include <dirent.h>


/* If you're going to change these macros, make sure that their sum+1 is divisible by 4 
 * so that the structure that has the output header in it is a nice size 
 */
#define MAX_PATH_LEN  128
#define MAX_FILE_LEN MAX_PATH_LEN-1
#define YYYYMMDD_FORMAT "%Y-%m-%d"
#define YYDOY_FORMAT "%Y-%j"
#define VERSION 19980730
#define OUTPUT_FILE_FORMAT "GOES%03d_%04d%02d%02d%02d_%04d,%03d,%04d,%03d.dat"

struct tm *timep;

time_t start,finish,t0, tt;
double t;

extern char *optarg;

extern int optind, opterr, optopt;


int main( int argc, char **argv) {

  static const char grid_goes_c_rcsid[]=
     "$Id$";
  FILE *fin,*fout;
  DIR  *dptr;
  char **ptr;
  struct dirent *dirent;
  short int *dat_data_int;  /* readin one line, 2-byte values  size=NELES*/
  char *dat_data_byt;  /* readin one line, 1-byte values  size=NELES*/
  short int *data;   /* sub-area array (pass to goes_gs.f) size=SLIN*SELE  */
  short int *ima,*iima; /* output array (pass to goes_gs.f) */
  int ima_l,ima_e; 

  int NLIN,NELE,NELES;  /* whole area */
  int SLIN,SELE,SELES;  /* sub-area */
  int LIN0,ELE0,RESL,RESE; /* start lin/ele , lin/ele resolution */
  int lin_start,lin_end,ele_start,ele_end;      /* area !! (start from 0) */ 
  int ilin_start,ilin_end,iele_start,iele_end; /* image !! (start from 1) */
  float minlat,maxlat,minlon,maxlon;   
  int  iminlat,imaxlat,iminlon,imaxlon;   
  long int pos;
  int f,i,j,file_no,nret;

  int dat_head[64],line_pre; 
  int dat_nav[640];       /* the NAV block */
  char buff[512];         /* the CAL block */

  int paras[17];

  /* 
  for pair_goes.f:
    0: SLIN0, 1: SELE0, 2: SLIN, 3: SELE, 4: RESL, 5: RESE, 6: SCALA, 7: SCALO,  
    8: minlat/minLIN 9: minlon/minELE,  10: maxlat/maxLIN 11: maxlon/maxELE 
    12: goes_type. 13: jday 14: time  15: ima_l 16: ima_e 
  */

  char filename[MAX_FILE_LEN+1],path[MAX_PATH_LEN+1],
      path0[MAX_PATH_LEN+1], time_string[7], areafileno_string[5],
      limit_string[MAX_PATH_LEN+1];
  char *goes_topdir;

  int area_no,area_start,flag,goes_type,goes_n1,goes_n2;
  int month,day,hour,yyday,jday,mmddhh,year,yr, doy;
  int ddd,hh,dddmax,hhmax,dtime,dat_bytes;
  
  /* Arguments for use with getopt */
  int path_set, file_set, time_set, areafileno_set, 
    filetype_set, goestype_set, sensortype_set, limit_set, outputfile_set;

  int ijunk;
  int areafileno,       /* the number, as parsed from cmdline */
    areafile_dirno,     /* the directory portion (i.e. 1st digit) */
    areafile_sensorno,  /* the sensor portion (2n digit) */
    areafile_areano;    /* the area number portion (last 2 digits)*/

  int goestype=10;   /* currently 8 or 10, Note that this it the type
                      *  specified on the command line, not the type as
                      *  given in the AREA file. 
		      */

  int sensortype=4;  /* sensor type (1=vis, 2=ir2, 3=ir3, 4=ir4 ) */
  char fullfilename[MAX_PATH_LEN+MAX_FILE_LEN+1], 
    outputfilename[MAX_PATH_LEN+MAX_FILE_LEN+1]; 

  int option;       /* the option returned by getopt(); */


  char sensordirs[][5] = {"vis/", "ir2/","ir3/","ir4/"};
  char goesdirs[][8] = {"goes8/","goes10/"};


  char dirent_test_string[MAX_FILE_LEN+1]; /* for directory parsing */


  /* misc variables */
  struct tm tm;
  char buf[100];
  int file_found=0;
  int test_ddd, test_hh;
  float test_time=-1, keep_time=-1;
  char test_file[MAX_PATH_LEN+MAX_FILE_LEN+1],
       tmpfile[MAX_PATH_LEN+MAX_FILE_LEN+1];


  /* Header for output file */
  struct header {
    int version;
    int type;
    int rows;
    int cols;
    int year;
    int doy;
    int hhmm;
    float resolution[2];
    float limits[4];
    char areafilename[MAX_PATH_LEN+MAX_FILE_LEN+1];
  } hdr;
  /* =============================================================
   * Begin Program
   * ============================================================= */




  /* Start timer */
  time(&t0);

#if 0
  /* comment out region 
  goes_topdir=getenv(GOES_TOPDIR_ENV_VAR);
  if (goes_topdir == NULL){
  }
# ifndef AREA_FILE_TOP
# define AREA_FILE_TOP "/disk4/vap/goes/"
# endif

  */
#endif
  
  ptr=argv;
  printf("Grid_goes called with arguments...\n");
  for (i=0;i<argc;i++) printf("%s\n",*ptr++);

  start = clock();
  timep = gmtime(&t0);
  yr    = timep->tm_year;
  year  = 1900+yr;
  doy   = timep->tm_yday+1;

  /* set up argument processing */
  (void) strncpy( time_string, "-1", 10 );
  memset(filename,(int) '\0', MAX_PATH_LEN+MAX_FILE_LEN+1);
  memset(path, (int) '\0', MAX_PATH_LEN+MAX_FILE_LEN+1);

  path_set = file_set = areafileno_set = 0;
  limit_set=outputfile_set=0;
  areafileno = areafile_areano = -1;
  minlat=maxlat=minlon=maxlon=0;


  /* Argument processing */
  while ( (option=getopt( argc, argv, "hn:f:d:t:g:s:l:o:")) != EOF ){
    switch (option) {
      case 'h':
          printf("Usage: \n\ngoes [-h] [-n abcd] [-d dir] [-f filename] [-l lon0,lat0,lon1,lat1] -o outputfile \n\n");
          printf(" -n xxyy: AREA file number where --\n");
          printf("    xx = 08 or 09 meaning, goes 10/8, respectively\n");
          printf("    yy is in range 00 to 99, inclusive\n");
          printf("-d dir: Specify directory\n");
	  printf("   Both -n and -d must be present if one is\n");
          printf("-f filename: Specify the FULLY QUALIFIED FILE NAME\n");
	  printf("-l lon0,lat0,lon1,lat1: Map limits.\n");
	  printf("    NO Spaces!! or it won't parse right\n");
	  printf("    West Longitude must be < 0, lon0,lat0 is lower left corner\n");
	  printf("    lon1,lat1 is upper right\n");
	  printf("    Defaults to 0,0,0,0, namely, grid the whole file\n\n");
	  printf("-o fqfn: the outpu FULLY QUALIFIED FILE NAME\n\n");
          printf("-h : prints this message \n\n");
	  printf(" You may specify the input file in one of two ways\n");
	  printf(" Either you use the combination (-n xxyy -d dir) or you use -f fqfn\n");
	  exit(0);
          break;
      case 'n':
        (void) strncpy( areafileno_string, optarg, 10);
	areafileno         = atoi(areafileno_string);
	areafile_dirno      = areafileno/1000;
	areafile_sensorno  = (areafileno - areafile_dirno*1000)/100;
	ijunk              = areafile_dirno*1000 + areafile_sensorno*100;
	areafile_areano    = areafileno - ijunk;
	sensortype         = areafile_sensorno;
	areafileno_set     = 1;
	break;
      case 'd':
        strncpy( path, optarg, MAX_PATH_LEN);
	if (path[ strlen(path)-1 ] != '/') 
	  strcat(path, "/");
        path_set=1;
        break;
      case 'f':
        strncpy( filename, optarg, MAX_FILE_LEN );
        file_set=1;
        break;
      case 'l':
        ijunk=sscanf( optarg, "%f,%f,%f,%f", &minlon, &minlat, &maxlon, &maxlat);
	if (ijunk != 4) {
	  fprintf(stderr," ERROR: parsing limitstring %s\n",optarg);
	  exit(1);
	}
        limit_set=1;
        break;
      case 'o':
	strncpy( outputfilename, optarg, 
		 MAX_PATH_LEN+MAX_FILE_LEN);
	outputfile_set=1;
        break;
      default:
        fprintf(stderr, "Unknown option, %c\n", option);
        exit(1);
	break;
    } /* end switch */
    
  } /*end while over cmd args */

  mmddhh=atoi(time_string);
  if (areafileno_set) {
    if (!path_set) {
      fprintf(stderr,"-n without -d: both or neither must be present!\n");
      exit(1);
    }
    strncpy( fullfilename, path, MAX_PATH_LEN );
    switch (areafile_dirno){
    case 8:
      strncat( fullfilename, goesdirs[1], 7 ); /* Goes 10 */
      goestype=10;
      break;
    case 9:
      strncat( fullfilename, goesdirs[0], 6 ); /* goes 8 */
      goestype=8;
      break;
    default: 
      fprintf(stderr,"ERROR: Invalid Directory number %d\n", areafile_dirno );
      exit(1);
    }
    if (sensortype >= 1  &&
	sensortype <= 4 ) {
      strncat( fullfilename, sensordirs[sensortype-1], 4 );
    } else {
      fprintf(stderr, "ERROR: invalid sensortype %d\n", sensortype );
      exit(1);
    }
    strcat( fullfilename, "AREA" );
    strcat( fullfilename, areafileno_string );
  } else {
  
    if (!file_set) {
      fprintf(stderr,"Must use either -f or (-n/-d)\n");
      exit(1);
    }
    strncpy( fullfilename, filename, MAX_PATH_LEN+MAX_FILE_LEN+1 );
  }

  /* Well, after all that, we should know the full name of the file we
   * want to open!  So, open it!  
   */

  /*  printf("Now opening %s for reading \n",fullfilename);*/
  if ( (fin=fopen( fullfilename, "rb")) == NULL ) {
    fprintf(stderr,"ERROR: Can't open file %s for reading \n",fullfilename );
    exit(1);
  }


  /*  printf("\n:: Now read in %s .. \n",fullfilename );*/

  /* Have to re-read the header block to make sure we're in the right
   * location */
  f=fread((int *)dat_head,4,64,fin);

  /* Don't care what anybody else says, the *real* timing info is in the file */
  year=dat_head[3]/1000. + 1900;
  doy=dat_head[3]-(dat_head[3]/1000)*1000;      /* Day of year */
  hour=dat_head[4]/10000;                /* hhmmss */


  /* temporary values, until I figure out what's wrong with strptime */
  month=1;
  day=1;

  doy2date( year, doy, &month, &day );

  /* Read the 'nav' block */
  f=fread((int *)dat_nav,4,640,fin);


  goes_type = goestype*10 + sensortype; 

  dat_bytes=dat_head[10]; /* image data in 16-bit or 8-bit */
  line_pre=dat_head[14]/dat_bytes; /* in int or char,length of the DATA block line prefix */
  NLIN=dat_head[8];
  NELE=dat_head[9];
  NELES=NELE+line_pre;

  LIN0=dat_head[5];
  ELE0=dat_head[6];
  RESL=dat_head[11];
  RESE=dat_head[12];

  paras[0]=LIN0;
  paras[1]=ELE0;
  paras[2]=NLIN;     
  paras[3]=NELE;
  paras[4]=RESL;
  paras[5]=RESE;
  paras[12]=goes_type;
  paras[13]=dat_head[3]-yr*1000;      /*jday*/
  paras[14]=dat_head[4]/100.;      /* hhmm , cut off sec in time*/
  /*
   * printf("\ncheck1: LIN0= %d ELE0= %d NLIN= %d NELE= %d res_l= %d res_e= %d",
   *	  LIN0,ELE0,NLIN,NELE,RESL,RESE);
   * printf("\ncheck2: Jday=%ld time=%ld line_pre=%d dat_nav[368]=%d\n\n",
   *       dat_head[3],dat_head[4],dat_head[14],dat_nav[368]);
   */
  
  f=fread((char *)buff,1,512,fin);
  /*
   * here1:
   * printf(">> Please input minlat,maxlat,minlon(lowest left),maxlon -- integers:\n ");
   * printf("  (Note: lon west - and 0 0 0 0 for the whole data area)\n   area limits= ");
   * scanf("%3d %3d %4d %4d",&minlat,&maxlat,&minlon,&maxlon);
  */
  if (maxlat<minlat) {
    /*puts(">> Error: must be minlat<=maxlat !");
    *goto here1;
    */
    fprintf(stderr,"ERROR: minlat>maxlat!\n");
    exit(1);
  }

  /* SET area boundaries: (for user, lon west - ; for inner process, west + !!) */
  paras[8]  = minlat;              /* inner-- paras[], user-- min/max lat/lon */
  paras[10] = maxlat;
  paras[9]  = -maxlon;
  paras[11] = -minlon;

  /*------------------------------------- SUB case ---------------- */
  if (maxlat>minlat) { 
    if ((minlon==maxlon)|| ((minlon>maxlon)&&(minlon*maxlon>0)))   {
      /* puts(">> Error: must minlon!=maxlon and minlon>maxlon only when crossing +-180)");
       * goto here1;
       */
      fprintf(stderr,"ERROR: misconfigured lon/lat ranges!\n");
      fprintf(stderr,"  minlon=maxlon OR minlon>maxlon and !(crossing +- 180)\n");
      fprintf(stderr,"  lonrange: %f, %f, latrange: %f, %f\n", 
	      minlon, maxlon, minlat, maxlat);
    }

    /****  use only paras[0 1 4 5 8 9 10 11] ***/
    nret=0;
    pair_goes_(dat_nav,paras,&nret);
    if (nret==1) {
      fprintf(stderr, "ERROR: Bad Return from pair_goes\n");
      exit(1);
    }
    /* return paras[8,9,10,11] -- minLIN/ELE maxLIN/ELE */
    
    ilin_start=paras[8];  
    iele_start=paras[9];  
    ilin_end=paras[10];     
    iele_end=paras[11];
    
    lin_end=(ilin_end-LIN0)/RESL+1+200; /* search larger lin/ele area */
    ele_end=(iele_end-ELE0)/RESE+1+1000;
    lin_start=(ilin_start-LIN0)/RESL+1-200;
    ele_start=(iele_start-ELE0)/RESE+1-1000;
    if (lin_start < 0) lin_start=0;
    if (ele_start < 0) ele_start=0;
    if (lin_end > NLIN) lin_end=NLIN;
    if (ele_end > NELE) ele_end=NELE;  

  } /* Come from if (maxlat>minlat) */



  if (maxlat==minlat)  {

    /* ============================================
     * User has chosen to interpolate the entire file 
     *============================================= */


    /***** use only paras[0 1 2 3 4 5 8 10] ***/
    nret=0;
    pair_goes_(dat_nav,paras,&nret);
    if (nret==1){
      fprintf(stderr, "ERROR: bad return from pair_goes\n");
      exit(1);
    }
    /* return min/max lat */
#ifdef DEBUG 
    printf("\n:: Estimated latitude boundaries (deg): %6.1f, %6.1f\n",
	   paras[8]/100.,paras[10]/100.);
#endif 
    
    /*------ SET area boundaries : */
    minlat=paras[8]/100;
    maxlat=paras[10]/100;
    if (dat_head[2] == 70) {
      maxlon=-10;
      minlon=-140;
    }
    if (dat_head[2] == 74) {
      maxlon=-70;
      minlon=160;
    }

    lin_start=0;
    ele_start=0;
    lin_end=NLIN;
    ele_end=NELE;
  } 

#ifdef DEBUG
  printf(":: search area line boundarie: (%d, %d)\n", lin_start,lin_end-1);
  printf(":: search area ele boundarie: (%d, %d)\n\n", ele_start,ele_end-1);
  ----------------------------------------------------*/
#endif 

  /*** set SLIN SELE SELES for sub-area ****/
  SLIN=lin_end-lin_start;
  SELE=ele_end-ele_start;
  SELES=SELE+line_pre;

  /*===== CASE1: read in image data in dat_bytes=2 bytes ======*/
  if (dat_bytes==2) {
    /**** allocate the data array: *********/
    if (
	((dat_data_int=(short int *)calloc(NELES,sizeof(short int)))==NULL)
	|| ((data=(short int *)calloc(SLIN*SELE,sizeof(short int)))==NULL)) {
      puts("space1 ? -- calloc failed");
      exit(1);
    }

    /*** cut off line prefix and shift 5-bit: */
    for (i=0;i<lin_end;i++) {
      f=fread((short int *)dat_data_int,2,NELES,fin); /* read one line each time */
      if (i>=lin_start)
	for (j=line_pre+ele_start;j<line_pre+ele_end;j++)
	  data[(i-lin_start)*SELE+j-(line_pre+ele_start)]=dat_data_int[j]/32; 
    }
    free(dat_data_int);
  }

  /*====== CASE2: read in image data in dat_bytes=1 bytes ======*/
  if (dat_bytes==1) {
    /**** allocate the data array: *********/
    if (((dat_data_byt=(char *)calloc(NELES,sizeof(char)))==NULL)
	|| ((data=(short int *)calloc(SLIN*SELE,sizeof(short int)))==NULL))
      {
	puts("space1 ? -- calloc failed");
	exit(1);
      }

    /*** cut off line prefix and no shift !! */
    for (i=0;i<lin_end;i++)
      {
	f=fread((char *)dat_data_byt,1,NELES,fin); /* read one line each time */
	if (i>=lin_start)
	  for (j=line_pre+ele_start;j<line_pre+ele_end;j++)
	    data[(i-lin_start)*SELE+j-(line_pre+ele_start)]=dat_data_byt[j];
      }
    free(dat_data_byt);
  }

  if (dat_bytes !=1 && dat_bytes !=2) {
    printf("ERROR: image data not in 16-bit or 8-bit, stopped processing\n");
    exit(1);
  }

  fclose(fin);

  /***** reSET paras[0][1][2][3] [6][7][8][9][10][11] [15][16] for navig_goes.f */
  paras[0]=LIN0+lin_start*RESL;
  paras[1]=ELE0+ele_start*RESE;
  paras[2]=SLIN;   
  paras[3]=SELE;

  if ((maxlat-minlat)<50) {
    paras[6]=50;  /* lat scale=1/precision (0.02) scale unit= pixel/deg */
    paras[7]=50;  /* lon scale */
  } else {
    paras[6]=20;  /* lat scale=1/precision (0.05)*/
    paras[7]=20;  /* lon scale */
  }
  paras[8]=minlat;    
  paras[9]=-maxlon; 
  paras[10]=maxlat;
  paras[11]=-minlon;

  /**** lat lon ranges: note crossing -180 case *********/
  ima_l=(maxlat-minlat)*paras[6];
  if (minlon>maxlon) ima_e=(360-(minlon-maxlon))*paras[7];
  if (minlon<maxlon) ima_e=(maxlon-minlon)*paras[7];
  paras[15]=ima_l;
  paras[16]=ima_e;

  if (
      ((ima=(short int *)calloc(ima_l*ima_e,sizeof(short int)))==NULL)
      || ((iima=(short int *)calloc(ima_l*ima_e,sizeof(short int)))==NULL)) {
    puts("space2 ? -- calloc failed");
    exit(1);
  }

  /* CALL navig_goes.f to navigate lin/ele to lat/lon 
   * This returns the navigated array in variable iima 
   * (in previous versions, this array was written out in navig_goes
   * but it's better to write it out here instead.
   */

  /* However, before we go into navig_goes, lets see if 
     we can open the output file. */
  if (!outputfile_set) {
    iminlon=(int)minlon;
    imaxlon=(int)maxlon;
    iminlat=(int)minlat;
    imaxlat=(int)maxlat;

    sprintf( outputfilename, OUTPUT_FILE_FORMAT, 
	     goes_type, year, month, day, hour, 
	     iminlon, iminlat, imaxlon, imaxlat );
  }
  if ( (fout=fopen( outputfilename, "wb" )) == NULL ) {
    fprintf(stderr,"ERROR: unable to open output file %s\n",
	    outputfilename);
    exit(1);
  }

  navig_goes_(dat_nav,data,paras,ima,iima);

  /* Write file */

  /*      OPEN(UNIT=60,FILE=outfilenm,
     +      FORM='UNFORMATTED')
      write(60) goes_type,ima_l,ima_e, paras(14),paras(15), 
     +           minlat,-lon1,maxlat,-lon0,preci_l,preci_e 
      write(60) ((iima(j,i),j=1,ima_e),i=1,ima_l) 
      close(60) */
      

  /* We're breaking the backwards compatibility here, since the older versions
   * of this data were written by the Fortran code navig_goes, in f77
   * format. But I think it's a small price to pay for cleaner output. 
   */

  strncpy( hdr.areafilename, fullfilename, MAX_PATH_LEN+MAX_FILE_LEN);
  hdr.version=VERSION;
  hdr.type=goes_type;
  hdr.rows=ima_l;
  hdr.cols=ima_e;
  hdr.year=year;
  hdr.doy=doy;
  hdr.hhmm=paras[14];
  hdr.resolution[0]=RESE;
  hdr.resolution[1]=RESL;
  hdr.limits[0]=minlon;
  hdr.limits[1]=minlat;
  hdr.limits[2]=maxlon;
  hdr.limits[3]=maxlat;

  fwrite( &hdr, sizeof(struct header), 1, fout);
  fwrite( iima, sizeof( short int ), ima_l*ima_e, fout );

  fclose(fout);
  free(data);
  free(ima);
  free(iima);

  endit:
  /*  finish=clock();
   * t=((double)(finish-start))/CLOCKS_PER_SEC;
   */
  printf("%s\n",outputfilename);
}



/************** MAIN END ***************/ 

int julday(mm,dd,yr)  /* get the julian days start from 0000*/
int mm,dd,yr;

{
  long ly,lm,lc,greg,la,lb,ls;

  /* Gregorian Calander was adopted on Oct. 15, 1582 */
  greg = 15 + 31 * (10 + 12 * 1582);

  if (yr < 0) yr++;
  if (mm > 2)
  {       ly =yr;
	  lm =mm + 1;
  }     else
  {       ly =yr - 1;
	  lm =mm + 13;
  }
  la =365.25*ly;
  lb=30.6001*lm;
  ls=la+lb+dd+1720995;
  if ((dd + 31 * (mm + 12 * yr)) > greg)
  {lc = 0.01 * ly;
  ls =ls+2-lc+0.25*lc;
  }
  return ls;
}


/******END END END END END*************************************/ 



/***  5 C code functions for pair_goes.f and navig_goes.f */

Fint4 ic_(void *buffer, Fint4 *offset) {
   unsigned char *buf;
   buf=buffer;
   return( (Fint4) buf[*offset]);
}


void stc_(Fint4 *val, void *buffer, Fint4 *offset) {
        unsigned char *buf;

	buf=buffer;
        buf[*offset] = *val;
}


/*   Return current day (YYDDD) */

void getday_(Fint4 *iday) {
   struct tm *timexx;
   time_t nowtime;

   time(&nowtime);
   timexx = gmtime(&nowtime);
   *iday = timexx->tm_year * 1000 + timexx->tm_yday + 1; 
}

/*   Return current time (HHMMSS) */
void gettim_(Fint4 *itime) {
   struct tm *timexx;
   time_t nowtime;

   time(&nowtime);
   timexx = gmtime(&nowtime);
   *itime = timexx->tm_hour * 10000 + timexx->tm_min * 100 + timexx->tm_sec;
}


/* Move num bytes from inbuf to outbuf (with offset) */

void movb_(Fint4 *num, void *inbuffer, void *outbuffer, Fint4 *offset) {
        long int off;
	unsigned char *outbuf;

	outbuf=outbuffer;
        off = *offset;
        memcpy(&outbuf[off] , inbuffer, *num);
}






