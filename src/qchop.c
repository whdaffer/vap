/* 
 * qchop.c
 *
 * Chop the sigma0s off the end of each MGDR record and write the
 * truncated result out to a file in the current working directory
 * whose name is QSYYYYDDMM.SHHM.EHHMM where the SHHMM is the starting
 * hour/minute and the EHHMM is the ending hour/minute. This name is
 * constructed from information contained in the header of the MGDR
 * file, specifically the DataStartTime and the DataEndTime.
 *
 *
 * $Id$
 *
 * Modifications:
 * $Log$
 * Revision 1.2  2000/06/08 19:35:39  vapuser
 * Added code to exit with error if nudging_method = "Highest-ambiguity
 * initialization", i.e. the VAN model field wasn't used to initialize
 * the matrix for some reason or other.
 *
 * Revision 1.1  2000/02/10 21:02:24  vapuser
 * Initial revision
 *
 * */
#define BUFD  4
#define BUFW 76
#define BUFL 1800
#define AMBIGS 4
/* #define MGDRSIZE 28 + BUFW*(13 + 26*BUFD + 10*AMBIGS)
 * #define WINDSIZE 28 + 12*BUFW + 10*BUFW*AMBIGS
 */
#define MGDRSIZE 13252
#define OLDMGDRSIZE 11960
#define OLDWINDSIZE 3980
#define DELTA MGDRSIZE-OLDMGDRSIZE
#define WINDSIZE OLDWINDSIZE + DELTA

/* Since the new stuff, which we want to keep, is at the end of the
 * MGDR record, which we previsously just threw away, we have to read
 * in a record, write out the first part, then jump to the end and
 * write out the end part. These are some macros that help do that.  
 */


#define JUNKSIZE OLDMGDRSIZE-OLDWINDSIZE
#define PART_1_SIZE OLDWINDSIZE
#define PART_2_SIZE DELTA

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

void main( int argc,char **argv ) {

  int c;
  extern char *optarg;
  extern int optind;
  int noavnok=0;


  int i,j,k,rec=0;
  FILE *ip,*op;
  char *p,*p1, *p2, *m,*w,*t;
  char buf1[81];
  char ofile[1025];
  char ymd[81], hm[81];
  int year,doy,month,day,hour,min,sec;
  int ws=WINDSIZE;
  int ms=MGDRSIZE;
  int oms=OLDMGDRSIZE;
  int delta=DELTA;
  int junksize=JUNKSIZE;
  int part_1_size=PART_1_SIZE;
  int part_2_size=PART_2_SIZE;
  
  struct tm tp;

  union iorec {
    char mgdr[MGDRSIZE];
    struct wind {
      char wind1[PART_1_SIZE];
      char junk[JUNKSIZE];
      char wind2[PART_2_SIZE];
    } rnoaa;
  } ior ;

  c = getopt(argc, argv, "n");
  if (c == 'n')
    noavnok=1;

  if (argc<2) 
    {
      fprintf(stdout,"Usage: qchop [-n] QS_NRT_FILE\n");
      exit(1);
    }

  if ( (ip=fopen((const char *) argv[1],"rb"))==NULL) 
    {
      fprintf(stderr,"Error opening file %s\n", *(argv+1) );
      exit(1);
    }


  /* Check the `nudging_method'.  Throw the file out if it's the
   * `Highest-ambiguity initialization' (this means the the AVN model
   * field wasn't available!) unless the noavnok flag is set.  
   */

  if (!noavnok) {
    p=buf1;
    do {
      p=fgets(p,80,ip);
      if (p == NULL) {
	fprintf(stderr, "<nudging_method> Truncated File! Exiting\n");
	exit(1);
      }
      p1 = strstr(p, "nudging_method" );
      p2 = strstr(p,"spare_metadata_element");
    } while (p1 == NULL && p2 == NULL);
    
    if (p2 != NULL) 
      {
	fprintf(stderr,"Can't find nudging_method" );
	fclose(ip);
	exit(1);
      }
    p2=strstr(p,"Highest-ambiguity");
    if (p2 != NULL) 
      {
	fprintf(stderr,"nudging_method = Highest-ambiguity initialization\n");
	fclose(ip);
	exit(1);
      }
  }

  fseek( ip, 0, SEEK_SET);
  p=buf1;
  
  /* Find the Start time */

  p=buf1;
  do {
    p=fgets(p,80,ip);
    if (p == NULL) {
      fprintf(stderr,"<DateStartTime> Truncated file! Exiting!\n");
      exit(1);
    }
    p1 = strstr(p, "DataStartTime" );
    p2 = strstr(p,"spare_metadata_element");
  } while (p1 == NULL &&  p2 == NULL);

  if (p2 != NULL) 
  {
    fprintf(stderr,"Can't find DataStartTime" );
    fclose(ip);
    exit(1);
  }
  if (datestring(p1, ymd, hm)) {
    fprintf(stderr,"qchop: Error converting string\n  <%s>\nExiting!\n",p1);
    exit(1);
  }


  strncpy(ofile,"QS",2);
  strncat(ofile,ymd,strlen(ymd));
  strncat(ofile,".S",2);
  strncat(ofile,hm,strlen(hm));
  strncat(ofile,".E",2);


  /* And the End Time */
  fseek( ip, 0, SEEK_SET);
  p=buf1;
  do {
    p=fgets(p,80,ip);
    if (p == NULL) {
      fprintf(stderr, "<DateEndTime> Truncated File! Exiting\n");
      exit(1);
    }
    p1 = strstr(p, "DataEndTime" );
    p2 = strstr(p,"spare_metadata_element");
  } while (p1 == NULL && p2 == NULL);

  if (p2 != NULL) 
  {
    fprintf(stderr,"Can't find DataEndTime\n" );
    fclose(ip);
    exit(1);
  }
  for (i=0;i<81;i++) ymd[i]='\0';
  for (i=0;i<81;i++) hm[i]='\0';
  if (datestring(p1, ymd, hm)) {
    fprintf(stderr,"qchop: Error converting string\n  <%s>\nExiting!\n",p1);
    exit(1);
  }

  strncat(ofile,hm,strlen(hm));

  if ( (op=fopen(ofile,"wb")) == NULL) 
  {
    fprintf(stderr, "Error opening %s\n", ofile);
    fclose(ip);
    exit(1);
  }

  fseek( ip, 0, SEEK_SET);
  rec=0;
  /*while (!feof(ip) ) */
  do 
  {
    if ((i=fread(ior.mgdr,MGDRSIZE,1,ip))==1) 
    {
      if (rec==0) {
	/* The first record is the header. Just write the first
	 * WINDSIZE bytes out.
	 */
	fwrite( ior.rnoaa.wind1,WINDSIZE,1,op);
      }else{
	fwrite( ior.rnoaa.wind1,PART_1_SIZE,1,op);
	fwrite( ior.rnoaa.wind2,PART_2_SIZE,1,op);
      }
      rec+=1;
    } 
    else 
    {
      if (!feof(ip) ) 
      { 
	fprintf(stderr,"Error reading record %d\n",rec+1);
	fclose(ip);
	fclose(op);
	exit(1);
      }
      break;
    }
  } while (!feof(ip) );
  
  fprintf(stdout,"Processed %d records\n", rec);
  fclose(ip);
  fclose(op);
  exit(0);

  
} /* End main*/


int datestring( char *s, char *ymd, char *hm) {

  /* Takes a string of the form " xxxx = yyyy-doyThh:mm:ss ;\r", where
   * xxxx may be 'datastarttime' or 'dataendtime' converts the yyy-doy
   * to yyyymmdd and the hh:mm to hhmm 
   */


  char *ss,*p1,*p2;
  char buf1[81], buf2[81];
  struct tm tp, *tp1;
  time_t time;
  int i, month, day;

  ss=strdup(s);
  p2=strchr(ss,(int) '=');
  strncpy( buf1, p2+1, 80);

  i=0;
  p2=buf1;
  while (*(p2)== ' ') p2++;
  strncpy(buf2, p2, 80);
  p2=buf2;
  
  while (*(p2) != ' ') p2++;
  *(p2++) = '\0';
  i=strlen(buf2);
  
  strncpy( buf1, buf2, i+1);
  *(buf1+i) = '\0';


  p1=buf1;
  for(i=0;i<81;i++) buf2[i]='\0';
  strncpy(buf2,p1,4);

  /* We now should be down to the date string portion
   * which must be of form NNNN-NNNTNN:NN:NN, and should be
   * 21 characters long.
   */
  if ( strlen(p1) < 21) {
    fprintf(stderr,
	    "datestring: Input string is too short (%d)\n", 
	    strlen(p1) ) ;
    return(1);
  }

  tp.tm_year = atoi(buf2)-1900;
  if (tp.tm_year <= 0 ) {
    fprintf(stderr,"Bad tm_year conversion!\n" );
    return(1);
  }
  p1 += 5;
  
  for(i=0;i<81;i++) buf2[i]='\0';
  strncpy(buf2,p1,3);
  tp.tm_yday = atoi(buf2)-1;
  if (tp.tm_yday < 0 ) {
    fprintf(stderr,"Bad tm_yday conversion!\n" );
    return(1);
  }

  p1 += 4;

  (void) doy2date(tp.tm_year+1900, tp.tm_yday+1, &month, &day);

  tp.tm_mon=month-1;
  tp.tm_mday=day;
  
  for(i=0;i<81;i++) buf2[i]='\0';
  strncpy(buf2,p1,2);
  tp.tm_hour = atoi(buf2);
  if (tp.tm_hour < 0 ) {
    fprintf(stderr,"Bad tm_hour conversion!\n" );
    return(1);
  }
  p1 += 3;
  
  for(i=0;i<81;i++) buf2[i]='\0';
  strncpy(buf2,p1,2);
  tp.tm_min=atoi(buf2);
  if (tp.tm_min < 0 ) {
    fprintf(stderr,"Bad tm_min conversion!\n" );
    return(1);
  }
  p1 += 3;
  
  for(i=0;i<81;i++) buf2[i]='\0';
  strncpy(buf2,p1,2);
  tp.tm_sec=atoi(buf2);
  if (tp.tm_sec < 0 ) {
    fprintf(stderr,"Bad tm_sec conversion!\n" );
    return(1);
  }
  p1 += 3;


  /*time = mktime(&tp);
  tp1 = localtime(&time);*/
  strftime(ymd, 80, "%Y%m%d", &tp);
  strftime(hm, 80, "%H%M", &tp);
  return(0);
}


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
