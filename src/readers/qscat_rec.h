/* 
 * qscat_rec.h
 * Define the record structure to be used in reading the Qscat Data.
 *
 *
 * William Daffer
 * William.Daffer@jpl.nasa.gov
 * 818-354-0161
 *
 * $Id$
 * 
 * Modifications:
 * 
 * $Log$
 * Revision 1.1  2001/02/23 18:27:17  vapuser
 * Initial revision
 *
 *
 * The basic file structure of our Vap RMGDR (Reduced Merged GDR
 * file), also known as RNOAA (Reduced NOAA) file, consists of one
 * record of header information, described below, of the same length
 * as the basic record structure, followed by as many binary records
 * as are needed to fill out the file.  
 */


#define QSCAT_REC_LEN 5272 
#define NCELLS 76

/* Header Structure: 
 *
 * The header is a series of "keyword=value;" lines, each of which is
 * terminated by a carriage-return/linefeed pair (\r\n) and is 80
 * characters long overall. There is insufficient data to fill the
 * whole record, so the remainder of the space is filled with
 * 'spare_meta_data_element= blanks ;\r\n' Unfortunately, 80 is not a
 * divisor of 5272, so when the records are chopped, the last
 * 'keyword=value' is truncated. This makes no difference whatsoever
 * to the program, but if you look at the file in a text editor,
 * you'll notice a discrepency that might suggest something
 * wrong. This is not the case, however.  
 */



/* The basic structure type for the SeaWinds on QuikSCAT Reduced MGDR
 * record structure produced by Vap processing on the NRT (Near Real
 * Time) data string */


typedef struct qscat_rec {
  char           row_time[24];
  short            rev                 ;         
  short          wvc_Row             ;
  short          wvc_lat[NCELLS]     ; 
  unsigned short wvc_lon[NCELLS]     ; 
  short          wvcqual_flag[NCELLS]; 
  short          model_speed[NCELLS] ; 
  unsigned short model_dir[NCELLS]   ; 
  char           nambig[NCELLS]      ; 
  short          windspd[NCELLS][4]  ;      
  unsigned short winddir[NCELLS][4]  ;      
  short          errspd[NCELLS][4]   ;     
  unsigned short errdir[NCELLS][4]   ;     
  short          mle_like[NCELLS][4] ;       
  char           wvc_sel[NCELLS]     ; 
  short          mp_rain_index[NCELLS] ; 
  char   	 nof_rain_index[NCELLS]; 
  short  	 tb_mean_H[NCELLS]     ; 
  short  	 tb_mean_V[NCELLS]     ; 
  short  	 tb_stdev_H[NCELLS]    ; 
  short  	 tb_stdev_V[NCELLS]    ; 
  char   	 num_tb_H[NCELLS]      ; 
  char   	 num_tb_V[NCELLS]      ; 
  short  	 tb_rain_rate[NCELLS]  ; 
  short  	 tb_rains_attenuation[NCELLS]; 
} QREC ;


