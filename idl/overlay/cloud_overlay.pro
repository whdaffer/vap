;+
; NAME: CLOUD_OVERLAY
;
; Time-stamp: <98/09/09 10:49:17 vapuser>
;
;
;
; PURPOSE:  This procedure is meant to be called in batch mode to make
;          an overlay of Qscat/SeaWinds data on a Goes (or perhaps in
;          the future, GMS) file.
; 
;
;
; AUTHOR: William Daffer
;
;
; CATEGORY: Plotting, image
;
;
;
; CALLING SEQUENCE: CLOUD_OVERLAY
;
;
; 
; INPUTS: ALL INPUTS ARE OPTIONAL AND TAKE DEFAULT VALUES IF NOT PRESENT
;
;
;
; OPTIONAL INPUTS: 
;
;
;	
; KEYWORD PARAMETERS: 
;
;
;
; OUTPUTS: 
;
;
;
; OPTIONAL OUTPUTS: 
;
;
;
; COMMON BLOCKS: 
;
;
;
; SIDE EFFECTS: 
;
;
;
; RESTRICTIONS: 
;
;
;
; PROCEDURE: 
;
;
;
; EXAMPLE: 
;
;
; $Id$
;
; Modification History:
;
; $Log$
;
;-

PRO cloud_overlay, cloud_file,     $ ; full name of grid file
                      date_time,     $ ; time of GOES data used in overlay
                                       ; (def=current time - 3 hours)
                                       ; ((yy)yy/mm/dd/hh, yy=00 means
                                       ; 2000) If the year field is 2
                                       ; characters, then 1900 will be
                                       ; added to it.
                      time_inc,      $ ; select wind files this number 
                                       ; of hours +/- time given 
                                       ; in date_time. def=3
                      wpath = wpath, $ ; path to wind files (def=$VAP_WINDS)
                      overlay_path = overlay_path,$ ; path to output overlay file
                                       ; def = $VAP_ROOT/overlay/daily
                      ps =  ps         ; make a postscript instead of a gif
                      


 Rcs_id = "$Id$";
  catch, error_status
  IF error_status NE 0 THEN BEGIN
    IF auto_cloud_overlay THEN $
     printf, llun, 'ERROR: ' + !err_string
    message, !err_string,/cont
    return
  ENDIF 

  IF !version.release LT 5.1 THEN BEGIN 
    Message,'This software requires idl 5.1',/cont
    return
  ENDIF 

  IF n_params() NE  2 THEN BEGIN 
    message,' Both paramters GOES_FILE/DATE_TIME are required ',/cont
    return
  ENDIF 


  ps =  keyword_set( ps );

  IF N_elements( time_inc ) EQ 0 THEN time_inc = 3
  IF n_elements( wpath ) EQ 0 THEN wpath =  '$VAP_WINDS'
  IF n_elements( overlay_path ) EQ 0 THEN overlay_path =  '$VAP_OVERLAY/daily'

  user = getenv('USER')
  cd,current=cur_dir


  auto_cloud_overlay = 0 ; flag for cronjob runs.
  IF (user NE "" ) THEN BEGIN 
    lockfile = (findfile('/tmp/' + user + '.cloud_overlay.lock', count=n))(0)
    IF n EQ 1 THEN auto_cloud_overlay = 1
  ENDIF 

  IF auto_cloud_overlay THEN BEGIN 
    openw, llun, lockfile, /get, error= err
    IF err NE 0 THEN BEGIN 
      message,!err_string,/cont
      return
   ENDIF 
  ENDIF 

  CASE 1 OF 
    strpos( cloud_file, 'GOES' ) NE -1: grid_type = 'GOES'
    strpos( cloud_file, 'GMS' ) NE -1: BEGIN 
      grid_type = 'GMS'
      str =  'ERROR: GMS not implemented'
      Message,str,/cont
      IF auto_cloud_overlay THEN BEGIN 
        printf, llun, str
        free_lun, llun
      ENDIF 
      return
    end
    ELSE: BEGIN 
      str =  'ERROR: Unknown Grid type in file ' + cloud_file
      Message,str,/cont
      IF auto_cloud_overlay THEN BEGIN 
        printf, llun, str
        free_lun, llun
      ENDIF 
      return
    END 
  ENDCASE 

  print,'$VAP_ROOT=',getenv('VAP_ROOT')
  print,' overlay_path = ',overlay_path
  t=where( strpos( overlay_path, '$VAP_' ) NE -1, NT )
  IF NT NE 0 THEN BEGIN
    tmp =  str_sep( overlay_path, '/' )
    FOR i=0,n_elements(tmp)-1 DO BEGIN
      IF strpos( tmp(i), '$VAP_') NE -1  THEN BEGIN
        tmp(i) = strmid(tmp(i),1,strlen(tmp(i))-1)
        tmp(i) = getenv(tmp(i))
      ENDIF 
    ENDFOR 
    overlay_path =  tmp(0) + '/'

    FOR i=1,n_elements(tmp)-1 DO overlay_path =overlay_path + tmp(i) + '/'
  ENDIF 


  cd,overlay_path

  openr,rlun, cloud_file, /get_lun, error= err
  IF err NE 0 THEN BEGIN
    message,!err_string,/cont
    IF auto_cloud_overlay THEN $
     printf, llun,' ERROR: ' + !err_string
    return
  ENDIF 
  free_lun,rlun

  str =  ' Taking wind data from   ' + wpath
  message,str,/info
  IF auto_cloud_overlay THEN $
    printf,llun, "INFO: " + str,/cont
  str =  ' Putting output in       ' + overlay_path
  Message,str ,/info
  IF auto_cloud_overlay THEN $
    printf,llun"INFO: " + str,/cont
  str = ' Using time increment of ' + strtrim( time_inc, 2 )
  Message,str,/info
  IF auto_cloud_overlay THEN $
    printf,llun,"INFO: " + str,/cont
  str = ' Using date_time of      ' + date_time
  Message,str,/info
  IF auto_cloud_overlay THEN $
    printf,llun,"INFO: " + str,/cont



  wf = getwindfiles( date_time, delta=time_inc, path= wpath, filter='Q*')
  nf = n_elements(wf)
  IF strlen(wf[0]) EQ 0 AND nf EQ 1 THEN BEGIN 
    str = 'ERROR: No wind files in dir ' + WPATH
    Message,str,/cont
    IF auto_cloud_overlay THEN BEGIN 
      printf,llun,str
      free_lun,llun
    ENDIF 
    return 
  ENDIF ELSE BEGIN
    CASE grid_type OF 
      'GOES': BEGIN 
        IF ps THEN  BEGIN 
          GOES_OVERLAY, cloud_file, wfiles=wf, $
            minspeed=2, maxspeed=20, thick=2, $
             len=2,getoutfile=ofile, /ps,/z
        ENDIF ELSE BEGIN 
          GOES_OVERLAY, cloud_file, wfiles=wf, $
            minspeed=2, maxspeed=20, thick=2, $
             len=2,xsiz=960,ysiz=800,getoutfile=ofile, /gif,/z
        ENDELSE 
      END
      'GMS' : BEGIN 
      END
    ENDCASE 
    IF auto_cloud_overlay THEN BEGIN 
      openw, wlun, '/tmp/auto_cloud_overlay_gif_file',/get,error=err
      IF err NE 0 THEN BEGIN
        str =  'ERROR: ' + !err_string
        printf,llun,str
        Message,str,/cont
        RETURN
      ENDIF 
      Printf, wlun, ofile
      free_lun, wlun
    ENDIF 
  ENDELSE  
  IF exist( llun ) THEN   free_lun,llun
  cd,cur_dir

END 
