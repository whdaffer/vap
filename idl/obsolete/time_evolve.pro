;+
; NAME: TIME_EVOLVE.PRO
;
; Time-stamp: <98/10/20 13:45:24 vapuser>
; MODIFICATION HISTORY: 
;
;   Fri Mar 7 13:14:17 1997, Vap User
;       <vapuser@haifung.jpl.nasa.gov>
;     1.0 - Initial version
;               
;
;
;
;
; PURPOSE: Makes a time evolution movie of the input interpolated fields.
;
;
; AUTHOR: William Daffer
;
;
; DATE:  
;
;
;   VER = 1.0
; 
;
; CATEGORY: Animation
;
;
;
; CALLING SEQUENCE: time_evolve, [ date_string, [time_inc, 
;                   [ roi = roi, $
;                   [ numfiles = numfiles ,$
;                   [ alonpar = alonpar ,$
;                   [ alatpar = alatpar ,$
;                   [ wind_path = wind_path ,$
;                   [ interp_path = interp_path ,$
;                   [ anim_path  = anim_path ] ] ] ] ] ] ] ]  ]
;
;
; 
; INPUTS: 
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
;
;-
PRO time_evolve, date_time, $ ; end time of data used in movie (def=current time)
                              ; (yy/mm/dd/hh, yy=00 means 2000)
                time_inc,  $  ;select wind files this number of days back in time
                              ; default = 7
                roi =  roi, $ ; one of 'nepac', 'nwpac', 'npac' or 'nwatl'
                              ; the 'region of interest', def=npac
                numfile =  numfiles,$; Number of files needed to make animation
                                     ; def=time_inc*2-1
                alonpar = alonpar ,$ ; [min,max] lon of movie 
                                     ; (vlonpar = [ lonpar-10,lonpar+10, 1.5] )
                alatpat = alatpar ,$ ; [min,max] lat of movie
                                     ; (vlatpar = [latpar-10,latpar+10, 1.5] )
                wpath = wpath, $     ; path to wind files (def=$VAP_WINDS)
                interp_path = interp_path , $; path to output interp file 
                                             ; (def=$VAP_ANIM)
                anim_path= anim_path,$ ; path for output animation files 
                                ; (def=$VAP_ANIM/roi/time_evolution)
                animpar =  animpar,$ ; parameters for animation, 
                                   ; 3-vector, [xsize, ysize, num_frames]
                                   ; def=[320,240,60]
                dateit =  dateit

; on_error, 1 ; return to main

dateit =  keyword_set( dateit )
user = getenv('USER')
cur_dir =  getenv('PWD')

days_in_months =  [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

IF n_elements(roi) EQ 0 THEN roi =  'NPAC'
cronjob = 0 ; flag for cronjob runs.
IF (user NE "" ) THEN BEGIN 
  lockfile = (findfile('/tmp/' + user + '.evolution_movie.lock', count=n))(0)
  IF n EQ 1 THEN cronjob = 1
ENDIF ELSE cronjob = 0

IF cronjob THEN BEGIN 
  openw, llun, lockfile, /get, error= err
  IF err NE 0 THEN BEGIN 
    message,!err_string,/cont
    return
 ENDIF 
ENDIF 

IF N_elements( time_inc ) EQ 0 THEN time_inc =  7
IF N_elements( numfiles ) EQ 0 THEN numfiles =  time_inc*2-1
IF n_elements( animpar ) EQ 0 THEN animpar = [320,240,60]

MESSAGE,' Looking for roi ' + roi,/cont
IF n_elements( roi ) THEN BEGIN 
  roistr =  READ_AUTO_MOVIE_DEFS( roi )
  IF strpos( roistr.desig, 'ERROR' ) NE -1 THEN BEGIN 
    message,"Error reading defaults for roi " + roi + " error = " + roistr.desig,/cont
    IF cronjob THEN begin
      printf, llun, str
      free_lun,llun
    ENDIF 
    return
  ENDIF 
ENDIF ELSE BEGIN 
  roistr =  READ_AUTO_MOVIE_DEFS( 'NEPAC' )
ENDELSE 
IF n_elements( wpath ) ne 0 THEN roistr.wpath =  wpath
IF n_elements( alonpar ) NE 0 THEN roistr.alonpar =  alonpar
IF n_elements( alatpar ) NE 0 THEN roistr.alatpar =  alatpar 
IF n_elements( interp_path ) NE 0 THEN roistr.interp_path =  interp_path
IF n_elements( anim_path ) NE 0 THEN roistr.anim_path =  anim_path ELSE $
 roistr.anim_path =  roistr.interp_path + '/evolution/'


alonpar     = roistr.alonpar       
alatpar     = roistr.alatpar       
interp_path = roistr.interp_path   
anim_path   = roistr.anim_path     
wpath       =  roistr.wpath

IF n_elements( date_time ) EQ 0 THEN BEGIN
 ; Get the current GMT doy and hour
  spawn,'date -u +%j/%Y/%m/%d/%H',ret
  date_time =  ret(0)
  tmp =  str_sep( ret(0), '/')
  actual_doy = tmp(0)
  test_year  = tmp(1)
  test_month = tmp(2)
  test_dom   = tmp(3)
  test_hour  = tmp(4)

  IF test_year GT 1996 THEN test_doy =  actual_doy + (test_year-1996)*365 + 1
  test_day =  test_doy + fix(test_hour)/24.
  test_year =  strtrim( fix(test_year)-1900,2 )
ENDIF ELSE BEGIN
  ; parse the users input date/time.
  tmp =  str_sep( date_time, '/' )
  test_year =  fix(tmp(0))
  test_month = tmp(1)
  test_dom =  tmp(2)
  test_hour =  tmp(3)
  IF test_year LT 95 THEN $
   test_year =  2000 + test_year ELSE $
   test_year =  1900 + test_year
  test_date_str =  test_dom+'-'+test_month+'-'+strtrim(test_year,2)
  actual_doy = DateStr2DoyStr( test_date_str )
  actual_doy =  strmid( actual_doy, 4, 3)
  IF test_year GT 1996 THEN test_doy =  actual_doy + (test_year-1996)*365 + 1
   test_day =  test_doy + fix(test_hour)/24.
  test_year =  strtrim( test_year-1900,2 )

ENDELSE 

anim_date_str =  test_year + test_month + test_dom + test_hour 


interp_field_files =  findfile(interp_path + '/*_interp.bin',count=nif)
IF nif GT numfiles THEN BEGIN

  filedays =  fltarr(nif)
  FOR i= 0,nif-1 DO BEGIN 
    tmp =  str_sep( interp_field_files(i), '/' )
    tmp =  tmp(n_elements(tmp)-1)
    year  =  fix( strmid( tmp, 0, 2 ) )
    xx =  where( year GE 96, nxx )
    yy =  where( year LT 96, nyy )
    IF nxx GT 0 THEN year(xx) =  year(xx) + 1900
    IF nyy GT 0 THEN year(yy) =  year(yy) + 2000

    month =  strmid(tmp,2,2)
    day   =  strmid(tmp,4,2 )
    hh    =  float(strmid(tmp,6,2) )/24.

    date_str =  day + '-' + month + '-' + strtrim( year, 2 )
    tmp =  DateStr2DoyStr(date_str)
    doy =  fix( strmid( tmp, 4,3))
    filedays(i) =  doy +  (year-1996)*365 + 1 + fix(hh)/24. ; 1996 was a leap year
  ENDFOR 

  xx =  where( filedays GT test_day - time_inc AND $
               filedays LE test_day, nxx )
  IF nxx GT 0 THEN BEGIN
    IF nxx GE numfiles THEN BEGIN
      iff =  interp_field_files(xx)
      nff =  nxx
      message,' Date_time = ' +date_time,/cont
      message,'Using time_inc = ' + string( time_inc, form='(i2)'),/cont
      message,'Wind Path = ' + wpath,/cont
      message,'Interp_path = ' + interp_path,/cont
      message,'anim_path = '+anim_path,/cont
      print,' Using files ',transpose(iff)

      ; Change directory to the animation path
      cd,anim_path


      fdims  =  [[0,360.,1],[-60,60,1]]
      lonpar =  roistr.alonpar
      latpar =  roistr.alatpar
      tmp =  (lonpar(0:1) + [-10,10])
      tmp =  fixlonrange(tmp)
      vlonpar =  [tmp, lonpar(2) ]
      vlatpar =  latpar
      vlatpar(0:1) =  -60 >  (vlatpar(0:1) + [-10,10]) <  60

      mps2knots = 1.9444        ; takes meters/sec to knots
      ANIMATE_WIND_FIELD,iff,$
       fdims ,$
       lonpar, $
       latpar, $
       vlon = vlonpar, $
       vlat = vlatpar,$
       animpar=animpar,$
       min_speed=1, $ ; meters/sec, will be converted to knots 
       max_speed=40/mps2knots,$ ; in animate_wind_field (max_speed=40 knots)
       title=strtrim( time_inc,2 ) + ' Days prior to ' + anim_date_str,$
       /write_gif
         omov_file =  'daily_' + strlowcase( roi ) + '_evol'
       IF dateit THEN omov_file =  omov_file + '_' + anim_date_str 
       omov_file =  omov_file + '.mov'
         
      ; construct string to send to spawn and execute it.
      exe_str =  'dmconvert -f qt -p video,comp=qt_cvid,squal=0.9,tqual=0.9,rate=15 ' + $
       ' -n gwind.0##,start=1,end=' + strtrim( animpar(2),2) + ',step=1 gwind.0## ' + omov_file
      message,' Making quicktime movie -- Perseverance Furthers, No blame.',/cont
      spawn,exe_str,ret
      IF ret(0) NE '' THEN BEGIN
        str =  'ERROR: in dmconvert '
        message,str, /cont
        IF cronjob THEN begin
          printf, llun, str
          free_lun,llun
        ENDIF 
      ENDIF 

    ENDIF ELSE BEGIN
      str =  'ERRPR: Only ' + strtrim( nxx,2 ) + $
       ' files in input time range in ' + interp_path
      message,str,/cont
      IF cronjob THEN BEGIN 
        printf,llun,str
      ENDIF 
    ENDELSE 
  ENDIF ELSE BEGIN
    str = 'ERROR: No wind files in input time range in ' + $
     interp_path
    message,str,/cont
    IF cronjob THEN BEGIN 
      printf,llun,str
    ENDIF 
  ENDELSE 
ENDIF ELSE BEGIN
  str =  'ERROR: Only ' + strtrim( nif ,2 ) + $
   ' interp wind field files in ' + interp_path
  message,str,/cont
  IF cronjob THEN BEGIN 
    printf,llun,str
  ENDIF 

ENDELSE 


return
END
