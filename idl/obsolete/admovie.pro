;+
; NAME: ADMOVIE
;
; Time-stamp: <98/10/20 13:43:58 vapuser>
; MODIFICATION HISTORY: 
;
; Tue Mar 18 13:45:03 1997, Vap User
;   1.0 Initial version
;       <vapuser@haifung.jpl.nasa.gov>
; 
;               
;
;
;
;
; PURPOSE: Makes an interpolated wind field using only data from the
;          24 hours prior to the input time using only data of the
;          requested type (ascending or descending)
;
;
;
; AUTHOR: William Daffer
;
;
; DATE:  March 18, 1997
;
;
;   VER = 1.0
; 
;
; CATEGORY: 
;
;
;
; CALLING SEQUENCE: admovie, time, type
;
;
; 
; INPUTS: time - string ('yy/mm/dd/hh', 00=2000) indicating end time of data 
;         to be used in the field. (default = current time)
;
;         type - string ('a' or 'd') indicating 'ascending' or
;                'descending', respectively (def='a')
;
;
;
; OPTIONAL INPUTS: NONE
;
;
;       
; KEYWORD PARAMETERS: ROI - Region of Interest (def=nepac) 
;
;                     INTERP_PATH - path to output interpolated data
;                     (def = $VAP_ANIM/a if type = 'a',
;                       $VAP_ANIM/d if type = 'd')
;
;                     ANIM_PATH - path to do animation in
;                                (def=$VAP_ANIM/a|d/daily)
;
;                     WPATH - path to wind files (def='$VAP_WINDS/')
;
;
; OUTPUTS: The file 'interp_path/yymmddhh_inter_type.bin'  containing the 
;          interpolated wind file having the following format.
;          
;
;          U - a 360 by 121 array of 4 byte floats, the U components
;              of the wind field (the U axis points due east)
;          V - a 360 by 121 array of 4 byte floats, the V components
;              of the wind field (the V axis points due north)
;
;
; OPTIONAL OUTPUTS: NONE
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
; PROCEDURE: parse the user time value and type string, set up
;            defaults, determine which rmgdr files need to be read (
;            the sticky point of this whole mess) read them, call
;            succor and write out its output.
;
;
;
; EXAMPLE: 
;
;
;
;-


PRO ADMOVIE, type, time,  $
             roi= roi, $
             anim_path= anim_path, $
             interp_path= interp_path, $
             wpath= wpath, $
             dateit= dateit


;on_error, 1 ; return to main

user = getenv('USER')
cur_dir =  getenv('PWD')
dateit =  keyword_set( dateit )

IF n_elements(type) EQ 0 THEN type = 'a'
type =  strlowcase(type)

IF vartype( type ) NE 'STRING' THEN $
  message,' parameter TYPE must be a STRING'

IF n_elements( roi ) EQ 0 THEN roi =  'NPAC'
s =  vartype(time)

IF s NE 'UNDEFINED' AND vartype( time ) NE 'STRING' THEN $
  message,' parameter TIME must be a STRING'


cronjob = 0 ; flag for cronjob runs.
IF (user NE "" ) THEN BEGIN 
  lockfile = (findfile('/tmp/' + user + 'admovie.lock', count=n))(0)
  IF n EQ 1 THEN cronjob = 1
ENDIF ELSE cronjob = 0

IF cronjob THEN BEGIN 
  openw, llun, lockfile, /get, error= err
  IF err NE 0 THEN BEGIN 
    message,!err_string,/cont
    return
 ENDIF 
ENDIF 


IF n_elements( interp_path ) EQ 0 THEN interp_path =  '$VAP_ANIM/' + $
 strlowcase(roi) + '/' + $
 type + '/'
IF n_elements( anim_path ) EQ 0 THEN anim_path =  interp_path 

MESSAGE,' Looking for roi ' + roi,/cont
roistr =  READ_AUTO_MOVIE_DEFS( roi )
IF strpos( roistr.desig, 'ERROR' ) NE -1 THEN BEGIN 
  str =  "ERROR: reading defaults for roi " + roi + $
   " error = " + roistr.desig
  message,str/cont
  IF cronjob THEN BEGIN 
    printf,llun,str
    free_lun,llun
  ENDIF 

  return
ENDIF 

IF n_elements( wpath ) ne 0 THEN roistr.wpath =  wpath
IF n_elements( alonpar ) NE 0 THEN roistr.alonpar =  alonpar
IF n_elements( alatpar ) NE 0 THEN roistr.alatpar =  alatpar 
IF n_elements( interp_path ) NE 0 THEN roistr.interp_path =  interp_path
IF n_elements( anim_path ) NE 0 THEN roistr.anim_path =  anim_path



alonpar     = roistr.alonpar       
alatpar     = roistr.alatpar       
interp_path = roistr.interp_path   
anim_path   = roistr.anim_path     
wpath       =  roistr.wpath



IF n_elements( time ) EQ 0 THEN BEGIN
 ; Get the current GMT doy and hour
  spawn,'date -u +%j/%H/%Y',ret
  tmp =  str_sep( ret(0), '/')
  actual_doy =  tmp(0)
  test_hour =  tmp(1)
  test_year =  tmp(2)
  IF test_year GT 1996 THEN test_doy =  actual_doy + (test_year-1996)*365 + 1
  test_day =  test_doy + fix(test_hour)/24.
ENDIF ELSE BEGIN
  ; parse the users input date/time.
  tmp =  str_sep( time, '/' )
  test_year =  fix(tmp(0))
  test_month = tmp(1)
  test_dom =  tmp(2)
  test_hour =  tmp(3)
  IF test_year LT 95 THEN $
   test_year =  2000 + test_year ELSE $
   test_year =  1900 + test_year
  test_date_str =  test_dom+'-'+test_month+'-'+strtrim(test_year,2)
  actual_doy = datestr2doystr( test_date_str )
  actual_doy =  strmid( actual_doy, 4, 3)
  
  IF test_year GT 1996 THEN test_doy =  actual_doy + (test_year-1996)*365 + 1
  test_day =  test_doy + fix(test_hour)/24.
ENDELSE 


wf =  findfile( wpath + '/N*', count=nf)
IF nf EQ 0 THEN BEGIN
  str =  'ERROR: No wind files in dir ' + WPATH
  message,str,/cont
  IF cronjob THEN BEGIN 
    printf,llun,str
    free_lun,llun
  ENDIF 
  RETURN
ENDIF ELSE BEGIN
  
  ; the file names  have this form.
  ;      0123456789012345678
  ;/path/N961118.S1725.E1859
  
  ; find the end of the path
  l =  strlen( wf(0) )
  t =  reverse( byte(wf(0)) )
  x = where( t EQ (byte('/'))(0), nx )
  IF nx NE 0 THEN name_off =  l-x(0) ELSE name_off = 0
  
  year       = strmid( wf, name_off+1, 2 )
  month      = strmid( wf, name_off+3, 2 )
  dom        = strmid( wf, name_off+5, 2 )
  start_hour = strmid( wf, name_off+9, 2 )
  start_min  = strmid( wf, name_off+11,2 )
  end_hour   = strmid( wf, name_off+15,2 )
  end_min    = strmid( wf, name_off+17,2 )
  ; convert day-of-month/month/year to doy
  x = where( fix(year) LT 96, nx )
  y = where( fix(year) GE 96, ny )
  IF nx EQ 0 THEN year =  '19' + year ELSE BEGIN
    year(y) =  '19' + year(y)
    year(x) =  '20' + year(x)
  ENDELSE 
  x = 0 &  y=0
  date_str =  dom + '-' + month + '-' + year
  doy = intarr(nf)
  FOR i=0,nf-1 DO BEGIN
    tmp =  DateStr2DoyStr(date_str(i))
    doy(i) =  fix( strmid( tmp, 4,3))
  ENDFOR 
  
  ; Convert to fractional days since 1-jan-1996
  ; (NB. 1996 is leap year, but 2000 isn't)
  filedays =  [ [(fix(year)-1996)*365 + doy + fix(start_hour)/24. + fix(start_min)/(24.*60.)], $
                [(fix(year)-1996)*365 + doy + fix(end_hour)/24.   + fix(end_min)/(24.*60.)] ]

  x = where( year GT 1996,nx )
  IF nx NE 0 THEN filedays(x,*) =  filedays(x,*)+1

    ; The following make the assumption that the only case where the
    ; end time will be < the start time is when a pass runs over the
    ; day boundary (a pass starts at 23 and goes to 00:40) Back in
    ; Feb. we were seeing passes that started at 15:00 (say) and went
    ; until 0800. In these cases, it was the start time was bogus,
    ; there were a few records at the start of the file that were
    ; from the previous day of data. We're hoping that those don't
    ; come back, if they do, we'll have to make the following code
    ; smarter. 


  x = where( float(end_hour) LT float(start_hour),nx )
  IF nx NE 0 THEN filedays(x,1) =  filedays(x,1)+1

  
    ; Find the file whose start time is before the end of our time
    ; range and and then find the file whose end time is
    ; after the . Failing that, take the file whose end time is closest to
    ; the end of our time range. Similarly, find the file whose start
    ; time preceeds our time range but whose stop time is inside of
    ; it. IF there is none such, find the file whose start time is
    ; closest to the start of the range but still greater than it.
  time_inc =  1 ; day
    ; files whose stop time is > the start of our time range
  x1 =  min( where( filedays(*,1) GT test_day-time_inc, nx1 ) )
    ; files whose start time is < the end of our time range  
  x2 =  max( where( filedays(*,0) LT test_day, nx2 ) )
  IF nx1 EQ 0 OR nx2 EQ 0 THEN nx =  0 ELSE nx =  x2-x1+1
  IF nx EQ 0 THEN BEGIN 
    str =  'ERROR: No windfiles in input time range '
    message, str,/cont
    IF cronjob THEN BEGIN 
      printf,llun,str
    ENDIF 
    return
  ENDIF 

  x =  indgen(nx)+x1
  wf =  wf(x)
  nf =  n_elements(wf)
  filedays =  filedays(x,*)

     ; Now find out how much time is actually covered by this data.

  total_time =  total( filedays(*,1)-filedays(*,0) )*24.


  IF total_time GE 22 THEN BEGIN 

    message,'Using time_inc = ' + string( time_inc, form='(i2)'),/cont
    message,'Wind Path = ' + wpath,/cont
    message,'Interp_path = ' + interp_path,/cont
    str =  ' Total Time covered by the input files: ' + $
     string( total_time, form='(f7.2)')
    message,str,/cont
    tt =  transpose(wf)
    message,'Using the following files in the interpolation',/cont
    print,tt &  tt=0
    dir_test =  0
    IF type EQ 'A' THEN dir_test =  1 

    READ_RMGDR_DATA,wf(0),uu,vv,llon,llat,mint=mmint,maxt=mmaxt, row=row
    dir =  row LE 406
    good =  where( dir EQ dir_test, ngood )
    continue = 0
    IF ngood NE 0 THEN BEGIN 
      uu =  uu(good)
      vv =  vv(good)
      llon =  llon(good)
      llat =  llat(good)
      continue = 1
      ii =  1;
    ENDIF ELSE BEGIN
      found_one =  0
      ii =  1
      REPEAT BEGIN 
        READ_RMGDR_DATA,wf(ii),uu,vv,llon,llat,mint=mmint,maxt=mmaxt, row=row
        dir =  row LE 406
        good =  where( dir EQ dir_test, ngood )
        continue = 0
        IF ngood NE 0 THEN BEGIN 
          uu =  uu(good)
          vv =  vv(good)
          llon =  llon(good)
          llat =  llat(good)
          mmint =  mint
          mmaxt =  maxt
          continue = 1
          found_one = 1
          ii =  ii + 1;
        ENDIF 
      ENDREP UNTIL found_one OR (ii GT nf-1)
    ENDELSE 
    IF continue THEN BEGIN 
      FOR i=ii,nf-1 DO BEGIN
        READ_RMGDR_DATA,wf(i),u,v,lon,lat,mint=mint,maxt=maxt, row=row
        dir =  row LE 406
        good =  where( dir EQ dir_test, ngood )
        IF ngood NE 0 THEN BEGIN 
          uu =  [uu,u(good)]
          vv =  [vv,v(good)]
          llon = [llon,lon(good)]
          llat = [llat,lat(good)]
          mmint =  [mmint,mint]
          mmaxt =  [mmaxt,maxt]
         ENDIF 
      ENDFOR 
      mint =  min(mmint)
      maxt =  max(mmaxt)
      tt =  frmt2secs( [ mint,maxt] )
      diff =  (tt(1)-tt(0))/3600.
      IF diff LT 22 THEN BEGIN 
        str = 'time range (' + string( maxt-mint ,form='(f7.2)') + ') < 22'
        message, str,/cont
      ENDIF 

        ; Now make the interpolated field
      ui = fltarr(360,121) &  vi=ui
      lonpar =  [0,360,1.] &  latpar=[-60., 60, 1]
      rainf = [12., 10., 6,   4 ] 
      ermax = [50., 20., 10., 5.]
      SUCCOR, llon,llat,uu,vv,ui,vi,lonpar,latpar,ermax,rainf
      rainf = [10.,6.,3.,2]
      ermax = [10.,6.,3.,2]
      SUCCOR, llon,llat,uu,vv,ui,vi,lonpar,latpar,ermax,rainf


        ; give it a befitting  name
      tmp =  doy2date( fix(test_year), fix(actual_doy) )
      anim_date_str =  strmid(strtrim(test_year,2),2,2) + tmp(0) + tmp(1) + test_hour 
      interp_filename =  anim_date_str  + '_interp_' + type + '.bin'

      ofile =  interp_path + '/' + interp_filename
      openw,wlun, ofile, /get_lun, error=err
      IF err NE 0 THEN message, !err_string
      writeu,wlun,ui,vi
      free_lun,wlun

        ; Go to where the animation will be done
      CD,anim_path
        ;
        ; Create the individual gif files for the animation.
        ;
      fdims  =  [[0,360.,1],[-60,60,1]]
      lonpar =  roistr.alonpar
      latpar =  roistr.alatpar
      tmp =  (lonpar(0:1) + [-10,10])
      tmp =  fixlonrange(tmp)
      vlonpar =  [tmp, lonpar(2) ]
      vlatpar =  latpar
      vlatpar(0:1) =  -60 >  (vlatpar(0:1) + [-10,10]) <  60
      ANIMATE_WIND_FIELD,ofile,$
       fdims ,$
       lonpar, $
       latpar, $
       vlon = vlonpar, $
       vlat = vlatpar,$
       animpar=[320,240,60],/noxinter,/write_gif

      omov_file =  'admovie_' + roi + '_' +  type

      IF dateit THEN omov_file =  omov_file + '_' + type 
      omov_file =  omov_file + '.mov'

      ; construct string to send to spawn and execute it.
      exe_str =  'dmconvert -f qt -p video,comp=qt_cvid,squal=0.9,tqual=0.9,rate=15 ' + $
       ' -n gwind.0##,start=1,end=60,step=1 gwind.0## ' + omov_file
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
      str =  " ERROR: No type " + type + " data in input files"
      message,str,/cont
      IF cronjob  THEN begin
        printf, llun, str
        free_lun,llun
      ENDIF 
    ENDELSE 
  ENDIF ELSE BEGIN
    str =  "ERROR: Time range of data (" +$
     string(total_time,form='(f7.2)') + ") TOO SMALL! --- exiting "
    message,str,/cont
    IF cronjob THEN begin
      printf, llun, str
      free_lun,llun
    ENDIF 
  ENDELSE 
ENDELSE 


return
END






