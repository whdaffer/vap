;+
;
; $Id$
; NAME:  AUTO_MOVIE
;
; Time-stamp: <98/10/02 16:01:04 vapuser>
;
;		
; 
;
;
; PURPOSE: automatically creates a quicktime movie of the requested
;          region of data. Regions, along with other info,  are
;          defined in the file $VAP_ROOT/auto_movie_defs.dat and
;          include the name of the region, the default location of the
;          winds files, where to put the interpolated wind file and
;          where to put the output of the animation.
;
;
; AUTHOR; William Daffer
;
; CATEGORY:  
;
;
;
; CALLING SEQUENCE:  
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
; MODIFICATION HISTORY:  
;
; $Log$
;
;-
PRO auto_movie, date_time, $ ; end time of data used in movie (def=current time)
                             ; ((yy)yy/mm/dd/hh. if yy <= 99 then 1900
                             ; will be added to it.)
                time_inc,  $ ;select wind files this number of hours back in time
                             ; def = 26
                roi =  roi, $ ; one of 'nepac', 'nwpac', 'npac' or 'nwatl'
                              ; the 'region of interest'
                alonpar = alonpar ,$ ; [min,max] lon of movie 
                                     ; (vlonpar = [lonpar-10,lonpar+10, alonpar(2)] )
                alatpat = alatpar ,$ ; [min,max] lat of movie
                                     ; (vlatpar = [latpar-10,latpar+10,alatpar(2)] )
                wpath = wpath, $  ; path to wind files (def=$VAP_WINDS)
                interp_path = interp_path , $; path to output interp file 
                                             ; (def=$VAP_ANIM)
                anim_path= anim_path,$     ; path for output animation files 
                                           ; (def=$VAP_ANIM/roi/daily)
                min_nvect =  min_nvect ,$ ; minimum number of vectors needed in ROI 
                                          ; to make movie.
                dateit =  dateit ,$
                animpar= animpar,$
                write_gif=write_gif,$
                write_pict=write_pict,$
                nomovie= nomovie


  catch, error_status
  IF error_status NE 0 THEN BEGIN
    IF auto_cloud_overlay THEN $
     printf, llun, 'ERROR: ' + !err_string
    message, !err_string,/cont
    return
  ENDIF 

  rcsid = "$Id$"

  write_gif = keyword_set(write_gif)
  write_pict = keyword_set(write_pict)
  IF write_gif AND write_pict THEN BEGIN 
    message," Only one of 'write_gif' or 'write_pict' may be set",/cont
    return
  ENDIF 
  IF write_pict THEN nomovie = 1

  user = getenv('USER')
  cur_dir =  getenv('PWD')

  dateit =  keyword_set( dateit )


  cronjob = 0 ; flag for cronjob runs.
  IF (user NE "" ) THEN BEGIN 
    lockfile = (findfile('/tmp/' + user + '.auto_movie.lock', count=n))(0)
    IF n EQ 1 THEN cronjob = 1
  ENDIF ELSE cronjob = 0

  IF cronjob THEN BEGIN 
    openw, llun, lockfile, /get, error= err
    IF err NE 0 THEN BEGIN 
      message,!err_string,/cont
      return
   ENDIF 
  ENDIF 
  IF N_elements( time_inc ) EQ 0 THEN time_inc =  26

  IF n_elements( roi ) THEN BEGIN 
    roistr =  READ_AUTO_MOVIE_DEFS( roi )
    IF strpos( roistr.desig, 'ERROR' ) NE -1 THEN BEGIN 
      message,"Error reading defaults for roi " + roi + " error = " + roistr.desig,/cont
      return
    ENDIF 
  ENDIF ELSE BEGIN 
    roi =  'NPAC'
    roistr =  READ_AUTO_MOVIE_DEFS( 'NPAC' )
  ENDELSE 
  IF n_elements( wpath )       NE 0 THEN roistr.wpath =  wpath     
  IF n_elements( alonpar )     NE 0 THEN roistr.alonpar =  alonpar 
  IF n_elements( alatpar )     NE 0 THEN roistr.alatpar =  alatpar 
  IF n_elements( interp_path ) NE 0 THEN roistr.interp_path =  interp_path
  IF n_elements( anim_path )   NE 0 THEN roistr.anim_path =  anim_path
  IF n_elements( min_nvect )   NE 0 THEN roistr.min_nvect =  min_nvect
  IF n_elements( animpar )     EQ 0 THEN animpar = [320,240,60]

  roi =  strupcase(roi)
  MESSAGE,' Looking for roi ' + roi,/cont

  alonpar     = roistr.alonpar       
  alatpar     = roistr.alatpar       
  interp_path = roistr.interp_path   
  anim_path   = roistr.anim_path     
  wpath       = roistr.wpath
  min_nvect   = roistr.min_nvect

  IF n_elements( date_time ) EQ 0 THEN BEGIN
   ; Get the current GMT doy and hour
    spawn,'date -u +%j/%H/%Y',ret
    tmp =  str_sep( ret(0), '/')
    actual_doy =  tmp(0)
    test_hour =  tmp(1)
    test_year =  tmp(2)
    tmp2 =  doy2date( test_year, actual_doy )
    date_string =  strmid( test_year, 2,2 ) + '/' + tmp2(0) + '/' + tmp2(1) + '/' + test_hour
    test_doy =  actual_doy
    IF test_year GT 1996 THEN test_doy =  actual_doy + (test_year-1996)*365 + 1
    test_day =  test_doy + fix(test_hour)/24.
  ENDIF ELSE BEGIN
    ; parse the users input date/time.
    tmp =  str_sep( date_time, '/' )
    date_string =  date_time
    test_year =  fix(tmp(0))
    test_month = tmp(1)
    test_dom =  tmp(2)
    test_hour =  tmp(3)
    IF test_year LT 95 THEN $
     test_year =  2000 + test_year ELSE $
     test_year =  1900 + test_year
    test_date_str =  test_dom+'-'+test_month+'-'+strtrim(test_year,2)
    actual_doy = date2doy( test_date_str )
    actual_doy =  strmid( actual_doy, 4, 3)

    IF test_year GT 1996 THEN test_doy =  actual_doy + (test_year-1996)*365 + 1
    test_day =  test_doy + fix(test_hour)/24.
  ENDELSE 


  wf =  findfile( wpath + '/N*', count=nf)
  IF nf EQ 0 THEN BEGIN
    wp =  getenv('VAP_WINDS')
    str =  'ERROR: No wind files in dir ' + wp
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
      tmp =  date2doy(date_str(i))
      doy(i) =  fix( strmid( tmp, 4,3))
   ENDFOR 

  ;  ; Convert to fractional days since 1-jan-1996
  ;  ; (NB. 1996 is leap year, but 2000 isn't)
  ;  filedays =  (fix(year)-1996)*365 + doy + fix(start_hour)/24.
  ;  x = where( year GT 1996,nx )
  ;  IF nx NE 0 THEN filedays(x) =  filedays(x)+1


  ;  x =  where( filedays GE test_day-time_inc/24. AND $
  ;              filedays LE test_day, nx )
  ;  IF nx EQ 0 THEN BEGIN 
  ;    str =  'ERROR: No windfiles in input time range '
  ;    message, str,/cont
  ;    IF cronjob THEN BEGIN 
  ;      printf,llun,str
  ;      return
  ;    ENDIF 
  ;  ENDIF 


    ; Convert to fractional days since 1-jan-1996
    ; (NB. 1996 is leap year, but 2000 isn't)
    filedays =  [ [(fix(year)-1996)*365 + doy + fix(start_hour)/24. + fix(start_min)/(24.*60.)], $
                  [(fix(year)-1996)*365 + doy + fix(end_hour)/24.   + fix(end_min)/(24.*60.)] ]
    x = where( year GT 1996,nx )
    IF nx NE 0 THEN filedays(x,*) =  filedays(x,*)+1

      ; The following make the assumption that the only case where the
      ; end time will be < the start time is when a pass runs over the
      ; day boundary (eg, a pass starts at 23 and goes to 00:40) Back in
      ; Feb. we were seeing passes that started at 15:00 (say) and went
      ; until 0800. In these cases, it was the start time that was bogus,
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
      ; files whose stop time is > the start of our time range
    x1 =  min( where( filedays(*,1) GT test_day-time_inc/24., nx1 ) )
      ; files whose start time is < the end of our time range  
    x2 =  max( where( filedays(*,0) LT test_day, nx2 ) )
    IF nx1 EQ 0 OR nx2 EQ 0 THEN nx =  0 ELSE nx =  x2-x1+1
    IF nx EQ 0 THEN BEGIN 
      str =  'ERROR: No windfiles in input time range '
      message, str,/cont
      IF cronjob THEN BEGIN 
        printf,llun,str
        free_lun, llun
      ENDIF 
      return
    ENDIF 

    x =  indgen(nx)+x1
    wf =  wf(x)
    nf =  n_elements(wf)
    filedays =  filedays(x,*)
    start_hour =  start_hour(x)
    end_hour =  end_hour(x)
    start_min =  start_min(x)
    end_min =  end_min(x)
       ; Now find out how much time is actually covered by this data.

    diff =  filedays(*,1)-filedays(*,0)
    total_time =  total( diff  )*24.
    diff =  transpose(diff)



    message,'Using time_inc = ' + string( time_inc, form='(i2)'),/cont
    message,'Wind Path = ' + wpath,/cont
    message,'Interp_path = ' + interp_path,/cont
    message,'anim_path = '+anim_path,/cont
    message,'date_time = ' + date_string,/cont
    print,' animpar = ',animpar
    tt =  transpose(wf)
    message,'Using the following files in the interpolation',/cont
    print,tt

    message,' Found ' + string(nf,form='(i3)') + ' files ',/cont
    lonpar =  roistr.alonpar
    latpar =  roistr.alatpar
    lonpar(2) =  1.
    latpar(2) =  1.

     READ_RMGDR_DATA,wf(0),uu,vv,llon,llat,mint=mmint,maxt=mmaxt
     FOR i=1,nf-1 DO BEGIN
       read_rmgdr_data,wf(i),u,v,lon,lat,mint=mint,maxt=maxt
       uu =  [uu,u]
       vv =  [vv,v]
       llon = [llon,lon]
       llat = [llat,lat]
       mmint = [mmint, mint]
       mmaxt =  [mmaxt,maxt]
    ENDFOR 

    x =  where( llon GT lonpar(0)-10 AND llon LE lonpar(1)+10 AND $
                llat GT latpar(0)-10 AND llat LE latpar(1)+10, nx )
    IF nx GT  min_nvect THEN BEGIN 

  ;  IF total_time GE 14 THEN BEGIN 
     ; 
     ui = fltarr(360,121) &  vi=ui
     t1 = systime(1)
     lonpar =  [0,360,1.] &  latpar=[-60., 60, 1]
     rainf = [12., 10., 6,   4 ] 
     ermax = [50., 20., 10., 5.]
     SUCCOR, llon,llat,uu,vv,ui,vi,lonpar,latpar,ermax,rainf
     rainf = [10.,6.,3.,2]
     ermax = [10.,6.,3.,2]
     SUCCOR, llon,llat,uu,vv,ui,vi,lonpar,latpar,ermax,rainf
     print,'Time to do 2 succors: ',systime(1)-t1, ' seconds '
     tmp =  doy2date( fix(test_year), fix(actual_doy) )
     anim_date_str =  strmid(strtrim(test_year,2),2,2) + tmp(0) + tmp(1) + test_hour 
     interp_filename =  anim_date_str  + '_interp.bin'
     ofile =  interp_path + '/' + interp_filename
     openw,wlun, ofile, /get_lun, error=err
     IF err NE 0 THEN BEGIN 
       message, !err_string,/cont
       IF cronjob THEN BEGIN 
         str =  "ERROR: " + !err_string
         printf, llun, str
         free_lun, llun
       ENDIF 
       return
     ENDIF 



     writeu,wlun,ui,vi
     free_lun,wlun

     IF keyword_set( nomovie ) THEN BEGIN 
       message,' keyword_set( nomovie ) = 1, returning ',/cont
       return
     ENDIF 
     cd,anim_path
                                  ;
                                  ; Create the individual gif files for the animation.
                                  ;
     ddims  =  [[0,360.,1],[-60,60,1]]
     lonpar =  roistr.alonpar
     latpar =  roistr.alatpar
     tmp =  (lonpar(0:1) + [-10,10])
     tmp =  fixlonrange(tmp)
     vlonpar =  [tmp, lonpar(2) ]
     vlatpar =  latpar
     vlatpar(0:1) =  -60 >  (vlatpar(0:1) + [-10,10]) <  60

     mps2knots = 1.9444 ; takes meters/sec to knots
     ANIMATE_WIND_FIELD,ofile,$
      ddims=ddims,$
      lonpar=lonpar, $
      latpar=latpar, $
      vlon = vlonpar, $
      vlat = vlatpar,$
      animpar=animpar,$
      min_speed=1,  $             ; meters/sec, will be converted to knots 
      max_speed=40/mps2knots, $   ; in animate_wind_field (max_speed=40 knots)
      title= strtrim( time_inc,2 ) + ' hrs prior to ' + anim_date_str ,$
      write_gif=write_gif, write_pict=write_pict

     omov_file =  'daily_' + strlowcase( roi ) 
     IF dateit THEN omov_file =  omov_file + '_' + anim_date_str
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
      str =  "ERROR: Not enough vectors in area: have " + strtrim( nx, 2 ) + $
       " need: " + strtrim( min_nvect,2 )

      message,str,/cont
      IF cronjob THEN begin
        printf, llun, str
        free_lun,llun
      ENDIF 
   ENDELSE 
  ENDELSE  

  cd,cur_dir
 
END

