;+
;
; $Id$
; NAME:  AUTO_MOVIE
;
; Time-stamp: <98/10/08 10:58:50 vapuser>
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
; INPUTS:  None are required
;
;
;
; OPTIONAL INPUTS:  
;
;       date_time - (I) string of form '(yy)yy/mm/dd/hh' 
;                   Default = current time. Defines the end time of
;                   the time range inside of which wind files must
;                   reside inorder for them to be used in the
;                   interpolation. If the year field is <=
;                   99, 1900 is added to it. 
;       time_inc - (I) start_time = date_time-time_inc. Defines the 'start time' of
;                  the range inside of which the routine will look for
;                  wind files. Default=26.
;
;	
; KEYWORD PARAMETERS:  
;
;                roi     : one of 'nepac', 'nwpac', 'npac' or 'nwatl'
;                          the 'region of interest' This Region of
;                          Interest is used to get run time
;                          information from a file named
;                          auto_movie_defs.dat, which currently
;                          resides in $VAP_LIB. 
;                
;                alonpar : [min,max] lon of movie (vlonpar =
;                          [lonpar-10,lonpar+10, alonpar(2)] )
;
;                        
;                alatpat :  [min,max] lat of movie (vlatpar =
;                           [latpar-10,latpar+10,alatpar(2)] )
;
;                wpath       : path to wind files (def=$VAP_WINDS)
;                interp_path : path to output interp file (def=$VAP_ANIM)
;                anim_path   : path for output animation files
;                              (def=$VAP_ANIM/roi/daily)
;
;                min_nvect   : minimum number of vectors needed in ROI to make movie.
;                dateit      : flag: if set, put a date string on the
;                              movie file.
;                animpar     : [xsize,ysize,nframes]
;                write_gif   : write gif files
;                write_pict  : write pict files
;                nomovie     : if 1, don't make movie, just individual
;                              frames
;                decimate    : scalar, decimate=n means take every
;                              n-th vector.
;                CRDecimate  : 2-vector, [p,q] means take every p-th
;                              column of every q-th row
;                ExcludeCols : a comma delimited string of consisting
;                              of fields of two sorts, scalars and
;                              expressions of the form m:n, which
;                              express arange. The string is expanded
;                              into a list of columns to exclude.
;                              Exapmple: ExcludeCols="0,40:42,75"
;                              excludes columns 0,40,41,42 and 75
;                Nscat       : If set, expect nscat data. In this
;                              case, Decimate, CRDecimate and
;                              ExcludeCols are ignored.
;                Pid         : Pid to expect to find in lock file, for
;                              cronjob operation.
;
;

;
; OUTPUTS:  
;
;    Output animpar[2] gif or pict files in the directory given in the
;    keyword 'anim_path,' if it is passed in, or whatever the ROI
;    structure has for this ROI. if the flag 'nomovie' is clear, it
;    will also output the animation file there.
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
; Revision 1.2  1998/10/06 00:20:23  vapuser
; Intermediate stage, not done yet.
;
; Revision 1.1  1998/10/02 23:01:17  vapuser
; Initial revision
;
;
;-
PRO auto_movie, date_time, $ ; (I) end time of data used in movie (def=current time)
                             ; ((yy)yy/mm/dd/hh. if yy <= 99 then 1900
                                ; will be added to it. Default=current
                                ; time )
                time_inc,  $ ; (I) select wind files this number of hours back in time
                             ; default = 26
                roi         =  roi, $ ; (I) one of 'nepac', 'nwpac', 'npac' or 'nwatl'
                                      ; the 'region of interest'
                alonpar     = alonpar ,$ ; (I) [min,max,inc] lon of movie 
                                            ; (vlonpar =
                                            ; [lonpar-10,lonpar+10, alonpar(2)] )
                alatpat     = alatpar ,$ ; (I) [min,max,inc] lat of movie
                                            ; (vlatpar =
                                            ; [latpar-10,latpar+10,alatpar(2)] )
                wpath       = wpath, $   ; (I) path to wind files (def=$VAP_WINDS)
                interp_path = interp_path , $; (I) path to output interp file 
                                             ; (def=$VAP_ANIM)
                anim_path   = anim_path,$ ; (I) path for output animation files 
                                          ; (def=$VAP_ANIM/roi/daily)
                min_nvect   =  min_nvect ,$ ; (I) minimum number of vectors 
                                            ; needed in ROI 
                                            ; to make movie.
                dateit      = dateit ,$     ; (I) Flag, if set, put date
                                            ; string on interp 
                                            ; file and output movie file.
                animpar     = animpar,$     ; [xsize,ysize,nframes] for animation
                write_gif   = write_gif,$   ; (I) flag. If set, output frames 
                                            ; as gif files
                write_pict  = write_pict,$  ; (I) flag. If set, output frames 
                                            ; as pict files 
                                            ; (will set nomovie flag)
                nomovie     = nomovie, $    ; (I) flag. If set, do NOT make 
                                            ; movie file
                                            ; NB, will automatically be set if
                                            ; write_pict is 
                decimate    = Decimate, $   ; (I), scalar, Decimate=n means 
                                            ; take every n-th vector.
                CRDecimate  = CRDecimate, $ ; (I), 2-vector. CRDecimate=[p,q] 
                                            ; means take every 
                                            ; p-th column from every q-th row.

                ExcludeCols = ExcludeCols,$ ; (I), string. A comma seperated 
                                            ; string consisting of two 
                                            ; types of fields, a
                                            ; scalar an a field of
                                            ; type 'm:n', where m and
                                            ; n are scalars. The
                                            ; string is expanded into
                                            ; a list of columns to
                                            ; exclude. Ex. ExcludeCol='0,40:42,75
                                            ; means exclude columns 0,
                                            ; 40,41,42 and 75.
                Nscat       = Nscat, $      ; (I) flag. If set, expect NSCAT data
                Pid         = Pid  , $      ; (I) scalar. PID to expect in lock file.
                interp_file= interp_file    ; (I) string. Fully Qualified name 
                                            ; of interpolated wind file file



  Forward_Function GetInterpFiles
  rcsid = "$Id$"

  user = getenv('USER')

  IF n_Elements(pid) NE 0 THEN $
  auto_movie_cronjob = ( CheckForLock( pid, 'auto_mocie.lock', $
                                     user, dir='/tmp') EQ 1)
  catch, error_status
  IF error_status NE 0 THEN BEGIN
    IF auto_movie_cronjob THEN $
     printf, llun, 'ERROR: ' + !err_string
    message, !err_string,/cont
    return
  ENDIF 


  write_gif = keyword_set(write_gif)
  write_pict = keyword_set(write_pict)
  IF write_gif AND write_pict THEN BEGIN 
    message," Only one of 'write_gif' or 'write_pict' may be set",/cont
    return
  ENDIF 
  IF write_pict THEN nomovie = 1

  CD,current=cur_dir

  dateit =  keyword_set( dateit )


  IF auto_movie_cronjob THEN BEGIN 
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

  ; Get the current GMT doy and hour
  TodayAsString = TodayAsString(seperator='/')
  
  IF n_elements( date_time ) EQ 0 THEN BEGIN 
    ; Get the current GMT doy and hour
    date_time = TodayAsString
  ENDIF ELSE BEGIN
    ; parse the users input date/time.
    tmp =  str_sep( date_time, '/' )
    nn = n_elements(tmp)
    IF nn lt 3 THEN BEGIN 
      str =  'ERROR: date_time must consist, minimally, of yyyy/mm/dd'
      Message,str,/cont
      IF auto_movie_cronjob THEN BEGIN 
        printf,llun,str
        free_lun,str
      ENDIF 
      return
    ENDIF 
    IF fix(tmp[0]) LE 99 THEN tmp[0] =  '19' + tmp[0]
    today = today()
    date_time = tmp[0] + '/' + tmp[1] + '/' + tmp[2]
    tmp2 = str_sep(TodayAsString,'/')

    IF nn LT 4 THEN date_time =  date_time + tmp2[3]
    IF nn LT 5 THEN date_time =  date_time + tmp2[4]

  ENDELSE 
   
  filter = 'Q*'
  IF nscat THEN filter = 'N*'
    ; Check to see whether there is an interpolated field within 12
    ; hours of the input time. If there is, read it, else , get the
    ; files and make one.
  Interp_files = GetInterpFiles( date_time, time_inc = Time_inc, $
                                 path = anim_path, nscat = nscat, $
                                 filetimes = filetimes, count = nif )
  IF nif NE 0 THEN BEGIN 
      ; Find which is closest to the input time. 
    julft = filetimes.julday + $
              filetimes.hour/24. + $
                filetimes.min/(24.*60)

    idldt_date_time = vaptime2idldt( date_time )
    j1 = idldt_date_time.julday + $
          idldt_date_time.hour/24. + $
           idldt_date_time.min/(60.*24)

    m = (min( abs(julft-j1), ii ))*24.

    IF m LE 12 THEN BEGIN 
      nif = 1
      interp_file = interp_files[ii]
    ENDIF ELSE nif=0
  ENDIF 

  IF nif EQ 0 THEN BEGIN 
      ; Can't find the interpolated field for this time range, so make
      ; one.
    wf =  GetWindFiles( date_time, time_inc=time_inc, path=wpath, $
                      filter=filter, nscat=nscat, count=nf)
    IF nf EQ 0 THEN BEGIN 
      str =  'ERROR: No windfiles in dir ' + wpath + ' in time range '
      Message, str,/cont
      IF auto_movie_cronjob THEN BEGIN 
        printf,llun,str
        free_lun, llun
      ENDIF 
      return
    ENDIF 


    message,'Using time_inc = ' + string( time_inc, form='(i2)'),/cont
    message,'Wind Path = ' + wpath,/cont
    message,'Interp_path = ' + interp_path,/cont
    message,'anim_path = '+anim_path,/cont
    message,'date_time = ' + date_string,/cont
    print,' animpar = ',animpar
    Message,'Using the following files in the interpolation',/cont
    print,transpose(wf)

    message,' Found ' + string(nf,form='(i3)') + ' files ',/cont
    lonpar =  roistr.alonpar
    latpar =  roistr.alatpar
    lonpar(2) =  1.
    latpar(2) =  1.


    data =  Read_Wind_Files( wf, $
                             Decimate=Decimate, $
                             CRDecimate=CRDecimate, $
                             ExcludeCols=ExcludeCols, Nscat=Nscat )


    x =  where( llon GT lonpar(0)-10 AND llon LE lonpar(1)+10 AND $
                 llat GT latpar(0)-10 AND llat LE latpar(1)+10, nx )
    IF nx GT  min_nvect THEN BEGIN 


      t1 = systime(1)
      lonpar =  [0,360,1.] &  latpar=[-60., 60, 1]
      rainf = [12., 10., 6,   4 ] 
      ermax = [50., 20., 10., 5.]
      status = RunSuccor( uu,vv,llon,llat,ui,vi,lonpar,latpar,$
                          rainf=rainf, ermax=ermax )
      IF NOT status THEN BEGIN 
        str = 'ERROR: Bad return from 1st succor'
        Message,str,/cont
        IF auto_movie_cronjob THEN BEGIN 
          printf, llun, str
          free_lun, llun
        ENDIF 
        return
      ENDIF 

      rainf = [10.,6.,3.,2]
      ermax = [10.,6.,3.,2]
      tmp = str_sep( CreationTime,'/')
      time_string = ''
      FOR i=0,3 DO time_string = time_string + tmp[i]
      Ofile = interp_path + "/" + 'QIF-' + time_string + '.hdf'
      status = RunSuccor( uu,vv,llon,llat,ui,vi,lonpar,latpar,$
                          rainf=rainf, ermax=ermax, /reuse, Ofile=Ofile )
      print,'Time to do 2 succors: ',systime(1)-t1, ' seconds '
      IF NOT status THEN BEGIN 
        str = 'ERROR: Bad return from 2nd succor'
        Message,str,/cont
        IF auto_movie_cronjob THEN BEGIN 
          printf, llun, str
          free_lun, llun
        ENDIF 
        return
      ENDIF 

      IF keyword_set( nomovie ) THEN BEGIN 
        message,' INFO: keyword_set( nomovie ) = 1, returning ',/info
        return
      ENDIF 
      CD,anim_path
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
      ANIMATE_WIND_FIELD,$
       ui=ui, vi=vi, $
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

    ENDIF ELSE BEGIN
      str =  "ERROR: Not enough vectors in area: have " + strtrim( nx, 2 ) + $
       " need: " + strtrim( min_nvect,2 )

      message,str,/cont
      IF auto_movie_cronjob THEN begin
        printf, llun, str
        free_lun,llun
      ENDIF 
    ENDELSE 
  ENDIF ELSE BEGIN 
      ; There is already an interpolated file to use.

    CD,anim_path
    mps2knots = 1.9444 ; takes meters/sec to knots
    ANIMATE_WIND_FIELD,Interp_File, $
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
    exe_str =  'dmconvert -f qt -p video,' + $
     ' comp=qt_cvid,squal=0.9,tqual=0.9,rate=15 ' + $
     ' -n gwind.0##,start=1,end=60,step=1 gwind.0## ' + omov_file
    spawn,exe_str,ret
    IF ret(0) NE '' THEN BEGIN
      str =  'ERROR: in dmconvert '
      message,str, /cont
      IF auto_movie_cronjob THEN begin
        printf, llun, str
        free_lun,llun
      ENDIF 
    ENDIF 
  ENDELSE 
  CD,cur_dir
 
END

