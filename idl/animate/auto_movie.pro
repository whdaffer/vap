;+
; $Id$
; NAME:  AUTO_MOVIE
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
;       time_inc - (I) start_time = date_time-time_inc. Defines the 
;                  'start time' of the range inside of which the
;                  routine will look for wind files. Default=14.
;
;	
; KEYWORD PARAMETERS:  
;
;                roi     : one of 'nepac', 'nwpac', 'npac' or 'nwatl'
;                          the 'region of interest' This Region of
;                          Interest is used to get run time
;                          information from a file named
;                          auto_movie_defs.dat, which currently
;                          resides in $VAP_LIBRARY. 
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
;                min_nvect   : minimum number of vectors needed in 
;                              ROI to make movie.
;                nodate      : flag: if set, DON'T put a date string on the
;                              movie file.
;                animpar     : [xsize,ysize,nframes]
;                gif   : write gif files
;                pict  : write pict files
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
;                Interp_file : The interpfile to use.
;                knots       : Report speed in knots. Also, expect
;                              min/max speed to be in knot instead of
;                              meters/sec.
;                minspeed    : minimum speed (m/s unless knots=1)
;                maxspeed    : maximum speed (m/s unless knots=1)
;                Length      : the length of the arrows.
;                thick       ; The thickness of the arrows.
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
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:  
;
; $Log$
; Revision 1.14  2001/02/21 01:04:46  vapuser
; Took out 'path=' in call to read_cfgfile
;
; Revision 1.13  2001/02/02 19:01:06  vapuser
; Fixed a mps/knots conversion, put some output of
; timing info to the log, changed the log directory to $VAP_ROOT/logs
; and the tmpfile directory to $VAP_ROOT/tmpfiles.
;
; Revision 1.12  2000/02/23 21:57:37  vapuser
; Moved around some error reporting code, changed where the
; lock file gets check/opened. Included the ROI in the names
; of files.
;
; Revision 1.11  2000/02/14 17:23:17  vapuser
; Write out .mov file to /tmp/auto_movie_mov_filename so that
; auto_movie.pl can read it in.
;
; Revision 1.10  1999/10/11 17:27:16  vapuser
; Added code to support user/system 'config file.' Expect min/max speed
; in knots if knots=1. Fixed some bugs.
;
; Revision 1.9  1999/10/06 17:19:46  vapuser
; Changed calls to animate_wind_field to reflect changes in that
; program.
;
; Revision 1.8  1999/10/05 17:09:22  vapuser
; Changed default time from 24 to 14. Changed 'write_xxx' to just 'xxx.'
; Cleaned up the header. Added some missing code having to do with the
; lockfile. Turned times into gmt times. Corrected mps to knots
; conversion.
;
; Revision 1.7  1999/02/04 21:32:26  vapuser
; Added a message about the interpolated field.
;
; Revision 1.6  1999/01/24 20:13:00  vapuser
; Robustified calling dmconvert.
;
; Revision 1.5  1998/10/22 21:04:49  vapuser
; Added some messages, some catch code around first
; auto_movie_cronjob test.
;
; Revision 1.4  1998/10/17 00:18:34  vapuser
; Final 'intermediate stage'. Should be ready to use.
;
; Revision 1.3  1998/10/12 22:36:15  vapuser
; Still intermediate
;
; Revision 1.2  1998/10/06 00:20:23  vapuser
; Intermediate stage, not done yet.
;
; Revision 1.1  1998/10/02 23:01:17  vapuser
; Initial revision
;
;
;-
PRO auto_movie, date_time, $ ; (I) end time of data used in movie 
                             ; (def=current time)
                             ; ((yy)yy/mm/dd/hh(/mi). if yy <= 99 then 1900
                                ; will be added to it. Default=current
                                ; time )
                time_inc,  $ ; (I) select wind files this number of hours 
                             ; back in time. default = 14.
                roi         =  roi, $ ; (I) one of 'nepac', 'nwpac', 
                                      ; 'npac' or 'nwatl', the 
                                      ; 'region of interest'
                alonpar     = alonpar ,$ ; (I) [min,max,inc] lon of movie 
                                            ; (vlonpar =
                                            ; [lonpar-10,lonpar+10, 
                                            ; alonpar(2)] )
                alatpat     = alatpar ,$ ; (I) [min,max,inc] lat of movie
                                            ; (vlatpar =
                                            ; [latpar-10,latpar+10,
                                            ; alatpar(2)] )
                wpath       = wpath, $   ; (I) path to wind files 
                                         ; (def=$VAP_WINDS)
                interp_path = interp_path , $; (I) path to output 
                                             ; interp file 
                                             ; (def=$VAP_ANIM)
                anim_path   = anim_path,$ ; (I) path for output animation 
                                          ; files 
                                          ; (def=$VAP_ANIM/roi/daily)
                min_nvect   =  min_nvect ,$ ; (I) minimum number of vectors 
                                            ; needed in ROI 
                                            ; to make movie.
                nodate      = nodate ,$     ; (I) Flag, if set, DO NOT put date
                                            ; string on interp 
                                            ; file and output movie file.
                animpar     = animpar,$     ; [xsize,ysize,nframes] for 
                                            ; animation
                gif   = gif,$   ; (I) flag. If set, output 
                                            ; frames as gif files
                pict  = pict,$  ; (I) flag. If set, output 
                                            ; frames as pict files 
                                            ; (will set nomovie flag)
                nomovie     = nomovie, $    ; (I) flag. If set, do NOT make 
                                            ; movie file
                                            ; NB, will automatically be 
                                            ; set if pict is 
                decimate    = Decimate, $   ; (I), scalar, Decimate=n means 
                                            ; take every n-th vector.
                CRDecimate  = CRDecimate, $ ; (I), 2-vector. 
                                            ; CRDecimate=[p,q] means take 
                                            ; every p-th column from every 
                                            ; q-th row.

                ExcludeCols = ExcludeCols,$ ; (I), string. A comma seperated 
                                            ; string consisting of two 
                                            ; types of fields, a
                                            ; scalar an a field of
                                            ; type 'm:n', where m and
                                            ; n are scalars. The
                                            ; string is expanded into
                                            ; a list of columns to
                                            ; exclude. Ex. 
                                            ; ExcludeCol='0,40:42,75
                                            ; means exclude columns 0,
                                            ; 40,41,42 and 75.
                Nscat       = Nscat, $      ; (I) flag. If set, expect 
                                            ; NSCAT data
                Pid         = Pid  , $      ; (I) scalar. PID to expect in 
                                            ; lock file.
                interp_file= interp_file,$ ; (I) string. Fully Qualified 
                                ; name of interpolated wind file
                knots     = knots, $ ; Report speed in knots, not meters/sec
                minspeed  = minspeed,$ ; min speed (m/s unless 'knots' is set)
                maxspeed  = maxspeed,$ ; max speed (m/s unless 'knots' is set)
                length    = length, $
                thick     = thick



  Forward_Function GetInterpFiles
  message,'Start Time: ' + systime(),/info
  lf = string(10b)
  CD,current=cur_dir

  rcsid = "$Id$"

  ;mps2knots = 0.51479 ; takes meters/sec to knots
  mps2knots = 1./0.51479 ; takes meters/sec to knots

  user = getenv('USER')
  dateit =  NOT keyword_set( nodate )
  nscat = keyword_set(nscat)


  catch, error_status
  IF error_status NE 0 THEN BEGIN
    IF auto_movie_cronjob THEN BEGIN 
      IF exist(llun) THEN $
        printf, llun, 'ERROR: ' + !error_State.msg
    ENDIF 
    message, 'ERROR: ' + !error_State.msg,/cont
    return
  ENDIF 

  read_cfgfile = 0
  cfgname = cfgname()
  cfgpath = '~/.idlcfg/' 
  ff = findfile(cfgpath + cfgname,count=nf)
  IF nf NE 0 THEN BEGIN 
    read_cfgfile = 1
  ENDIF ELSE BEGIN 
    IF getenv('VAP_LIBRARY') NE '' THEN BEGIN 
      cfgpath = deenvvar('$VAP_LIBRARY')
      ff = findfile(cfgpath + cfgname,count=nf)      
      read_cfgfile = (nf NE 0)
    ENDIF
  ENDELSE   

  IF read_cfgfile THEN BEGIN 
    cfgname = cfgpath + '/' + cfgname
    print,' Reading CFG file ' + cfgname
    read_cfgfile,cfgname, cfg
    IF n_elements(cfg) NE 0 THEN BEGIN 
      print,'CFG found! Details follow:'
      help,cfg,/st
    ENDIF 
  ENDIF 

  chkcfg,'ROI',roi,cfg
  IF n_elements( roi ) NE 0 THEN BEGIN 
    roistr =  READ_AUTO_MOVIE_DEFS( roi )
    IF strpos( roistr.desig, 'ERROR' ) NE -1 THEN BEGIN 
      str =  "ERROR: reading defaults for roi " + roi + " error = " + $
       roistr.desig
;      IF auto_movie_cronjob THEN BEGIN 
;        printf,llun, str
;        free_lun, llun
;      ENDIF 
      Message,str,/cont
      return
    ENDIF 
  ENDIF ELSE BEGIN 
    roi =  'NPAC'
    roistr =  READ_AUTO_MOVIE_DEFS( 'NPAC' )
  ENDELSE 

  lroi = strlowcase(roi)
  auto_movie_cronjob = 0
  IF n_Elements(pid) NE 0 THEN BEGIN 
    lockfile = 'auto_movie_' + lroi + '.lock'
    tmpfilesdir =  getenv('VAP_ROOT') + '/tmpfiles'
    auto_movie_cronjob = ( CheckForLock( pid, lockfile, $
                                     user, dir=tmpfilesdir) EQ 1)
  ENDIF 
  IF auto_movie_cronjob THEN $
     lockfile = tmpfilesdir + '/' + user + '.' + lockfile

  IF auto_movie_cronjob THEN BEGIN 
    openw, llun, lockfile, /get, error= err
    IF err NE 0 THEN BEGIN 
      message,!error_State.msg,/cont
      return
   ENDIF 
  ENDIF 

  

  chkcfg,'GIF',gif,cfg,/bool
  chkcfg,'PICT',pict,cfg,/bool
  ;gif = keyword_set(gif)
  ;pict = keyword_set(pict)

  IF gif AND pict THEN BEGIN 
    str =  " ERROR: Only one of 'gif' or 'pict' may be set" 
    IF auto_movie_cronjob THEN BEGIN 
      printf, llun, str
      free_lun,llun
    ENDIF 
    Message,str,/cont
    return
  ENDIF

  IF NOT (gif OR pict) THEN gif = 1
  IF pict THEN nomovie = 1

  CD,current=cur_dir

  chkcfg,'TIME_INC',time_inc,cfg
  IF N_elements( time_inc ) EQ 0 THEN time_inc =  14.


  chkcfg,'wpath',wpath,cfg
  chkcfg,'alonpar',alonpar,cfg
  chkcfg,'alatpar',alatpar,cfg
  chkcfg,'alonpar',alonpar,cfg
  chkcfg,'interp_path',interp_path,cfg
  chkcfg,'anim_path',anim_path,cfg
  chkcfg,'min_nvect',min_nvect,cfg
  chkcfg,'decimate',decimate,cfg
  chkcfg,'crdecimate',crdecimate,cfg
  chkcfg,'excludecols',excludecols,cfg
  chkcfg,'animpar',animpar,cfg
  chkcfg,'minspeed',minspeed,cfg
  chkcfg,'maxspeed',maxspeed,cfg
  chkcfg,'length',length,cfg
  chkcfg,'thick',thick,cfg

  chkcfg,'knots',knots,cfg,/bool

  IF n_elements( wpath )       EQ 0 THEN wpath       = roistr.wpath 
  IF n_elements( alonpar )     NE 3 THEN alonpar     = roistr.alonpar 
  IF n_elements( alatpar )     NE 3 THEN alatpar     = roistr.alatpar 
  IF n_elements( interp_path ) EQ 0 THEN interp_path = roistr.interp_path
  IF n_elements( anim_path )   EQ 0 THEN anim_path   = roistr.anim_path
  IF n_elements( min_nvect )   EQ 0 THEN min_nvect   = roistr.min_nvect
  IF n_elements( decimate )    EQ 0 THEN decimate    = roistr.decimate
  IF n_elements( CRDecimate )  NE 2 THEN CRDecimate  = roistr.CRDecimate
  IF n_elements( ExcludeCols ) EQ 0 THEN ExcludeCols = roistr.ExcludeCols
  IF n_elements( animpar )     NE 3 THEN animpar     = roistr.anim_par 

  IF n_elements(minspeed) EQ 0 THEN BEGIN 
    minspeed = 1
    IF knots THEN minspeed = minspeed*mps2knots
  ENDIF 
  IF n_elements(maxspeed) EQ 0 THEN BEGIN 
    maxspeed = 30
    IF knots THEN maxspeed = maxspeed*mps2knots
  ENDIF 

  IF n_elements(length) EQ 0 THEN length = 3
  IF n_elements(thick) EQ 0 THEN thick = 1

  message,'wpath      = ' + wpath,/info
  message,'alonpar    = ' + string(alonpar,form='(3(f7.2,:,","))'),/info
  message,'alatpar    = ' + string(alatpar,form='(3(f7.2,:,","))'),/info
  message,'interp_path= ' + interp_path ,/info
  message,'anim_path  = ' + anim_path   ,/info
  message,'min_nvect  = ' + strtrim(min_nvect,2),/info
  message,'decimate   = ' + strtrim(decimate,2)    ,/info
  message,'CRDecimate = ' + string(CRDecimate,form='(2(i2,:,","))')  ,/info
  message,'ExcludeCols= ' + ExcludeCols,/info
  message,'animpar    = ' + string(animpar,form='(3(f7.2,:,","))')     ,/info
  message,'minspeed   = ' + strtrim(minspeed,2),/info
  message,'maxspeed   = ' + strtrim(maxspeed,2),/info
  message,'knots      = ' + strtrim(knots,2),/info
  message,'length      = ' + strtrim(length,2),/info
  message,'thick      = ' + strtrim(thick,2),/info  


  roi =  strupcase(roi)
  MESSAGE,' Looking for roi ' + roi,/info


  ; Get the current GMT doy and hour
  TodayAsString = TodayAsString(separator='/',/gmt)
  
  IF n_elements( date_time ) EQ 0 THEN BEGIN 
    ; Get the current GMT doy and hour
    date_time = TodayAsString
  ENDIF ELSE BEGIN 
    ; parse the users input date/time.
    tdate_time = regularizeVapTime(date_time)
    IF VarType(tdate_time) NE 'STRING' THEN BEGIN 
      str =  'ERROR: Regularizing date_time: ' + date_time
      IF auto_movie_cronjob THEN BEGIN 
        printf, llun, str
        free_lun, llun
      ENDIF 
      Message,str,/cont
      return
    ENDIF ELSE date_time = tdate_time
  ENDELSE 
   
  filter = 'Q*'
  IF nscat THEN filter = 'N*'
    ; Check to see whether there is an interpolated field within 2
    ; hours of the input time. If there is, read it, else , get the
    ; files and make one.
  Interp_file = GetInterpFiles( date_time, time_inc = Time_inc, $
                                 interp_path = interp_path, $
                                 Wpath=Wpath, nscat = nscat, $
                                 decimate=decimate, CRDecimate=CRDecimate, $
                                 ExcludeCols=ExcludeCols, min_nvect=min_nvect,$
                                 filetimes = filetimes, count = nif )
  IF nif EQ 0 THEN BEGIN 
    str =  "ERROR: in GetInterpFiles, " + lf + $
        " either can't find interp files or can't make them"
    IF auto_movie_cronjob THEN BEGIN 
      printf,llun, str
      free_lun, llun
    ENDIF 
    Message,str,/cont
    return
  ENDIF 
  interp_file = interp_file[0]
  Message,' Using interpolated field in file: ' + Interp_file ,/cont

  anim_date_str =  dt2timestr( filetimes[0],sep='')

;  IF nif NE 0 THEN BEGIN 
;      ; Find which is closest to the input time. 
;    julft = filetimes.julday + $
;              filetimes.hour/24. + $
;                filetimes.min/(24.*60)

;    idldt_date_time = vaptime2idldt( date_time )
;    j1 = idldt_date_time.julday + $
;          idldt_date_time.hour/24. + $
;           idldt_date_time.min/(60.*24)

;    m = (min( abs(julft-j1), ii ))*24.

;    IF m LE 12 THEN BEGIN 
;      nif = 1
;      interp_file = interp_files[ii]
;    ENDIF ELSE nif=0
;  ENDIF 

;  IF nif EQ 0 THEN BEGIN 
;      ; Can't find the interpolated field for this time range, so make
;      ; one.
;    wf =  GetWindFiles( date_time, time_inc=time_inc, path=wpath, $
;                      filter=filter, nscat=nscat, count=nf)
;    IF nf EQ 0 THEN BEGIN 
;      str =  'ERROR: No windfiles in dir ' + wpath + ' in time range '
;      Message, str,/cont
;      IF auto_movie_cronjob THEN BEGIN 
;        printf,llun,str
;        free_lun, llun
;      ENDIF 
;      return
;    ENDIF 


;    message,'Using time_inc = ' + string( time_inc, form='(i2)'),/cont
;    message,'Wind Path = ' + wpath,/cont
;    message,'Interp_path = ' + interp_path,/cont
;    message,'anim_path = '+anim_path,/cont
;    message,'date_time = ' + date_string,/cont
;    print,' animpar = ',animpar
;    Message,'Using the following files in the interpolation',/cont
;    print,transpose(wf)

;    message,' Found ' + string(nf,form='(i3)') + ' files ',/cont
;    lonpar =  roistr.alonpar
;    latpar =  roistr.alatpar
;    lonpar(2) =  1.
;    latpar(2) =  1.


;    data =  Read_Wind_Files( wf, $
;                             Decimate=Decimate, $
;                             CRDecimate=CRDecimate, $
;                             ExcludeCols=ExcludeCols, Nscat=Nscat )


;    x =  where( llon GT lonpar(0)-10 AND llon LE lonpar(1)+10 AND $
;                 llat GT latpar(0)-10 AND llat LE latpar(1)+10, nx )
;    IF nx GT  min_nvect THEN BEGIN 


;      t1 = systime(1)
;      lonpar =  [0,360,1.] &  latpar=[-60., 60, 1]
;      rainf = [12., 10., 6,   4 ] 
;      ermax = [50., 20., 10., 5.]
;      status = RunSuccor( uu,vv,llon,llat,ui,vi,lonpar,latpar,$
;                          rainf=rainf, ermax=ermax )
;      IF NOT status THEN BEGIN 
;        str = 'ERROR: Bad return from 1st succor'
;        Message,str,/cont
;        IF auto_movie_cronjob THEN BEGIN 
;          printf, llun, str
;          free_lun, llun
;        ENDIF 
;        return
;      ENDIF 

;      rainf = [10.,6.,3.,2]
;      ermax = [10.,6.,3.,2]
;      tmp = str_sep( CreationTime,'/')
;      time_string = ''
;      FOR i=0,3 DO time_string = time_string + tmp[i]
;      Ofile = interp_path + "/" + 'QIF-' + time_string + '.hdf'
;      status = RunSuccor( uu,vv,llon,llat,ui,vi,lonpar,latpar,$
;                          rainf=rainf, ermax=ermax, /reuse, Ofile=Ofile )
;      print,'Time to do 2 succors: ',systime(1)-t1, ' seconds '
;      IF NOT status THEN BEGIN 
;        str = 'ERROR: Bad return from 2nd succor'
;        Message,str,/cont
;        IF auto_movie_cronjob THEN BEGIN 
;          printf, llun, str
;          free_lun, llun
;        ENDIF 
;        return
;      ENDIF 

;      IF keyword_set( nomovie ) THEN BEGIN 
;        message,' INFO: keyword_set( nomovie ) = 1, returning ',/info
;        return
;      ENDIF 
;      CD,anim_path
;                                  ;
;                                  ; Create the individual gif files for the animation.
;                                  ;
;      ddims  =  [[0,360.,1],[-60,60,1]]
;      lonpar =  roistr.alonpar
;      latpar =  roistr.alatpar
;      tmp =  (lonpar(0:1) + [-10,10])
;      tmp =  fixlonrange(tmp)
;      vlonpar =  [tmp, lonpar(2) ]
;      vlatpar =  latpar
;      vlatpar(0:1) =  -60 >  (vlatpar(0:1) + [-10,10]) <  60

;      mps2knots = 1.9444 ; takes meters/sec to knots
;      ANIMATE_WIND_FIELD,$
;       ui=ui, vi=vi, $
;       ddims=ddims,$
;       lonpar=lonpar, $
;       latpar=latpar, $
;       vlon = vlonpar, $
;       vlat = vlatpar,$
;       animpar=animpar,$
;       min_speed=1,  $             ; meters/sec, will be converted to knots 
;       max_speed=30, $   ; in animate_wind_field (max_speed=30 m/s)
;       title= strtrim( time_inc,2 ) + ' hrs prior to ' + anim_date_str ,$
;       gif=gif, pict=pict

;    ENDIF ELSE BEGIN
;      str =  "ERROR: Not enough vectors in area: have " + strtrim( nx, 2 ) + $
;       " need: " + strtrim( min_nvect,2 )

;      message,str,/cont
;      IF auto_movie_cronjob THEN begin
;        printf, llun, str
;        free_lun,llun
;      ENDIF 
;    ENDELSE 
;  ENDIF ELSE BEGIN 
      ; There is already an interpolated file to use.

    CD,anim_path
;    IF knots THEN BEGIN 
;      minspeed = minspeed/mps2knots
;      maxspeed = maxspeed/mps2knots
;    ENDIF
 
    lonpar =  alonpar[0:1]
    latpar =  alatpar[0:1]
    tmp =  (lonpar(0:1) + [-10,10])
    tmp =  fixlonrange(tmp)
    vlonpar =  [tmp, alonpar[2] ]
    vlatpar =  alatpar
    vlatpar(0:1) =  -60 >  (vlatpar(0:1) + [-10,10]) <  60

    ANIMATE_WIND_FIELD,Interp_File, $
     ddims=ddims,$
     lonpar=lonpar, $
     latpar=latpar, $
     vlon = vlonpar, $
     vlat = vlatpar,$
     animpar=animpar,$
     min_speed=minspeed,  $       ; min/max speed. M/s unless knots=1
     max_speed=maxspeed, $        
     length=length, $
     thick=thick, $
     title= strtrim( time_inc,2 ) + ' hrs prior to ' + anim_date_str ,$
     gif=gif, pict=pict, knots=knots



    omov_file =  'daily_' + strlowcase( roi ) 
    IF dateit THEN omov_file =  omov_file + '_' + anim_date_str
    omov_file =  omov_file + '.mov'
    ; construct string to send to spawn and execute it.
    ;exe_str =  'dmconvert -f qt,loop=loop -p video,' + $
    ;'comp=qt_cvid,squal=0.9,tqual=0.9,rate=15 ' + $
    ; ' -n gwind.0##,start=1,end=60,step=1 gwind.0## ' + omov_file
    exe_str =  '/usr/people/vapuser/scr/DMCONVERT ' + omov_file
    Message,'Calling dmconvert with command line: ',/cont
    print,'    ' + exe_str
    spawn,exe_str,ret
    nn = n_elements(ret)
    i = -1
    good = 1
    done = 0
    REPEAT BEGIN 
      i = i+1
      s1 = strpos(ret[i],'dmconvert') 
      s2 = strpos(ret[i],'bad') 
      
      IF s1 NE -1 OR s2 NE -1 THEN BEGIN
        good = 0
        str =  'ERROR: in dmconvert, Error message:  ' + ret
        done = 1
        IF auto_movie_cronjob THEN begin
          printf, llun, str
          free_lun,llun
        ENDIF 
        message,str, /cont
      ENDIF 
    ENDREP UNTIL done OR (i EQ nn-1)
    IF good AND auto_movie_cronjob THEN BEGIN 
      openw,lun,tmpfilesdir + '/auto_movie_mov_filename_'+lroi,/get,error=err
      IF err THEN BEGIN 
        printf,llun,'ERROR: ' + !error_state.msg
        free_lun, llun
      ENDIF 
      printf, lun, omov_file
      free_lun, lun
    ENDIF 
    free_lun, llun
;  ENDELSE 
   CD,cur_dir
   message,'End Time: ' + systime(),/info
   Message,'Done!',/info
 
END




