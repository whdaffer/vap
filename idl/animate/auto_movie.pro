;+
; $Id$
; NAME:  AUTO_MOVIE
;
; PURPOSE: automatically creates a quicktime movie of the requested
;          region of data. Regions, along with other info,  are
;          defined in the file $VAP_LIBRARY/auto_movie_defs.dat and
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
;                wpath       : path to wind files (def=$VAP_DATA_TOP)
;                interp_path : path to output interp file (def=$VAP_OPS_ANIM)
;                anim_path   : path for output animation files
;                              (def=$VAP_OPS_ANIM/roi/daily)
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
;                lockfile    : Name of lockfile. If this 
;                              keyword isn't present but PID is, 
;                              the routine will create the name of the lockfile 
;                              and look for that file.
;                Interp_file : The interpfile to use.
;                knots       : Report speed in knots. Also, expect
;                              min/max speed to be in knot instead of
;                              meters/sec.
;                minspeed    : minimum speed (m/s unless knots=1)
;                maxspeed    : maximum speed (m/s unless knots=1)
;                Length      : the length of the arrows.
;                thick       ; The thickness of the arrows.
;                outbase     ; (I) the base of the names for the
;                              output frames. the actual names will be
;                              'basename.frame_number.ext' where 'ext'
;                              is 'gif', 'jpeg' or 'pict.' Basename
;                              defaults to 'wind'
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
; Revision 1.19  2002/08/13 20:11:19  vapdev
; Add windfilter keyword to propagate windfilter from Perl down
; to getwindfiles, where it's used.
;
; Revision 1.18  2002/08/12 22:55:10  vapdev
; Add jpeg/outbase keywords and required code.
; Default output to jpeg. took out some commented out code.
;
; Revision 1.17  2002/08/09 23:40:33  vapdev
; Added lockfile keyword and modified how
; to find and use the lockfile.
;
; Revision 1.16  2002/05/03 01:06:25  vapdev
; Changes environmental variables to reflect new vapdev/vaprun env variables.
; Also made sure that all the various env variable routines were being
; called correctly.
;
; Revision 1.15  2001/12/08 00:02:35  vapdev
; Getting rid of obsolete RSI routines and fixing ENV vars
;
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
                                         ; (def=$VAP_DATA_TOP)
                windfilter =  windfilter, $ ; (I). File glob filter used to retrieve 
                                ; wind files for interpolating, of the
                                ; sort used by getwindfiles(), to which
                                ; it will be passed. Default =
                                ; '{QS,SW}*', i.e. take both QuikSCAT
                                ; and SeaWinds data
                interp_path = interp_path , $; (I) path to output 
                                             ; interp file 
                                             ; (def=$VAP_OPS_ANIM)
                anim_path   = anim_path,$ ; (I) path for output animation 
                                          ; files 
                                          ; (def=$VAP_OPS_ANIM/roi/daily)
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
                lockfile =  lockfile , $    ; Name of lockfile. If this 
                                            ; keyword isn't present but PID is, 
                                            ; the routine will create a name and 
                                            ; look for that.
                interp_file= interp_file,$ ; (I) string. Fully Qualified 
                                ; name of interpolated wind file
                knots     = knots, $ ; Report speed in knots, not meters/sec
                minspeed  = minspeed,$ ; min speed (m/s unless 'knots' is set)
                maxspeed  = maxspeed,$ ; max speed (m/s unless 'knots' is set)
                length    = length, $ ; (I) the length of the arrows.
                thick     = thick,  $   ; (I) the thickness of the arrows
                outbase =  outbase    ; (I) the base of the template
                                       ; from which the names of the
                                       ; output frames will be built. The
                                       ; files will have the name
                                       ; outbase.frame_number.ext where ext
                                       ; is 'gif' or 'jpeg' or 'pict.' Mostly
                                       ; used in automated processing to
                                       ; set/get the name of the first frame
                                       ; to use on the web page.




  Forward_Function GetInterpFiles
  message,'Start Time: ' + systime(),/info
  lf = string(10b)

  tmpfilesdir =  getenv('VAP_OPS_TMPFILES')
  IF strlen(tmpfilesdir) EQ 0 THEN BEGIN 
    Message,'Env Variable VAP_OPS_TMPFILES is undefined! Exiting!',/cont
    return
  ENDIF 

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

  ; Check to see if the lockfile name has been passed in and use that
  ; name if it has, otherwise construct the name as of old and check
  ; for that lockfile.

  IF n_elements(lockfile) EQ 0 THEN BEGIN 
    IF n_Elements(pid) NE 0 THEN BEGIN 
      lockfile = 'auto_movie_' + lroi + '.lock'
      auto_movie_cronjob = ( CheckForLock( pid, lockfile, $
                                       user, dir=tmpfilesdir) EQ 1)
    ENDIF 
    IF auto_movie_cronjob THEN $
       lockfile = tmpfilesdir + '/' + user + '.' + lockfile
  ENDIF ELSE auto_movie_cronjob = 1;

  IF auto_movie_cronjob THEN BEGIN 
    openw, llun, lockfile, /get, error= err
    IF err NE 0 THEN BEGIN 
      message,!error_State.msg,/cont
      return
   ENDIF 
  ENDIF 

  

  chkcfg,'GIF',gif,cfg,/bool
  chkcfg,'PICT',pict,cfg,/bool
  chkcfg,'JPEG',jpeg,cfg,/bool

  IF gif AND pict AND jpeg THEN BEGIN 
    str =  " ERROR: Only one of 'gif', 'jpeg' or 'pict' may be set" 
    IF auto_movie_cronjob THEN BEGIN 
      printf, llun, str
      free_lun,llun
    ENDIF 
    Message,str,/cont
    return
  ENDIF

  IF NOT (gif OR pict OR jpeg) THEN jpeg = 1
  IF pict THEN nomovie = 1

  CD,current=cur_dir

  chkcfg,'TIME_INC',time_inc,cfg
  IF N_elements( time_inc ) EQ 0 THEN time_inc =  14.


  chkcfg,'wpath',wpath,cfg
  chkcfg,'windfilter',windfilter,cfg
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
  chkcfg,'outbase',outbase,cfg,/bool

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
  IF n_elements(windfilter) EQ 0 THEN windfilter = '{QS,SW}*'
  IF n_elements(outbase) EQ 0 THEN outbase =  'wind'

  message,'wpath      = ' + wpath,/info
  message,'windfilter    = ' + windfilter,/info
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
  message,'length     = ' + strtrim(length,2),/info
  message,'thick      = ' + strtrim(thick,2),/info  
  message,'outbase    = ' + outbase,/info

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
   
    ; Check to see whether there is an interpolated field within 2
    ; hours of the input time. If there is, read it, else , get the
    ; files and make one.
  Interp_file = GetInterpFiles( date_time, time_inc = Time_inc, $
                                interp_path = interp_path, $
                                Wpath=Wpath, $
                                windfilter=windfilter, $
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

  CD,anim_path

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
   gif=gif, pict=pict, jpeg=jpeg, $
   knots=knots, outbase=outbase



  omov_file =  strlowcase( roi ) 
  IF dateit THEN omov_file =  omov_file + '_' + anim_date_str
  omov_file =  omov_file + '.mov'
  exe_str =  'DMCONVERT ' + omov_file
  CASE 1 OF 
    jpeg EQ 1: ext = "jpeg"
    gif EQ 1: ext =  "gif"
    pict EQ 1: ext =  "pict"
  ENDCASE 
  exe_str =  exe_str + " " + outbase + ".0\#\#." + ext
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
    tmpfile = tmpfilesdir + '/auto_movie_mov_filename_'+lroi
    IF n_elements(pid) NE 0 THEN tmpfile =  tmpfile + '_' +strtrim(pid,2)
    openw,lun,tmpfile,/get,error=err
    IF err THEN BEGIN 
      printf,llun,'ERROR: ' + !error_state.msg
      free_lun, llun
    ENDIF 
    printf, lun, omov_file
    printf, lun, ext
    free_lun, lun
  ENDIF 

  IF auto_movie_cronjob THEN $
     IF n_elements(llun) NE 0 THEN free_lun, llun

  CD,cur_dir
  message,'End Time: ' + systime(),/info
  Message,'Done!',/info
 
END




