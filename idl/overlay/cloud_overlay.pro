;+
;
; NAME: CLOUD_OVERLAY
; $Id$
;
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
; CALLING SEQUENCE: 
;
;       CLOUD_OVERLAY, cloud_file,
;                      date_time,     $ ; time of GOES data used in overlay
;                                       ; (def=current time - 3 hours)
;                                       ; ((yy)yy/mm/dd/hh) If the year field 
;                                       ; is 2 characters, then 1900 will be
;                                       ; added to it.
;                      time_inc,      $ ; select wind files this number 
;                                       ; of hours +/- time given 
;                                       ; in date_time. def=6
;                      wpath = wpath, $ ; path to wind files (def=$VAP_WINDS)
;                      overlay_path = overlay_path,$ ; path to output overlay file
;                                       ; def = $VAP_ROOT/overlay
;                      decimate=decimate,$ ; (I), scalar, decimate=n
;                                          ; means take every n-th vector
;                      CRDecimate=CRDecimate,$ ; (I), 2-vector,
;                                              ; CRDecimate=[p,q
;                                              ; means take every p-th
;                                              ; column of every q-th row
;                      ExcludeCols=ExcludeCols,$ ; (I) string,
;                                             ; excludeCols='0,38:40,75'
;                                             ; means exclude columns 
;                                             ; 0, 38,39,40 and 75.
;                      Length=Length    ; Vector Length
;                      jpeg=jepg        ; Make a jpeg file
;                      gif=gif          ; Make a gif file
;                      ps =  ps         ; make a postscript instead of
;                                       ; a gif or jpeg
;                      gmsType = gmsType, $     ; GmsType, IF set, treat the 
;                                       ; 'cloud_file' name as the 
;                                       ; datetime used in gms5readall 
;                                       ; (for instance)
;                      mapLimits = mapLimits,$ ; for use with GMS5 overlays
;                      min_speed = min_speed, $
;                      max_speed = max_speed, $
;                      thick     = thick, $
;                      rainflag    = rainflag, $
;                      rf_action = rf_action, $
;                      rf_color  = rf_color 
;                      
;
;
;
; 
; INPUTS: 
;
;        Cloud_file - A gridded file (i.e. one created by the compiled
;                     program 'grid_goes')
;        date_time  - The date/ time around which to retrieve
;                     wind files overplotted on the cloud data.
;                     default=current_time-3hours
;
;
; OPTIONAL INPUTS: 
;
;        time_inc   - Select wind files within this time of
;                     'date_time' default=+/- 6 hours
;
;
;	
; KEYWORD PARAMETERS: 
;
;        wpath       - Path to wind files
;                      Default=$VAP_WINDS
;        overlay_path- path to output overlay files,
;                     Default=$VAP_ROOT/overlay/
;        Decimate    - (I) scalar, decimate=n means take
;                      every n-th vector. Default=1, take every
;                      vector. Decimate is ignored if CRDecimate is
;                      present.
;        CRDecimate  - (I), 2-vector, CRDecimate=[p,q] means take
;                      every p-th column from every q-th row.
;                      Default=[1,1] meaning, take every vector.
;        ExcludeCols - (I) string, ExcludeCols='0,38:40,75' means
;                      exclude columns 0, 38,39,40 and 75
;        Length      - Vector Length
;        jpeg        - make a jpeg, the default
;        ps          - Make Postscript file 
;        gif         - make a gif
;        pid         - used with cronjobs
;        MapLimits   - [lonmin, latmin, lonmax, latmax] (only for GMS
;                      overlays.)
;        min_speed   - the minimum WVC speed
;        max_speed   - the maximum WVC speed
;        thick       - the 'thickness' of the arrows.
;        rainflag      - 0=don't use rain flag, 1=use rain flag
;        rf_action   - What to do with the flagging. If 0, don't plot
;                      flagged vectors. If 1, plot using rf_color.
;        rf_color    - The color to plot the vectors, if rf_action=1.
;
;     
;       
;
;
; OUTPUTS: 
;
;  A jpeg file with the overlay.
;
;
; OPTIONAL OUTPUTS:  None
;
;
;
; COMMON BLOCKS:  None
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
; Revision 1.12  2000/03/09 21:09:02  vapuser
; Switched over to the Z-buffer version of goes_overlay.
;
;
; Revision 1.11  2000/03/01 16:38:06  vapuser
; Added 'status' flag in call to overlay
; routines. Check same. Deprecated pseudocolor
; goes_overlay.
;
; Revision 1.10  2000/02/28 18:02:34  vapuser
; Redid some documentation. Put in flags for use
; with Rain Flagged data.
;
; Revision 1.9  1999/10/11 17:29:03  vapuser
; Added jpeg, {min_,max_}speed keywords. Added support for user config
; files.
;
; Revision 1.8  1999/10/05 17:14:02  vapuser
; Changed a 'ne' to 'lt' in nparams() test. Changed default time_inc to
; 6 from 3. Made the call to GetWindFiles look forward and backward in
; time (this to support the hurricane regions). General maintenance.
;
; Revision 1.7  1999/04/06 18:36:37  vapuser
; Added in GMS 5 code
;
; Revision 1.6  1999/01/25 19:46:47  vapuser
; Can't remember.
;
; Revision 1.5  1998/11/20 19:59:47  vapuser
; Incorporated goes_overlay24, making whatever other changes
; were required
;
; Revision 1.4  1998/10/17 00:15:10  vapuser
; Added CRDecimate, ExcludeCols, decimate keywords.
; Killed a few bugs
;
; Revision 1.3  1998/10/06 00:21:57  vapuser
; Added DeEnvVar
;
; Revision 1.2  1998/09/09 17:49:41  vapuser
; Just added some RCS Header Macros
;
;
;-

PRO cloud_overlay, cloud_file,     $ ; full name of grid file
                                ; If keyword GMS is set, this string
                                ; is the 'datetime' used in all the
                                ; 'gms5...pro' routines. See, for
                                ; instance, gms5readall.pro.
                      date_time,     $ ; time used in searching for wind files.
                                       ; (format = yyyy/mm/dd/hh) 
                      time_inc,      $ ; select wind files this number 
                                       ; of hours +/- time given 
                                       ; in date_time. def=6
                      wpath = wpath, $ ; path to wind files (def=$VAP_WINDS)
                      overlay_path = overlay_path,$ ; path to output overlay file
                                       ; def = $VAP_OVERLAY
                      decimate=decimate,$ ; (I), scalar, decimate=n
                                          ; means take every n-th vector
                      CRDecimate=CRDecimate,$ ; (I), 2-vector,
                                              ; CRDecimate=[p,q
                                              ; means take every p-th
                                              ; column of every q-th row
                      ExcludeCols=ExcludeCols,$ ; (I) string,
                                             ; excludeCols='0,38:40,75'
                                             ; means exclude columns 
                                             ; 0, 38,39,40 and 75.
                      length = length, $ ; Length of vectors
                      ps =  ps,$       ; make a postscript file.
                      gif=gif,$        ; Make gif file
                      jpeg=jpeg, $     ; make jpeg file.
                      pid=pid,$        ; Used with cron jobs
                      gmsType = gmsType, $     ; GmsType, IF set, treat the 
                                       ; 'cloud_file' name as the 
                                       ; datetime used in gms5readall 
                                       ; (for instance)
                      mapLimits = mapLimits,$ ; for use with GMS5 overlays
                      min_speed = min_speed, $
                      max_speed = max_speed, $
                      thick     = thick, $
                      rainflag    = rainflag, $
                      rf_action = rf_action, $
                      rf_color  = rf_color 
                      


 Rcs_id = "$Id$";

  auto_cloud_overlay = n_elements(pid)  NE 0 ; flag for cronjob runs.
  user = getenv('USER')
  IF (user NE "" ) AND auto_cloud_overlay THEN BEGIN 
    lockfile = (findfile('/tmp/' + user + '.cloud_overlay.lock', count=n))(0)
    IF n NE 0 THEN BEGIN 
      openr, lun, lockfile, /get, error=err
      IF err ne 0 THEN BEGIN 
        Message,!error_state.msg,/cont
        return
      ENDIF 
      ppid = 0L
      readf, lun, ppid
      free_lun, lun
      IF ppid ne pid THEN auto_cloud_overlay = 0 ; not our lock file.
    ENDIF ELSE BEGIN 
      Message,'ERROR: No lock file!',/cont
      return
    ENDELSE 
  ENDIF 


  catch, error_status
  IF error_status NE 0 THEN BEGIN
    IF auto_cloud_overlay THEN $
     IF exist(llun) THEN printf, llun, 'ERROR: ' + !err_string
    message, !err_string,/cont
    return
  ENDIF 

  IF n_params() LT   2 THEN BEGIN 
    message,' Both paramters (CLOUD_FILE & DATE_TIME) are required ',/cont
    return
  ENDIF 

  
  read_cfgfile = 0
  cfgname = cfgname()
  cfgpath = '~/.idlcfg/' 
  ff = findfile(cfgpath + cfgname,count=nf)
  IF nf NE 0 THEN BEGIN 
    read_cfgfile = 1
  ENDIF ELSE BEGIN 
    IF getenv('VAP_LIB') NE '' THEN BEGIN 
      cfgpath = deenvvar('$VAP_LIB')
      ff = findfile(cfgpath + cfgname,count=nf)      
      read_cfgfile = (nf NE 0)
    ENDIF
  ENDELSE   

  IF read_cfgfile THEN BEGIN 
    print,' Reading CFG file ' + cfgname
    read_cfgfile,cfgname, cfg,path=cfgpath
    IF n_elements(cfg) NE 0 THEN BEGIN 
      print,'CFG found! Details follow:'
      help,cfg,/st
    ENDIF 
  ENDIF 

  gms =  0
  IF N_Elements(gmsType) NE 0  THEN BEGIN
    gms = 1
    gms5datetime = cloud_file
  ENDIF 
    

  chkcfg,'PS',ps,cfg,/bool
  chkcfg,'GIF',gif,cfg,/bool
  chkcfg,'JPEG',jpeg,cfg,/bool

  ;ps =  keyword_set( ps );
  ;gif = keyword_set(gif)
  ;jpeg = (gif OR ps ) EQ 0;

  jpeg =  jpeg OR ( (gif AND ps) EQ 0)

  CASE 1 OF 
    ps: OutputType = 'Postscript'
    gif: OutputType =  'Gif'
    Jpeg: OutputType =  'Jpeg'
    ELSE:
  ENDCASE
  Message,'File will be output as ' + OutputType,/info

  chkcfg,'TIME_INC',time_inc,cfg
  chkcfg,'WPATH',wpath,cfg
  chkcfg,'OVERLAY_PATH',overlay_path,cfg
  chkcfg,'CRDECIMATE',crdecimate,cfg
  chkcfg,'DECIMATE',decimate,cfg

  chkcfg,'MIN_SPEED',min_speed,cfg
  chkcfg,'MAX_SPEED',max_speed,cfg
  chkcfg,'LENGTH',length,cfg
  chkcfg,'thick',thick,cfg

  chkcfg,'RAINFLAG',rainflag,cfg
  chkcfg,'RF_ACTION',rf_action,cfg
  chkcfg,'RF_COLOR',rf_color,cfg

  IF N_elements( time_inc ) EQ 0 THEN time_inc = 6
  IF n_elements( wpath ) EQ 0 THEN wpath =  '$VAP_WINDS'
  IF n_elements( overlay_path ) EQ 0 THEN overlay_path =  '$VAP_OVERLAY'

  IF n_elements( min_speed ) EQ 0 THEN min_speed = 2
  IF n_elements( max_speed ) EQ 0 THEN max_speed = 25
  IF n_elements(length) EQ 0 THEN length = 2
  IF n_elements( thick     ) EQ 0 THEN thick = 1

  IF n_elements(CRDecimate) NE 2 THEN BEGIN 
    IF n_elements(decimate) EQ 0 THEN CRDecimate = [1,1]
  ENDIF 

  IF n_elements(rainflag) EQ 0 THEN rainflag = 0
  IF n_elements(rf_action) EQ 0 THEN rf_action = 1
  IF n_elements(rf_color) EQ 0 THEN rf_color =  0l

  cd,current=cur_dir



  IF auto_cloud_overlay THEN BEGIN 
    openw, llun, lockfile, /get, error= err
    IF err NE 0 THEN BEGIN 
      message,!err_string,/cont
      return
   ENDIF 
  ENDIF 

  CASE 1 OF 
    strpos( cloud_file, 'GOES' ) NE -1: grid_type = 'GOES'
    gms EQ 1 : BEGIN 
      grid_type = 'GMS'
;      str =  'ERROR: GMS not implemented'
;      Message,str,/cont
;      IF auto_cloud_overlay THEN BEGIN 
;        printf, llun, str
;        free_lun, llun
;      ENDIF 
;      return
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
  overlay_path = DeEnvVar(overlay_path)
  print,' overlay_path = ',overlay_path



    ; CD to the directory where the overlay will reside.
  CD,overlay_path

  IF grid_type EQ 'GOES' THEN BEGIN 
    openr,rlun, cloud_file, /get_lun, error= err
    IF err NE 0 THEN BEGIN
      message,!err_string,/cont
      IF auto_cloud_overlay THEN $
       printf, llun,' ERROR: ' + !err_string
      return
    ENDIF 
    free_lun,rlun
  ENDIF 

  str =  ' Taking wind data from   ' + wpath
  message,str,/info
  IF auto_cloud_overlay THEN $
    printf,llun, "INFO: " + str
  str =  ' Putting output in       ' + overlay_path
  Message,str ,/info
  IF auto_cloud_overlay THEN $
    printf,llun,"INFO: " + str
  str = ' Using time increment of ' + strtrim( time_inc, 2 ) + ' hours'
  Message,str,/info
  IF auto_cloud_overlay THEN $
    printf,llun,"INFO: " + str
  str = ' Using date_time of      ' + date_time
  Message,str,/info
  IF auto_cloud_overlay THEN $
    printf,llun,"INFO: " + str

  str = ' Using length of      ' + strtrim(length,2)
  Message,str,/info
  IF auto_cloud_overlay THEN $
    printf,llun,"INFO: " + str


  str = ' Using thick of      ' + strtrim(thick,2)
  Message,str,/info
  IF auto_cloud_overlay THEN $
    printf,llun,"INFO: " + str

  str = ' Using min_speed of      ' + strtrim(min_speed,2)
  Message,str,/info
  IF auto_cloud_overlay THEN $
    printf,llun,"INFO: " + str

  str = ' Using max_speed of      ' + strtrim(max_speed,2)
  Message,str,/info
  IF auto_cloud_overlay THEN $
    printf,llun,"INFO: " + str

  str = ' Using rainflag of       ' + strtrim(rainflag,2)
  Message,str,/info
  IF auto_cloud_overlay THEN $
    printf,llun,"INFO: " + str

  str =  ' IDL Release Env = ' + strtrim(getenv('IDL_RELEASE_ENV'),2)
  Message,str,/info
  IF auto_cloud_overlay THEN $
    printf,llun,"INFO: " + str

  wf = GetWindFiles( date_time, delta=time_inc, path= wpath, filter='Q*', /twoway)
  nn = where(strlen(wf) NE 0, nf)
  IF nf NE 0 THEN wf = wf[nn]

  
;  IF strlen(wf[0]) EQ 0 AND nf EQ 1 THEN BEGIN 
;    str = 'ERROR: No wind files in dir ' + WPATH
;    Message,str,/cont
;    IF auto_cloud_overlay THEN BEGIN 
;      printf,llun,str
;      free_lun,llun
;    ENDIF 
;    return 
;  ENDIF ELSE BEGIN

      ; Get the visual name, it determines which 
      ; version of goes_overlay to call.
;    Device,Get_Visual_Name= visual
;    visual = strupcase(visual)
    

    str = 'INFO: Found ' + strtrim(nf,2) + ' wind files'
    Message,str,/info
    IF auto_cloud_overlay THEN $
      printf,llun,"INFO: " + str
    FOR ff=0,nf-1 DO BEGIN 
      str = 'INFO: ' + wf[ff]
      Message,str,/info
      IF auto_cloud_overlay THEN $
        printf,llun,"INFO: " + str
    ENDFOR 
          

    CASE grid_type OF 
      'GOES': BEGIN 
;        IF visual EQ 'PSEUDOCOLOR' THEN BEGIN 
;          GOES_OVERLAY, cloud_file, wfiles=wf, $
;           minspeed=min_speed, maxspeed=max_speed, xsize=960,ysiz=720,$
;            len=length,outfile=ofile, thumbnail=thumbnail, $
;             Decimate=decimate, CRDecimate=CRDecimate, $
;              ExcludeCols=ExcludeCols, ps=ps, jpeg=jpeg, gif=gif, $
;               thick=thick, rainflag=rainflag, rf_action=rf_action, $
;                 rf_color=rf_color, status=status
;          Message,'Pseudo-color mode obselete!'
;        ENDIF ELSE BEGIN 
          GOES_OVERLAY,cloud_file,windFiles=wf,$
           minspeed=min_speed, maxspeed=max_speed, xsize=960,ysiz=720, $
            len=length,outfile=ofile, thumbnail=thumbnail, $
             Decimate=decimate, CRDecimate=CRDecimate, $
              ExcludeCols=ExcludeCols, ps=ps, gif=gif, jpeg=jpeg, $
                thick=thick, rainflag=rainflag, $
                 rf_action=rf_action, rf_color=rf_color, status=status
;        ENDELSE 
      END
       'GMS' : BEGIN 
         gms5_overlay, gms5datetime, gmsType, windfiles=wf,$
          minspeed=min_speed, maxspeed=max_speed, $
            len=length,outfile=ofile, thumbnail=thumbnail, $
             Decimate=decimate, CRDecimate=CRDecimate, $
              ExcludeCols=ExcludeCols, ps=ps, jpeg=jpeg, gif=gif, $
                maplimits=MapLimits, thick=thick, rainflag=rainflag, $
                  rf_action=rf_action, rf_color=rf_color, $
                    status = status
      END
    ENDCASE 

    IF status NE 1 THEN $
      Message,"Error in overlay processing"

    IF auto_cloud_overlay THEN BEGIN 
      openw, wlun, '/tmp/auto_cloud_overlay_output_file',/get,error=err
      IF err NE 0 THEN BEGIN
        str =  'ERROR: ' + !err_string
        printf,llun,str
        Message,str,/cont
        RETURN
      ENDIF 
      Printf, wlun, ofile
      printf, wlun, thumbnail
      free_lun, wlun
    ENDIF 
;  ENDELSE  
  IF exist( llun ) THEN   free_lun,llun
  cd,cur_dir

END 

