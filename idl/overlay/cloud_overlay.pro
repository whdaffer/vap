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
;       CLOUD_OVERLAY, cloud_file, wfilter[, date_time, time_inc,
;                      wpath = wpath, $
;                      overlay_path = overlay_path,$
;                      decimate=decimate,$
;                      CRDecimate=CRDecimate,$
;                      ExcludeCols=ExcludeCols,$
;                      Length=Length,$
;                      jpeg=jepg,$
;                      gif=gif,$
;                      ps =  ps,$
;                      gmsType = gmsType, $ 
;                      mapLimits = mapLimits,$ 
;                      min_speed = min_speed, $
;                      max_speed = max_speed, $
;                      thick     = thick, $
;                      rainflag  = rainflag, $
;                      rf_action = rf_action, $
;                      rf_color  = rf_color, $
;                      oplot     = oplot, $
;                      keepaspect = keepaspect, $
;                      gridlines  = gridlines, $
;                      help        = help ]
;                      
;
;
;
; 
; INPUTS: 
;
;        Cloud_file - A gridded file (i.e. one created by the compiled
;                     program 'grid_goes') (required)
;
;        wfilter -    Filter to use in searching for wind data (required)
;
;
;
; OPTIONAL INPUTS: 
;
;        date_time  - The date/ time around which to retrieve
;                     wind files overplotted on the cloud data.
;                     default=current_time - time_inc/2
;
;        time_inc   - Select wind files within this time of
;                     'date_time': default=+/- 6 hours
;
;
;	
; KEYWORD PARAMETERS: 
;
;        wpath       - Path to wind files
;                      Default=$VAP_DATA_TOP
;        overlay_path- path to output overlay files,
;                      Default=$VAP_OPS_OVERLAY
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
;        lockfile    - used with cronjobs
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
;        oplot         : (I), array of structures (can be singleton)
;                        having the following form (NB, only those
;                        quantities marked *required* are ... um.. well
;                        required. Everything else takes default values if
;                        absent.)
;
;                    { lon        : float array (*required*); 
;                         (The 'x' locations where to
;                          plot symbol, data cordinates)
;                      lat        : float array (*required*) ; 
;                         (The 'y' location where to
;                          plot symbol, data cordinates)
;                      psym: int    
;                            (the symbol to use, between 0 and
;                               7, default=1 a '+')
;                      symsize: float 
;                               (size of symbol, def=1)
;                      title      : string array,  
;                                 (annotation for for (each) symbol)
;                                 (default = '', which means (see note
;                                 below, 'no annotation')
;                      x_title    : float array, 
;                                  (x location for annotation, 
;                                   `data' corrds unless normal=1,
;                                   default=lon, which will most
;                                   likely be wrong, if you've set normal=1)
;                      y_title    : float array, 
;                                   (y location for annotation, 
;                                   `data' corrds unless normal=1
;                                   default=lat, see note for x_title)
;                      alignment  : float array,    
;                                  (justification of `title'
;                                   on [xy]_title, 0=left, 1=right,
;                                   0.5 is center, between 0 and 1,
;                                   default=1)
;                      orientation: float array 
;                                  (degrees CCW from
;                                           horizontal of title,
;                                   default=0.0)
;                      normal: 0|1 
;                              depending on whether [x|y]_title
;                              are data or normal
;                              coordinates. default=0 => [x|y]_title
;                              are in data corrdinates.
;                      charsize: float (size of characters, def=1)
;                      charthick: int (character thickness, def=1) }
;
;                    
;                      No annotation is done when oplot[i].title is a
;                      null string. The 'alignments' and
;                      'orientations' will be reused, i.e. if there
;                      are 5 titles but only 3 orientations, the 4-th
;                      title will have the same orientation as the
;                      first. Siimilarly for 'alignment.' To be most
;                      sure of the results, the 'title', 'x_title',
;                      'y_title', 'alignment', 'orientation' arrays
;                      should all be the same size, with null strings
;                      in the 'title' array where you don't want any
;                      annotation.
;
;                      If psym is less than 0 the points are connected
;                      by a line.
;
;                      This array of structures is passed directly
;                      through cloud_overlay to
;                      goes_overlay|gms5_overlay without
;                      interpretation. It is those routines that make
;                      use of the structures.
;
;       keepaspect: Flag: keep the aspect ratio of the lat/lon limits,
;                   if possible!
;       gridlines: Flag: put lat/lon graticule on plot.
;
;       help:  emit a help message.
;
; OUTPUTS: 
;
;  A jpeg file with the overlay.
;
; OPTIONAL OUTPUTS:  None
; COMMON BLOCKS:  None
; SIDE EFFECTS: 
; RESTRICTIONS: 
; PROCEDURE: 
; EXAMPLE: 
;
;
; $Id$
;
; Modification History:
;
; $Log$
; Revision 1.20  2002/05/03 01:09:24  vapdev
; Changed various overlay routines to use new vapdev/vaprun env variables.
;
; Revision 1.19  2001/12/08 00:02:36  vapdev
; Getting rid of obsolete RSI routines and fixing ENV vars
;
; Revision 1.18  2001/02/21 01:02:54  vapuser
; Took out 'path=' in call to read_cfgfile
;
; Revision 1.17  2001/02/02 19:02:00  vapuser
; Added keepaspect and gridlines keywords
;
; Revision 1.16  2000/08/15 16:57:28  vapuser
; Added help and oplot keywords. The `oplot' keyword allows users to
; overplot symbols and annotation on the output image. This keyword is
; not used by this routine, but is passed uninterpreted to
; goes|gms5_overlay. The `help' keyword emits a message and exits.
;
; Revision 1.15  2000/05/17 20:44:50  vapuser
; Write file with output filename to OVERLAY_PATH (typically
; $VAP_OPS_OVERLAY).  Give it a name unique to this run using the pid
; communicated through the lock file
;
; Revision 1.14  2000/05/17 16:52:12  vapuser
; Make the routine continue to the end even if the cloud file isn't
; there or can't be read.
;
; Revision 1.13  2000/05/16 15:05:16  vapuser
; Changed from multi-valued 'use_rf' to single-valued 'rainflag'
;
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
                                ; instance, gms5readall.pro. (required)
                      wfilter,       $ ; filter to use in searching
                                       ; for wind files (required)
                      date_time,     $ ; time used in searching for wind files.
                                       ; (format = yyyy/mm/dd/hh/mm) 
                                       ; default=current_time - time_inc/2
                      time_inc,      $ ; select wind files this number 
                                       ; of hours +/- time given 
                                       ; in date_time. def=6
                      wpath = wpath, $ ; path to wind files (def=$VAP_DATA_TOP)
                      overlay_path = overlay_path,$ ; path to output overlay file
                                       ; def = $VAP_OPS_OVERLAY
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
                      lockfile=lockfile,$ ; Used with cron jobs
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
                      rf_color  = rf_color, $
                      oplot     = oplot, $
                      keepaspect=keepaspect, $
                      gridlines=gridlines, $
                      help      = help
                      


 Rcs_id = "$Id$";

  catch, error_status
  IF error_status NE 0 THEN BEGIN
    catch,/cancel
    IF auto_cloud_overlay THEN $
     IF exist(llun) THEN BEGIN 
       printf, llun, 'ERROR: ' + !err_string
       free_lun, llun
     ENDIF 
    message, !err_string,/cont
    return
  ENDIF 

  IF n_params() LT  2  OR keyword_set(help) THEN BEGIN 
    message,' Both paramters (CLOUD_FILE & WIND_FILE_FILTER) are required ',/cont
    Usage, "CLOUD_OVERLAY, cloud_file, wfilter [,date_time,time_inc,wpath=wpath,overlay_path=overlay_path,decimate=decimate,CRDecimate=CRDecimate,ExcludeCols=ExcludeCols,Length=Length,/jpeg|/gif|/ps,gmsType=gmsType,mapLimits=mapLimits,min_speed=min_speed,max_speed=max_speed,thick=thick,rainflag=rainflag,rf_action=0|1,rf_color=rf_color,oplot=oplot,keepaspect=keepaspect, gridlines=gridlines, /help]"
    return
  ENDIF 

  IF size(wfilter,/type) NE size("",/type) THEN message,'Wfilter must be a string!'
  IF strlen(wfilter) EQ 0 THEN message,'wfilter must be a *NON-EMPTY STRING!'

  cloud_file = strcompress(cloud_file,/remove_all)
  auto_cloud_overlay = n_elements(lockfile)  NE 0 ; flag for cronjob runs.
  keepaspect = keyword_set(keepaspect)
  gridlines = keyword_set(gridlines)

  
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
    cfgname = cfgpath + "/" + cfgname
    print,' Reading CFG file ' + cfgname
    read_cfgfile,cfgname, cfg
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

  chkcfg,'KEEPASPECT',keepaspect,cfg,/bool
  chkcfg,'GRIDLINES',gridlines,cfg,/bool

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
  IF n_elements( wpath ) EQ 0 THEN wpath =  '$VAP_DATA_TOP'
  IF n_elements( overlay_path ) EQ 0 THEN overlay_path =  '$VAP_OPS_OVERLAY'

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



  print,'$VAP_OPS_TOP=',getenv('VAP_OPS_TOP')
  overlay_path = DeEnvVar(overlay_path)
  print,' overlay_path = ',overlay_path



    ; CD to the directory where the overlay will reside.
  CD,overlay_path, current=cur_dir
  

  IF auto_cloud_overlay THEN BEGIN 
    openr, llun, lockfile,/ get, error=err
    IF err NE 0 THEN BEGIN 
      Message,!error_state.msg,/cont
      return
    ENDIF 
    pid = 0L
    readf, llun, pid &  free_lun, llun
    pid = strtrim(pid,2)
    openw, llun, lockfile, /get, error= err
    IF err NE 0 THEN BEGIN 
      message,!err_string,/cont
      return
   ENDIF 
  ENDIF 

  IF gms EQ 1  THEN $
    grid_type = 'GMS' ELSE $
    grid_type = 'GOES'

  str =  ' Taking wind data from   ' + wpath
  message,str,/info
  IF auto_cloud_overlay THEN $
    printf,llun, "INFO: " + str

  str =  ' Wind Filter ' + wfilter
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


  str =  ' KeepAspect = ' + strtrim(keepaspect,2)
  Message,str,/info
  IF auto_cloud_overlay THEN $
    printf,llun,"INFO: " + str

  str =  ' Gridlines = ' + strtrim(gridlines,2)
  Message,str,/info
  IF auto_cloud_overlay THEN $
    printf,llun,"INFO: " + str

  wf = GetWindFiles( date_time, delta=time_inc, $
                     path= wpath, filter=wfilter, /twoway)
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
                 rf_action=rf_action, rf_color=rf_color, $
                  mapLimits=mapLimits, status=status, oplot=oplot, $
                    keepaspect=keepaspect, gridlines=gridlines
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
                    status = status, oplot=oplot, $
                      keepaspect=keepaspect, gridlines=gridlines
      END
    ENDCASE 

    IF status NE 1 THEN $
      Message,"Error in overlay processing"

    IF auto_cloud_overlay THEN BEGIN 
      file = OVERLAY_PATH + "/auto_cloud_overlay_output_file."+pid
      openw, wlun, file,/get,error=err
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

