;+
; NAME:  Tropical_Storms_Overlay.pro
; $Id$
; PURPOSE:  Wrapper to goes_overlay.pro and gms5_overlay.pro, just
;          like cloud_overlay.pro, to facilitate automatic web processing.
;
; AUTHOR:  whd
;
; CATEGORY:  Qscat/Seawinds Vap processing
;
; CALLING SEQUENCE:  Tropical_Storms_overlay, cloud_file, date_time,
;                   time_inc, wpath=wpath, overlay_path=overlay_path,
;                   windfiles=windfiles, decimate=decimate, crdecimate=crdecimate,
;                   exclude_cols=exclude_cols, length=length, ps=ps,
;                   gif=gif, jpeg=jpeg, maplimits=maplimits,
;                   minspeed=minspeed, maxspeed=maxspeed, thick=thick,
;                   rainflag=rainflag, rf_action=rf_action,
;                   rf_color=rf_color, oplot=oplot,
;                   keepaspect=keepaspect, gridlines=gridlines, help=help
;
; 
; INPUTS: 
;
;        Cloud_file - A gridded file (i.e. one created by the compiled
;                     program 'grid_goes')
;
; OPTIONAL INPUTS: 

;
;        date_time  - The date/ time around which to retrieve
;                     wind files overplotted on the cloud data.
;                     default=current_time-3hours
;
;
;        time_inc   - Select wind files within this time of
;                     'date_time' default=+/- 6 hours
;
;
;
;    One may either input the wind files directly, using the WFILES
;    keyword, or one may specify a date_time/time_inc and this routine
;    will find those files which have *any* data within this
;    timerange. 
;
;    If wfiles is present and non-empty, date_time/time_inc are ignored.
;	
; KEYWORD PARAMETERS: 
;
;        wpath       - Path to wind files
;                      Default=$VAP_WINDS
;        overlay_path- path to output overlay files,
;                     Default=$VAP_ROOT/tropical_storms/
;
;        windfiles   - (I) string array. List of fully qualified wind
;                      files to be overlaid on the cloud data (if it
;                      exists)
;
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
;        outfile     - (I) scalar string. Communicate the name of the
;                      output file. If this isn't set, the name is
;                      constructed in {gms5,goes}_overlay and passed
;                      back to this routine, from whence it is written
;                      out.
;        MapLimits   - [lonmin, latmin, lonmax, latmax] (only for GMS
;                      overlays.)
;        title       - A string to be sent to {gms5,goes}_overlay. 
;                      This string is prepended to a string
;                      constructed within those routines giving
;                      information about the cloud data used. This
;                      keyword is uninterpreted by this routine and is
;                      passed without modification to the called
;                      routine.
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
;                      ptitle      : string array,  
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
;       keepaspect: flag. If set, try to maintain the aspect ratio as
;                   defined by the lat/lon range. This keyword is
;                   passed directly to goes/gms5_overlay, which
;                   actually implements it.
;
;       gridlines: flag. If set, put a lat/lon graticule on the plot.
;
;       help:  emit a help message.
;
; OUTPUTS:  
;
;  An image file of the specified sort.
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
; Revision 1.1  2001/02/21 01:04:26  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

PRO tropical_storms_overlay, cloud_file,     $ ; full name of grid file
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
                      windfiles=windfiles, $; List of wind files to be overlaid.
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
                      outfile=outfile,$;
                      lockfile=lockfile,$ ; Used with cron jobs
                      gmsType = gmsType, $     ; GmsType, IF set, treat the 
                                       ; 'cloud_file' name as the 
                                       ; datetime used in gms5readall 
                                       ; (for instance)
                      mapLimits = mapLimits,$ ; for use with GMS5 overlays
                      title=title, $
                      subtitle=subtitle, $
                      min_speed = min_speed, $
                      max_speed = max_speed, $
                      thick     = thick, $
                      rainflag    = rainflag, $
                      rf_action = rf_action, $
                      rf_color  = rf_color, $
                      oplot     = oplot, $
                      gridlines=gridlines, $
                      keepaspect=keepaspect, $
                      help      = help
                      


 Rcs_id = "$Id$";

  catch, error_status
  IF error_status NE 0 THEN BEGIN
    IF cronjob THEN $
     IF exist(llun) THEN BEGIN 
       printf, llun, 'ERROR: ' + !error_state.msg
       free_lun, llun
     ENDIF 
    message, !error_state.msg,/cont
    return
  ENDIF 

  IF n_params() LT 1 OR keyword_set(help) THEN BEGIN 
    message,' Paramter CLOUD_FILE is required ',/cont
    Usage, "TROPICAL_STORMS_OVERLAY, cloud_file [ (, date_time[,time_inc] | wfiles=wfiles),wpath=wpath,overlay_path=overlay_path,decimate=decimate,CRDecimate=CRDecimate,ExcludeCols=ExcludeCols,Length=Length,/jpeg|/gif|/ps,gmsType=gmsType,mapLimits=mapLimits,min_speed=min_speed,max_speed=max_speed,thick=thick,rainflag=rainflag,rf_action=0|1,rf_color=rf_color,oplot=oplot,gridlines=gridlines, keepaspect=keepaspect, /help]"
    status = 0
    return
  ENDIF 


  cloud_file = strcompress(cloud_file,/remove_all)


  cronjob = n_elements(lockfile)  NE 0 ; flag for cronjob runs.
  IF n_elements(keepaspect) NE 0 THEN keepaspect = keyword_set(keepaspect)
  IF n_elements(gridlines) NE 0 THEN gridlines = keyword_set(gridlines)

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
    cfgname =  cfgpath + "/" + cfgname
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

;  IF n_elements(date_time) EQ 0 THEN $
;    date_time =  idldt2vaptime( dt_subtract( today(), hour=time_inc) )

  IF n_elements( wpath ) EQ 0 THEN wpath =  '$VAP_DATA_TOP'
  IF n_elements( overlay_path ) EQ 0 THEN overlay_path =  '$VAP_OPS_TS_OVERLAY'

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



  overlay_path = DeEnvVar(overlay_path)
  print,' overlay_path = ',overlay_path



    ; CD to the directory where the overlay will reside.
  CD,overlay_path, current=cur_dir
  

  IF cronjob THEN BEGIN 
    openr, llun, lockfile,/ get, error=err
    IF err NE 0 THEN Message,!error_state.msg
    pid = 0L
    readf, llun, pid &  free_lun, llun
    pid = strtrim(pid,2)
    openw, llun, lockfile, /get, error= err
    IF err NE 0 THEN Message,!error_state.msg
  ENDIF 

  IF gms EQ 1  THEN $
    grid_type = 'GMS' ELSE $
    grid_type = 'GOES'

  str =  ' Taking wind data from   ' + wpath
  message,str,/info
  IF cronjob THEN $
    printf,llun, "INFO: " + str
  str =  ' Putting output in       ' + overlay_path
  Message,str ,/info
  IF cronjob THEN $
   printf,llun,"INFO: " + str


  str = ' Using length of      ' + strtrim(length,2)
  Message,str,/info
  IF cronjob THEN $
    printf,llun,"INFO: " + str


  str = ' Using thick of      ' + strtrim(thick,2)
  Message,str,/info
  IF cronjob THEN $
    printf,llun,"INFO: " + str

  str = ' Using min_speed of      ' + strtrim(min_speed,2)
  Message,str,/info
  IF cronjob THEN $
    printf,llun,"INFO: " + str

  str = ' Using max_speed of      ' + strtrim(max_speed,2)
  Message,str,/info
  IF cronjob THEN $
    printf,llun,"INFO: " + str

  str = ' Using rainflag of       ' + strtrim(rainflag,2)
  Message,str,/info
  IF cronjob THEN $
    printf,llun,"INFO: " + str


  str = ' Using time increment of ' + strtrim( time_inc, 2 ) + ' hours'
  Message,str,/info
  IF cronjob THEN $
    printf,llun,"INFO: " + str


  IF n_elements(maplimits) EQ 4 THEN BEGIN 
    str = ' Using mapLimits: [' + $
      string(maplimits,format="(4(f7.2,:,','))") + ']'
    Message,str,/info
  IF cronjob THEN $
    printf,llun,"INFO: " + str
  ENDIF 

  str =  ' IDL Release Env = ' + strtrim(getenv('IDL_RELEASE_ENV'),2)
  Message,str,/info
  IF cronjob THEN $
    printf,llun,"INFO: " + str

  IF n_elements(windfiles) eq 0 THEN BEGIN

    IF n_elements(date_time) NE 0 THEN BEGIN 
      str = ' Using date_time of      ' + date_time
      Message,str,/info
      IF cronjob THEN $
        printf,llun,"INFO: " + str

      windfiles = GetWindFiles( date_time, delta=time_inc, path= wpath, $
                                filter='Q*', /twoway)
    ENDIF 

  ENDIF 


  IF n_elements(windfiles) NE 0 THEN BEGIN 
    nn = where(strlen(windfiles) NE 0, nf)
    IF nf NE 0 THEN BEGIN 
      windfiles = windfiles[nn]

      str = 'INFO: Found ' + strtrim(nf,2) + ' wind files'
      Message,str,/info
      IF cronjob THEN $
       printf,llun,"INFO: " + str
      FOR ff=0,nf-1 DO BEGIN 
        str = 'INFO: ' + windfiles[ff]
        Message,str,/info
        IF cronjob THEN $
         printf,llun,"INFO: " + str
      ENDFOR 

    ENDIF 
  ENDIF 

  CASE grid_type OF 
    'GOES': GOES_OVERLAY,cloud_file,windFiles=windfiles,$
      minspeed=min_speed, maxspeed=max_speed, xsize=960,ysiz=720, $
       len=length,outfile=outfile, thumbnail=thumbnail, $
        Decimate=decimate, CRDecimate=CRDecimate, $
         ExcludeCols=ExcludeCols, ps=ps, gif=gif, jpeg=jpeg, $
          thick=thick, rainflag=rainflag, $
            rf_action=rf_action, rf_color=rf_color, $
             mapLimits=mapLimits, status=status, oplot=oplot, Title=Title, $
              subtitle=subtitle, keepaspect=keepaspect, gridlines=gridlines
    'GMS' : gms5_overlay, gms5datetime, gmsType, windfiles=windfiles,$
          minspeed=min_speed, maxspeed=max_speed, $
            len=length,outfile=outfile, thumbnail=thumbnail, $
             Decimate=decimate, CRDecimate=CRDecimate, $
              ExcludeCols=ExcludeCols, ps=ps, jpeg=jpeg, gif=gif, $
                maplimits=MapLimits, thick=thick, rainflag=rainflag, $
                  rf_action=rf_action, rf_color=rf_color, $
                    status = status, oplot=oplot, Title=Title, $
                       subtitle=subtitle, keepaspect=keepaspect, gridlines=gridlines
  ENDCASE 

  IF status NE 1 THEN $
    Message,"Error in overlay processing"

;  IF cronjob THEN BEGIN 
;    file = "cronjob_output_file."+pid
;    openw, wlun, file,/get,error=err
;    IF err NE 0 THEN BEGIN
;      str =  'ERROR: ' + !err_string
;      printf,llun,str
;      Message,str,/cont
;      status = 0
;      RETURN
;    ENDIF 
;    Printf, wlun, outfile
;    printf, wlun, thumbnail
;    free_lun, wlun
;    status = 1
;  ENDIF 
;  IF exist( llun ) THEN   free_lun,llun

  cd,cur_dir
  
END 
