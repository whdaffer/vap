;+
; NAME:  goes_overlay
; $Id$
; PURPOSE:  does Goes overlays in true color
;
;
; AUTHOR:  William Daffer
;
;
; CATEGORY:  Qscat Vap
;
;
;
; CALLING SEQUENCE:  goes_overlay, goesfile, 
;                    windfiles   = windfiles, $ 
;                    xsize       = xsize, $     
;                    ysize       = ysize, $     
;                    CRDecimate  = CRDecimate,$
;                    Decimate    = Decimate, $
;                    ExcludeCols = ExcludeCols, $
;                    verbose     = verbose, $
;                    minpix      = minpix, $
;                    minspeed    = minspeed, $
;                    maxspeed    = maxspeed,$
;                    length      = length,$
;                    thick       = thick, $
;                    title       = title,$
;                    BrightMin   = BrightMin, $
;                    BrightMax   = BrightMax, $
;                    SatMin      = SatMin, $
;                    SatMax      = SatMax, $
;                    LandRGB     = LandRGB,$
;                    WaterRGB    = WaterRGB,$
;                    LandHue     = LandHue,$
;                    WaterHue    = WaterHue,$
;                    outfile     = outfile,$
;                    gif         = gif,$        
;                    ps          = ps, $        
;                    scalefac    = scalefac,$
;                    jpeg        = jpeg,$
;                    quality     = quality, $
;                    config      = config, $
;                    scalevec    = scalevec, $
;                    gridlines   = gridlines, $
;                    rainflag      = rainflag, $
;                    rf_action   = rf_action, $
;                    rf_color    = rf_color, $
;                    status      = status, $
;                    oplot       = oplot, $
;                    keepaspect = keepaspect, $
;                    help        = help
;
;
;
; 
; INPUTS:  
;
;   goesfile: A gridded Goes file, as output by Paul Chang's Goes
;             Gridding program.
;
; OPTIONAL INPUTS:  None
;
;
;	
; KEYWORD PARAMETERS:  
;
;     windfiles   : Vector of Strings, List of wind files  to
;                   read. Must be fully qualified filenames
;     xsize       : Xsize of resulting picture (def=640, unless
;                   overridden by `keepaspect')
;     ysize       : ditto, y size (def=480, unless
;                   overridden by `keepaspect')
;
;                 NB. keepaspect will take the values of xsize and
;                 ysize and modify one of them so that the aspect
;                 ratio defined by the latitude/longitude limits in
;                 the grid file will be maintained in the output
;                 picture. To do this, it uses one of the values
;                 (xsize if the lonrange/latrange>=1, ysize if not)
;                 and uses that value and the aspect ratio to solve
;                 for the other. Of course, this only happens when the
;                 cloud files is successfully read, otherwise
;                 keepaspect has *no* effect. So, if aspectratio is,
;                 say 1.3 and the routine has taken the default x,y
;                 size of 640,480, then the new x,y size will be
;                 640,492; 492=640/1.3. The same will occur with
;                 postscript output.
;
;     CRDecimate  : Column/Row Decimate, 2 vector, CRdecimate=[2,3]
;                   means take every 2nd column, every 3rd
;                   row. (def=[1,1], i.e. take every vector.)
;     Decimate    : Decimate: scalare. decimate=m means take every
;                   m-th vector.
;     ExcludeCols : String, A comma seperated list of columns or
;                   ranges of columns to exclude, independently of 
;                   whatever is excluded by CRDecimate and Decimate. 
;                   Exclude='0,38:42,75' will exclude columns 0, 38 ;
;                   through 42, inclusive and column 75. Note, the
;                   string should be ; input using single quotes, as
;                   the IDL interpreter has problems with ; "n where n
;                   is a number. It thinks it's an octal number, and
;                   fails ; with a syntax error when it sees a comma
;                   or colon.
;
;     minpix      : pixels below this number are set to 0 in cloud mask
;     minspeed    : minimum speed (in Meters/sec) to display for wind vectors
;     maxspeed    : maximum speed (in Meters/sec) to display for wind vectors
;     length      : length of vectors (in IDL 'character' units)
;     thick       : Thickness of vectors (in IDL 'character' units)
;     title       : title for plot. This string is prepended to one 
;                   built by the routine, containing the data of the
;                   Goes file used.
;     Subtitle    : A subtitle. No subtitle will appear, if this
;                   string is absent.
;     outfile     : (I/O) if set to a non-empty string on input, the
;                   output file will have this name. If available for
;                   output, but not set to non-empty string, outfile
;                   will return the name of the output file
;
;     BrightMin   : The abscissa at which the brightness transfer
;                   function starts it's linear ramp to 1. The
;                   ordinates of the transfer function for abscissa
;                   less than this abscissa will be set to 0. Input as
;                   a float between 0 and 1 (default=0)
;     BrightMax   : The abscissa at which the brightness function
;                   reaches 1. The ordinates of the transfer function
;                   for abscissa greater than this one will be set to
;                   1. Input as float between 0 and 1. 
;                   (Default = 0.8)
;     SatMin      : The abscissa at which the Saturation transfer
;                   function starts its linear ramp to 0. Ordinates of
;                   the transfer function less than this one will be
;                   set to 1. Input as a float between 0 and 1. (Default=0.)
;     SatMax      : The abscissa at whith the saturation transfer
;                   function reachs 0. Ordinates of the transfer
;                   function for abscissa greater than this one are ;
;                   set to 0. Input as a float between 0 and
;                   1. (Default=0.55)
;     LandRGB     : The 3-vector containing the RGB values for the
;                   Land color (default = [25, 110,   0]

;     WaterRGB    : The 3-vector containing the RGB values for the
;                   Water color. Default=[28, 15,  80]
;
;     LandHue     : the Hue value for land (from a Hue/Brightness/Saturation
;                   triple )
;     WaterHue    : the Hue value for water (from a
;                   Hue/Brightness/Saturation triple
;
;
;     gif         : output as Gif file 
;     scalefac    : scale factor to be used in making Postscript files.
;                   (def=0.05)
;     ps          : output as Postscript file
;     jpeg        : output file as Jpeg file (the default)
;     Quality     : Use this quality when making Jpeg files (def=75)
;     verbose     : flag, if set, emit many messages
;     thumbnail   : if present, this will contain the bytarr with a
;                   thumbnail (30% of the size of the full image)
;                   Not applicable in the case of Postscript output.
;     Config      : Flag, if set a 'configuration window' will open
;                   and allow you to chose the {bright,Sat}{min,max}
;                   values.
;
;     ScaleVec   : Flag, if set, the vectors will be scaled by their
;                   speed. (Mind the similarity with 'scalefac')
;
;     GridLines    : Put down map grid lines after TVing the image.
;
;     Status       : (0), 0 means failure, 1 means success.
;
;     Rainflag       : (I), Flag , 0|1, 0=don't use flag, 1=use flag
;
;     FL_Action    : (I), flag, 0|1 depending on whether you want to
;                    skip plotting rain flagged data (0) or plot it
;                    with the color given in FL_Color(1), Default = 1,
;                    use FL_color.
;
;     FL_Color     : (I), long integer. The 24 bit color to be used
;                    when plotting the rain flagged data, provide
;                    FL_Action=1. The default is black. (Although I
;                    like '80541e'xl, which is a sort of muddy
;                    brown. It's index 12 in the pv colortable. Much
;                    more visible and 'dirty' looking.
;
;    oplot         : (I), array of structures (can be singleton)
;                    having the following form (NB, only those
;                    quantities marked *required* are ... um.. well
;                    required. Everything else takes default values if
;                    absent.)
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
;
;    keepaspect    : (I), flag: If set, maintain the aspect ratio
;                    which present in the gridded file. This keyword
;                    will override input values of xsize/ysize!
;
;     Help:   emit a (long) help message.
;
;
; OUTPUTS:  A file, either a .gif, .jpeg (the default) or a .ps file,
;          depending on the status of these three flags having either
;          a name built from the input goes file name (the default) or
;          whatever is passed in via the 'outfile' keyword. The
;          default output format is Gif and the default name is :
; 
;          GOES_nn_mmm_yyyymmddThh:mm-%aaaa,bbb,cccc,ddd%.ext (ext=gif,jpeg,ps)
;
;          where 
;          nn = 8 or 10
;          mmm = VIS, IR1,IR2,IR3, OR IR4 
;          yyyymmdd = year, month, day, e.g. 19981202
;          hh:mm  = hour:minute
;          aaaa = minimum longitude, 4 digit, signed 
;          bbb  = minimum latitude, 3 digit, signed 
;          cccc = maximum longitude, 4 digit, signed 
;          ddd  = maximum latitude, 3 digit, signed 
;
;          The keyword 'outfile' will contain the output file name if
;          it (i.e. outfile) is  present for output.
;
; OPTIONAL OUTPUTS:  None
;
; COMMON BLOCKS:  goes_overlay_cmn
;
; SIDE EFFECTS:  Vast reduction of memory; this program uses ALOT.
;
; RESTRICTIONS:  
;
; PROCEDURE:  Way to complicated to describe here.
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.18  2001/02/21 01:03:33  vapuser
; Took out 'path=' in call to read_cfgfile
;
; Revision 1.17  2001/02/02 19:06:07  vapuser
; Added keepaspect, gridlines and oplot keywords and
; associated code (alot of it for `oplot')
;
; Revision 1.16  2000/08/15 16:47:41  vapuser
; Added a help keyword. Added the oplot keyword, so that we can now
; overplot symbols on the images.
;
; Revision 1.14  2000/05/17 20:41:03  vapuser
; Fixed a problem with latitude calculation.
;
;
; Revision 1.13  2000/05/17 16:49:52  vapuser
; Make the routine continue to the end even if the goes file isn't
; there or can't be read.
;
; Revision 1.11  2000/05/15 22:58:45  vapuser
; Changed from multi-valued 'use_rf' to the new single-valued rainflag.
;
; Revision 1.10  2000/03/09 21:02:28  vapuser
; Rewrote goes_overlay24 so that it would do everything in a Z buffer
; a plane at a time. So, you get 24bit color in an 8 bit environment
; without having to connect to the X server. Cool, eh?
;
;
;
;  ===== Removed mod log from goes_overlay24.pro ===
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
PRO goes_overlay, goesfile, $
                    windfiles   = windfiles, $ 
                    xsize       = xsize, $     
                    ysize       = ysize, $     
                    CRDecimate  = CRDecimate,$
                    Decimate    = Decimate, $
                    ExcludeCols = ExcludeCols, $
                    verbose     = verbose, $
                    minpix      = minpix, $
                    minspeed    = minspeed, $
                    maxspeed    = maxspeed,$
                    length      = length,$
                    thick       = thick, $
                    title       = title,$
                    subtitle    = subtitle, $
                    outfile     = outfile,$
                    BrightMin   = BrightMin, $
                    BrightMax   = BrightMax, $
                    SatMin      = SatMin, $
                    SatMax      = SatMax, $
                    LandRGB     = LandRGB,$
                    WaterRGB    = WaterRGB,$
                    LandHue     = LandHue,$
                    WaterHue    = WaterHue,$
                    gif         = gif,$        
                    ps          = ps, $        
                    scalefac    = scalefac, $
                    jpeg        = jpeg,$
                    quality     = quality, $
                    thumbnail   = thumbnail, $
                    config      = config , $
                    scalevec    = scalevec, $
                    gridlines   = gridlines, $
                    status      = status, $
                    rainflag    = rainflag, $
                    rf_action   = rf_action, $
                    rf_color    = rf_color , $
                    maplimits   = maplimits, $
                    oplot       = oplot, $
                    keepaspect  = keepaspect, $
                    help        = help


; COMMON goes_overlay_cmn, landel


  status = 1
  savedevice = !d.name
  IF (n_elements(goesfile) EQ 0 AND n_elements(windfiles) EQ 0) OR $
     keyword_set(help) THEN BEGIN 
    Message,'Usage: goes_overlay, goesfile [,windfiles=windfile, xsize=xsize, ysize=ysize, ps=ps | gif=gif | jepg=jpeg,title=title,subtitle=subtitle, CRDecimate=CRDecimate,Decimate=Decimate,ExcludeCols=ExcludeCols, verbose=verbose, minspeed=minspeed, maxspeed=maxspeed, length=length, thick=thick,BrightMin=BrightMin,BrightMax=BrightMax,SatMin=SatMin,SatMax=SatMax,LandRGB=LandRGB,WaterRGB=WaterRGB,LandHue=LandHue,WaterHue=WaterHue,ScaleVec=ScaleVec,rainflag=0|1,rf_action=0|1,rf_color=24bitnumber,mapLimits=mapLimits, oplot=oplot, help=help ] ',/cont
    status = 0
    return
  ENDIF 

  clouddata = 1
  winddata = n_elements(windfiles) NE 0 
  clouddata =  isa(goesfile,/string,/nonempty)

  IF n_elements(keepaspect) NE 0 THEN keepaspect = keyword_set(keepaspect) ELSE keepaspect = 1
  IF n_elements(gridlines) NE 0 THEN gridlines = keyword_set(gridlines) ELSE gridlines = 1

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

  config =  keyword_set(config)
  chkcfg,'GRIDLINES',GridLines,CFG,/bool

  chkcfg,'DEFAULT_WATERRGB',Default_WaterRGB,CFG
  IF n_elements(Default_WaterRGB) EQ 0 THEN $
    Default_WaterRGB = [11,  11, 122] 
  chkcfg,'DEFAULT_LANDRGB',default_LandRGB,CFG
  IF n_elements(Default_LandRGB) EQ 0 THEN $
     Default_LandRGB =  [25, 110,   0]

  Color_Convert, Default_WaterRGB[0],Default_WaterRGB[1],Default_WaterRGB[2],$
   Default_WaterHue,l,s,/rgb_hls
  Color_Convert, Default_LandRGB[0],Default_LandRGB[1],Default_LandRGB[2],$
   Default_LandHue,l,s,/rgb_hls

  chkcfg,'PS',ps,cfg,/bool
  chkcfg,'GIF',gif,cfg,/bool
  chkcfg,'JPEG',jpeg,cfg,/bool
  chkcfg,'VERBOSE',verbose,cfg,/bool

  ;ps = keyword_set(ps)
  ;gif = keyword_set(gif)
  ;jpeg = keyword_set(jpeg)
  ;verbose = keyword_set(verbose)

  IF ps AND gif OR $
     ps AND jpeg OR $
     gif AND jpeg THEN BEGIN 
    Message,'Only one of PS, GIF  or JPEG may be set',/cont
;    tvlct,orig_red,orig_green,orig_blue
;    genv,/restore
    status = 0
    return
  ENDIF 

  IF NOT (ps OR gif OR jpeg) THEN BEGIN 
    Message,' Defaulting to jpeg output ',/info
    jpeg = 1
  ENDIF 

  chkcfg,'QUALITY',quality,cfg

  IF n_elements( Quality ) EQ 0 THEN Quality = 75
  Quality =  1 > Quality < 100

  chkcfg,'SCALEFAC',scalefac,cfg

  IF n_elements(scalefac) EQ 0 THEN BEGIN 
    IF ps THEN scalefac = 0.05 ELSE scalefac = 1
  ENDIF ELSE BEGIN 
    IF gif OR jpeg THEN BEGIN 
      Message,'Keyword SCALEFAC is ignored when creating GIFs/JPEGs',/cont
      scalefac = 1
    ENDIF 
  ENDELSE 

  chkcfg,'KEEPASPECT',keepaspect,cfg
  chkcfg,'MINSPEED',minspeed,cfg
  chkcfg,'MAXSPEED',maxspeed,cfg
  chkcfg,'LENGTH',length,cfg
  chkcfg,'THICK',thick,cfg
  chkcfg,'BRIGHTMIN',brightmin,cfg
  chkcfg,'BRIGHTMAX',brightmax,cfg
  chkcfg,'SATMIN',satmin,cfg
  chkcfg,'SATMAX',satmax,cfg

  IF N_Elements(minspeed) EQ 0 THEN minspeed = 2
  IF n_Elements(maxspeed) EQ 0 THEN maxspeed = 25
  IF n_elements(length)   EQ 0 THEN length = 2.5
  IF n_elements(thick)    EQ 0 THEN thick = 1
  IF n_elements(BrightMin) EQ 0 THEN BrightMin = 0.3
  IF n_elements(BrightMax) EQ 0 THEN BrightMax = 1.
  IF n_elements(SatMin) EQ 0 THEN SatMin = 0.0
  IF n_elements(SatMax) EQ 0 THEN SatMax = 1.

  BrightMin = 0> float(BrightMin) < 1.
  BrightMax = BrightMin+.01> BrightMax < 1.

  SatMin = 0> float(SatMin) < 1.
  SatMax = SatMin+.01> SatMax < 1.


  chkcfg,'LANDHUE',landhue,cfg
  chkcfg,'WATERHUE',waterhue,cfg

  chkcfg,'LANDRGB',landRGB,cfg
  chkcfg,'WATERRGB',waterRGB,cfg

  IF n_elements(LandHue) EQ 0 THEN BEGIN 
    IF n_Elements(LandRGB) NE 3 THEN BEGIN 
      IF n_Elements(LandRGB) NE 0 THEN $
        Message,'LandRGB must be a 3-vector',/cont
      Message,'  Taking default Land Hue = ' + $
       '[' + strjoin( strtrim( Default_LandRGB,2)," ") + ']',/cont
      LandRGB = Default_LandRGB
    ENDIF 
    Color_Convert, LandRGB[0], LandRGB[1], LandRGB[2], LandHue,LandLight,LandSat,/rgb_hls
  ENDIF ELSE BEGIN 
    LandLight = 0.5
    LandSat = 1.
  ENDELSE 

  IF n_elements(WaterHue) EQ 0 THEN BEGIN 
    IF n_Elements(WaterRGB) NE 3 THEN BEGIN 
      IF n_Elements(WaterRGB) NE 0 THEN $
        Message,'WaterRGB must be a 3-vector',/cont
      Message,'  Taking default Water Hue= ' + $
       '[' + strjoin( strtrim( Default_WaterRGB,2)," ") + ']',/cont
      WaterRGB = Default_WaterRGB
    ENDIF 
    Color_Convert, WaterRGB[0], WaterRGB[1], WaterRGB[2], WaterHue,WaterLight,WaterSat,/rgb_hls
  ENDIF  ELSE BEGIN 
    WaterLight = 0.5
    WaterSat = 1.
  ENDELSE 
  
  LandHue =  0> LandHue < 360.
  WaterHue =  0> WaterHue < 360.
  

  chkcfg,'XSIZE',xsize,cfg
  chkcfg,'YSIZE',ysize,cfg

  IF n_Elements(xsize) EQ 0 THEN BEGIN 
    IF ps THEN xsize = 8.4 ELSE xsize = 640 
  ENDIF 
  xoffset = 1.2

  IF n_Elements(ysize) EQ 0 THEN BEGIN 
    IF ps THEN ysize = 6.5 ELSE ysize = 480 
  ENDIF 
  yoffset = 9.5



  chkcfg,'RAINFLAG',rainflag,cfg
  chkcfg,'RF_ACTION',rf_action,cfg
  chkcfg,'RF_COLOR',rf_color,cfg

  IF n_elements(rainflag) EQ 0 THEN rainflag = 0
  IF n_elements(rf_action) EQ 0 THEN rf_action = 1
    ; if rf_action=1, plot the rain flagged data as black
  IF n_elements(rf_color) EQ 0 THEN rf_color =  0l


  IF n_elements(oplot) NE 0 THEN BEGIN 
    IF vartype(oplot) NE 'STRUCTURE' THEN BEGIN 
      Message,"<oplot> must be a STRUCTURE, ignoring it!",/info
    ENDIF ELSE BEGIN 
      tags =  tag_names(oplot)
      x1 = where( tags EQ 'LON' OR tags EQ 'LAT',nx1)
      IF nx1 EQ 0 THEN BEGIN 
        Message,"Structure OPLOT MUST have the two field 'LON' and 'LAT'! -- ignoring it!",/info
      ENDIF ELSE BEGIN 
        checkfor =  ['PSYM','SYMSIZE','CHARSIZE','CHARTHICK',$
                     'TEXTCOLOR','SYMCOLOR','ALIGNMENT','ORIENTATION',$
                     'PTITLE', 'X_TITLE', 'Y_TITLE', 'NORMAL']
        FOR i=0,n_elements(checkfor)-1 DO BEGIN 
          x = where(strpos(tags,checkfor[i]) NE -1, nx )
          IF nx EQ 0 THEN BEGIN 
            CASE checkfor[i] OF 
              'PSYM': PSYM = 1
              'SYMSIZE': symsize = 1
              'CHARSIZE': charsize = 1
              'CHARTHICK': charthick = 1
              'TEXTCOLOR': textcolor = 255
              'SYMCOLOR': symcolor = 255
              'ALIGNMENT': alignment =  1.
              'ORIENTATION': orientation = 0.
              'PTITLE': ptitle = replicate('',n_elements(oplot.lon) )
              'X_TITLE': x_title = oplot.lon
              'Y_TITLE': y_title = oplot.lat
              'NORMAL': normal = 0
            ENDCASE 
          ENDIF ELSE s = execute( checkfor[i] + ' = oplot.' + checkfor[i] )
        ENDFOR 
        toplot = { lon: oplot.lon, lat: oplot.lat, symcolor: symcolor, $
                   psym: psym, symsize: symsize, $
                   ptitle: ptitle, x_title: x_title, y_title: y_title, $
                   alignment: alignment, orientation: orientation, $
                   normal:normal, textcolor: textcolor, charsize: charsize, $
                   charthick: charthick }
      ENDELSE 
    ENDELSE 
  ENDIF 


    ; ============ Start the processing ==================


  start_time = systime(1)
  IF clouddata THEN BEGIN 
    Read_PCGoes,goesfile,limits,GoesData,hdr=hdr, status=status
 
    
    IF NOT status THEN BEGIN 
      Message,'Error Reading file: ' + goesfile,/info
      Message,' -- Trying to get information from filename!',/info
      GoesFilenameStruct = ParseGoesFileName( goesfile )
      IF VarType( GoesFilenameStruct ) NE 'STRUCTURE' THEN BEGIN 
        Message," Trouble parsing filename " + Goesfile + $
          "for info, can't continue!"  ,/info
        status = 0
        return
      ENDIF 

      status = 1
      clouddata = 0

      sat_name = strtrim(GoesFilenameStruct.SatName,2) + " " + $
       strtrim(GoesFilenameStruct.SatNum,2 )

      sensornum = GoesFilenameStruct.Sensornum
      sensors = ['VIS','IR2','IR3','IR4']
      sensor    =  sensors[ sensornum-1 ]
      goes_date = strcompress( $
                  PadAndJustify(GoesFilenameStruct.year, 4, /right ) + $
                  PadAndJustify(GoesFilenameStruct.mm, 2, /right ) + $
                  PadAndJustify(GoesFilenameStruct.dd, 2, /right ) + $
                  'T' + $
                  PadAndJustify(GoesFilenameStruct.hh, 2, /right ) + $
                  PadAndJustify(GoesFilenameStruct.mm, 2, /right ), /remove_all)

      limits = GoesfilenameStruct.limits

      goes_string =  sat_name + ' ' + sensor + ' ('  + goes_date + ')'

      ofile_tmplt = $
        strcompress(sat_name + '_' + sensor + '_' + goes_date ,/remove_all)

    ENDIF ELSE BEGIN 

      ;; We've successfully read the cloud data!

      sat_name = "GOES " + strtrim(hdr.type/10,2)
      sensornum =  hdr.type-(hdr.type/10)*10
      sensors = ['VIS','IR2','IR3','IR4']
      sensor    =  sensors[ sensornum-1 ]

      date = doy2date(hdr.year,hdr.doy)
      year = strtrim(hdr.year)
      hh = (hdr.hhmm)/100
      mm = hdr.hhmm-hh*100

      goes_date = strcompress(year + date[0] + date[1]+ 'T' + $
                  PadAndJustify(hh, 2, /right ) + $
                  PadAndJustify(mm, 2, /right ),/remove_all)

      goes_string =  sat_name + ' ' + sensor + ' ('  + goes_date + ')'
      
      ofile_tmplt = $
        strcompress(sat_name + '_' + sensor + '_' + goes_date ,/remove_all)

      IF keepaspect THEN BEGIN 
        aspect = (hdr.limits[2]-hdr.limits[0])/(hdr.limits[3]-hdr.limits[1])
        IF aspect GE 1 THEN $
         xsize = ysize*aspect ELSE $
         ysize = xsize/aspect
      ENDIF 

    ENDELSE 

  ENDIF ELSE BEGIN 

    Message,"No cloud file, taking data from MapLimits keyword",/info
    IF n_elements(MapLimits) NE 4 THEN BEGIN 
      Message,"No MapLimits! Exiting!",/info
      status = 0
      return
    ENDIF 
    goes_date =  'No_Cloud_Data'
    goes_string =  goes_date
    ofile_tmplt =  strcompress("UNK_UNK_" + goes_date,/remove_all)
    limits = 1.0*MapLimits
    clouddata = 0
    status = 1
  ENDELSE 


  limits = double(limits)
  lonrange = FixLonRange( [ limits[0],limits[2] ])
  



  IF clouddata THEN BEGIN 
    IR =  ( sensornum GT 1 )
    IF ir THEN GoesData = 1023-temporary(GoesData)
    cloudmask = scale(temporary(GoesData),minv=0,maxv=1023)*99

  ENDIF 

  IF N_elements(minpix) EQ 0 THEN minpix = 0

  IF getenv('OVERLAY_CT') NE '' THEN BEGIN 
    ptr = ReadColorTable('$OVERLAY_CT')
  ENDIF ELSE BEGIN 
    ptr = ReadColorTable('$VAP_COLORTABLES/goes_overlay24.ct')
  ENDELSE 
  IF NOT Ptr_Valid(ptr) THEN BEGIN 
    Message,'Error Reading ColorTable!',/cont
    status = 0
    return
  ENDIF 
  CT = *ptr
  ptr_free,ptr

  WIND_START = 1
  N_COLORS = n_elements(ct[0,*])
  N_WIND_COLORS = n_colors-2

  IF winddata NE 0 THEN nn = where(strlen(windfiles) NE 0 , nf) ELSE nf = 0
  IF nf NE 0 THEN BEGIN 
    WindFiles = WindFiles[nn]
    t0 = systime(1)
    windData = Read_Wind_Files(windFiles,$
                               CRDecimate=CRDecimate,$
                               Decimate=Decimate,$
                               ExcludeCols=ExcludeCols, $
                               rainflag=rainflag, $
                               rf_action=rf_action, $
                               rf_index=rfi)
    t1 = systime(1)
    IF verbose THEN print,' Read_wind_Files took: ', t1-t0,$
      ' Seconds '
    t0 = t1

    ndims = size(windData,/n_dim)
    IF ndims NE 2 THEN BEGIN 
      Message,'Error reading data from windfiles ',/cont
      print,'Input Windfiles are ', transpose(windFiles)
      print,'Continuing with overlay without the Wind Data!'
    ENDIF ELSE BEGIN 
      u = windData[*,0]
      v = windData[*,1]
      lon = windData[*,2]
      lat = windData[*,3]
        
      good = where( finite(u) AND finite(v), ngood )
      IF ngood NE 0 THEN BEGIN 
        IF rf_action EQ 1 AND $
         rfi[0] NE -1 THEN BEGIN 
          ii = lonarr(n_elements(u))
          ii[rfi] = 1
          rfi =  where( ii[good], nn)&  ii=0
        ENDIF 

        u = u[good]
        v = v[good]
        lon = lon[good]
        lat = lat[good]

        speed = sqrt( u^2+v^2)
        good = where( speed NE 0, ngood )
        IF ngood NE 0 THEN BEGIN 
          IF ngood NE n_elements(u) THEN BEGIN 
            IF rf_action EQ 1 AND $
              rfi[0] NE -1 THEN BEGIN 
              ii = lonarr(n_elements(u))
              ii[rfi] = 1
              rfi =  where( ii[good],nn)&  ii=0
            ENDIF 

            u = u[good]
            v = v[good]
            lon = lon[good]
            lat = lat[good]
            speed = speed[good]

          ENDIF 
          good = where( lon GE lonrange[0] AND $
                        lon LE lonrange[1] AND $
                        lat GE limits[1] AND $
                        lat LE limits[3], ngood )
          IF ngood NE 0 THEN BEGIN 
            IF rf_action EQ 1 AND $
             rfi[0] NE -1 THEN BEGIN 
              ii = lonarr(n_elements(u))
              ii[rfi] = 1
              rfi =  where( ii[good],nn)&  ii=0
            ENDIF 
            u = u[good]
            v = v[good]
            lon = lon[good]
            lat = lat[good]
            speed = speed[good]
            veccol = BytScl( speed, min=minspeed, $
                             max=maxspeed, $
                             top=N_WIND_COLORS-1) + $
                                WIND_START
            col24 = Rgb2True( veccol, colortable=ct)
              
          ENDIF ELSE BEGIN 
            u = 0
            v = 0
            lon = 0
            lat = 0
            speed = 0
            col24 = 0
          ENDELSE 
          t0 = systime(1)
            
        ENDIF 
      ENDIF 
    ENDELSE 

  ENDIF   


  IF clouddata THEN BEGIN 
    sz = size(cloudmask,/dimensions)
    nlon = 1.0*sz[0]
    nlat = 1.0*sz[1]
    loninc = (lonrange[1]-lonrange[0])/nlon
    latinc = (limits[3]-limits[1])/nlat
    
  ENDIF ELSE BEGIN 

    IF ps THEN BEGIN 
      loninc = (lonrange[1]-lonrange[0] +1)/(2.*72*xsize)
      latinc = (limits[3]-limits[1] +1)/(2.*72*ysize)
    ENDIF ELSE BEGIN 
      loninc = (lonrange[1]-lonrange[0] +1)/xsize
      latinc = (limits[3]-limits[1] +1)/ysize
    ENDELSE 

    nlon =  fix((lonrange[1]-lonrange[0])/loninc+1)
    nlat =  fix((limits[3]-limits[1])/latinc+1)


  ENDELSE 

  lonmin = lonrange[0]
  latmin = limits[1]
  loni = (findgen(nlon)*loninc+lonmin)#(replicate(1.,nlat))
  lati = replicate(1.,nlon)#(findgen(nlat)*latinc+latmin)

  t0 = systime(1)
  land = where( runLandMask(loni,lati), nland )
  t1 = systime(1) 
  IF verbose THEN print,'Time for Landmask : ',t1-t0, ' Seconds '
  t0 = t1

  t1 = systime(1) 
  IF verbose THEN print,'Time for TopoIm : ',t1-t0, ' Seconds '
  t0 = t1

  loni = 0
  lati = 0


  IF ps THEN BEGIN 
    set_plot,'PS'
    ps_form = { XSIZE          : xsize   ,$ 
                XOFF           : xoffset ,$ 
                YSIZE          : ysize   ,$ 
                YOFF           : yoffset ,$ 
                INCHES         : 1       ,$  
                COLOR          : 1       ,$  
                BITS_PER_PIXEL : 8       ,$
                ENCAPSULATED   : 0       ,$
                LANDSCAPE      : 1        }
    device,_extra=ps_form
    ; device,font='
  ENDIF ELSE BEGIN 
    Message,'Setting Z device',/info
    set_plot,'z'
    Message,'Configuring Z device',/info
    device,set_resolution=[xsize,ysize],z_buff=0
    finalim = bytarr(xsize,ysize,3)
    ;window,/free, /pixmap, xsize=xsize, ysize=ysize
    ;pixmap = !d.window
    !p.font = 0
    ;device,font='Helvetica Bold',/tt_font,/font
    ;device,set_font='-adobe-helvetica-medium-r-normal--14-100-100-100-p-76-iso8859-1'
  ENDELSE 
  Message,'Done Configuring Z device',/info

  loncent = mean(lonrange)
;    Map_Set,0,loncent,$
;     limit=[ limits[1],lonrange[0],limits[3],lonrange[1] ],/noborder,$
;      Ymargin=[4.,4]


  IF n_Elements(outfile) EQ 0 THEN BEGIN 

    t = long([lonrange[0],limits[1],lonrange[1],limits[3]])
    lim_str =  '%'+ strcompress(StrJoin(t,','),/remove_all) +  '%'

    ofileroot =  ofile_tmplt + lim_str

    CASE 1 OF 
      gif : ext = '.gif'
      jpeg: ext = '.jpeg'
      ps  : ext = '.ps'
    ENDCASE  
    OutputFilename = strcompress(ofileroot + ext,/remove_all)

  ENDIF ELSE outputFilename =  strcompress(outfile,/remove_all)

  IF ps THEN device,filename=OutputFilename

  Hue = fltarr(nlon,nlat)+WaterHue

  IF nland NE 0 THEN Hue[land] = LandHue

  IF config AND clouddata THEN $
    CLOUD_OVERLAY_CONFIG, $
      landwater=hue, $
        cloudmask=cloudmask, $
         brightmin=brightmin, $
          brightmax=brightmax, $
           satmin=satmin, $
            satmax=satmax, $
             lonmin=lonrange[0], $
              lonmax=lonrange[1], $
               latmin=limits[1], $
                 latmax=limits[3]

    ; Re-establish the plotting environs.
  Map_Set,0,loncent,$
   limit=[ limits[1],lonrange[0],limits[3],lonrange[1] ],/noborder,$
    Ymargin=[4.,4]

    ; Define the new Brightness/Saturation mappings
  xx=findgen(100)/99.

  bi = 0> interpol( [0.,1], [BrightMin, BrightMax], xx ) < 1
  si = 0> interpol( [1.,0], [SatMin,    SatMax],xx ) < 1

    ; Use 'cloudmask' to create new Brightness/Saturation values

  IF clouddata THEN BEGIN 
    
    b2=bi[cloudmask]
    s2=si[temporary(cloudmask)]
    cloudmask = 0
  ENDIF ELSE BEGIN 
    b2 = hue*0.+WaterLight
    b2[land] = LandLight
    s2 = hue*0. + WaterSat
    s2[land] = LandSat
  ENDELSE 

    ; Substitute these new Brightness/Saturation values in for those in
    ; mapIm and convert back to RGB 

  Color_Convert, Hue,b2,s2, imr, img, imb, /hls_rgb
  Hue = (b2=(s2=0))
  Im = [ [[temporary(imr)]], [[temporary(img)]], [[temporary(imb)]] ]

  FOR i=0,2 DO BEGIN 
    tmpIm = Map_Image( Im[*,*,i], $
                       xs,ys,txsize,tysize,$
                       lonmin=lonrange[0],$
                       latmin=limits[1],$
                       lonmax=lonrange[1],$
                       latmax=limits[3],$
                       scale=scalefac, /compress, /bilinear )


    IF i EQ 0 THEN BEGIN 
      dim = size(tmpIm,/dim)
      mapIm = bytarr(dim[0], dim[1], 3)
    ENDIF 
    mapIm[*,*,i] =  temporary(tmpIm)
  ENDFOR 
  im = 0

    ; Tv the final image. Put on grid lines and plot the vectors.

  IF n_elements(title) NE 0 THEN $
     Title= title + ' ' + goes_string ELSE $
     Title= goes_string 

    ; Calculate where to put Colorbar
  sz = size( mapIm[*,*,0],/dim)
  xyz = Convert_Coord( 0, ys+sz[1]/scalefac,/device,/to_normal)

  y = xyz[1]
  ycb = [3*y+2, 2*y+3]/5

    ; Lay down the title
  xyz = Convert_Coord(0,ys,/device,/to_normal)
  ytitle = xyz[1]/2.

  xyz = Convert_Coord( 0, fix(1.5*!D.Y_CH_Size), /device, /to_normal )
  ysubtitle = ytitle-xyz[1]



  nn = n_Elements(u)

  IF ps THEN BEGIN 

    text_color = '000000'xl
    Tv,mapIm,xs,ys,xsize=txsize,ysize=tysize,true=3 
    tvlct,orig_red,orig_green,orig_blue,/get
    tvlct,transpose(ct)
    IF nn GT 1 THEN $
      PlotVect,u,v,lon,lat,len=length,$
       thick=thick,start_index=WIND_START,ncolors=N_WIND_COLORS, $
         minspeed=minspeed, maxspeed=maxspeed, scale=scaleVec
      
    IF rfi[0] NE -1 AND rf_action EQ 1 THEN BEGIN 
      TVLCT,rf_color AND 'ff'xl,$
       ishft(rf_color,-8) AND 'ff'xl, $
       ishft(rf_color,-16) AND 'ff'xl,1
      PlotVect,u[rfi],v[rfi],lon[rfi],lat[rfi],len=length,$
       thick=thick,minspeed=minspeed, maxspeed=maxspeed, $
       scale=scaleVec,color=1
    ENDIF 

    FOR ii=0,n_elements(toplot)-1 DO BEGIN 
      IF toplot[ii].psym LT 0 THEN BEGIN 
        Plots,toplot[ii].lon, toplot[ii].lat, psym=toplot[ii].psym, $
           symsize=toplot[ii].symsize,color=toplot[ii].symcolor
      ENDIF ELSE BEGIN 
        FOR jj=0,n_elements(toplot[ii].lon)-1 DO BEGIN 
          Plots,toplot[ii].lon[jj], toplot[ii].lat[jj], psym=toplot[ii].psym, $
            symsize=toplot[ii].symsize ,color=toplot[ii].symcolor
        ENDFOR 
      ENDELSE 
      xx = where( strlen(toplot[ii].ptitle) NE 0,nxx)
      IF nxx NE 0 THEN BEGIN 
        FOR jj=0,nxx-1 DO BEGIN 
          nal = n_elements(toplot[ii].alignment)
          nor = n_elements(toplot[ii].orientation)
          IF toplot[ii].normal THEN BEGIN 
             xyouts, toplot[ii].x_title[xx[jj]], $
                toplot[ii].y_title[xx[jj]],$
                 toplot[ii].ptitle[xx[jj]], $
                align=toplot[ii].alignment[xx[jj] MOD nal ], $
                  orient=toplot[ii].orientation[xx[jj] MOD nor ], $
                    charsize=toplot[ii].charsize, $
                       charthick=toplot[ii].charthick,color=toplot[ii].textcolor,/normal
           ENDIF ELSE BEGIN 
             xyouts, toplot[ii].x_title[xx[jj]], $
                toplot[ii].y_title[xx[jj]],$
                 toplot[ii].ptitle[xx[jj]], $
                align=toplot[ii].alignment[xx[jj]], $
                  orient=toplot[ii].orientation[xx[jj]], $
                    charsize=toplot[ii].charsize, $
                       charthick=toplot[ii].charthick,color=toplot[ii].textcolor,/data
          ENDELSE 
        ENDFOR 
      ENDIF 
    ENDFOR 
    IF gridlines THEN BEGIN 
      Map_Set,0,loncent,$
       limit=[ limits[1],lonrange[0],limits[3],lonrange[1] ],/noborder,$
       Ymargin=[4.,4],/noerase,/grid,/lab,color=255b
    ENDIF 


    ColBar, bottom=Wind_Start, nColors=N_Wind_Colors,$
           position=[0.25,ycb[0], 0.75, ycb[1]], $
             Title='Wind Speed (m/s)',Min=minspeed, $
               max=maxspeed,divisions=4, format='(f5.0)', $
                pscolor=ps, /true, table=ct, charsize=0.75, $
                  color=text_color


    xyouts, 0.5, ytitle, title, align=0.5, $
     /normal, charsize=1.05, color=text_color

    IF n_Elements(subtitle) NE 0 THEN BEGIN 
       xyouts, 0.5, ysubtitle, subtitle, align=0.5, $
          /normal, charsize=1.0, color=text_color
    ENDIF 

    IF rainflag NE 0 AND rf_action EQ 1 THEN BEGIN 
      newct = ct
      newct[*,0] =  [rf_color AND 'ff'xl, $
                      ishft(rf_color,-8) AND 'ff'xl, $
                       ishft(rf_color,-16) AND 'ff'xl]

      Colbar,pos=[0.49,0.005,0.51,0.025],bottom=0,ncolors=1,min=0,max=1,$
           table=newct,/true,/noannot,color=text_color
      xyouts,0.49,0.005,'Rain ',align=1,/normal,color=text_color
      xyouts,0.51,0.005,' Flagged',/normal,color=text_color


    ENDIF 
    tvlct,orig_red,orig_green,orig_blue
  ENDIF ELSE BEGIN 

      ; Here we do the gif/jpeg processing. Since the device we're
      ; using isn't a native 24 bit device, we do it 'plane by
      ; plane.'


      ; Set some constant quantities.

    text_color = 255b

      ; Now loop over each color plane, plotting vectors and putting
      ; down color bars, titles and such.

    FOR i=0,2 DO BEGIN 
      Tv,mapIm[*,*,i],xs,ys

      Map_Set,0,loncent,$
       limit=[ limits[1],lonrange[0],limits[3],lonrange[1] ],/noborder,$
         Ymargin=[4.,4],/noerase

      ; Plot the vectors (if there are any.)
      IF nn GT 1 THEN BEGIN 
        CASE i OF 
          0: col =  col24 AND 'ff'xl
          1: col =  ishft(col24,-8) AND 'ff'xl
          2: col =  ishft(col24,-16) AND 'ff'xl
        ENDCASE 
        PlotVect, u,v,lon,lat, color=col, len=length, thick=thick, $
          scale=scaleVec,minspeed=minspeed,maxspeed=maxspeed
        IF rfi[0] NE -1 AND rf_action EQ 1 THEN BEGIN 
          PlotVect,u[rfi],v[rfi],lon[rfi],lat[rfi],len=length,$
            thick=thick,minspeed=minspeed, maxspeed=maxspeed, $
              scale=scaleVec, color=rf_color
        ENDIF 
      ENDIF 

      FOR ii=0,n_elements(toplot)-1 DO BEGIN 
        IF toplot[ii].psym LT 0 THEN BEGIN 
          Plots,toplot[ii].lon, toplot[ii].lat, psym=toplot[ii].psym, $
             symsize=toplot[ii].symsize, color=toplot[ii].symcolor
        ENDIF ELSE BEGIN 
          FOR jj=0,n_elements(toplot[ii].lon)-1 DO BEGIN 
            Plots,toplot[ii].lon[jj], toplot[ii].lat[jj], $
               psym=toplot[ii].psym, $
                symsize=toplot[ii].symsize , color=toplot[ii].symcolor
          ENDFOR 
        ENDELSE 
        xx = where( strlen(toplot[ii].ptitle) NE 0,nxx)
        IF nxx NE 0 THEN BEGIN 
          FOR jj=0,nxx-1 DO BEGIN 
            nal = n_elements(toplot[ii].alignment)
            nor = n_elements(toplot[ii].orientation)
            IF toplot[ii].normal EQ 1 THEN BEGIN 
               xyouts, toplot[ii].x_title[xx[jj]], $
                  toplot[ii].y_title[xx[jj]],$
                   toplot[ii].ptitle[xx[jj]], $
                  align=toplot[ii].alignment[xx[jj] MOD nal ], $
                    orient=toplot[ii].orientation[xx[jj] MOD nor ], $
                      charsize=toplot[ii].charsize, $
                         charthick=toplot[ii].charthick,$
                           color=toplot[ii].textcolor,/normal
             ENDIF ELSE BEGIN 
               xyouts, toplot[ii].x_title[xx[jj]], $
                  toplot[ii].y_title[xx[jj]],$
                   toplot[ii].ptitle[xx[jj]], $
                  align=toplot[ii].alignment[xx[jj]], $
                    orient=toplot[ii].orientation[xx[jj]], $
                      charsize=toplot[ii].charsize, $
                         charthick=toplot[ii].charthick,$
                           color=toplot[ii].textcolor, /data
             ENDELSE 
          ENDFOR 
        ENDIF 
      ENDFOR 

      col = bytarr(3,n_elements(ct[0,*]))
      col[0,*] =  ct[i,*]
      ;tvlct,r,g,b,/get
      ;tvlct,transpose(col)
      ColBar, bottom=Wind_Start, nColors=N_Wind_Colors,$
             position=[0.25,ycb[0], 0.75, ycb[1]], $
               Title='Wind Speed (m/s)',Min=minspeed, $
                 max=maxspeed,divisions=4, format='(f5.0)', $
                 charsize=0.75,color=255, table=col, /true

      ;tvlct,r,g,b


      xyouts, 0.5, ytitle, title, align=0.5, $
       /normal, charsize=1.05, color=text_color


      IF n_Elements(subtitle) NE 0 THEN BEGIN 
          xyouts, 0.5, ysubtitle, subtitle, align=0.5, $
            /normal, charsize=1.0, color=text_color
      ENDIF 




      IF rainflag NE 0 AND rf_action EQ 1 THEN BEGIN 
        newct = ct
        CASE i OF
          0: newct[0,0] = rf_color AND 'ff'xl
          1: newct[0,0] = ishft(rf_color,-8) AND 'ff'xl
          2: newct[0,0] = ishft(rf_color,-16) AND 'ff'xl
        ENDCASE 

        ;tvlct,r,g,b,/get
        ;tvlct,transpose(newct)
        Colbar,pos=[0.49,0.005,0.51,0.025],bottom=0,ncolors=1,min=0,max=1,$
             table=newct,/true,/noannot,color=255
        ;tvlct,r,g,b
        xyouts,0.49,0.005,'Rain ',align=1,/normal,color=text_color
        xyouts,0.51,0.005,' Flagged',/normal,color=text_color


      ENDIF 


      IF gridlines THEN BEGIN 
        Map_Set,0,loncent,$
         limit=[ limits[1],lonrange[0],limits[3],lonrange[1] ],/noborder,$
          Ymargin=[4.,4],/noerase,/grid,/lab,color=0b
      ENDIF 


      finalim[*,*,i] =  tvrd()
    ENDFOR 

    t1 = systime(1)
    IF verbose THEN print,' Plotvect took: ', t1-t0,' Seconds '
    t0 = t1

  ENDELSE 


  CASE 1 OF 
    gif: BEGIN 
      ;If output is 'gif' quantize it down to 175 colors.
      t0 = systime(1)
      ;finalim = tvrd(true=3)
      im = color_Quan( finalim, 3, r,g,b, colors=175 )
      t1 = systime(1)
      IF verbose THEN print,' Color_Quan took: ', t1-t0,' Seconds '
      t0 = t1

      Write_Gif, OutputFilename, im, r,g,b
      t1 = systime(1)
      IF arg_present(thumbnail) THEN  BEGIN 
        dims = size(im,/dimension)
        thumbnail = OutputFilename + '.TN'
        thumbnailIm =  congrid( im, dims[0]*0.3, dims[1]*0.3)
        Write_Gif, thumbnail, temporary(thumbnailIm), r,g,b
      ENDIF 
      IF verbose THEN print,' Write_Gif took: ', t1-t0,' Seconds '
      t0 = t1
    END

    jpeg: BEGIN 
      ;finalim = tvrd(true=3)
      Write_Jpeg, OutputFilename, finalim, $
             quality=quality, true=3
      IF arg_present(thumbnail) THEN  BEGIN 
        dims = size(finalim,/dimen)
        thumbnailIm = congrid( finalim,  dims[0]*0.3, dims[1]*0.3,dims[2] )
        thumbnail = OutputFilename + '.TN'
        Write_Jpeg, thumbnail, temporary(thumbnailIm), $
         quality=quality, true=3
      ENDIF 
    END

    ps: device,/close

  ENDCASE
  
  end_time = systime(1)
  IF Verbose THEN print,'Total Time: ', (end_time-start_time)/60. ,' Minutes'
  IF Arg_Present(Outfile) THEN Outfile =  OutputFilename
  set_plot,savedevice
  ;IF NOT ps THEN Wdelete,pixmap
;  genv,/restore

END

