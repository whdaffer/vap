;+
; NAME:  gms5_overlay
; $Id$
; PURPOSE:  Overlay Wind data on GMS 5 data
;
;
; CALLING SEQUENCE:  gms5_overlay, datetime, gmstype
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
;                    LandHue     = LandHue,$
;                    WaterHue    = WaterHue,$
;                    outfile     = outfile,$
;                    gif         = gif,$        
;                    ps          = ps, $        
;                    scalefac    = scalefac,$
;                    jpeg        = jpeg,$
;                    quality     = quality, $
;                    MapLimits   = MapLimits, $
;                    status      = status, $
;                    thumbnail   = thumbnail, $
;                    config      = config,$
;                    scale       = scale, $
;                    rainflag      = rainflag, $
;                    rf_action   = rf_action, $
;                    rf_color    = rf_color, $
;                    oplot       = oplot, $
;                    gridlines   = gridlines, $
;                    keepaspect  = keepaspect
;
;
;
; 
; INPUTS:  
;
;   DateTime: String of form YYMMDDHHMM (e.g. 9812042322 ) detailing
;             the basename of the files needed to do the overlay.
;
;     gmsType     : scalar string, one of 'ir1','ir2','ir3', 'vis' (def='ir1')
;
; OPTIONAL INPUTS:  None
;
;
;       
; KEYWORD PARAMETERS:  
;
;     windfiles   : Vector of Strings, List of wind files  to
;                   read. Must be fully qualified filenames
;     xsize       : Xsize of resulting picture (def=640)
;     ysize       : ditto, y size (def=480)
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
;                   built by the routine, containing the date of the
;                   GMS5 file used.
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
;                   Land color. (default=[25, 110,   0])
;     WaterRGB    : The 3-vector containing the RGB values for the
;                   Water color. (default=[28, 15,  80])
;     LandHue     : the Hue value for land (from a Hue/Brightness/Saturation
;                   triple ) default will be whatever Hue results when
;                   the default LandRGB of [25, 110,   0] is converted
;                   to Hue
;     WaterHue    : the Hue value for water (from a
;                   Hue/Brightness/Saturation triple) Default will be
;                   whatever hue results when the default WaterRGB,
;                   triple of [28, 15,  80] is converted to Hue
;     gif         : output as Gif file 
;     scalefac    : scale factor to be used in making Postscript files.
;                   (def=0.05)
;     ps          : output as Postscript file
;     jpeg        : output file as Jpeg file (the default)
;     Quality     : Use this quality when making Jpeg files (def=75)
;     verbose     : flag, if set, emit many messages
;     MapLimits   : float 4-vector. Map limits having the form
;                   [lonmin, latmin, lonmax, latmax] where the first
;                   two are the lower left corner and the 2nd two the
;                   upper right. Default will be to grid the entire
;                   file, but this will be at a reduced resolution. 
;                   The maximum limits are:
;
;                    70<lon<200
;                    -60<lat<60
;
;                   If the Lon is in [80,200] the grid file in the
;                   'grid' subdirectory is used, otherwise the grid
;                   file in the 'grida' subdirectory will be used.
;
;     status      : 1 if successful, 0 otherwise
;     thumbnail   : if present for output, create a thumbnail that is
;                   30% the size of the original and output its name
;                   in this variable
;     config      : flag, if set, will call a overlay configurator
;     scale       : flag, if set, the vectors will be scaled by their
;                   speed, otherwise, they're all the same length.
;
;     Rainflag       : (I), Flag , 0|1. 0=don't use flag, 1=use flag.
;
;     RF_Action    : (I), flag, 0|1 depending on whether you want to
;                    skip plotting rain flagged data (0) or plot it
;                    with the color given in FL_Color(1), Default = 1,
;                    use FL_color.
;
;     RF_Color     : (I), long integer. The 24 bit color to be used
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
;
;
;    gridlines: flag. Put map graticule after TVing image if set.
;
;    keepaspect: flag. Maintain the aspect ratio of the latlon
;                limits. This keyword will *partially* override the
;                values of the xsize/ysize keywords. Depending on the
;                value of the aspectratio, it will set one or the
;                other of them to whatever value is necessary in
;                order to preserve the aspect ratio.
;                
;
; OUTPUTS:  A file, either a .gif, .jpeg (the default) or a .ps file,
;          depending on the status of these three flags having either
;          a name built from the input goes file name (the default) or
;          whatever is passed in via the 'outfile' keyword. The
;          default output format is Gif and the default name is :
; 
;          GMS_5_mmm_yyyymmddThh:mm-%aaaa,bbb,cccc,ddd%.gif
;
;          where 
;          mmm = VIS, IR1,IR2,IR3
;          yyyymmdd = year, month, day, e.g. 19981202
;          hh:mm  = hour:minute
;          aaaa = minimum longitude, 4 digit, signed 
;          bbb  = minimum latitude, 3 digit, signed 
;          cccc = maximum longitude, 4 digit, signed 
;          ddd  = maximum latitude, 3 digit, signed 
;
;          The keyword 'outfile' will contain the output file name if
;          it (i.e. outfile) is present for output. If this quantity
;          is empty, there has been a failure. 
;
; OPTIONAL OUTPUTS:  None
;
;
;
; COMMON BLOCKS:  gms_overlay_cmn
;
;
;
; SIDE EFFECTS:  Vast reduction if memory, as this program uses ALOT
;
;
;
; RESTRICTIONS: A word about input file restrictions. This program
;               uses 4 files. Each file has the input parameter
;               'datetime' as it's basename. Thereafter they are
;               distinguished by their location within a preset directory
;               structure and, possibly, an extention. Assuming the
;               'datetime' is 9812042322 and the top of the GMS files
;               tree is /dir1/gms5, the four files are:
;
;               /dir1/gms5/doc/9812042322.txt.Z
;               /dir1/gms5/cal/9812042322.cal.Z
;               /dir1/gms5/grid/9812042322.hdf.Z 
;                  ( -or- /dir1/gms5/grida/9812042322.hdf.Z depending
;                     on the longitude limits)
;               /dir1/gms5/xxx/9812042322.hdf.Z 
;                   where xxx = ir1, ir2, ir3 or vis, depending on the
;               value of the 'gmstype' keyword.
;
;               If any of these files are missing, the routine will
;               fail.
;
;               
;
;
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.23  2001/12/08 00:02:36  vapdev
; Getting rid of obsolete RSI routines and fixing ENV vars
;
; Revision 1.22  2001/02/21 01:03:49  vapuser
; Took out 'path=' in call to read_cfgfile
;
; Revision 1.21  2001/02/02 19:26:11  vapuser
; Put in `gridlines' and 'keepaspect.' Did a little work (ptitle) on the
; oplot mechanism.
;
; Revision 1.20  2000/08/15 16:43:15  vapuser
; Added the 'oplot' keyword to overplot symbols. Also added a 'help'
; keyword.
;
; Revision 1.19  2000/08/15 16:09:20  vapuser
; Fixed some small postscript bugs
;
; Revision 1.18  2000/05/17 20:46:06  vapuser
; Wrap all output filename constructors with strcompress(/remove_all)
;
; Revision 1.17  2000/05/17 16:51:09  vapuser
; Make the routine continue to the end even if the gms5 file isn't
; there or can't be read.
;
; Revision 1.16  2000/05/15 22:58:54  vapuser
; Changed from multi-valued 'use_rf' to the new single-valued rainflag.
;
; Revision 1.15  2000/03/14 16:12:13  vapuser
; Made same changes to this routine as in goes_overlay.pro
; so that'll now run in Z buffer.
;
;
; Revision 1.14  2000/03/13 21:03:06  vapuser
; Commented out all the tlvct's and loadct's
;
; Revision 1.13  2000/03/01 16:40:08  vapuser
; Uncommented 'catch', added a few 'status = 0's in case of
; failure.
;
; Revision 1.12  2000/02/29 23:34:05  vapuser
; Added colorbar for rain flagged data.
;
; Revision 1.11  2000/02/29 15:58:12  vapuser
; Added rain flag code
;
; Revision 1.10  1999/10/05 17:27:40  vapuser
; Added 'config' and 'scale' keywords and support code.
;
; Revision 1.9  1999/06/24 21:18:36  vapuser
; Added test for good data after gms5ReadAll
;
; Revision 1.8  1999/06/23 22:00:23  vapuser
; Move the map_set until after config.
;
; Revision 1.7  1999/06/23 18:11:27  vapuser
; Added CONFIG keyword
;
; Revision 1.6  1999/06/21 14:50:52  vapuser
; Updated documentation
;
; Revision 1.5  1999/04/08 22:02:43  vapuser
; Replaced Colorbar with ColBar
;
; Revision 1.4  1999/04/08 20:19:12  vapuser
; Changed color24 to rgb2true
;
; Revision 1.3  1999/04/06 18:35:43  vapuser
; Changed default x/y size to defaulting xsize to 960 and
; ysize to whatever has same ratio as (long_range)/(lat_range)
; Also biased `bright' vector (bi) to >= 0.2.
;
; Revision 1.2  1999/04/02 17:50:44  vapuser
; Removed reference to calibration data. It's not required for
; what we're doing.
;
; Revision 1.1  1999/04/02 17:49:47  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-


PRO gms5_overlay, datetime, gmsType, $
                  windFiles   = WindFiles, $
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
                  LandHue     = LandHue,$
                  WaterHue    = WaterHue,$
                  LandRGB     = LandRGB, $
                  WaterRGB    = WaterRGB,$
                  gif         = gif,$        
                  ps          = ps, $        
                  jpeg        = jpeg,$
                  scalefac    = scalefac, $
                  quality     = quality, $
                  mapLimits   = mapLimits,$
                  status      = status, $
                  thumbnail   = thumbnail, $
                  config      = config, $
                  scaleVec    = scaleVec, $
                  rainflag      = rainflag, $
                  rf_action   = rf_action, $
                  rf_color    = rf_color, $
                  oplot       = oplot, $
                  gridlines   = gridlines, $
                  keepaspect  = keepaspect, $
                  help        = help





  savedevice = !d.name
  status = 1

; genv,/save
;  tvlct,orig_red,orig_green,orig_blue,/get
;  loadct,0,/silent

  IF n_Params() EQ 0 OR keyword_set(help) THEN BEGIN 
    Usage, 'gms5_overlay, datetime, gmsType,windfiles = windfiles, xsize = xsize, ysize = ysize, CRDecimate = CRDecimate, Decimate = Decimate, ExcludeCols = ExcludeCols, verbose = verbose, minpix  = minpix, minspeed  = minspeed, maxspeed  = maxspeed,length  = length,thick = thick, title = title,subtitle = subtitle, outfile = outfile,BrightMin = BrightMin, BrightMax = BrightMax, SatMin = SatMin, SatMax = SatMax, LandHue = LandHue,WaterHue = WaterHue,LandRGB=landRGB, WaterRGB=WaterRGB, gif = gif, ps = ps, scalefac = scalefac, jpeg = jpeg,quality = quality, ScaleVec=ScaleVec, rainflag=rainflag, rf_action=rf_action, rf_color=rf_color, oplot=oplot, gridlines=gridlines, keepaspect=keepaspect, help=help'
;    tvlct,orig_red,orig_green,orig_blue
;    genv,/restore
    status = 0
    return
  ENDIF 

  winddata =  n_elements(windfiles) NE 0
  IF n_elements(gridlines) NE 0 THEN gridlines = keyword_set(gridlines)
  IF n_elements(keepaspect) NE 0 THEN keepaspect =  keyword_set(keepaspect)
  config =  keyword_set(config)


  ;[31,  33, 129]
  ;Default_WaterRGB = [28, 15,  80]
  Default_WaterRGB = [11,  11, 122] 
  Default_LandRGB =  [25, 110,   0]

  Color_Convert, Default_WaterRGB[0],Default_WaterRGB[1],Default_WaterRGB[2],$
   Default_WaterHue,l,s,/rgb_hls
  Color_Convert, Default_LandRGB[0],Default_LandRGB[1],Default_LandRGB[2],$
   Default_LandHue,l,s,/rgb_hls


  clouddata = 1
  IF isa(datetime,/string,/nonempty) THEN BEGIN 
    ;Message,"Input parameter DATETIME must be a nonempty STRING!",/cont

    datetime_str = Gms5ParseDatetime(Datetime)
    IF NOT isa(datetime_str,/structure,name='GMS5DATETIME') THEN BEGIN 
      Message,'Bad Datetime!',/info
      clouddata = 0
    ENDIF 
  ENDIF ELSE clouddata = 0

  IF n_elements(gmsType) EQ 0 THEN gmsType =  'ir1'

  IF n_elements(maplimits) LT 4 THEN $
     MapLimits =  [80,-60,200,60]

  mapLimits = 1.0*mapLimits
  lonlim = MapLimits[ [0,2] ]
  latlim = MapLimits[ [1,3] ]

  lonlim = fixlonrange(lonlim)
  
  IF lonlim[0] LT 70 OR lonlim[1] GT 200 OR $
     latlim [0] LT -60 OR latlim[1] GT 60 THEN BEGIN 
    Message,'MapLimits out of range!',/cont
    print,' mapLimits must be bound by [70,-60,200,60]'
    status = 0
    return
  ENDIF 

  
  ps = keyword_set(ps)
  gif = keyword_set(gif)
  jpeg = keyword_set(jpeg)

  lonrange = lonlim[1]-lonlim[0]
  latrange = latlim[1]-latlim[0]
  aspectratio = float(latrange)/lonrange
  
  IF n_elements(xsize ) EQ 0 AND $
     n_elements(ysize) EQ 0 THEN BEGIN 
    IF NOT ps THEN BEGIN 
      xsize = 960
      ysize = fix(xsize*AspectRatio)
    ENDIF ELSE BEGIN 
      xsize = 8.4 
      ysize = xsize*AspectRatio
    ENDELSE 
  ENDIF ELSE BEGIN 
    IF n_elements(xsize) EQ 0 THEN BEGIN 
      IF NOT ps THEN BEGIN 
        xsize = 960
        Message,'Xsize defaulting to 960',/info
      ENDIF ELSE BEGIN 
        xsize = 8.4
        Message,'Xsize defaulting to 8.4',/info
      ENDELSE 
    ENDIF ELSE IF ps THEN xsize =  xsize < 11
    IF n_elements(ysize) EQ 0 THEN BEGIN 
      IF NOT ps THEN BEGIN 
        ysize = 720
        Message,'Ysize defaulting to 720',/info
      ENDIF ELSE BEGIN 
        ysize = 6.5
        Message,'Ysize defaulting to 6.5',/info
      ENDELSE 
    ENDIF ELSE IF ps THEN ysize = ysize <  8
  ENDELSE 

  xoffset = 1.2
  yoffset = 9.5


  IF keepaspect THEN BEGIN 
    IF aspectratio GE 1. THEN $
      xsize=ysize/aspectRatio ELSE $
      ysize=xsize*aspectRatio

    IF NOT ps THEN BEGIN 
      xsize = fix(xsize)
      ysize = fix(ysize)
    ENDIF 
  ENDIF 

  IF NOT ps THEN BEGIN 
    Message,"[Xsize, Ysize]: [" + $
     string( [xsize,ysize], form='(i4,",",i4)') + ']',/info
  ENDIF ELSE BEGIN 
    Message,"[Xsize, Ysize]: [" + $
     string( [xsize,ysize], form='(f5.2,",",f5.2)') + ']',/info
    Message,"[xoff,yoff]: [" + $
     string( [xoffset,yoffset], form='(f5.2,",",f5.2)') + ']',/info
  ENDELSE 



  verbose = keyword_set(verbose)

  IF ps AND gif  OR $
     ps AND jpeg OR $
     jpeg AND gif THEN BEGIN 
    Message,'Only one of PS or GIF or JPEG may be set',/cont
;    tvlct,orig_red,orig_green,orig_blue
;    genv,/restore
    status = 0
    return
  ENDIF 

  IF NOT (ps OR gif OR jpeg) THEN BEGIN 
    Message,' Defaulting to Jpeg output ',/info
    jpeg = 1
  ENDIF 

  IF n_elements(scalefac) EQ 0 THEN BEGIN 
    IF ps THEN scalefac = 0.05 ELSE scalefac = 1
  ENDIF


  IF n_Elements(minspeed) EQ 0 THEN minspeed = 2
  IF n_Elements(maxspeed) EQ 0 THEN maxspeed = 25
  IF n_elements(length)   EQ 0 THEN length = 2
  IF n_elements(thick)    EQ 0 THEN thick = 1

  
  IF n_elements(BrightMin) EQ 0 THEN BrightMin = 0.3
  IF n_elements(BrightMax) EQ 0 THEN BrightMax = 1.0
  IF n_elements(SatMin) EQ 0 THEN SatMin = 0.0
  IF n_elements(SatMax) EQ 0 THEN SatMax = 1.0

  BrightMin = 0> float(BrightMin) < 1.
  BrightMax = BrightMin+.01> BrightMax < 1.

  SatMin = 0> float(SatMin) < 1.
  SatMax = SatMin+.01> SatMax < 1.

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
  ENDIF ELSE BEGIN 
    WaterLight = 0.5
    WaterSat = 1.
  ENDELSE 
  
  LandHue =  0> LandHue < 360.
  WaterHue =  0> WaterHue < 360.

;  catch, error
;  IF error NE 0 THEN BEGIN 
;    catch,/cancel
;    Message,!error_state.msg,/cont
;    tvlct,orig_red,orig_green,orig_blue
;    genv,/restore
;    status = 0
;    return
;  END



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
    cfgname = cfgpath+"/"+cfgname
    print,' Reading CFG file ' + cfgname
    read_cfgfile,cfgname, cfg
    IF n_elements(cfg) NE 0 THEN BEGIN 
      print,'CFG found! Details follow:'
      help,cfg,/st
    ENDIF 
  ENDIF 

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
                   normal:normal,textcolor: textcolor, charsize: charsize, $
                   charthick: charthick }
      ENDELSE 
    ENDELSE 
  ENDIF 

  chkcfg,'GRIDLINES',gridlines,cfg
  chkcfg,'KEEPASPECT',keepaspect,cfg

    ;=============== Start the processing =================



  start_time = systime(1)

  IF getenv('OVERLAY_CT') NE '' THEN BEGIN 
    ptr = ReadColorTable('$OVERLAY_CT')
  ENDIF ELSE BEGIN 
    ptr = ReadColorTable('$VAP_COLORTABLES/goes_overlay24.ct')
  ENDELSE 
  IF NOT Ptr_Valid(ptr) THEN BEGIN 
    Message,'Error Reading ColorTable!',/cont
;    tvlct,orig_red,orig_green,orig_blue
;    genv,/restore
    status = 0
    return
  ENDIF 
  CT = *ptr
  ptr_free,ptr

  WIND_START = 1
  N_WIND_COLORS = 25
  N_COLORS = 27

  IF NOT ps  THEN BEGIN 
    lonpar =  [ lonlim, ( lonlim[1]-lonlim[0] +1)/xsize ]
    latpar =  [ latlim, ( latlim[1]-latlim[0] +1)/ysize ]
    set_plot,'z'
    device,set_resolution=[xsize,ysize],z_buff=0
  ENDIF ELSE BEGIN 
    lonpar =  [ lonlim, (lonlim[1]-lonlim[0]+1)/(2.*72*xsize) ]
    latpar =  [ latlim, (latlim[1]-latlim[0]+1)/(2.*72*ysize) ]

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
  ENDELSE 

  gmsType = strupcase(gmsType)
  CASE gmsType OF 
    'IR1':
    'IR2':
    'IR3':
    'VIS':
    ELSE: BEGIN 
      Message,$
       "Unknown gmsType!, must be one of 'IR1','IR2','IR3' or 'VIS'",/cont
      status = 0
      return
    END
  ENDCASE 
  IF clouddata THEN BEGIN 
    Message,"Datetime: " + datetime + ', Gmstype: ' + gmstype + $
      ", Lonpar: " + string(lonpar[0:1],form='(2(f7.2,2x))'),/info
    allData = Gms5ReadAll(datetime,gmsType,lonpar=lonpar)
    IF NOT isa(alldata,/structure) THEN BEGIN 
      Message,"Failure Reading GMS5 data",/info
      clouddata = 0
      time_string = "No_Cloud_Data"
    ENDIF ELSE BEGIN 
      image = allData.imagedata.image
      ;calTemps = allData.CalData.ir[0].Temps
      xloc = *(allData.griddata.xloc)
      yloc = *(allData.griddata.yloc)
      ptr_free,allData.griddata.xloc,allData.griddata.yloc
      minlon = allData.griddata.minlon

      newGrid = gms5idx(lonpar,latpar)
      loni=newGrid.loni
      lati=newGrid.lati
      newx=interpolate(temporary(xloc),loni,lati,/grid)
      newy=interpolate(temporary(yloc),$
                       temporary(loni),$
                       temporary(lati),/grid)

        ; If ever we want to use the caltemps
        ; file, this is the way we'd do it.
        ;  tempim= temporary(caltemps(temporary(image($
        ;                     temporary(newx),temporary(newy)))) )

      cloudmask = temporary(image($
                              temporary(newx),$
                              temporary(newy) ))

      time_string = $
        datetime_str.year + $
          datetime_str.month + $
           datetime_str.day + $
            datetime_str.hour + $
              datetime_str.min
    ENDELSE 
  ENDIF ELSE time_string =  "No_Cloud_Data"


  IF n_Elements(outfile) EQ 0 THEN BEGIN 

    lim_str = $
     PadAndJustify(lonlim[0],4,pad='0',/right) + ','+$
     PadAndJustify(latlim[0],3,pad='0',/right) + ','+$
     PadAndJustify(lonlim[1],4,pad='0',/right) + ','+$
     PadAndJustify(latlim[1],3,pad='0',/right) 
    dlm =  '_'


    ofileroot = 'GMS5' +  dlm + $
     gmsType + dlm + $
     time_string + dlm + lim_str

    CASE 1 OF 
      gif:  OutputFilename = ofileroot+'.gif'
      jpeg: OutputFilename = ofileroot+'.jpeg'
      ps:   OutputFilename = ofileroot+'.ps'
      ELSE: Message,"Job security!",/info
    ENDCASE 
     outputfilename = strcompress(outputfilename,/remove_all)

  ENDIF ELSE outputFilename =  strcompress(outfile,/remove_all)

  IF ps THEN device,filename=OutputFilename

  outfile = OutputFilename

  ;sz = Size(cloudmask,/dim)

  lon0 = lonpar[0]
  lon1 = lonpar[1]
  loninc = lonpar[2]


  lat0 = latpar[0]
  lat1 = latpar[1]
  latinc = latpar[2]
  nlon =  fix((lon1-lon0)/loninc+1)
  nlat =  fix((lat1-lat0)/latinc+1)
  loni = (findgen(nlon)*loninc+lon0)#replicate(1.,nlat)
  lati = replicate(1.,nlon)#(findgen(nlat)*latinc+lat0)

  tt0 = systime(1)
  landmask = runLandMask(loni,lati)
  land = where(landmask)
  water = where(temporary(landmask) EQ 0)
  tt1 = systime(1)

  IF verbose THEN print,'Time for Landmask : ',tt1-tt0, ' Seconds '

  ;nlon = sz[0]
  ;nlat = sz[1]


  Hue = fltarr(nlon,nlat)+WaterHue
  Hue[land] = LandHue
 
   
  IF config AND clouddata THEN $
    CLOUD_OVERLAY_CONFIG, $
     landwater=hue, $
      cloudmask=cloudmask, $
        brightmin=brightmin, $
         brightmax=brightmax, $
          satmin=satmin, $
           satmax=satmax, $
            lonmin=lonlim[0], $
             lonmax=lonlim[1], $
              latmin=latlim[0],$
                latmax=latlim[1]


  Map_Set, 0, mean(lonpar[0:1]), /noborder, $
     limit=[ latpar[0], lonpar[0], latpar[1], lonpar[1] ],$
      Ymargin=[4.,4]

    ; Define the new Brightness/Saturation mappings
  xx=findgen(100)/99.

  bi = 0.2> interpol( [0.,1], [BrightMin, BrightMax], xx ) < 1
  si = 0> interpol( [1.,0], [SatMin,    SatMax],xx ) < 1

    ; Use 'cloudmask' to create new Brightness/Saturation values

  IF clouddata THEN BEGIN 
    cloudmask = scale( temporary(cloudmask),minv=0,maxv=255)*99
    b2=bi[cloudmask]
    s2=si[temporary(cloudmask)]
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
                       xs,ys,mxsize,mysize,$
                       lonmin=lonlim[0],$
                       latmin=latlim[0],$
                       lonmax=lonlim[1],$
                       latmax=latlim[1],$
                       scale=scalefac, /compress, /bilinear )


    IF i EQ 0 THEN BEGIN 
      dim = size(tmpIm,/dim)
      mapIm = bytarr(dim[0], dim[1], 3)
    ENDIF 
    mapIm[*,*,i] =  temporary(tmpIm)
  ENDFOR 
  im = 0


;    ; Tv the final PS image
;  IF ps THEN BEGIN 
;
;  ENDIF ELSE BEGIN 
;      Tv,mapIm[*,*,i],xs,ys
;  ENDELSE 

  IF winddata THEN nn = where(strlen(windfiles) NE 0 , nf) ELSE nf = 0
  IF nf NE 0 THEN BEGIN 
    windFiles = windFiles[nn]
    tt0 = systime(1)
    windData = Read_Wind_Files(windFiles,$
                               CRDecimate=CRDecimate,$
                               Decimate=Decimate,$
                               ExcludeCols=ExcludeCols, $
                               rainflag=rainflag, rf_action=rf_action, $
                               rf_index=rfi)
    tt1 = systime(1)
    IF verbose THEN print,' Read_wind_Files took: ', tt1-tt0,$
      ' Seconds '

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
          rfi =  where( ii[good], nn) & ii=0
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
              rfi =  where( ii[good], nn) & ii=0
            ENDIF 
            u = u[good]
            v = v[good]
            lon = lon[good]
            lat = lat[good]
            speed = speed[good]

          ENDIF 
          veccol = BytScl( speed, min=minspeed, $
                           max=maxspeed, $
                           top=N_WIND_COLORS-1) + $
                           WIND_START
          col24 = rgb2true( veccol, colortable=ct)
          t0 = systime(1)

        ENDIF 
      ENDIF 
    ENDELSE 

  ENDIF   


    ; Calculate where to put Colorbar
  sz = size( mapIm[*,*,0],/dim)
  xyz = Convert_Coord( 0, ys+sz[1]/scalefac,/device,/to_normal)
  y = xyz[1]
  yCB = [3*y+2, 2*y+3]/5

    ; And the titles
  gms5_string =  'GMS5 Overlay Test'
  gms5_string =  'Gms5' + gmsType + '(' + time_string + ')'
  IF n_elements(title) NE 0 THEN $
    Title= title + ' ' + gms5_string ELSE $
    Title= gms5_string 

  IF ps THEN text_color = 0b ELSE text_color = 255b
  xyz = Convert_Coord(0,ys,/device,/to_normal)
  ytitle = xyz[1]/2.
  ysubtitle = xyz[1]/4.



    ; ====== Plot the vectors (if there are any.) =======


  nn = n_Elements(u)
  IF ps THEN BEGIN 
    Tv,mapIm,xs,ys,xsize=mxsize,ysize=mysize,true=3 
    IF nn GT 1 THEN BEGIN 
      tvlct,orig_red,orig_green,orig_blue,/get
      tvlct,transpose(ct)
      PlotVect,u,v,lon,lat,len=length,$
        thick=thick,start_index=WIND_START,$
          ncolors=N_WIND_COLORS, scale=scaleVec, minspeed=minspeed, $
           maxspeed=maxspeed
      IF rfi[0] NE -1 AND rf_action EQ 1 THEN BEGIN 
         TVLCT,ishft(rf_color,-16),ishft(rf_color,-8) AND '0000ff'xl, $
           rf_color AND '0000ff'xl,1
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
                            color=toplot[ii].textcolor,/data
            ENDELSE 
          ENDFOR 
        ENDIF 
      ENDFOR 
      IF gridlines THEN BEGIN
        Map_Set, 0, mean(lonpar[0:1]), /noborder, $
           limit=[ latpar[0], lonpar[0], latpar[1], lonpar[1] ],$
           Ymargin=[4.,4],/noerase,/grid,/lab,color=255b
      ENDIF 
      tvlct,orig_red,orig_green,orig_blue
    ENDIF 

    ColBar, bottom=Wind_Start, nColors=N_Wind_Colors,$
     position=[0.25,yCB[0], 0.75, yCB[1]], $
     Title='Wind Speed (m/s)',Min=minspeed, $
     max=maxspeed,divisions=4, format='(f5.0)', $
     pscolor=ps, /true, table=ct, charsize=0.75

    xyouts, 0.5, ytitle, title, align=0.5, $
        /normal, charsize=1.05, color=text_color
    IF n_elements(subtitle) NE 0 THEN BEGIN 
      xyouts, 0.5, ysubtitle, subtitle, align=0.5, $
        /normal, charsize=0.75, color=text_color
    ENDIF 

    IF rainflag NE 0 AND rf_action EQ 1 THEN BEGIN 

      newct = ct
      newct[*,0] =  [rf_color AND 'ff'xl, $
                      ishft(rf_color,-8) AND 'ff'xl, $
                       ishft(rf_color,-16) AND 'ff'xl]

      Colbar,pos=[0.49,0.005,0.51,0.025],bottom=0,ncolors=1,min=0,max=1,$
           table=newct,/true,/noannot,color='ffffff'xl
      xyouts,0.49,0.005,'Rain ',align=1,/normal
      xyouts,0.51,0.005,' Flagged',/normal


    ENDIF 

  ENDIF ELSE BEGIN 


     ;; ======= NON postscript processing ===============


    finalim = bytarr(xsize,ysize,3)

    FOR i=0,2 DO BEGIN 

      tv,mapIm[*,*,i],xs,ys
      IF nn GT 1 THEN BEGIN 
        CASE i OF 
          0: col =  col24 AND 'ff'xl
          1: col =  ishft(col24,-8) AND 'ff'xl
          2: col =  ishft(col24,-16) AND 'ff'xl
        ENDCASE 

          ;; ===== Reestablish the plotting env.

        Map_Set, 0, mean(lonpar[0:1]), /noborder, $
           limit=[ latpar[0], lonpar[0], latpar[1], lonpar[1] ],$
           Ymargin=[4.,4],/noerase

        PlotVect, u,v,lon,lat, color=col, len=length, $
          thick=thick, scale=scaleVec, $
            minspeed=minspeed,maxspeed=maxspeed
        IF rfi[0] NE -1 AND rf_action NE 0 THEN BEGIN 
          PlotVect,u[rfi],v[rfi],lon[rfi],lat[rfi],len=length,$
           thick=thick,minspeed=minspeed, maxspeed=maxspeed, scale=scaleVec,$
           color=rf_color
        ENDIF 
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
                           color=toplot[ii].textcolor,/data
            ENDELSE 
          ENDFOR 
        ENDIF 
      ENDFOR 

        ; == Put the grid lines on, if requested!

      IF gridlines THEN BEGIN 
        Map_Set, 0, mean(lonpar[0:1]), /noborder, $
           limit=[ latpar[0], lonpar[0], latpar[1], lonpar[1] ],$
           Ymargin=[4.,4],/noerase,/grid,/lab,color=0b
      ENDIF 

        ; ======= Do annotations ==========


      col = bytarr(3,n_elements(ct[0,*]))
      col[0,*] =  ct[i,*]

      ColBar, bottom=Wind_Start, nColors=N_Wind_Colors,$
       position=[0.25,yCB[0], 0.75, yCB[1]], $
        Title='Wind Speed (m/s)',Min=minspeed, $
         max=maxspeed,divisions=4, format='(f5.0)', $
          pscolor=ps, /true, table=col, charsize=0.75, $
           color=255
 
      xyouts, 0.5, ytitle, title, align=0.5, $
          /normal, charsize=1.05, color=text_color
      IF n_elements(subtitle) NE 0 THEN BEGIN 
        xyouts, 0.5, ysubtitle, subtitle, align=0.5, $
          /normal, charsize=0.75, color=text_color
      ENDIF 

      IF rainflag NE 0 AND rf_action EQ 1 THEN BEGIN 

        newct = ct
        newct[*,0] =  [rf_color AND 'ff'xl, $
                        ishft(rf_color,-8) AND 'ff'xl, $
                         ishft(rf_color,-16) AND 'ff'xl]

        Colbar,pos=[0.49,0.005,0.51,0.025],bottom=0,ncolors=1,min=0,max=1,$
             table=newct,/true,/noannot,color=255b
        xyouts,0.49,0.005,'Rain ',align=1,/normal
        xyouts,0.51,0.005,' Flagged',/normal


      ENDIF 
      finalim[*,*,i] = tvrd()
    ENDFOR 


  ENDELSE 

  mapIm = 0

  t1 = systime(1)
  IF verbose THEN print,' Plotvect took: ', t1-t0,' Seconds '
  t0 = t1



    ; ======== Write the output ===============


  CASE 1 OF 
    gif: BEGIN 
        ; Quantize it down to 175 colors.
      tt0 = systime(1)
      im = color_Quan( finalim, 3, r,g,b, colors=175 )
      tt1 = systime(1)
      IF verbose THEN print,' Color_Quan took: ', tt1-tt0,' Seconds '
      tt0 = tt1

      Write_Gif, OutputFilename, im, r,g,b
      tt1 = systime(1)
      IF arg_present(thumbnail) THEN  BEGIN 
        dims = size(im,/dimension)
        thumbnail = OutputFilename + '.TN'
        thumbnailIm =  congrid( im, dims[0]*0.3, dims[1]*0.3)
        Write_Gif, thumbnail, temporary(thumbnailIm), r,g,b
      ENDIF 
      IF verbose THEN print,' Write_Gif took: ', tt1-tt0,' Seconds '
      tt0 = tt1

    END

    jpeg: BEGIN 
      
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
  Outfile = OutputFileName
  set_plot,savedevice
;  genv,/restore
   
END


