;+
; NAME: ANIMATE_WIND_FIELD 
; $Id$
;
;
; PURPOSE: Given an file containing a interpolated wind field and some
;          (acutally alot) of other information to control the
;          animation, this routine  will produce the frames that
;          then can be streamed together to make an movie (e.g
;          quicktime, mpeg... ) Currently only tested under IDL
;          4.0.1. Testing is ongoing under IDL 5.0, but I'm not done
;          yet and there are still some bugs under 5.0.
;
; Authors: William Daffer (current maintainer)
;          Mike Spencer
;          Jim Huddleston.
;          
;
;
; CATEGORY: Animation
;
;
;
; CALLING SEQUENCE:
;
;
; 
; INPUTS:  file    - name of file containing the underlying wind field
;
;
; OPTIONAL INPUTS:
;
;
;       
; KEYWORD PARAMETERS:
;          ddims   - 3x2 matrix containing the dimensions for the matrix of
;                    data in that file. This vector has  the form 
;                    [ [ lonmin, lonmax, loninc], $
;                       [latmin, latmax, latinc ] ]

;          lonpar  - 2-vector of longitude extrema defining area of
;                    interest (default = [0., 359.])
;          latpar  - 2-vector of latitude extrema defining area of 
;                    interest (default = [-60., 60])
;          ui      - Interpolated wind fields U component
;          vi      - Interpolated wind fields V component
;          vlonpar - 3-vector of longitude parameters defining the
;                    area over which to calculate the animated wind
;                    field. The vector has the form [min, max, inc ]
;                    It must be the case that vlonpar(0) <= lonpar(0)
;                    and vlonpar(1) >= lonpar(1) with equality only
;                    when lonpar(0:1) are at their respective extrema.
;                    This restriction arises from the desirability of
;                    vector field extending beyond the edges of the
;                    visible frame, so that the wind vectors seems to
;                    enter and exit the frame from 'off-stage' 
;                    (default = [ 0>lonpar(0)-10, lonpar(1)+10<360. ]
;          vlatpar - 3-vector latitude analog of vlonpar
;                    (default = [ -90>latpar(0)-10, latpar(1)+10<90. ]
;          animpar - 3-vector of parameters for animation, having the
;                    form [ window xsize, window ysize, number of frames ]
;                    (def = [640,480,60]
;          pad     - 2-vector indicating how many pixels of blank space 
;                    to put around the image. This is so you can make
;                    a movie which is 'tv safe' which can be put on
;                    video tape for use on in TV broadcast.
;          path_inc - amount to move a vector at each step of the
;                     animation. I have never used this keyword and
;                     have now idea what it would do if
;                     used.(def=0.04)
;          help -    prints out alot of help mumbo-jumbo
;          min_speed - minimum speed to be used in the wind speed
;                     contour  and wind speed color bar. (m/s unless knots=1)
;          max_speed - maximum speed to be used in the wind speed
;                     contour  and wind speed color bar. (m/s unless knots=1)
;
;            NB: Internal to this routine, all calculations are in
;                Meters/Second. And the units of U/V are *ALWAYS* M/S,
;                since this is how they appear in the data. DO NOT
;                CONVERT THEM TO KNOTS if you want to display the
;                colorbar as knots. Only when the speed is reported is
;                the conversion made to knots. Nevertheless, it seems
;                wiser to have the input of the min_speed and max_speed
;                keywords done in whatever units are used on output.
;
;          debug - useful for debugging. If set, the routine will stop
;                  in the context of the error, so that you can
;                  investigate the problem, instead of immediately
;                  returning to the main level.
;          gif - writes a gif file for each frame (default)
;          pict - write pict_file
;          ps - write postscript file
;          tiff - writes out a tiff file.
;          jpeg - writes out a jpeg file
;          save_first - will save the first set of images in an IDL
;                       save set 'first_frame.save'. Useful for
;                       debugging purposes.
;          title  - title string to put on the top of the frame
;          noxinter - obselete, does nothing, backwards compatibility
;          harmonic - Reset the vectors on this harmonic of the
;                     fundamental (number of frames)
;          knots - flag, report the speed in knots. Otherwise, report
;                  the speed in meters/second. Also, if this flag is
;                  set, min/max speed must be input as knots.
;
;
;                  Nota Bene! The dimensions of U and V are always
;                  M/S, this is how they appear in the data! Don't
;                  convert them to knots in order to have the colorbar
;                  display as knots. The KNOTS keyword applies only to
;                  how the speed colorbar is configured and the units
;                  of the min_speed/max_speed keywords.
;
; ABOUT THE METHOD AND SOME OF THE DESCRIPTIONS:
;
;  It's a little hard to describe how the animation is done. I tend to
;  get caught up in vocabulary difficulties. So I'll try to define my
;  terms precisely. However, alot of the inline documentation that
;  follows was written while I was developing the code, so it may not be
;  as precise. I'm sorry about that, but I'm not going to go change
;  it. I think that it's clear enough, for the most part.
;
;  First, let me discuss what it means to create an animation of a wind
;  field and how it is done. The method is as follows:
;
;  Onto an input wind field - call this the WFG for Wind Field Grid - we
;  superpose a grid of locations - call this the VFL for Vector Field
;  Locations. I call this the Vector Field Locations because these
;  locations will ultimately be where we plot vectors on each finished
;  frame. The WFG is also a vector field, it is the interpolated wind
;  vector field, but to minimize terminological problems we'll call it
;  the WFG. We assume that the WFG is a sampling of a continuous wind
;  field. Clearly, at each of the location given by the VFL, this
;  underlying field has a certain value, which we get by interpolating
;  the WFG to the locations in VFL. We then move each of these locations
;  by the these newly interpolated quantities. That is, if VFL(i) has
;  location long(i),lat(i) and wind components u(i),v(i) obtained by
;  interpolating the WFG to the location VFL(i) then VFL(i+1) has
;  locations long(i)+u(i)*path_inc, lat(i)+v(i)*path_inc. Path_inc is an
;  empirically derived scaling factor used to keep the vectors from
;  moving too fast around the screen. It is configurable by means of the
;  keyword 'path_inc' although I've never used the keyword myself.
;
;  There are four positional  parameters and two keywords that are
;  crucial in controling the animation. The first two positional
;  parameters, the 'files' parameter and the 'fdims' parameter tell
;  where to get the file(s) that contain the WFGs and what shape the
;  WFGs have. The next two positional parameters, 'lonpar' and 'latpar'
;  determine the longitude and latitude extent of the output
;  picture. The two keyword parameters, vlonpar and vlatpar, determine
;  the latitude, longitude and spacing of the VFL. Note that the VFL can
;  have spacing different from the WFG and the longitude and latitude
;  spacing are individually specifiable.
;
;

; A NOTE about the interaction of the keywords ANIMPAR and PAD:
;
;          The frame is guarenteed to be animpar(0) by animpar(1) in
;         size. The amount of that frame containing
;         non-blank/non-annotative pixels is reduced in the presence of
;         the pad keyword: pad(0) gives the margin in the X direction,
;         pad(1) the margin in the Y direction on each side of the the
;         actual image. There is an addition margin of 40 pixels on the
;         top of the image, for title and color bar. So, if
;         animpar=[640,580,60] and pad=[40,40] then the portion of the
;         frame with non-blank/non-title pixels will be [640-2*40,
;         480-2*40-40] = [560,360] in size and will be centered in the x
;         direction. It'll be low by 20 pixels in the Y direction
;         because of the extra 40 pixels margin in the Y direction.
;
;
;         The question arrises: Why bother using the pad keyword?
;         Normally you won't need to unless you're making frames for
;         an animation which will might be broadcast on
;         television. When doing that you have to make the animation
;         'TV safe' which means that you can't use all the space. So
;         this is a way of specifying a frame size with enough margin
;         around the edges so that it's TV safe. If you don't need to
;         make such a creature, don't bother with the pad keyword,
;         since it reduces the usable area of the output frame.
;      
;
; ABOUT THE STRUCTURE OF THE INPUT INTERPOLATED WIND FILE(s). 
;   (i.e. Those input using the paramater 'files' ).
;
; The file should contain two float arrays (4 byte floats), the first array
; should be the U component of the wind field and the second should be
; the V component.
;
; The U coordinate direction points due East and the V coordinate direction
; points due North. The resultant vector points in the direction of
; the flow, so that a vector whose components were [0,1] would
; indicate a wind going from South to North.
;
; The dimensions of the vector are meters-per-second, so that the
; vector given above represents a 1 m/s wind going from South to
; North.
;
; if 
;
;    ddims = [ [lonmin,lonmax,loninc], [latmin,latmax,latinc] ] 
;
; then the each array is
; 
;  (lonmax-lonmin)/loninc + 1   by  (latmax-latmin)/latinc + 1 
;
;  The routine will try to create the following arrays and try to
;  execute the following read statement.  ;

; u=fltarr[ (lonmax-lonmin)/loninc + 1, (latmax-latmin)/latinc + 1 ] & v=u
;
; readu, interp_lun, u,v 
;
; Note well: In IDL, the first index of the array varies fastest,like
;            Fortran. This is the opposite of C. You MUST take this 
;            into account if you're creating your interpolated fields 
;            using C.
;
; For instance, in order for an array calculated by C code to be
; correctly read into this routine, it would have to be dimensioned
; and written out as follows.  follows (using our default of 360x121 1
; degree grids
;
;      float U[121][360], V[121][360];
;                 .
;                 .
;                 .
;          calculations done here.
;                 .
;                 .
;                 .
;      fwrite(U,360*121,sizeof(float),flun)
;      fwrite(V,360*121,sizeof(float),flun)
;
;
; A NOTE ABOUT 'FILES' VERSUS THE UI/VI  KEYWORDS:
;
; Now (sep98) there is a way to pass the data directly into this
; routine, rather than making the routine read a file. It only works
; when animating a single field, not an 'evolution' movie. Pass the
; UI/VI directly in, rather than first writing it out to a file. 
;
;
; OUTPUTS: All output is to the files specified in the 'xxxx'
;          keyword, default=gif
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS: prs - used to pass data too and from CALCVECTORFIELD
;                colors - used to stores color table
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
; SUBROUTINES/FUNCTIONS CALLED:
;
;    CALCVECTORFIELD: calculates the next position each vector in the vector
;    field (idl source)
;    LAND_MASK - Calculates the mask for the input points. 0=water, 1=land. This
;    in conjunction with the land elevation database and the color table gives
;    the pretty color land mask. This routine is a link image routine, with all
;    the woes attendant upon it for being so.
;    COLBAR - puts a color bar on each frame
;    UNPACK_WHERE - turns a 1 dim vector output from the 'where' IDL 
;    internal call to a 1,2,3 OR 4 dim array, depending on the input.
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
; IDL> animate_wind_field,$
; '/path/to/the/interp/wind/file/wind_file',$ ; file with interpolated field
;  ddims=[[0,359,1],[-60,60,1]],$ ; file dimensions 360 by 121
;  lonpar=[110,270],
;  latpar=[-20,60]    ,$ ; lon/lat of view
;  vlonpar=[100,280,1.5] ,$ ; lon start/stop/increment of vector field
;  vlatpar=[-10,60,1.5]  ,$ ; lat start/stop/increment of vector field
;  animpar=[640,480,60]  ,$ ; x size/y size /number of frames in animation
;  /gif               ; write it out as a series of gif files.
;
;  This command lines says that there is a file named 'wind_file' in directory 
;  /path/to/the/interp/wind/file which has an interpolated wind field in
;  it. This field has dimensions 360 by 121, i.e. a one degree grid in
;  lon/lat running from 0 to 359 in longitude and -60 to 60 in
;  latitude. The users wants an animation of the portion of the globe
;  with 110 <= longitude <= 270 and -20<=latitude<=60. The vector field
;  should be calculated with 100<=longitude<=280 and
;  -10<=latitude<=60. Note that you should always overlap the vector
;  field with the 'view' so that the vectors are seen as smoothly
;  entering and exiting the view. The routine will create 60 frames,
;  writing each out as a gif file, with dimensions 640 by 480. Since the
;  pad keyword wasn't used, the actual images with each 640by480 frame
;  will be approx 640 by 440, the other 40 being used for titles and
;  colorbar.
;

; IDL> animate_wind_field,ui=ui, vi=vi, ddims=[[0,359,1],[-60,60,1]],
;  lonpar=[110,270],
;  latpar=[-20,60]    ,$ ; lon/lat of view
;  vlonpar=[100,280,1.5] ,$ ; lon start/stop/increment of vector field
;  vlatpar=[-10,60,1.5]  ,$ ; lat start/stop/increment of vector field
;  animpar=[640,480,60]  ,$ ; x size/y size /number of frames in animation
;  /gif               ; write it out as a series of gif files.
;
; Same as above, but here we're passing the data in via the
;  ui/vi/ddims keywords.
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.9  2000/02/28 19:19:22  vapuser
; Fixed a problem with the Title.
;
; Revision 1.8  2000/01/13 17:43:24  vapuser
; Changed call to qmodel->get from the old 'procedure' to the newer
; 'function'.  this is required since there is no procedure
; anymore. (Why this didn't fail outright I don't understand!?)
;
; Revision 1.7  1999/10/11 17:22:13  vapuser
; Added support for reading user 'config files.' Change all internal
; calculations using 'speed' to Meters/second, reverting to 'knots' only
; when composing title/colorbar. Adopt standard that knots=1 implies
; min/max speed are given in knots.
;
; Revision 1.6  1999/10/06 17:05:51  vapuser
; Took out meters_per_sec keyword, put in knots keyword. Default to
; meters/sec and max_speed=30. Put in some support code for knots keyword.
;
; Revision 1.5  1999/10/05 17:02:00  vapuser
; Changed the 'write_xxx' keywords to just 'xxx', thereby increasing
; their uniqueness.  Added the 'harmonic' and 'meters_per_second'.
; Corrected some misuses of keyword_set() and bad meters_per_second
; to knots conversion. Changed location of land_elevations.bin to
; VAP_LIB. General maintenance.
;
; Revision 1.4  1999/04/08 22:01:56  vapuser
; Replaced Colorbar with ColBar
;
; Revision 1.3  1999/04/08 16:59:14  vapuser
; Added call to readcolortable.pro and cleaned up some comments.
;
; Revision 1.2  1998/10/17 00:19:40  vapuser
; Worked on file reading section, incorporated use
; of 'qmodel' object. Killed some bugs.
;
; Revision 1.1  1998/10/06 00:17:44  vapuser
; Initial revision
;
;
; Pre-RCS mods:
;
; Fri Jul 25 11:41:29 1997, Vap User <vapuser@haifung>
;
;   aka, William Daffer, <daffer@rainy.jpl.nasa.gov> Added pad keyword
;   and fixed the way it interacts with the animpar keyword. Now, when
;   the users puts in animpar(0)=640 and animpar(1)=480, (for
;   instance) then that, by god, will be the size of the output
;   frame. The 40 pixel y padding for the title and the x/y padding
;   given in the pad keyword will be subtracted from animpar(0:1) at
;   the beginning of the processing so that the resulting frame will
;   be the size given in animpar(0:1), which is, after all, WHAT YOU'D
;   EXPECT!
;
;
; 6-mar-1996 to 25-jul-1997: whd, William Daffer, vapuser, whatever.
; made many modifications which I didn't record and won't record now.
;               
;
;
; 5-mar-1996: whd, modified version of animate_contour.pro from Mike
;             Spencer.
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
;
PRO ANIMATE_WIND_FIELD, files, $ ; fully qualified grid file name(s) (no default)
                        ddims = ddims,$ ; dimension of grid (3 by 2 array )
                               ; ( [ [ start_lon, stop_lon, lon_inc ],
                               ;   [ [ start_lat, stop_lat, lat_inc ] )
                            ; (def = [ [0, 359. 2.5],[-90.,90.,2.5]])
                            ; Unneccessary if the file is an HDF file.
                    lonpar = lonpar, $  ; longitude dims of output animation 
                               ; [ start_lon, stop_lon ]
                               ; (def=[0.,359]
                               ; Unnecessary if HDF file
                    latpar =  latpar, $  ; latitude dims of output animation
                               ; [ start_lat, stop_lat]
                               ; (def=[-60.,60])
                               ; Unnecessary if HDF file
                    ui     = ui,$ ; Interpolated wind field's U comp
                    vi     = vi,$ ; Interpolated wind fields V comp
                    vlonpar = vlonpar,$ ; long dims of vector field
                                        ; [ start, stop, inc ]
                                        ; (def=[0>(lonpar(0)-10),
                                        ;       (lonpar(1)+10)<360,1]
                    vlatpar = vlatpar,$ ; lat dims of vector field
                                        ; [ start, stop, inc ]
                                        ; (def=[-90>(latpar(0)-10),(
                                        ;             latpar(1)+10)<90,1])
                    animpar = animpar,$ ; window and animation parameters
                                        ; [ xsize, ysize, num frames ]
                                        ; (def=[640,480,60])
                    path_inc= path_inc,$ ; scale factor to determine how far
                                         ; along projected path a vector moves
                    gif =  gif, $ ; flag to write gif file 
                                              ; (the default)
                    pict= pict ,$  ; flag to write pict file
                    ps= ps,$      ; flag to write postscript file
                    tiff= tiff,$      ; flag to write tiff
                                                  ; file
                    jpeg = jpeg, $          ; Write Jpeg
                    debug = debug       ,$  ; for debugging
                    title=title ,$          ; Title for each frame.
                    min_speed=min_speed,$  ; Minimum wind speed 
                                           ; (m/s unless knots=1)
                    max_speed=max_speed,$  ; Maximum wind speed 
                                           ; (m/s unless knots=1)
                    length=length,$        ; length of vectors
                    help= help,$           ; set to get help
                    pad= pad,$ ; padding for tv safe, [xpad, ypad ]
                    save_first= save_first,$
                    interpolate=interpolate,$
                    nologo= nologo,$   ; flags, set if you don't
                                       ; want a logo
                    tvsafe = tvsafe ,$ ; sets pad = 
                                       ; [xsize*0.15,ysize*0.15]
                    titsafe = titsafe,$;Title safe, 
                                       ; Adds an additional 
                                       ; 10% pad onto tvsafe. 
                                       ; Implies tvsafe.
                    thick = thick, $   ; Thickness of vectors (def=1)
                    harmonic=harmonic, $; 
                     knots  = knots; report the speed in knots, instead of 
                                   ; the default of m/s
                    noxinter= noxinter ; obsolete, does nothing, 
                            ; it's just here so 
                            ; a lot of other software doesn't
                            ; break.


COMMON prs, long_sel, lats_sel, lons, lats, uu, vv, uu_sel, vv_sel, $
           ileft, iright, itop, ibot, dist_left, dist_right, dist_top, $
           dist_bot, dist, weights, invdist, xfinc, xf0, yfinc, yf0, eps

COMMON colors, r_curr, g_curr, b_curr, r_orig, g_orig, b_orig

   rcsid = "$Id$"

   catch, error
   IF error NE 0 THEN BEGIN 
     Message,!error_state.msg,/cont
     return
   ENDIF 


  GENV,/save ; Save Graphical environment.

  lf =  string(10b)
  hstr =  'Usage: ' + lf + $
  'ANIMATE_WIND_FIELD, files, $ ; fully qualified grid file name(s)' + lf + $
  '   ddims=ddims, $   ; dimension of grid (3 by 2 array ) ' + lf + $
  '              ; ( [ [ start_lon, stop_lon, lon_inc ],' + lf + $
  '              ;   [ [ start_lat, stop_lat, lat_inc ] )' + lf + $
  '  lonpar=lonpar, $  ; longitude dims of output animation ' + lf + $
  '             ; [ start_lon, stop_lon ]' + lf + $
  '  latpar=lonpar, $  ; latitude dims of output animation' + lf + $
  '             ; [ start_lat, stop_lat]' + lf + $
  '  vlonpar = vlonpar,$ ; long dims of vector field' + lf + $
  '                      ; [ start, stop, inc ]' + lf + $
  '  vlatpar = vlatpar,$ ; lat dims of vector field' + lf + $
  '                                            ; [ start, stop, inc ]' + lf + $
  '  animpar = animpar,$ ; window and animation parameters' + lf + $
  '                      ; [ xsize, ysize, num frames ]' + lf + $
  '  path_inc= path_inc,$ ; scale factor to determine how far' + lf + $
  '                       ; along projected path a vector moves' + lf + $
  '  gif =  gif, $ ; flag to write gif file' + lf + $
  '  pict= pict ,$  ; flag to write pict file' + lf + $
  '  ps= ps,$      ; flag to write postscript file' + lf + $
  '  tiff= tiff,$      ; flag to write postscript file' + lf + $
  '  debug = debug       ,$  ; for debugging' + lf + $
  '  title=title ,$          ; Title to put at top of frame' + lf + $
  '  min_speed=min_speed,$  ; Minimum wind speed' + lf + $
  '  max_speed=max_speed,$  ; Maximum wind speed' + lf + $
  '  pad=pad,$    ; padding, for tv safe animations [ xpad,ypad] ' + lf + $
  '  help= help,$           ; set to get help' + lf + $
  '  noxinter= noxinter ; obselete, does nothing, its just here so ' + lf + $
  '                     ; a lot of other software doesnt break.' 

IF keyword_set( help ) THEN BEGIN
  print, hstr
  return
ENDIF 

debug =  keyword_set( debug )
IF NOT debug THEN on_error,1 ; return to main

IF n_elements(files) EQ 0 AND $
   (n_elements(ui) EQ 0  AND n_elements(vi) EQ 0) THEN BEGIN 
  Message,'You must specify either FILES or "UI/VI"',/cont
  print,hstr
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
  

chkcfg,'INTERPOLATE',interpolate,cfg,/bool
chkcfg,'NOLOGO',nologo,cfg,/bool
dologo        = (nologo EQ 0)
chkcfg,'PICT',pict,cfg,/bool
chkcfg,'GIF',gif,cfg,/bool
chkcfg,'PS',ps,cfg,/bool
chkcfg,'TIFF',tiff,cfg,/bool
chkcfg,'JPEG',jpeg,cfg,/bool

chkcfg,'HARMONIC',harmonic,cfg
chkcfg,'THICK',thick,cfg
chkcfg,'KNOTS',knots,cfg,/bool

chkcfg,'MIN_SPEED',min_speed,cfg
chkcfg,'MAX_SPEED',max_speed,cfg
chkcfg,'LENGTH',length,cfg
chkcfg,'TITLE',title,cfg



;pict    = KEYWORD_SET( pict )  
;gif     = KEYWORD_SET( gif )  
;ps      = KEYWORD_SET( ps )   
;tiff    = KEYWORD_SET( tiff )  
;jpeg    = KEYWORD_SET( jpeg )  


IF n_elements(harmonic) EQ 0 THEN harmonic = 1;
IF n_elements(thick) EQ 0 THEN thick = 1
;knots =  keyword_set(knots)



IF NOT pict AND $
   NOT gif AND $
   NOT ps AND $
   NOT tiff AND $
   NOT jpeg THEN gif =  1

mps2knots =  1./0.51479 ; converts meters/sec to knots.



IF n_elements( min_speed ) EQ 0 THEN BEGIN 
  min_speed =  1                ; meters/sec
  IF knots THEN min_speed = min_speed*mps2knots 
ENDIF 


IF n_elements( max_speed ) EQ 0 THEN BEGIN 
  max_speed =  30               ; meters/sec
  IF knots THEN max_speed = max_speed*mps2knots 
ENDIF 



IF n_elements( length ) EQ 0 THEN length =  3;
IF n_elements( title ) EQ 0 THEN title =  '' 

  ; To the maintainer. As a optimzation move, do all internal
  ; calculations in meters/second. Otherwise, we have to keep
  ; multiplying big arrays by mps2knots instead of just using it when
  ; we really need it, which is when the colorbar is applied, and this
  ; is only done if we're putting a logo on! We have to go through the
  ; rigamorole above with min/max speed to make sure that everything
  ; was in the right units.

  ; Later, after we report the min/max speed, we'll convert back to
  ; m/s, keeping it in these units until the final step, when, if
  ; needed, we convert to knots in order to put the color bars on the
  ; frames.

  ; Sorry!


ncolors = 29

CASE 1 OF
  gif: message,' Files will be output in GIF format',/cont
  pict: message,' Files will be output in PICT format',/cont
  ps: message,' Files will be output in PS format',/cont
  tiff: message,' Files will be output in TIFF format',/cont
  jpeg: Message,' Files will be output in JPEG format',/cont
  ELSE : BEGIN
    message,' One file format MUST be selected',/cont
    return
  END  
ENDCASE 

  ; Check for requests to pad the frames

chkcfg,'PAD',pad,cfg,/bool
chkcfg,'TVSAFE',tvsafe,cfg,/bool
chkcfg,'TITSAFE',titsafe,cfg,/bool

;padframe =  keyword_set( pad ) OR $
;            keyword_set(tvsafe) OR $
;            keyword_set(titsafe)

padframe = pad  OR tvsafe OR titsafe


eps =  0.000102
east_long = 1   ; default to east long, unless some input 
                ; quantity is in west longitude
setup_logo =  1 ; Flag for logo setup code

ON_IOERROR, stop
  ; set path increment. This is the factor applied to the u/v vector
  ; components to get the location of the vector in the next frame of
  ; the animation.

chkcfg,'PATH_INC',path_inc,cfg
chkcfg,'LONPAR',lonpar,cfg
chkcfg,'LATPAR',latpar,cfg
chkcfg,'VLONPAR',vlonpar,cfg
chkcfg,'VLATPAR',vlatpar,cfg
chkcfg,'ANIMPAR',animpar,cfg

IF n_elements(path_inc) EQ 0 THEN path_inc =  0.04 

IF n_elements( lonpar ) NE 2 THEN BEGIN
  lonpar =  [0.,359]
  message,' Taking default lonpar = [0., 359. ]',/cont
ENDIF 

IF n_elements( latpar ) NE 2 THEN BEGIN 
  latpar =  [-60., 60 ]
  message,' Taking default latpar = [-60., 60]',/cont
ENDIF 

IF n_elements(vlonpar ) NE 3 THEN BEGIN 
  vlonpar =  [0. >  (lonpar[0]-5), 360<(lonpar[1]+5), 1. ]
  message,' Taking default vlonpar ',/info
  print,vlonpar
ENDIF 

IF n_elements( vlatpar ) NE 3 THEN BEGIN 
  vlatpar =  [-90. > (latpar[0]-5), 90 <  (latpar[1]+5), 1. ]
  message,' Taking default vlatpar', /info
  print,vlatpar
ENDIF 
IF n_elements( animpar ) NE 3 THEN BEGIN 
  animpar =  [640, 512, 60 ]
  message,' Taking default animpar = [640,512, 60 ] ',/cont
ENDIF 

nframes =  animpar[2] ; number of frames in the animation.
IF nframes MOD harmonic NE 0 THEN BEGIN 
  Message,'Nframes != 0 (Mod Harmonic): Returning',/cont
  print,'nframes: ', nframes, ' harmonic: ', harmonic
  return
ENDIF 

IF tvsafe THEN $
 pad = [ round(animpar[0]*0.09375/2.),round(animpar[1]*0.0916667/2.) ]
IF titsafe THEN $
 pad =  [ round( animpar[0]*0.184375/2.),round(animpar[1]*0.184/2.) ]


message,'path_inc = ' + string(path_inc,form='(f7.2)'),/info
message,'lonpar   = ' + string(lonpar, form='(2(f7.2,:,","))'),/info
message,'latpar   = ' + string(latpar, form='(2(f7.2,:,","))'),/info
message,'vlonpar  = ' + string(vlonpar,form='(3(f7.2,:,","))'),/info
message,'vlatpar  = ' + string(vlatpar,form='(3(f7.2,:,","))'),/info
message,'animpar  = ' + string(animpar,form='(3(f7.2,:,","))'),/info
message,'nologo   = ' + strtrim(nologo,2),/info
message,'knots    = ' + strtrim(knots,2),/info
message,'length   = ' + strtrim(length,2),/info
message,'thick    = ' + strtrim(thick,2),/info

IF knots THEN BEGIN 
  message,'min_speed (knots) = ' + string( min_speed, form= '(f7.2)' ),/info
  message,'max_speed (knots) = ' + string( max_speed, form= '(f7.2)' ),/info

  ; Now convert the speeds back to m/s for optimal code.
  min_speed = min_speed/mps2knots
  max_speed = max_speed/mps2knots

ENDIF ELSE BEGIN 
  message,'min_speed  = ' + string( min_speed, form= '(f7.2)' ),/info
  message,'max_speed  = ' + string( max_speed, form= '(f7.2)' ),/info
ENDELSE 


  ;
first = 1
  ;
  ; Define the color table. On rainy, $NSCAT-VAP = /nscat-vap
  ;
;red =  bytarr(51) &  green=red &  blue=red
;openr,1,'$VAP_LIB/Resources/Color_Tables/nscat-vap-animation.ct2', error= err
;readu,1,red,green,blue
;close,1

PtrToColorTable = ReadColorTable( $
     "$VAP_RESOURCES/Color_Tables/vap-animation.ct")
IF NOT ptr_valid(ptrToColorTable) THEN BEGIN 
  Message,"Can't Read Color table",/cont
  return
ENDIF 
CT = *PtrToColorTable &  ptr_free, ptrToColorTable
Red   = reform(CT[0,*])
Green = reform(CT[1,*])
Blue  = reform(CT[2,*])
r_curr =  red &  g_curr= green &  b_curr= blue
r_orig =  red &  g_orig= green &  b_orid= blue

windStart = 1
windEnd = 28
nwindcolors = windEnd-windStart+1

; n_colors = n_Elements(red)

  ; initialize window /buffer size for 
  ; output file options.

set_plot,'z'
ap = animpar
  ; subtract 40 from animpar[1] (the y size). this'll get added back
  ; in when we put the logo on.

IF dologo THEN ap[1] = ap[1]-40

IF padframe THEN ap(0:1) =  ap(0:1) - 2*pad
device,set_resolution=[ap[0],ap[1]]

tvlct,red,green,blue

  ; Read in the land elevation file;
  ; Find the subarray which applies for this run and extract it.
openr,1,'$VAP_LIB/land_elevations.bin'
landel =  intarr( 12*360, 12*180 + 1 )
readu,1, landel
close,1

read_success = 1
IF n_elements(UI) NE 0 AND n_elements(VI) NE 0 THEN BEGIN 

  uu0 = temporary(ui)
  vv0 = temporary(vi)

  IF n_elements( ddims ) EQ 0 THEN BEGIN
      ddims =  [ [ 0., 359., 1] , $
                 [ -60., 60, 1] ]
    Message,' Taking default dimensions: 360 x 121 x 1deg grid ',/cont
  ENDIF 

    ; If the data is being passed directly
    ; in, Check to make sure ddims agrees with data arrays.
    ; [xy]f: f means file, x means longitude, y means latitude
    ; So, xfinc is the increment in the file in the longitude direction.
    ;

  xfinc =  ddims(2,0)
  yfinc =  ddims(2,1)
  xf0   =  ddims(0,0) &  xf1= ddims(1,0)
  yf0   =  ddims(0,1) &  yf1= ddims(1,1)
  nxf    =  (xf1-xf0)/xfinc+1
  nyf    =  (yf1-yf0)/yfinc+1



  szui = size( UI )
  szvi = size( VI )
  x = where(szui-szvi,nx)
  IF nx NE 0 THEN BEGIN 
    Message,'Error: UI must have same dimensions as VI',/cont
    return
  ENDIF 
  sz = size( UI,/Dimensions )
  IF szui[1] NE nxf AND szui[2] NE nyf THEN BEGIN 
    Message,'UI/VI disagree with DDIMS ',/cont
    print,'UI has dimensions ',sz
    print,'DDims = ', ddims
    return
  ENDIF 

ENDIF ELSE BEGIN 

  ; Reading from a file, ddims will be defined by the file.
  nfiles =n_elements( files )  

  IF nfiles EQ 1 THEN BEGIN 

      ; Only one file.
    q = Obj_New('qmodel',file=files[0])
    IF Obj_Isa(q, 'qmodel') THEN BEGIN 
      s = q-> getPlotData(U,V,Lon,Lat)
      uu0 = U
      vv0 = V
      fac =  1.
      s = q-> get(lonpar = londims, latpar=latdims)
      ddims = [ [londims],[latdims]]

      xfinc =  ddims(2,0)
      yfinc =  ddims(2,1)
      xf0   =  ddims(0,0) &  xf1= ddims(1,0)
      yf0   =  ddims(0,1) &  yf1= ddims(1,1)
      nxf    =  (xf1-xf0)/xfinc+1
      nyf    =  (yf1-yf0)/yfinc+1

    ENDIF ELSE BEGIN 
      Message,'Error initializing Object qmodel with file ' + files[0],/cont
      read_success =  0
    ENDELSE 

  ENDIF ELSE BEGIN

    ; More than one file, Read the first to get the dimensionality.

    q = Obj_New('qmodel',file=files[0])
    IF Obj_Isa(q, 'qmodel') THEN BEGIN 
      s = q-> getPlotData(U,V,Lon,Lat)
      s = q-> get(lonpar = londims, latpar =latdims)
      Obj_Destroy,q
      ddims = [ [londims],[latdims]]

      xfinc =  ddims(2,0)
      yfinc =  ddims(2,1)
      xf0   =  ddims(0,0) &  xf1= ddims(1,0)
      yf0   =  ddims(0,1) &  yf1= ddims(1,1)
      nxf    =  (xf1-xf0)/xfinc+1
      nyf    =  (yf1-yf0)/yfinc+1

    ENDIF ELSE read_success =  0

    uu0 =  fltarr(nxf,nyf, nfiles) &  vv0=uu0 
    uu0[*,*,0] =  U
    vv0[*,*,0] =  V
    file_cntr = 1

    WHILE file_cntr LT nfiles AND read_success EQ 1 DO BEGIN
      q =  Obj_New('qmodel',file=files[file_cntr])
      IF Obj_Isa(q, 'qmodel') THEN BEGIN 
        s = q-> get(lonpar = londims, latpar=latdims)
        tdims = [[londims],[latdims]]
        x = where(ddims-tdims,nx)
        IF nx EQ 0 THEN BEGIN 
          s = q-> getPlotData(U,V,Lon,Lat)
          uu0[*,*,file_cntr] = U
          vv0[*,*,file_cntr] = V
          Obj_Destroy,q
        ENDIF ELSE BEGIN 
          read_success = 0
          Message,'All files must have same dimensionality!',/cont
          print,'  First file had dimensions ',ddims
          print,'  File ', strtrim(file_cntr,2),' has dimensions ', tdims
        ENDELSE 
      ENDIF ELSE read_success =  0
      file_cntr = file_cntr+1
    ENDWHILE 
  ENDELSE 
ENDELSE 

IF read_success THEN BEGIN 
  ;
  lonmin =  lonpar[0] &  lonmax= lonpar[1] 
  latmin =  latpar[0] &  latmax= latpar[1] 


  lons = (findgen( nxf )*xfinc + xf0) # (fltarr( nyf ) + 1.)
  lats = (fltarr( nxf ) + 1. )        # (findgen( nyf )*yfinc + yf0)

  ; check to make sure everything is in east longitude and make the
  ; appropriate changes if it isn't 
  t1 =  where( lonpar LT 0, nt1 )
  t2 =  where( vlonpar LT 0, nt2 )
  IF nt1  NE 0  OR nt2 NE 0 THEN BEGIN 
    east_long = 0
    IF nt1 NE 0 THEN BEGIN
      x =  where( lonpar[0:1] GT 180, nx )
      IF nx THEN lonpar[x] =  lonpar[x] -360.
    ENDIF 

    IF nt1 NE 0 THEN BEGIN
      x =  where( vlonpar(0:1) GT 180, nx )
      IF nx THEN vlonpar[x] =  vlonpar[x] -360.
    ENDIF 

    x =  where( lons GT 180, nx )
    IF nx NE 0 THEN lons[x] =  lons[x] - 360.
    IF xf0 EQ 0 AND lonpar[0]*lonpar[1] LT 0 THEN BEGIN 
      ; the requested area crosses the prime meridian and the file is
      ; arranged in  East longitude. So, in order to make the array
      ; indexing argumentation work in the sections below, we have to
      ; rearrange the uu/vv/lons/lats arrays so that they start at -180. 
      ; This section will undoubtably have to be revisited when we
      ; start making interpolation which don't cover the whole globe.
      x = where( lons[*,0] LT 0, nx )
      lons =  shift( lons, nxf - x[0], 0 )
      lats =  shift( lats, nxf - x[0], 0 )
      FOR i=0,nfiles-1 DO BEGIN 
        uu0[*,*,i] =  shift( uu0[*,*,i], nxf-x[0], 0 ) 
        vv0[*,*,i] =  shift( vv0[*,*,i], nxf-x[0], 0 )
      ENDFOR 
      xf0 =  min(lons, max=xf1)
      yf0 =  min(lats, max=yf1)

    ENDIF 
      
  ENDIF 
  ;
  ;
  ;
  ;
  ;This piece does the animation and interpolation stuff
  ;
  xv0 =  vlonpar[0] &  xv1= vlonpar[1] &  xvinc= vlonpar[2]
  yv0 =  vlatpar[0] &  yv1= vlatpar[1] &  yvinc= vlatpar[2]

  nxv =  long( (xv1-xv0)/xvinc)
  nyv =  long( (yv1-yv0)/yvinc)

  ; offset in the underlying wind field array of xv0 and yv0 
  xfoff =  (xv0 - xf0)/xfinc
  yfoff =  (yv0 - yf0)/yfinc

  nn =  nxv*nyv
  dist        = fltarr( nn,4 )   
  weights     = dist               

  long_sel1 = reform( (fltarr(nyv)+1)#((findgen(nxv)*xvinc) +xv0), nn ) 
  lats_sel1 = reform( (findgen(nyv)*yvinc+yv0) # (fltarr(nxv)+1)  ,nn ) 
  long_sel  = long_sel1
  lats_sel  = lats_sel1
  time_mov  = fix( ((nframes-1)/harmonic)*randomu( seed,nn ) )
  ;

  ;
  real_start_time =  systime(1)
  tottime =  0.
  iter =  0l

  ss = min_speed> sqrt( uu0[*,*,0]^2 + vv0[*,*,0]^2 )       < max_speed ; 

  ;
  ; *********************************** Main LOOP *****************************
  ;
  ; First we spin up the animation by iterating over the (
  ; possibly initial, if multiple) map for 60 iterations, then
  ; start laying down the frames.

  uu =  uu0[*,*,0]
  vv =  vv0[*,*,0]


  ; Here's how we handle multiple files.
  ; If there are only two, than take the first frame as equal to the
  ; first file and the last frame as equal to the last and interpolate
  ; linearly between them. If there are more than two files than
  ; divide the number of frames by the number (nfiles-1) = number of
  ; segments. With the frames starting at zero, each frame that is
  ; equivalent to 0 mod nfiles-1 will be a file. 
  ;
  ; An example might help...

  ; An animation of 18 frames over 6 files.
  ; nframes = 18
  ; nfiles = 6
  ; cutoff = 18/(6-1) = 16/5 = 3 frame per segment. 
  ;
  ;   So, every frame divisible by 3
  ;   will be from one of the files
  ;
  ;                                              1
  ; frames         0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5  6  7
  ; file frames    *        *        *        *        *              *
  ;                                                    |--------------|
  ;                                                   more in this segment than
  ;                                                   any of the others.


  cutoff =  0
  IF nfiles GT 1 THEN cutoff =  nframes/(nfiles-1) ; num frames per segment.
  testend =  cutoff*(nfiles-2)
  
  ; We'll have to check whether we're in the last segment and handle
  ; the interpolation accordingly.

  n_in_last_seg =  nframes-testend ; then next to last file will
                                       ; have index (nfiles-2)

  n_runup_frames = nframes/harmonic

  print,' cutoff = ',cutoff
  print,' nfiles = ',nfiles
  print,' testend = ',testend
  print,' n_in_last_seg = ', n_in_last_seg 
  print,' n_runup_frames: ', n_runup_frames
  print,' Harmonic: ', harmonic
    ; used later in multiple file processing
  oi1 = -1
  oi2 = -1

  FOR istep = 0l,nframes + n_runup_frames -1  DO BEGIN
    st =  systime(1);
    CALCVECTORFIELD
    

      ; spin up on the animation using the field in the first file.
      ; Then start cycling the through the interpolated fields after the
      ; spin up.


    IF ( istep-n_runup_frames GE 0L ) THEN BEGIN 
      frm =  istep-n_runup_frames
      ; print,'   frm=',frm
      last_segment =  frm GT cutoff*(nfiles-2) 
      
      ; Okay, we've gone through 'n_runup_frames' iterations on the original map to
      ; spin up the animation to a steady state. Now we start laying
      ; down the frames for the animation.
      IF nfiles GT 1 THEN BEGIN
        ; More than one file! We have to calculate a new field for
        ; each iteration. This means that we have to interpolate
        ; linearly between the files. There are basically two cases,
        ; two files and more than two files. If there are only two
        ; files, then those two become the end framea and we just
        ; interpolate between them. If there are more than two, it
        ; gets a bit more complicated, since now one or more of the
        ; frames, in addition to the end frames, won't be
        ; interpolations but will just be the fields from files
        ; themselves. We have to determine which frames these are and
        ; to which files they correspond.  Frames which are directly
        ; from the files read in will be those for which the following
        ; holds.
        ; 
        ; LET 
        ; frame_num = frame number (zero indexed, so frame_num=0 is
        ;                           first frame) 
        ;    and
        ; nfiles = number of files 
        ;    and 
        ; nframe = number of frames in the animation (=animpar[2] here)
        ; 
        ; THEN the frames which will be directly from the files and not
        ; interpolated are all those for which 
        ;
        ; frame_num/nfiles mod nframes EQ 0. except the last frame
        ; which will be from the last file.  
        IF nfiles EQ 2 THEN BEGIN 
          du =  uu0[*,*,1] - uu0[*,*,0]
          dv =  vv0[*,*,1] - vv0[*,*,0]
          CASE 1 OF 
            frm EQ nframes-1: BEGIN 
              uu =  uu0[*,*,1]
              vv =  vv0[*,*,1]
            END 
            frm EQ 0:  BEGIN 
              uu =  uu0[*,*,0]
              vv =  vv0[*,*,0]
            END
            ELSE : BEGIN 
              IF interpolate THEN BEGIN 
                uu =  uu0[*,*,0] + 1.0*frm*du/nframes
                vv =  vv0[*,*,0] + 1.0*frm*dv/nframes
              ENDIF ELSE BEGIN 
                uu =  (uu0[*,*,1]*frm + uu0[*,*,0]*(nframes-frm))/nframes
                vv =  (vv0[*,*,1]*frm + vv0[*,*,0]*(nframes-frm))/nframes
              ENDELSE 
            END
          ENDCASE 
        ENDIF ELSE BEGIN
          IF last_segment THEN BEGIN 
            print,'      last segment! '
            du =   uu0[*,*,nfiles-1]-uu0[*,*,nfiles-2]
            dv =   vv0[*,*,nfiles-1]-vv0[*,*,nfiles-2]
            IF frm EQ nframes-1 THEN BEGIN 
              uu =  uu0[*,*,nfiles-1]
              vv =  vv0[*,*,nfiles-1]
            ENDIF ELSE BEGIN 
              i1 = nfiles-2
              i2 =  i1+1
              ii =  1.0*frm-testend
              nn =  1.0*n_in_last_seg
              print,'     i1 = ',i1
              print,'     i2 = ',i2 
              print,'     ii = ',ii
              print,'     nn = ',nn
              print,'     ii/nn,(nn-ii)/nn = ',ii/nn,(nn-ii)/nn
              IF interpolate THEN BEGIN 
                uu = du*ii/nn + uu0[*,*,nfiles-2]
                vv = dv*ii/nn + vv0[*,*,nfiles-2]
              ENDIF ELSE BEGIN 
                uu =  (uu0[*,*,i2]*ii + uu0[*,*,i1]*(nn-ii))/nn
                vv =  (vv0[*,*,i2]*ii + vv0[*,*,i1]*(nn-ii))/nn
              ENDELSE 
            ENDELSE 
                        
          ENDIF ELSE BEGIN 
              ; more than 2 files, not last segment.
            CASE 1 OF 
              frm EQ 0:  BEGIN 
                print,' frm=0'
                uu =  uu0[*,*,0]
                vv =  vv0[*,*,0]
              END
              ( frm  MOD cutoff ) EQ 0: BEGIN 
                print,' frm mod cutoff eq 0:, frm/cutoff = ',frm/cutoff
                uu =  uu0[*,*,frm/cutoff]
                vv =  vv0[*,*,frm/cutoff]
              END 
              ELSE : BEGIN 
                i1   = frm/cutoff 
                i2   = i1+1
                print,'  NOT last segment, i1 = ',frm/cutoff
                print,'                    i2 = ',i2

                frm2 = frm MOD cutoff
                print,'                  frm2 = ',frm2

                IF interpolate THEN BEGIN 
                  IF oi1 NE i1 OR oi2 NE i2 THEN BEGIN 
                    du =  uu0[*,*,i2]-uu0[*,*,i1]
                    dv =  vv0[*,*,i2]-vv0[*,*,i1]
                  ENDIF 
                  uu = du*frm2/cutoff + uu0[*,*,i1]
                  vv = dv*frm2/cutoff + vv0[*,*,i1]
                ENDIF ELSE BEGIN 
                  print,'frm2/cutoff, (cutoff-frm2)/cutoff = ',$
                   1.0*frm2/cutoff,1.0*(cutoff-frm2)/cutoff
                  uu =  (uu0[*,*,i2]*frm2 + uu0[*,*,i1]*(cutoff-frm2))/cutoff
                  vv =  (vv0[*,*,i2]*frm2 + vv0[*,*,i1]*(cutoff-frm2))/cutoff
                ENDELSE 
                oi1 = i1
                oi2 = i2
              END
            ENDCASE 
          ENDELSE 
        ENDELSE 
        CALCVECTORFIELD
          ss =  min_speed > sqrt( uu^2 + vv^2 )<  max_speed ; 

      ENDIF 

      IF first THEN BEGIN 
        ; this is the first frame for the animate, so set up the
        ; contour, land and land elevation image masks.
        first =  0l
        continent_im =  bytarr(ap[0], ap[1]) + 255b
                

        loncent =  (lonpar[1]+lonpar[0])/2.
        latcent =  0.
        map_set,latcent,loncent,/noborder, $
           limit = [ latpar[0], lonpar[0], latpar[1], lonpar[1] ]

        tt = where( lons ge lonmin and lons le lonmax and $ ; take out the '5's
                    lats ge latmin and lats le latmax, ntt ) 
        unpack_where, ss, tt, c,r 
        c = minmax(c) &  r=minmax(r)
        ; !x/y.window gives the coordinates (in normal coordinates) of
        ; the plot area. Use convert_coord to convert these to device
        ; coords, fudge inward 3 row/colums and find those point in
        ; contour_im that are inside the 'fudge' border which are
        ; non-zero. There will be the points that the previouse call
        ; to contour set down and hence the ones for which we need to
        ; determine the land/water value.
        o = convert_coord( !x.window, !y.window,/norm,/to_dev )
        xw = transpose( o[0,*] ) & yw = transpose( o[1,*] )  
        ; Fudge factor
        xw = [ ceil(xw[0] )+3, floor( xw[1] )-3 ] ; +/- 3
        yw = [ ceil(yw[0] )+3, floor( yw[1] )-3 ] ; +/- 3


        ; Find where the latitude and longitudes are within the
        ; requested plot range, then lay down a 'countour ' field
        ; consisting of only one color. This will enable us to read it
        ; from the screen, find which pixels in the window we want
        ; land/water info about (i.e. the non-black ones), convert
        ; those to data coordinates, send the data coords to land_mask
        ; to get the land/water mask value.

        contour,ss( c[0]:c[1], r[0]:r[1])*0b + !d.n_colors-1,$
         lons(c[0]:c[1], r[0]:r[1]),$
         lats(c[0]:c[1], r[0]:r[1]), $
         level = !d.n_colors-1, c_colors=!d.n_colors-1,$
         /overplot,/cell_fill 

        contour_im =  tvrd() ; Read the screen (buffer?)

;        ; Find all points with color in them
        g = where( contour_im ne 0, ng ) 

;        ; Exclude the 'borders'
        unpack_where, contour_im, g, cc, rr  

        g = where( cc GE xw[0] AND cc LE xw[1] and $
                   rr GE yw[0] AND rr LT yw[1], ng ) 

        cc =  cc(g) &  rr=rr(g)
        g = 0

;        cc =  indgen( xw[1]-xw[0]+1 )+xw[0]
;        rr =  indgen( yw[1]-yw[0]+1 )+yw[0]

        ; Convert those pixels back to data coords
        o =  convert_coord( cc, rr, /dev, /to_data )

        lon =  transpose( o(0,*) )
        lat = transpose( o(1,*) )
        ; There may still be some bad ones, eliminate them here.
        g = where( lon LE 360. AND lat LE 90., ng )
        IF ng NE 0 THEN BEGIN 
          cc = cc(g) 
          rr = rr(g)
          lon =  lon(g)
          lat =  lat(g)
        ENDIF ELSE BEGIN 
          stop,' No good lon/lats'
        ENDELSE 

         ; Handle east/west longitude.
        IF east_long THEN BEGIN  
          t =  where( lon LT 0, nt )
          IF nt NE 0 THEN lon(t) =  lon(t) + 360.
        ENDIF ELSE BEGIN
          x =  where( lon GT 180, nx )
          IF nx NE 0 THEN lon(x) =  lon(x) - 360.
        ENDELSE 

        lonmin1 =  min( lon, max=lonmax1 )
        latmin1 =  min( lat, max=latmax1 )

        ; extend the extrema 1/2 of the vector grid increment out and
        ; use this extent to chose the vectors to plot.
        lonmin1 =  lonmin1 - vlonpar(2)/2.
        lonmax1 =  lonmax1 + vlonpar(2)/2.

        latmin1 =  latmin1 - vlatpar(2)/2.
        latmax1 =  latmax1 - vlatpar(2)/2.

        IF east_long AND (lonmin1 LT 0) THEN lonmin1 =  lonmin1 + 360.
        IF east_long AND (lonmax1 LT 0) THEN lonmax1 =  lonmax1 + 360.


        mask =  long( lat*0l )
        t1 =  systime(1)
        ; Call the linkimage routine land_mask.sl 
        ; to find land/water flags
        LAND_MASK, lon, lat, mask
        t2 =  systime(1)
        print,' land mask took ' , t2-t1 ,' seconds '
        land =  where( mask EQ 1, nland )



        ; Set land values to 0
        IF nland GT 0 THEN BEGIN 
          continent_im( cc(land), rr(land) ) =  0b

            ; make 2nd array for later machinations. switch values so that
            ; it's zero on water and 255 in land.
          continent_im2 =  continent_im XOR 255b

            ; convert the cc(land), rr(land) to data coords
          o =  convert_coord( cc(land), rr(land) ,/dev, /to_data )
          tlandx =  o(0,*) &  tlandy= o(1,*) &  o=0;
            ; tlandx/y are the data coordinates of all the points in the
            ; continent_im array that are 'land'. 

          xx =  where( tlandx LT 0, nxx )
          IF nxx NE 0 THEN tlandx(xx) =  tlandx(xx) + 360.
          ix =  round(tlandx*12.)
          iy =  round((tlandy+90.)*12.)
          continent_im2( cc(land), rr(land) ) =  (landel( ix, iy ) +31) < 50
        ENDIF ELSE $
          continent_im2 =  continent_im XOR 255b

          ; put the wind speed contour 
        tt = bytscl(min_speed >  ss( min(c):max(c),min(r):max(r)) < max_speed, $
                    min=min_speed, max=max_speed, top=windEnd)
        contour,tt,lons( c[0]:c[1],r[0]:r[1])     ,$
           lats( c[0]:c[1],r[0]:r[1]),$
             level = findgen( nwindcolors ),$
               c_colors=bindgen(nwindcolors)+windStart,$
                 /overplot,/cell_fill 
        contour_im =  tvrd()

      ENDIF  ; come from 'if first'

      IF frm GT 0 THEN BEGIN 
        IF nfiles GT 1 THEN BEGIN 
          tt = bytscl(min_speed >  ss( c[0]:c[1],r[0]:r[1]) < max_speed, $
                      min=min_speed, max=max_speed, top=windEnd)
            Contour,tt,$
             lons( c[0]:c[1],r[0]:r[1])     ,$
              lats( c[0]:c[1],r[0]:r[1])     ,$
                level = findgen(nwindcolors),$
                   c_colors=bindgen(nwindcolors)+windStart,$
                     /overplot,/cell_fill 
        ENDIF ELSE tv,contour_im
      ENDIF 

      xx =  where( long_sel GE lonmin1 AND long_sel LT lonmax1 AND $
                   lats_sel GE latmin1 AND lats_sel LT latmax1, nxx )

      IF nxx NE 0 THEN BEGIN 
        ; plot only those points in the plot window
        tuu_sel   =  uu_sel[xx]
        tvv_sel   =  vv_sel[xx]
        tlong_sel =  long_sel[xx]
        tlats_sel =  lats_sel[xx]
        PLOTVECT, tuu_sel, tvv_sel, tlong_sel, tlats_sel, $
         length=length, color=!d.n_colors-1,/scale, thick=thick
      ENDIF 

        ; Read the screen, 'and' it with the 'continent image' array and
        ; add the continent_im2 array
      im2 =  tvrd()
      im =  ( tvrd() AND continent_im ) + continent_im2
      tv,im
      IF keyword_set( save_first ) THEN $
       save,ss,c,r,lons,lats,contour_im,continent_im,continent_im2,im,im2,$
       tuu_sel,tvv_sel,tlong_sel,tlats_sel,red,green,blue,$
       lonpar,latpar,vlonpar,vlatpar,loncent,latcent,file='first_frame.save'

;      print,' Working on Frame # ', frm
      IF frm LT 0 THEN BEGIN
        stop,' frm < 0'
      ENDIF 
        ; Need to reread the screen since we put a grid and labels on
        ; it 
      im =  tvrd()
      IF dologo THEN BEGIN 
        IF setup_logo THEN BEGIN 
            ; Add 40 pixels to the top of the
            ; image and put on the logo, colorbar
            ; and any annotation the user may have passed in through the
            ; 'title' keyword.
          sz =  size(im)
          ttmp =  bytarr( sz[1], sz[2] + 40 )
            ; NB animpar[1]=ap[1]+40
          device, set_resolution=[sz[1],sz[2]+40]
          tv,ttmp
            ; Now we know that the top 40 pixels
            ; are blank, so figure out the normal coordinates of these
            ; pixels and use those coordidnates.
          x =  [0,0]
          y =  [sz[2], sz[2]]
          coords =  convert_coord( x,y, /devi, /to_normal )
            ; We'll arrange the annotation in halves, title in top
            ; half, colorbar and nasa/jpl logo in bottom

          y =  coords(1,*)
          y1 =  y[0] &  y2= y1 + (1-y[0])/4.


          ; Set up Title/min/max speed for color bar.
          cbtitle = 'Wind Speed (m/s)'
          mnspeed = min_speed
          mxspeed = max_speed

          IF knots THEN BEGIN 
            cbtitle = 'Wind Speed (knots)'
            mnspeed = min_speed*mps2knots
            mxspeed = max_speed*mps2knots
          ENDIF 

          COLBAR,bottom=1,ncolors=ncolors,$
            position=[0.25,y1,0.75,y2],$
              Title=cbtitle,division=4,$
                min=mnspeed, max=mxspeed, $
                  charsize=0.65,format='(F4.0)'

            ; Set up nasa/jpl logo
          y1 =  y[0]  ; + (1-y[0])/6.
          xyouts, 0.17, y1, '!17NASA!X', /normal, align=1.0,size=0.0
          xyouts, 0.85, y1, '!17JPL!X', /normal, align=0.0,size=0.9
          y1 =  y[0] + 3*(1-y[0])/4.
          xyouts, 0.5, y1,'!17' + title + '!X',/normal,align=0.5, size=0.9

         logo_colorbar_im =  tvrd()

           ; re-establish old coordinate system so that subsequent
           ; iterations won't mess up.
         device, set_resolution=[ap[0],ap[1]]

           ; We have to re-establish the coordinate system, since the
           ; COLBAR procedure destroy's it by it's call to 'PLOT' so
           ; another call to map_set.

         MAP_SET,latcent,loncent,/noborder, $
          limit = [ latpar[0], lonpar[0], latpar[1], lonpar[1] ]

         setup_logo = 0; 

        ENDIF; come from if (setup_logo)


          ; Now load the image into the augemented image array.
        ttmp =  logo_colorbar_im*0b
        sz =  size(im)
        ttmp( 0:sz[1]-1, 0:sz[2]-1 ) =  im
        im =  ttmp OR logo_colorbar_im
      ENDIF 
      sz =  size(im)
      IF padframe THEN BEGIN 
        outim =  bytarr( sz[1]+2*pad[0], sz[2]+2*pad[1] )
        outim( pad[0]:pad[0]+sz[1]-1, pad[1]:pad[1]+sz[2]-1) =  im
      ENDIF ELSE outim =  im

      frame = PadAndJustify(frm+1,3,pad='0',/right)
      IF gif THEN BEGIN 
        gfile =  'gwind.' + frame  
        write_gif, gfile, outim, red,green,blue
      ENDIF 
      IF pict THEN BEGIN 
        pfile =  'pwind.' + frame  
        write_pict, pfile, outim, red,green,blue
      ENDIF 
      IF ps THEN BEGIN 
        pfile =  'pswind.' + frame  
        ps, file=pfile,/land
        DEVICE,/color,bits_per_pixel=8
        tv, outim, xsiz=640/0.02, ysiz=480/0.02
        device,/close
        set_plot,'z'
      ENDIF 
      IF jpeg THEN BEGIN 
        im = [ [[red[outim]]], [[green[outim]]],[[blue[outim]]] ]
        outim = 0
        jfile = 'jwind.' + frame
        write_jpeg,jfile,im,quality=100,true=3
        im = 0
      ENDIF 
      IF tiff THEN BEGIN
          ; Convert from 'bottom to top' 
          ; ordering to 'top to bottom', just to be sure.
        outim = reverse(outim,2)
        IF frame EQ '001' THEN BEGIN 
          rr =(gg=(bb=bytarr(256)))
          nn = n_elements(red)
          rr[0:nn-1] =  red
          rr[255] = 255b
          gg[0:nn-1] =  green
          gg[255] = 255b
          bb[0:nn-1] =  blue
          bb[255] = 255b
        ENDIF 
        tfile =  'twind.' + frame
        tiff_write, tfile, outim, 1, $
         red= rr, green= gg, blue=bb,$
         xresol=600,yresol=600
      ENDIF 

    ENDIF ; if istep-n_runup_frames ge 0
    ;

    ; Now, calculate the next location in each orbit by adding the u componenet
    ; to the x location (long_sel) AND the v component to the y location
    ; (lats_sel). The configurable quantity path_inc is used to scale
    ; the motion. (see path_inc keyword)


    long_sel = long_sel + path_inc*uu_sel
    lats_sel = lats_sel + path_inc*vv_sel
    time_mov = time_mov+1


    x =  where( time_mov GE nframes/harmonic, nx )
    IF nx NE 0 THEN BEGIN
      long_sel(x) =  long_sel1(x)
      lats_sel(x) =  lats_sel1(x)
      time_mov(x) =  0
    ENDIF 
    ;
    ;
    IF iter GT n_runup_frames THEN begin
      et =  systime(1)
      time =  et-st
;      print, ' Elapsed time for iteration ',iter, ' is ', time, ' seconds '
      tottime =  tottime+time
    ENDIF 
    iter =  iter+1

  ENDFOR ; loop over animation frames
  A =  tottime/(iter-n_runup_frames)
  real_tot_time = et- real_start_time 
  print,' ***** Total time for interior iterations is ', tottime
  print,' ***** Average time for each iteration is ', a
  print,' ***** Total time for whole process is  ', real_tot_time

  ;
ENDIF ELSE BEGIN
  message," Can't open files for reading ",/cont
  return
ENDELSE 
GOTO, good_end

STOP:
  print, "Some I/O Error has occurred!"
  print,!err_string

good_end:
genv,/restore

RETURN
END
