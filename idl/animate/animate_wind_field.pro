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
;                     contour  and wind speed color bar.
;          max_speed - maximum speed to be used in the wind speed
;                     contour  and wind speed color bar.
;          nmc     - if set, the file is an nmc file and has one
;                    row of data info (obselete, don't use)
;          debug - useful for debugging. If set, the routine will stop
;                  in the context of the error, so that you can
;                  investigate the problem, instead of immediately
;                  returning to the main level.
;          write_gif - writes a gif file for each frame (default)
;          write_pict - write pict_file
;          write_ps - write postscript file
;          write_tiff - writes out a tif file.
;          save_first - will save the first set of images in an IDL
;                       save set 'first_frame.save'. Useful for
;                       debugging purposes.
;          title  - title string to put on the top of the frame
;          noxinter - obselete, does nothing
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
; OUTPUTS: All output is to the files specified in the 'write_xxxx'
;          keyword, default=write_gif
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
;    COLORBAR - puts a color bar on each frame
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
;  /write_gif               ; write it out as a series of gif files.
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
;  /write_gif               ; write it out as a series of gif files.
;
; Same as above, but here we're passing the data in via the
;  ui/vi/ddims keywords.
;
; MODIFICATION HISTORY:
;
; $Log$
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
                        lonpar = lonpar, $  ; longitude dims of output animation 
                                   ; [ start_lon, stop_lon ]
                                   ; (def=[0.,359]
                        latpar =  latpar, $  ; latitude dims of output animation
                                   ; [ start_lat, stop_lat]
                                   ; (def=[-60.,60])
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
                        nmc     =  nmc, $         ; flag for nmc files
                        write_gif =  write_gif, $ ; flag to write gif file 
                                                  ; (the default)
                        write_pict= write_pict ,$  ; flag to write pict file
                        write_ps= write_ps,$      ; flag to write postscript file
                        write_tiff= write_tiff,$      ; flag to write tiff
                                                      ; file
                        debug = debug       ,$  ; for debugging
                        title=title ,$          ; Title for each frame.
                        min_speed=min_speed,$  ; Minimum wind speed
                        max_speed=max_speed,$  ; Maximum wind speed
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
                        noxinter= noxinter ; obsolete, does nothing, 
                                ; it's just here so 
                                ; a lot of other software doesn't
                                ; break.


COMMON prs, long_sel, lats_sel, lons, lats, uu, vv, uu_sel, vv_sel, $
           ileft, iright, itop, ibot, dist_left, dist_right, dist_top, $
           dist_bot, dist, weights, invdist, xfinc, xf0, yfinc, yf0, eps

COMMON colors, r_curr, g_curr, b_curr, r_orig, g_orig, b_orig


rcsid = "$Id$"

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
  '  nmc     =  nmc, $         ; flag for nmc files' + lf + $
  '  write_gif =  write_gif, $ ; flag to write gif file' + lf + $
  '  write_pict= write_pict ,$  ; flag to write pict file' + lf + $
  '  write_ps= write_ps,$      ; flag to write postscript file' + lf + $
  '  write_tiff= write_tiff,$      ; flag to write postscript file' + lf + $
  '  debug = debug       ,$  ; for debugging' + lf + $
  '  title=title ,$          ; unimplemented, for future use' + lf + $
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

interpolate   = KEYWORD_SET( interpolate )
dologo        = (KEYWORD_SET( nologo ) EQ 0)
write_pict    = KEYWORD_SET( write_pict )  
write_gif     = KEYWORD_SET( write_gif )  
write_ps      = KEYWORD_SET( write_ps )   
write_tiff    = KEYWORD_SET( write_tiff )  
IF NOT write_pict AND $
   NOT write_gif AND $
   NOT write_ps AND $
   NOT write_tiff THEN write_gif =  1

mps2knots =  1.944 ; converts meters/sec to knots.

IF n_elements( min_speed ) EQ 0 THEN min_speed =  1 ; meters/sec
IF n_elements( max_speed ) EQ 0 THEN max_speed =  40/mps2knots ; meters/sec
IF n_elements( length ) EQ 0 THEN length =  3;
IF n_elements( title ) EQ 0 THEN title =  '' ; meters/sec
ncolors = 29

CASE 1 OF
  write_gif: message,' Files will be output in GIF format',/cont
  write_pict: message,' Files will be output in PICT format',/cont
  write_ps: message,' Files will be output in PS format',/cont
  write_tiff: message,' Files will be output in TIFF format',/cont
  ELSE : BEGIN
    message,' One file format MUST be selected',/cont
    return
  END  
ENDCASE 

  ; Check for requests to pad the frames
padframe =  keyword_set( pad ) OR $
            keyword_set(tvsafe) OR $
            keyword_set(titsafe)


eps =  0.000102
east_long = 1   ; default to east long, unless some input 
                ; quantity is in west longitude
setup_logo =  1 ; Flag for logo setup code

ON_IOERROR, stop
  ; set path increment. This is the factor applied to the u/v vector
  ; components to get the location of the vector in the next frame of
  ; the animation.

IF NOT(keyword_set(path_inc) ) THEN path_inc =  0.04 
nmcflag =  keyword_set( nmc )
IF n_elements( ddims ) EQ 0 THEN BEGIN
    ddims =  [ [ 0., 359., 1] , $
               [ -60., 60, 1] ]
  Message,' Taking default dimensions: 360 x 121 x 1deg grid ',/cont
ENDIF 

IF n_elements( lonpar ) EQ 0 THEN BEGIN
  lonpar =  [0.,359]
  message,' Taking default lonpar = [0., 359. ]',/cont
ENDIF 

IF n_elements( latpar ) EQ 0 THEN BEGIN 
  latpar =  [-60., 60 ]
  message,' Taking default latpar = [-60., 60]',/cont
ENDIF 

IF NOT( keyword_set( vlonpar ) ) THEN BEGIN 
  
  vlonpar =  [0, 360., 1. ]
  vlonpar =  [0. >  lonpar(0), 360 <  lonpar(1), 1. ]
  message,' Taking default vlonpar ',/cont
  print,vlonpar
ENDIF 

IF NOT( keyword_set( vlatpar ) ) THEN BEGIN 
  vlatpar =  [-90., 90, 1. ]
  vlatpar =  [-90. >  latpar(0), 90 <  latpar(1), 1. ]
  message,' Taking default vlatpar', /cont
  print,vlatpar
ENDIF 
IF NOT( keyword_set( animpar ) ) THEN BEGIN 
  animpar =  [640, 512, 60 ]
  message,' Taking default animpar = [640,512, 60 ] ',/cont
ENDIF 

IF keyword_set( tvsafe ) THEN $
 pad = [ round(animpar(0)*0.09375/2.),round(animpar(1)*0.0916667/2.) ]
IF keyword_set( titsafe) THEN $
 pad =  [ round( animpar(0)*0.184375/2.),round(animpar(1)*0.184/2.) ]


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



IF exist(UI) AND exist(VI) THEN BEGIN 
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
ENDIF 

  ;
first = 1
  ;
  ; Define the color table. On rainy, $NSCAT-VAP = /nscat-vap
  ;
red =  bytarr(51) &  green=red &  blue=red
openr,1,'$VAP_ROOT/animate/nscat-vap-animation.ct2', error= err
readu,1,red,green,blue
close,1
r_curr =  red &  g_curr= green &  b_curr= blue
r_orig =  red &  g_orig= green &  b_orid= blue

;color_bar =  bytarr( 10,30, 15  ) 
;FOR i=0,29 DO color_bar(*,i,*) =  i
;color_bar =  reform( color_bar, 300, 15 )



  ; initialize window /buffer size for 
  ; output file options.
set_plot,'z'
ap = animpar
  ; subtract 40 from animpar(1) (the y size). this'll get added back
  ; in when we put the logo on.
IF dologo THEN ap(1) = ap(1)-40
IF padframe THEN ap(0:1) =  ap(0:1) - 2*pad
device,set_resolution=[ap(0),ap(1)]
tvlct,red,green,blue

  ; Read in the land elevation file;
  ; Find the subarray which applies for this run and extract it.
openr,1,'$VAP_ROOT/animate/land_elevations.bin'
landel =  intarr( 12*360, 12*180 + 1 )
readu,1, landel
close,1


IF n_elements(UI) NE 0 AND n_elements(VI) NE 0 THEN BEGIN 
  read_success = 1
  uu0 = temporary(ui)
  vv0 = temporary(vi)
ENDIF ELSE BEGIN 
  read_success =  1
  nfiles =n_elements( files )  

  IF nfiles EQ 1 THEN BEGIN 
    openr, rlun, files(0),/get_lun, error= err
    IF err EQ  0 THEN BEGIN 
      IF nmcflag THEN BEGIN 
          ; The NMC file has one row with data information in it.
        b = intarr( nxf )
        readu,rlun,b
        yr = b( 0 ) & mo = b( 1 ) & day = b( 2 ) & hr = b( 3 )
        syr = strtrim( string( yr ),2 )
        smo = strtrim( string( mo ),2 )
        sday = strtrim( string( day ),2 )
        shr = strtrim( hr, 2 ) +'Z'
        fac =  0.01
        uu0 =  intarr( nxf, nyf ) &  vv0 = uu0
      ENDIF ELSE BEGIN 
        uu0=fltarr( nxf,nyf ) & vv0 = uu0
        fac =  1.
      ENDELSE 
      readu,rlun,uu0
      readu,rlun,vv0
      free_lun, rlun
      uu0 =  uu0*fac
      vv0 =  vv0*fac
    ENDIF ELSE read_success =  0
  ENDIF ELSE BEGIN
    uu0 =  fltarr(nxf,nyf, nfiles) &  vv0=uu0 
    FOR i=0, nfiles-1 DO BEGIN
      openr,rlun, files(i), /get_lun, error= err
      IF err EQ 0 THEN BEGIN
        IF nmcflag THEN BEGIN 
            ; The NMC file has one row with data information in it.
          b = intarr( nxf )
          readu,rlun,b
          yr = b( 0 ) & mo = b( 1 ) & day = b( 2 ) & hr = b( 3 )
          syr = strtrim( string( yr ),2 )
          smo = strtrim( string( mo ),2 )
          sday = strtrim( string( day ),2 )
          shr = strtrim( hr, 2 ) +'Z'
          fac =  0.01
          tuu =  intarr( nxf, nyf ) &  tvv = tuu
        ENDIF ELSE BEGIN 
          tuu=fltarr( nxf,nyf ) & tvv = tuu
          fac =  1.
        ENDELSE 
        readu,rlun,tuu, tvv
        free_lun, rlun
        x =  where(abs(tuu) LT 0.5, nxx )
        IF nxx NE 0 THEN tuu(x) =  0.
        x =  where(abs(tvv) LT 0.5, nxx )
        IF nxx NE 0 THEN tvv(x) =  0.
        uu0(*,*,i) =  tuu*fac
        vv0(*,*,i) =  tvv*fac
      ENDIF ELSE BEGIN 
        errors =  [errors, !err_string]
        read_success =  0
      ENDELSE 
    ENDFOR 
  ENDELSE 
ENDELSE 

IF read_success THEN BEGIN 
  ;
  lonmin =  lonpar(0) &  lonmax= lonpar(1) 
  latmin =  latpar(0) &  latmax= latpar(1) 


  lons = (findgen( nxf )*xfinc + xf0) # (fltarr( nyf ) + 1.)
  lats = (fltarr( nxf ) + 1. )        # (findgen( nyf )*yfinc + yf0)

  ; check to make sure everything is in east longitude and make the
  ; appropriate changes if it isn't 
  t1 =  where( lonpar LT 0, nt1 )
  t2 =  where( vlonpar LT 0, nt2 )
  IF nt1  NE 0  OR nt2 NE 0 THEN BEGIN 
    east_long = 0
    IF nt1 NE 0 THEN BEGIN
      x =  where( lonpar(0:1) GT 180, nx )
      IF nx THEN lonpar(x) =  lonpar(x) -360.
    ENDIF 

    IF nt1 NE 0 THEN BEGIN
      x =  where( vlonpar(0:1) GT 180, nx )
      IF nx THEN vlonpar(x) =  vlonpar(x) -360.
    ENDIF 

    x =  where( lons GT 180, nx )
    IF nx NE 0 THEN lons(x) =  lons(x) - 360.
    IF xf0 EQ 0 AND lonpar(0)*lonpar(1) LT 0 THEN BEGIN 
      ; the requested area crosses the prime meridian and the file is
      ; arranged in  East longitude. So, in order to make the array
      ; indexing argumentation work in the sections below, we have to
      ; rearrange the uu/vv/lons/lats arrays so that they start at -180. 
      ; This section will undoubtably have to be revisited when we
      ; start making interpolation which don't cover the whole globe.
      x = where( lons(*,0) LT 0, nx )
      lons =  shift( lons, nxf- x(0), 0 )
      lats =  shift( lats, nxf-x(0), 0 )
      FOR i=0,nfiles-1 DO BEGIN 
        uu0(*,*,i) =  shift( uu0(*,*,i), nxf-x(0), 0 ) 
        vv0(*,*,i) =  shift( vv0(*,*,i), nxf-x(0), 0 )
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
  xv0 =  vlonpar(0) &  xv1= vlonpar(1) &  xvinc= vlonpar(2)
  yv0 =  vlatpar(0) &  yv1= vlatpar(1) &  yvinc= vlatpar(2)

  nxv =  fix( (xv1-xv0)/xvinc)
  nyv =  fix( (yv1-yv0)/yvinc)

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
  nframes =  animpar(2) ; number of frames in the animation.
  time_mov  = fix( (nframes-1)*randomu( seed,nn ) )
  ;

  ;
  real_start_time =  systime(1)
  tottime =  0.
  iter =  0l
  ss =  min_speed >  sqrt( uu0(*,*,0)^2 + vv0(*,*,0)^2 ) <  max_speed ; 
  ;
  ; *********************************** Main LOOP *****************************
  ;
  ; First we spin up the animation by iterating over the (
  ; possibly initial, if multiple) map for 60 iterations, then
  ; start laying down the frames.

  uu =  uu0(*,*,0)
  vv =  vv0(*,*,0)


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


  print,' cutoff = ',cutoff
  print,' nfiles = ',nfiles
  print,' testend = ',testend
  print,' n_in_last_seg = ', n_in_last_seg 

    ; used later in multiple file processing
  oi1 = -1
  oi2 = -1

  FOR istep = 0l,nframes + 60 -1  DO BEGIN
    st =  systime(1);
    CALCVECTORFIELD
    

      ; spin up on the animation using the field in the first file.
      ; Then start cycling the through the interpolated fields after the
      ; spin up.


    IF ( istep-60 GE 0L ) THEN BEGIN 
      frm =  istep-60
      print,'   frm=',frm
      last_segment =  frm GT cutoff*(nfiles-2) 
      
      ; Okay, we've gone through 60 iterations on the original map to
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
        ; nframe = number of frames in the animation (=animpar(2) here)
        ; 
        ; THEN the frames which will be directly from the files and not
        ; interpolated are all those for which 
        ;
        ; frame_num/nfiles mod nframes EQ 0. except the last frame
        ; which will be from the last file.  
        IF nfiles EQ 2 THEN BEGIN 
          du =  uu0(*,*,1) - uu0(*,*,0)
          dv =  vv0(*,*,1) - vv0(*,*,0)
          CASE 1 OF 
            frm EQ nframes-1: BEGIN 
              uu =  uu0(*,*,1)
              vv =  vv0(*,*,1)
            END 
            frm EQ 0:  BEGIN 
              uu =  uu0(*,*,0)
              vv =  vv0(*,*,0)
            END
            ELSE : BEGIN 
              IF interpolate THEN BEGIN 
                uu =  uu0(*,*,0) + 1.0*frm*du/nframes
                vv =  vv0(*,*,0) + 1.0*frm*dv/nframes
              ENDIF ELSE BEGIN 
                uu =  (uu0(*,*,1)*frm + uu0(*,*,0)*(nframes-frm))/nframes
                vv =  (vv0(*,*,1)*frm + vv0(*,*,0)*(nframes-frm))/nframes
              ENDELSE 
            END
          ENDCASE 
        ENDIF ELSE BEGIN
          IF last_segment THEN BEGIN 
            print,'      last segment! '
            du =   uu0(*,*,nfiles-1)-uu0(*,*,nfiles-2)
            dv =   vv0(*,*,nfiles-1)-vv0(*,*,nfiles-2)
            IF frm EQ nframes-1 THEN BEGIN 
              uu =  uu0(*,*,nfiles-1)
              vv =  vv0(*,*,nfiles-1)
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
                uu = du*ii/nn + uu0(*,*,nfiles-2)
                vv = dv*ii/nn + vv0(*,*,nfiles-2)
              ENDIF ELSE BEGIN 
                uu =  (uu0(*,*,i2)*ii + uu0(*,*,i1)*(nn-ii))/nn
                vv =  (vv0(*,*,i2)*ii + vv0(*,*,i1)*(nn-ii))/nn
              ENDELSE 
            ENDELSE 
                        
          ENDIF ELSE BEGIN 
              ; more than 2 files, not last segment.
            CASE 1 OF 
              frm EQ 0:  BEGIN 
                print,' frm=0'
                uu =  uu0(*,*,0)
                vv =  vv0(*,*,0)
              END
              ( frm  MOD cutoff ) EQ 0: BEGIN 
                print,' frm mod cutoff eq 0:, frm/cutoff = ',frm/cutoff
                uu =  uu0(*,*,frm/cutoff)
                vv =  vv0(*,*,frm/cutoff)
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
                    du =  uu0(*,*,i2)-uu0(*,*,i1)
                    dv =  vv0(*,*,i2)-vv0(*,*,i1)
                  ENDIF 
                  uu = du*frm2/cutoff + uu0(*,*,i1)
                  vv = dv*frm2/cutoff + vv0(*,*,i1)
                ENDIF ELSE BEGIN 
                  print,'frm2/cutoff, (cutoff-frm2)/cutoff = ',$
                   1.0*frm2/cutoff,1.0*(cutoff-frm2)/cutoff
                  uu =  (uu0(*,*,i2)*frm2 + uu0(*,*,i1)*(cutoff-frm2))/cutoff
                  vv =  (vv0(*,*,i2)*frm2 + vv0(*,*,i1)*(cutoff-frm2))/cutoff
                ENDELSE 
                oi1 = i1
                oi2 = i2
              END
            ENDCASE 
          ENDELSE 
        ENDELSE 
        CALCVECTORFIELD
        ss =  min_speed >  sqrt( uu^2 + vv^2 ) <  max_speed ; 

      ENDIF 

      IF first THEN BEGIN 
        ; this is the first frame for the animate, so set up the
        ; contour, land and land elevation image masks.
        first =  0l
        continent_im =  bytarr(ap(0), ap(1)) + 255b
                

        loncent =  (lonpar(1)+lonpar(0))/2.
        latcent =  0.
        map_set,latcent,loncent,/noborder, $
           limit = [ latpar(0), lonpar(0), latpar(1), lonpar(1) ]

        tt = where( lons ge lonmin and lons le lonmax and $ ; take out the '5's
                    lats ge latmin and lats le latmax, ntt ) 
        unpack_where, ss, tt, c,r 

        ; !x/y.window gives the coordinates (in normal coordinates) of
        ; the plot area. Use convert_coord to convert these to device
        ; coords, fudge inward 3 row/colums and find those point in
        ; contour_im that are inside the 'fudge' border which are
        ; non-zero. There will be the points that the previouse call
        ; to contour set down and hence the ones for which we need to
        ; determine the land/water value.
        o = convert_coord( !x.window, !y.window,/norm,/to_dev )
        xw = transpose( o(0,*) ) & yw = transpose( o(1,*) )  
        ; Fudge factor
        xw = [ ceil(xw(0) )+3, floor( xw(1) )-3 ] ; +/- 3
        yw = [ ceil(yw(0) )+3, floor( yw(1) )-3 ] ; +/- 3


        ; Find where the latitude and longitudes are within the
        ; requested plot range, then lay down a 'countour ' field
        ; consisting of only one color. This will enable us to read it
        ; from the screen, find which pixels in the window we want
        ; land/water info about (i.e. the non-black ones), convert
        ; those to data coordinates, send the data coords to land_mask
        ; to get the land/water mask value.

        contour,ss( min(c):max(c), min(r):max(r))*0b + !d.n_colors-1,$
         lons(min(c):max(c), min(r):max(r)),$
         lats(min(c):max(c), min(r):max(r)), $
         level = !d.n_colors-1, c_colors=!d.n_colors-1,$
         /overplot,/cell_fill 

        contour_im =  tvrd() ; Read the screen (buffer?)

;        ; Find all points with color in them
        g = where( contour_im ne 0, ng ) 

;        ; Exclude the 'borders'
        unpack_where, contour_im, g, cc, rr  

        g = where( cc GE xw(0) AND cc LE xw(1) and $
                   rr GE yw(0) AND rr LT yw(1), ng ) 

        cc =  cc(g) &  rr=rr(g)
        g = 0

;        cc =  indgen( xw(1)-xw(0)+1 )+xw(0)
;        rr =  indgen( yw(1)-yw(0)+1 )+yw(0)

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
          x =  where( lons GT 180, nx )
          IF nx NE 0 THEN lons(x) =  lons(x) - 360.
        ENDELSE 

        lonmin1 =  min( lon, max=lonmax1 )
        latmin1 =  min( lat, max=latmax1 )

        ; extend the extrema 1/2 of the vector grid increment out and
        ; use this extent to chose the vectors to plot.
        lonmin1 =  lonmin1 + vlonpar(2)/2.
        lonmax1 =  lonmax1 - vlonpar(2)/2.

        latmin1 =  latmin1 + vlatpar(2)/2.
        latmax1 =  latmax1 - vlatpar(2)/2.

        IF lonmin1 LT 0 THEN lonmin1 =  lonmin1 + 360.
        IF lonmax1 LT 0 THEN lonmax1 =  lonmax1 + 360.


        mask =  long( lat*0l )
        t1 =  systime(1)
        ; Call the linkimage routine land_mask.sl 
        ; to find land/water flags
        LAND_MASK, lon, lat, mask
        t2 =  systime(1)
        print,' land mask took ' , t2-t1 ,' seconds '
        land =  where( mask EQ 1, nland )
        ; Set land values to 0
        IF nland GT 0 THEN continent_im( cc(land), rr(land) ) =  0b

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
        ix =  tlandx*12.
        iy =  (tlandy+90.)*12.
        continent_im2( cc(land), rr(land) ) =  (landel( ix, iy ) +31) < 50

       ; put the wind speed contour 
        contour,min_speed >  ss( min(c):max(c),min(r):max(r)) < max_speed,$
         lons( min(c):max(c),min(r):max(r))     ,$
         lats( min(c):max(c),min(r):max(r))     ,$
         level = findgen( ncolors ),c_colors=bindgen(ncolors),$
         /overplot,/cell_fill 
        contour_im =  tvrd()

      ENDIF  ; come from 'if first'

      IF frm GT 0 THEN BEGIN 
        IF nfiles GT 1 THEN BEGIN 
          contour,min_speed > ss( min(c):max(c),min(r):max(r)) < max_speed,$
           lons( min(c):max(c),min(r):max(r))     ,$
           lats( min(c):max(c),min(r):max(r))     ,$
           level = findgen(ncolors),c_colors=bindgen(ncolors),$
           /overplot,/cell_fill 
        ENDIF ELSE tv,contour_im
      ENDIF 

      xx =  where( long_sel GE lonmin1 AND long_sel LT lonmax1 AND $
                   lats_sel GE latmin1 AND lats_sel LT latmax1, nxx )

      IF nxx NE 0 THEN BEGIN 
        ; plot only those points in the plot window
        tuu_sel   =  uu_sel(xx)
        tvv_sel   =  vv_sel(xx)
        tlong_sel =  long_sel(xx)
        tlats_sel =  lats_sel(xx)
        plotvect, tuu_sel, tvv_sel, tlong_sel, tlats_sel, $
         length=length, color=!d.n_colors-1
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
          ttmp =  bytarr( sz(1), sz(2) + 40 )
            ; NB animpar(1)=ap(1)+40
          device, set_resolution=[sz(1),sz(2)+40]
          tv,ttmp
            ; Now we know that the top 40 pixels
            ; are blank, so figure out the normal coordinates of these
            ; pixels and use those coordidnates.
          x =  [0,0]
          y =  [sz(2), sz(2)]
          coords =  convert_coord( x,y, /devi, /to_normal )
            ; We'll arrange the annotation in halves, title in top
            ; half, colorbar and nasa/jpl logo in bottom

          y =  coords(1,*)
          y1 =  y(0) &  y2= y1 + (1-y(0))/4.
          COLORBAR,bottom=1,ncolors=ncolors,position=[0.25,y1,0.75,y2],$
           Title='Wind Speed (knots)',division=4,min=0,max=max_speed*mps2knots, $
           charsize=0.65,format='(F4.0)'
          y1 =  y(0)  ; + (1-y(0))/6.
          xyouts, 0.17, y1, '!17NASA!X', /normal, align=1.0,size=0.0
          xyouts, 0.85, y1, '!17JPL!X', /normal, align=0.0,size=0.9
          y1 =  y(0) + 3*(1-y(0))/4.
          xyouts, 0.5, y1,'!17' + title + '!X',/normal,align=0.5, size=0.9

         logo_colorbar_im =  tvrd()

           ; re-establish old coordinate system so that subsequent
           ; iterations won't mess up.
         device, set_resolution=[ap(0),ap(1)]

           ; We have to re-establish the coordinate system, since the
           ; COLORBAR procedure destroy's it by it's call to 'PLOT' so
           ; another call to map_set.

         MAP_SET,latcent,loncent,/noborder, $
          limit = [ latpar(0), lonpar(0), latpar(1), lonpar(1) ]

         setup_logo = 0; 

        ENDIF; come from if (setup_logo)


          ; Now load the image into the augemented image array.
        ttmp =  logo_colorbar_im*0b
        sz =  size(im)
        ttmp( 0:sz(1)-1, 0:sz(2)-1 ) =  im
        im =  ttmp OR logo_colorbar_im
      ENDIF 
      sz =  size(im)
      IF padframe THEN BEGIN 
        outim =  bytarr( sz(1)+2*pad(0), sz(2)+2*pad(1) )
        outim( pad(0):pad(0)+sz(1)-1, pad(1):pad(1)+sz(2)-1) =  im
      ENDIF ELSE outim =  im

      tmp =  strtrim( string( frm+1 ), 2 )
      frm =  '000'
      strput, frm, tmp, 3-strlen(tmp)
      IF write_gif THEN BEGIN 
        gfile =  'gwind.' + frm  
        write_gif, gfile, outim, red,green,blue
      ENDIF 
      IF write_pict THEN BEGIN 
        pfile =  'pwind.' + frm  
        write_pict, pfile, outim, red,green,blue
      ENDIF 
      IF write_ps THEN BEGIN 
        pfile =  'pswind.' + frm  
        ps, file=pfile,/land
        DEVICE,/color,bits_per_pixel=8
        tv, outim, xsiz=640/0.02, ysiz=480/0.02
        device,/close
        set_plot,'z'
      ENDIF 
      IF write_tiff THEN BEGIN
        tfile =  'twind.' + frm
        tiff_write, tfile, outim, 1, $
         red= red, green= green, blue=blue,$
         xresol=600,yresol=600
      ENDIF 

    ENDIF ; if istep-60 ge 0
    ;

    ; Now, calculate the next location in each orbit by adding the u componenet
    ; to the x location (long_sel) AND the v component to the y location
    ; (lats_sel). The configurable quantity path_inc is used to scale
    ; the motion. (see path_inc keyword)


    long_sel = long_sel + path_inc*uu_sel
    lats_sel = lats_sel + path_inc*vv_sel
    time_mov = time_mov+1


    x =  where( time_mov GE nframes, nx )
    IF nx NE 0 THEN BEGIN
      long_sel(x) =  long_sel1(x)
      lats_sel(x) =  lats_sel1(x)
      time_mov(x) =  0
    ENDIF 
    ;
    ;
    IF iter GT 60 THEN begin
      et =  systime(1)
      time =  et-st
      print, ' Elapsed time for iteration ',iter, ' is ', time, ' seconds '
      tottime =  tottime+time
    ENDIF 
    iter =  iter+1

  ENDFOR ; loop over animation frames
  A =  tottime/(iter-60.)
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
set_plot,'x'

RETURN
END