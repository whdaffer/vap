;+
; NAME:  GoTN.pro
; $Id$
; PURPOSE:  Goes Overlay ThumbNail
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Goes Image processing (obselete?)
;
; CALLING SEQUENCE:  
; 
; INPUTS:  
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  
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
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-
; $Id$
;
PRO gotn, goesfile= goesfile, $ 
          data= data,$          
          wdata= wdata,$     
          wfiles = wfiles,$         ; (I) vector containing the
                                    ; fully qualified filenames
                                           ; OF windfiles to read 
           minpix    = minpix,$     ; minimum pixel value for
                                    ; clouds. pixesl with
                                    ; values below this  are
                                    ; are set to be land OR water 
           uu        = uu   ,$      ; (I) u value of vectors
           vv        = vv   ,$      ; (I) v value
           llon      = llon ,$      ; (I) long
           llat      = llat ,$      ; (I) lat
           file_str  = file_str ,$  ; (I) string to append to file name
           minspeed  = minspeed ,$
           maxspeed  = maxspeed ,$
           watcolor    = watcolor ,$   ; use only this color index
                                       ; for water (allowable
                                       ; range: 1<watcolor<11)
           windcolor    = windcolor ,$ ; either a color index
                                       ; (int) in which case use only
                                       ; this color index
                                       ; for wind (allowable
                                       ; range: 73<windcolor<97)
                                       ; or a color keyword.
                                       ; available keywords are
                                       ; 'red', 'green','blue','white'
           l2        =  l2    ,$    ; (I) if set, wind data is in 
                                    ; nscat level 2 files
           date_str =  date_str,$   ; (O) return string containing date info
                                    ; as mm/dd/yy.
           debug     = debug ,$     ; 
           limits    = limits       ; (I/O) 4 vector (minlat, minlon,
                                    ; maxlat,maxlon).containing limits as
                                    ; read from read_pcgoes. 
                                    ; NB, this is NOT the same as the
                                    ; keyword of the same name in 
                                    ; GOES_OVERLAY.PRO !!!
                                         
                  
                  


; Calling sequence
; See goes_overlay. This routine does almost the same thing, but it
; produces a thumbnail plot instead of a full sized one. It also does
; the map_image first and then the land masking, to save time.


COMMON goes_overlay_cmn, landel
COMMON colors, r_curr, g_curr, b_curr, r_orig, g_orig, b_orig
debug =  keyword_set(debug)
IF NOT debug THEN on_error, 2 ; return to caller
GENV,/save ; save graphics environment 
nparams =  n_params(0);
l2  =  keyword_set(l2)
PLOTVECT = 0;
IF NOT keyword_set( minspeed ) THEN minspeed =  0
IF NOT keyword_set( maxspeed ) THEN maxspeed =  15
;
; Handle watcolor requests.
;
IF keyword_set( watcolor ) THEN BEGIN
  IF watcolor LT 1 OR watcolor GT 11 THEN BEGIN 
    message,' keyword WATCOLOR must be in range [1, 11] ',/cont
    return
  ENDIF 
ENDIF 

sensors =  [ 'Visible','IR2','IR3','IR4','IR5']
tmp       =  str_sep( goesfile, '/' )
base_name =  tmp(n_elements(tmp)-1)
sensornum =  strmid( base_name, 5,1)
sensor    =  sensors( sensornum-1 )
sat_num =  fix( strmid( base_name, 4,1 ) )
sat_name =  strmid( base_name, 0, 4 ) + ' ' + strmid( base_name, 4, 1 )
IR =  ( sensornum GT 1 )
; check for wind data input .
;

CASE 1 OF 
  keyword_set( wfiles ) : BEGIN
    plotvect =  1
    IF L2 THEN READ_L2_DATA,    wfiles, uu,vv,llon,llat ELSE $
               READ_RMGDR_DATA, wfiles, uu,vv,llon,llat
    IF n_elements( uu ) EQ 0 THEN BEGIN 
      message,' No data found for input rmgdr files ',/cont
      print, wfiles
      print,' Are the filenames fully qualified ?'
      print,' *** Returning *** '
      return
    ENDIF 
    IF nparams GE 3 THEN wdata =  transpose( [[uu],[vv],[llon],[llat]] )

  END 
  keyword_set( vv )   AND keyword_set( uu ) AND $
  keyword_set( llon ) AND keyword_set( llat ) : BEGIN
    plotvect =  1
  END
  ELSE : BEGIN 
    ; if one is set, they all must be!
    IF keyword_set( uu ) THEN BEGIN
      IF NOT keyword_set(vv) THEN $
        message,' If you set one wind keyword, you must set them all '
      IF NOT keyword_set(llon) THEN $
        message,' If you set one wind keyword, you must set them all '
      IF NOT keyword_set(llat) THEN $
        message,' If you set one wind keyword, you must set them all '
    ENDIF 

    IF keyword_set( vv ) THEN BEGIN
      IF NOT keyword_set(uu) THEN $
        message,' If you set one wind keyword, you must set them all '
      IF NOT keyword_set(llon) THEN $
        message,' If you set one wind keyword, you must set them all '
      IF NOT keyword_set(llat) THEN $
        message,' If you set one wind keyword, you must set them all '
    ENDIF     

    IF keyword_set( llon ) THEN BEGIN
      IF NOT keyword_set(vv) THEN $
        message,' If you set one wind keyword, you must set them all '
      IF NOT keyword_set(uu) THEN $
        message,' If you set one wind keyword, you must set them all '
      IF NOT keyword_set(llat) THEN $
        message,' If you set one wind keyword, you must set them all '
    ENDIF 

    IF keyword_set( llat ) THEN BEGIN
      IF NOT keyword_set(vv) THEN $
        message,' If you set one wind keyword, you must set them all '
      IF NOT keyword_set(llon) THEN $
        message,' If you set one wind keyword, you must set them all '
      IF NOT keyword_set(uu) THEN $
        message,' If you set one wind keyword, you must set them all '
    ENDIF 
    IF NOT( keyword_set( uu ) AND keyword_set( vv ) AND $
            keyword_set( llon) AND keyword_set( llat ) ) THEN BEGIN 
       ; if none are set, then don't plot the vectors
       plotvect = 0
    ENDIF 
  END 
ENDCASE 


IF NOT keyword_set( minpix ) THEN BEGIN
  IF NOT IR THEN minpix =  150
ENDIF ELSE BEGIN 
  IF minpix EQ  -1 AND NOT ir THEN minpix =  150
ENDELSE 


BACKGROUND  = 0b
WATER_START = 1b
LAND_START  = 13b
CLOUD_START = 33b
WIND_START  = 73b
WHITE       = 99b
N_WIND_COLORS =  WHITE-WIND_START

openr,rlun,'$VAP_ROOT/goes-vis-overlay-ct.txt',/get,error=err
IF err NE 0 THEN BEGIN 
  message,!err_string,/cont
  return
ENDIF 
spawn,' wc -l $VAP_ROOT/goes-vis-overlay-ct.txt', nlines
nlines =  nlines(0)
tmp =  bytarr(3,nlines)
top_color_index =  nlines-1
readf,rlun, tmp
free_lun,rlun
tmp =  transpose( tmp )
red   =  tmp( *,0)
green =  tmp(*,1)
blue  =  tmp(*,2) &  tmp=0


; Handle windcolor requests
IF n_elements( windcolor ) NE 0 THEN BEGIN
  s =  size( windcolor )
  IF type(windcolor) EQ 'STRING' THEN BEGIN
    color =  GETCOLOR( windcolor )
    windcolor = wind_start
    tvlct,color,wind_start
  ENDIF ELSE BEGIN
    IF windcolor LT  wind_start OR $
       windcolor GT white THEN BEGIN
      str =  ' Out of range wind color, keep 73 <=windcolor,98!'
      message,str,/cont
      return
    ENDIF 
  ENDELSE 
ENDIF ELSE windcolor = -1

r_curr = red 
g_curr = green
b_curr = blue
r_orig = red
g_orig = green
b_orig = blue



; Set device to 'Z' buffer.
set_plot,'z'
device, set_resolution=[160,120]

tvlct,red,green,blue
IF keyword_set( goesfile ) THEN BEGIN 
  ; read the goes file

  READ_PCGOES, goesfile, limits, data, image, year, jday, $
               time, nlon,nlat, lonsize, latsize, info_string
  IF year EQ 0 THEN BEGIN
      spawn,'date', ret_str
      tmp =  str_sep( ret_str(0), ' ' )
      year =  tmp( n_elements(tmp)-1 )
  ENDIF 

  tmp =  doy2date( fix(year), fix(jday) )
  date_str =  tmp(0) + '/' + tmp(1) + '/' + $
     strmid( strtrim( year,2 ), 2,2 )


ENDIF ELSE date_str =  ''

IF n_elements( landel ) EQ 0 THEN BEGIN 
  ; read the land elevation file if it isn't in the common
  openr,1,'$VAP_ROOT/animate/land_elevations.bin'
  landel =  intarr( 12l*360, 12l*180 + 1 )
  readu,1, landel
  close,1
ENDIF 

; get the section of the land el file we need
lonpar =  limits([1,3])
latpar =  limits([0,2])
x = where( lonpar LT 0., nx )
IF nx GT 0 THEN lonpar(x) =  lonpar(x) + 360.
landlon   = fix( [lonpar(0),lonpar(1)]*12. )
landlat   = fix( ([latpar(0),latpar(1)]+90)*12 )
nlandlon  = landlon(1)-landlon(0)+1 
nlandlat  = landlat(1)-landlat(0)+1 
landlon   = findgen(nlandlon)/12. + lonpar(0); #(fltarr(nlandlat)+1)   
landlat   = findgen(nlandlat)/12. + latpar(0); ##(fltarr(nlandlon)+1)

minlat =  limits(0) &  minlon= limits(1)
tlimits =  limits ; copy to protect limits variable

latcent =  0
loncent = 0
minlon =  tlimits(1) &  maxlon= tlimits(3)
y =  [1,3]
x =  where( tlimits(y) LT 0,nx )
IF nx NE 0 THEN tlimits(y(x)) =  tlimits(y(x)) + 360.

loncent =  mean( tlimits([1,3]) )
If keyword_set( title ) THEN tit = title $
                        ELSE tit = ''

MAP_SET, latcent, loncent,  limit=tlimits,/noborder;, title=title_str


image = map_image(data,xs,ys,xsiz,ysiz, $
                  latmin=tlimits(0),$
                  latmax=tlimits(2), $
                  lonmin=tlimits(1), $
                  lonmax=tlimits(3),$
                  /whole_map,compress=1)



IF ir THEN BEGIN 

  xx =  where( image )
  image =  1024-image ; reverse for IR
  IF NOT keyword_set( minpix ) THEN BEGIN 
    minpix =   mean( image(xx) )
    message,' IR Image !, minpix set to ' + strtrim( minpix, 2 ),/cont
  ENDIF 
  IF keyword_set( minpix ) THEN BEGIN
    IF minpix EQ -1 THEN BEGIN 
      minpix =   mean( image(xx) )
      message,' IR Image !, minpix set to ' + strtrim( minpix, 2 ),/cont
    ENDIF 
  ENDIF 
ENDIF 
smallpix =  where( image LE minpix, nsmallpix )

; Scale data array to 40 indices starting at 33
bimage =  bytscl( image, MIN=minpix, top=40 ) + CLOUD_START
unpack_where, image, smallpix, col, row
sz =  size( bimage )
ncols =  sz(1)
nrows =  sz(2)
diff =  !d.y_vsize- nrows- ys
im_coords =  convert_coord( col+xs,row+ys, /device,/to_data )

t7 =  systime(1)
lon =  transpose( im_coords(0,*) ) ; fix it here
xx =  where( lon LT 0, nxx )
IF nxx NE 0 THEN lon(xx) =  lon(xx)+360.
lat =  transpose( im_coords(1,*) )
im_coords =  0
xx = where( lon ge 0. and lon le 360. and $
            lat ge -90. and lat le 90., nxx ) 
lon = lon(xx) & lat = lat(xx) & row = row(xx) & col = col( xx ) 
smallpix =  smallpix(xx)
mask =  long( lon*0 )

; Call the linkimage routine 'land_mask' to calculate which of each
; point is land and which is water. Set the appropriate color index
; in each.
LAND_MASK, lon, lat, mask
;lon =  0
;lat =  0

land =  where( mask EQ 1, nland )
IF nland NE 0 THEN BEGIN 
  ; convert the land coordinates from device to data coordinates,
  ; and replace these numbers in the bimage array with the land
  ; elevation indices.
  o =  convert_coord( col(land)+xs,row(land)+ys,/dev,/to_data)

  tlandlon =  transpose( o(0,*) ) &  tlandlat= transpose( o(1,*)) &  o=0 ;
  xx =  where( tlandlon LT 0, nxx )
  IF nxx NE 0 THEN tlandlon(xx) =  tlandlon(xx) + 360.
  xxx =  where( tlandlon GT 0. AND tlandlon LE 360. AND $
                tlandlat GE -90. AND tlandlat LE 90. )
  tlandlon = tlandlon(xxx) 
  land   = land(xxx)
  tlandlat = tlandlat(xxx)

  ix =  tlandlon*12
  iy = ( tlandlat+90.)*12.
  bimage( col(land), row(land) ) =  landel( ix, iy ) + LAND_START
ENDIF 


sea =  where( mask EQ 0, nsea )
IF nsea NE 0 THEN BEGIN 
  IF keyword_set( watcolor ) THEN $
    bimage( smallpix( sea ) ) =  watcolor $  
 ELSE $
    bimage( smallpix( sea ) ) =  $
      bytscl( image( smallpix( sea ) ), min= cloud_start, $
           top= WATER_START + 11 )   + water_start

ENDIF 
TV, bimage, xs, ys


;
; over plot the wind vectors, if there are any.
IF plotvect THEN BEGIN 
  IF windcolor EQ -1 THEN $
    PLOTVECT, uu,vv,llon,llat,$
     length = 2,$
     start_index = wind_start, $
     ncolors = n_wind_colors, $
     maxspeed= 25. $
  ELSE $ 
    PLOTVECT, uu,vv,llon,llat,$
     length = 2,$
     color=windcolor
ENDIF 

;IF grid THEN BEGIN 
;  latrange =  limits(2)-limits(0)
;  lonrange =  limits(3)-limits(1)
;  IF latrange/5. GT 20 THEN latdel = 10 ELSE latdel = 5.
;  IF lonrange/5. GT 20 THEN londel = 10 ELSE londel = 5.

;  map_grid, latdel = latdel, londel = londel, $
;   lonlab = minlat-.3,latlab = minlon, $
;   charsize=.8, latalign=1, color = 255
;ENDIF 

tmp =  '0000'
strput, tmp, time, 4-strlen(time)
time =  tmp

ofileroot = strtrim( sat_name, 2 ) + '_' + $
 sensor + '_' + $
   jday + time 
sp =  strpos( ofileroot,' ' )
strput, ofileroot, '_', sp
lim_str   =  strtrim( long(limits), 2 )
lim_str =  '%' + lim_str(1) + ',' + lim_str(0) + ',' + $
                 lim_str(3) + ',' + lim_str(2) +  '%'
dlm =  '_'

ofileroot =  ofileroot + '-' + lim_str
IF keyword_set( file_str ) AND $
   strlen( file_str(0) ) GT 0 THEN BEGIN 
  s =  str_sep( file_str,' ' )
  tt =  '_' + s(0)
  FOR i=1,n_elements(s)-1 DO tt =  tt + '_' + s(i)
  ofileroot =  ofileroot + tt
ENDIF 
ofile =  ofileroot + '.tn.gif'

im =  tvrd()
write_gif, ofile, im, red, green, blue

GENV,/restore ; restore graphics environment 

RETURN
END

