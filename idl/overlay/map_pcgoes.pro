PRO map_pcgoes, data, limits, image, xs, ys, $
                title  = title, $
                nogrid = nogrid, $
                usedata = usedata, $
                startpos = startpos ,$
                screensize = screensize, $
                nocontinents= nocontinents,$
                z =  z

; keyword usedata: if set, map_pcgoes uses the data in variable 'data'
; as the actual image, rather than calculating it again.
;
;
grid =  NOT ( keyword_set( nogrid ))
usedata =  keyword_set( usedata )
minlat =  limits(0) &  minlon= limits(1)
tlimits =  limits ; copy to protect limits variable
Z =  keyword_set( z )
IF Z THEN $ 
  set_plot,'z' $
ELSE $
  set_plot,'x'

IF usedata THEN BEGIN 
  IF NOT keyword_set( screensize )  THEN BEGIN
    str =  ' Keyword SCREENSIZE must be set to [x,y] size ' + $
     'of plotting window'
    message,str,/cont
    return
  ENDIF 
  IF NOT keyword_set( startpos )  THEN BEGIN
    str =  ' Keyword STARTPOS must be set to DEVICE coords [x,y] of ' + $
     'lower left corner of image.'
    message,str,/cont
    return
  ENDIF 
  IF NOT Z THEN $
    window, xsiz=screensize(0),ysize=screensize(1),/free ELSE $
    device, set_resolution=[screensize(0),screensize(1)]
  image =  data
  xs =  startpos(0)
  ys =  startpos(1)
ENDIF ELSE BEGIN 
  IF NOT Z THEN $
    window, xsiz=640,ysize=480,/free ELSE $
    device, set_resolution=[640,480]
ENDELSE 

latcent =  0
loncent = 0
minlon =  limits(1) &  maxlon= limits(3)
IF minlon GT maxlon THEN BEGIN 
  ; if min(lon) > max(lon)
  loncent =  -180.
  tlimits(1) =  -360 + limits(1);
ENDIF 
IF keyword_set( title ) THEN tit = title $
                        ELSE tit = ''

MAP_SET, latcent, loncent,  limit=tlimits,/noborder, title=tit


IF NOT usedata THEN BEGIN 
  if (minlon lt maxlon) then begin
    image = map_image(data,xs,ys,$
                      latmin=tlimits(0),$
                      latmax=tlimits(2), $
                      lonmin=tlimits(1), $
                      lonmax=tlimits(3),$
                      /whole_map,compress=1)
  ;  resimag=bytscl(resimag,min=0,max=255)
  ENDIF ELSE BEGIN 
   image = map_patch(data,$
                     lat0=tlimits(0),$
                     lat1=tlimits(2), $
                     lon0=tlimits(1),$
                     lon1=tlimits(3)+0.01,$
                     xstart=xs,$
                     ystart=ys)
  ENDELSE 
ENDIF 

tvscl, image, xs, ys
IF NOT( keyword_set( nocontinents ) ) THEN map_continents, /coasts,color=255


IF grid THEN BEGIN 
  latrange =  limits(2)-limits(0)
  lonrange =  limits(3)-limits(1)
  IF latrange/5. GT 20 THEN latdel = 10 ELSE latdel = 5.
  IF lonrange/5. GT 20 THEN londel = 10 ELSE londel = 5.

  map_grid, latdel = latdel, londel = londel, $
   lonlab = minlat-.3,latlab = minlon, $
   charsize=.8, latalign=1, color = 255
ENDIF 

END
