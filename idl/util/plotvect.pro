;+
; NAME:  plotvect
; $Id$
; PURPOSE:  A general purpose vector plotting tool. Based on velovect
;          (I think)
;
;
; AUTHOR: William Daffer (with help from Mike Spencer, Jim Huddelston
;         and whoever wrote Velovect.)
;
;
; CATEGORY:  Vector Plotting
;
;
;
; CALLING SEQUENCE: 
;
;
;      PLOTVECT,u,v,x,y, $
;             skip  = skip, $
;             minspeed = minspeed ,$
;             maxspeed= maxspeed ,$
;             Length = length, $
;             Title = title, $
;             color = color ,$
;             start_index = start_index ,$
;             ncolors = ncolors, $
;             thick =  thick, $
;             Dots=Dots 
; 
; INPUTS:  
;
;   U : vector of any number of dimensions
;       The 'X' component of each vector
;   V : vector of any number of dimensions
;       The 'Y' component of each vector
;   X : The X location of the vector.
;   Y : The y location of the vector.
;
;       The vector is draw so that the X/Y point at in the middle of
;       the stem of the vector.
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;    skip        : Take those vector = 0 mod skip
;    minspeed    : Only plot those vectors whose speed >= minspeed
;    maxspeed    : Only plot those vectors whose speed <= minspeed
;    Length      : Make them this long 
;    thick       : And this thick
;    Title       : Put this title on the plot
;    color       : Don't use speed of vector to determine color,
;                  rather plot them all with this color index
;    start_index : When using speed to determine the color, use this
;                  color index as the starting index
;    ncolors     : And use this many colors.
;    Dots        : Flag, if set, don't plot vectors, plot dots.
;
;   
;
;
; OUTPUTS:  A pretty picture
;
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
; SIDE EFFECTS:  None
;
;
;
; RESTRICTIONS:  None
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
;
; MODIFICATION HISTORY:
; $Log$
; Revision 1.1  1998/10/23 22:22:36  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1996, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

PRO plotvect,u,v,x,y, $
             skip= skip, $
             minspeed = minspeed ,$
             maxspeed= maxspeed ,$
             Length = length, $
             Title = title, $
             color = color ,$
             start_index = start_index ,$
             ncolors = ncolors, $
             thick =  thick , $
             Dots=Dots 

  lf = string(10b)
  IF n_params() NE 4 THEN BEGIN 
    str = 'Usage: ' + lf + $
     '        PlotVect,U,V,Lon,Lat,' + lf + $
     '          skip= skip, $ ' + lf + $
     '           minspeed = minspeed ,$' + lf + $
     '            maxspeed= maxspeed ,$' + lf + $
     '             Length = length, $' + lf + $
     '              Title = title, $' + lf + $
     '               color = color ,$' + lf + $
     '                start_index = start_index ,$' + lf + $
     '                 ncolors = ncolors, $' + lf + $
     '                   thick =  thick ' 
    Message,str,/cont
  ENDIF  

  IF N_Elements(length)        EQ 0 THEN length = 1
  IF N_Elements( skip )        EQ 0 THEN skip = 1
  IF N_Elements( ncolors )     EQ 0 THEN ncolors =  !d.n_colors
  IF N_Elements( start_index ) EQ 0 THEN start_index = 0
  IF N_Elements( minspeed )    EQ 0 THEN minspeed =  2
  IF N_Elements ( maxspeed )   EQ 0 THEN maxspeed =  37
  IF N_elements( thick )       EQ 0 THEN thick =  0
  Dots = Keyword_set(Dots)
  skip = skip > 1
    
  good1 = where(finite(u) AND finite(v),ngood1)
  IF ngood1 EQ 0 THEN return
  n = size(u)
  speed = sqrt(u[good1]^2+v[good1]^2)
  good = where( speed NE 0 , ngood)
  IF ngood NE 0 THEN BEGIN 
    speed = speed[good]
    dx = length/37.*u[good1[good]]     ;(sin)
    dy = length/37.*v[good1[good]]     ;(cos)
    x2 = x[good1[good]]
    y2 = y[good1[good]]
  ENDIF ELSE BEGIN 
    Message,' There are NO Vectors of Non-Zero length ',/cont
    return
  ENDELSE 
  speed =  minspeed >  speed < maxspeed

  IF NOT( keyword_set(color ) ) THEN  BEGIN
    col = bytscl( speed, min=minspeed,max=maxspeed, $
                  top= ncolors-1) + byte(start_index)
  ENDIF ELSE BEGIN 
    IF n_elements( color ) EQ 1 THEN BEGIN 
      IF color EQ -1 THEN $
         col = Bytscl( speed, min=minspeed,max=maxspeed, $
                       top= ncolors-1) + byte( start_index)  ELSE $
         col= bytarr( n_elements( speed ) ) + byte(color )
    ENDIF ELSE $
          col =  byte(color)
  ENDELSE 
  ;
  r = .2                          ;len of arrow head
  angle = 22.5 * !dtor            ;Angle of arrowhead
  st = r * sin(angle)             ;sin 22.5 degs * length of head
  ct = r * cos(angle)
  ;

  keep =  Lindgen( n_elements(x2)/skip )*skip
  x2 = x2[keep]
  dx = dx[keep]
  y2 = y2[keep]
  dy = dy[keep] &  keep=0

  x0 = x2-dx/2.
  x1 = x0+dx
  y0 = y2-dy/2.
  y1 = y0+dy
  nn = N_Elements(x0)

  IF Dots THEN BEGIN 
    plots,x2,y2,psym=3, color=col
  ENDIF ELSE BEGIN 
    FOR i=0l,nn-1 DO BEGIN     ;Each point
        Plots,[x0[i],x1[i],$
               x1[i]-(ct*dx[i]-st*dy[i]),$
               x1[i],x1[i]-(ct*dx[i]+st*dy[i])], $
              [y0[i],y1[i],$
               y1[i]-(ct*dy[i]+st*dx[i]),$
               y1[i],y1[i]-(ct*dy[i]-st*dx[i])], $
              color=col(i), thick= thick
    ENDFOR 
  ENDELSE 

RETURN 
END



