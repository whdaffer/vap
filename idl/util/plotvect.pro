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
;             Dots=Dots , $
;             Table=Table, $
;             trueColor=trueColor, $
;             scale=scale
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
;    skip        : Take those vectors whose location == 0 mod skip
;    minspeed    : Only plot those vectors whose speed >= minspeed
;    maxspeed    : Only plot those vectors whose speed <= minspeed
;    Length      : Make them this long 
;    thick       : And this thick
;    Title       : Put this title on the plot
;    color       : if color=-1L, then use speed, even if truecolor is
;                  set. This can be done because in a 24 bit system,
;                  -1L is not a legitimate color value (because it has
;                  the most significant bit set, and that is more than 24
;                  bits.) If this value is set to some number other
;                  than -1L, don't use speed of vector to determine color,
;                  rather plot them all with this color index, if 8
;                  bit system,  or this color, if truecolor is set.
;
;                  If color=-1L or color='undefined' (in the sense of
;                  'n_elements(color) eq 0', AND truecolor=1, this
;                  routine will bytscl the speed array into the range
;                  specified by start_index and n_colors and then do
;                  one of two things. It will either get the color
;                  table from the system by tvlct and then, using the
;                  bytscl'd speed array as color indices into this
;                  color table, compute 24 bit colors from it, or it
;                  will compute the 24 bit colors using the whatever
;                  table is input via the 'Table' keyword. 
;
;                  The only other method to specify color in a
;                  true/direct color environment is to set
;                  the 'color' keyword to an array having the same
;                  dimensions as the u/v/lon/lat arrays containing the
;                  desired 24 bit numbers to be used as colors.
;                  
;   
;    start_index : When using speed to determine the color, use this
;                  color index as the starting index
;    ncolors     : And use this many colors.
;    Dots        : Flag, if set, don't plot vectors, plot dots.
;    Truecolor   : convert Colors 'indices' to 24 bit values.
;    Table       : and use this table, if truecolor=1., $
;    Scale       : Scale the vectors according to their magnitude.
;
;   
;
;
; OUTPUTS:  A pretty picture, hopefully ;->
;
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
; RESTRICTIONS: 
;
;  The true color stuff probably only works with decomposed color. I
;  haven't figured out color when decomposed=0.  
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
; Revision 1.5  1999/10/05 16:34:04  vapuser
; Added 'scale' keyword, corrected a misuse of 'keyword_set' and put a
; 'noclip' in call to plots
;
; Revision 1.4  1999/04/08 21:48:40  vapuser
; Updated some comments and did some cleanup.
;
; Revision 1.3  1998/11/20 20:03:32  vapuser
; Accomidate 24bit color
;
; Revision 1.2  1998/10/28 23:34:22  vapuser
; added 'dots' keyword. Took out norm calculation.
;
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
             Dots=Dots ,$
             table=Table,$
             truecolor=truecolor, $
             scale=scale

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
     '                   thick =  thick, $'  + lf + $
     '                    truecolor=truecolor, $' + lf + $
     '                      table=table, $'+ lf + $
     '                        scale=scale'
    Message,str,/cont
  ENDIF  

  IF N_Elements(length)        EQ 0 THEN length = 1
  IF N_Elements( skip )        EQ 0 THEN skip = 1
  IF N_Elements( ncolors )     EQ 0 THEN ncolors =  !d.n_colors
  IF N_Elements( start_index ) EQ 0 THEN start_index = 0
  IF N_Elements( minspeed )    EQ 0 THEN minspeed =  2
  IF N_Elements ( maxspeed )   EQ 0 THEN maxspeed =  37
  IF N_elements( thick )       EQ 0 THEN thick =  0
  IF n_elements(color)         EQ 0 THEN color = -1l

  truecolor = keyword_set(truecolor)

  Dots = Keyword_set(Dots)
  skip = skip > 1
    
  noscale = keyword_set(scale) EQ 0

  good1 = where(finite(u) AND finite(v),ngood1)
  IF ngood1 EQ 0 THEN return
  n = size(u)
  speed = sqrt(u[good1]^2+v[good1]^2)
  good = where( speed NE 0 , ngood)
  IF ngood NE 0 THEN BEGIN 
    speed = speed[good]
    dx = length/37.*u[good1[good]]     ;(sin)
    dy = length/37.*v[good1[good]]     ;(cos)
    IF noscale THEN BEGIN 
      dx = 12*dx/speed
      dy = 12*dy/speed
    ENDIF 
    x2 = x[good1[good]]
    y2 = y[good1[good]]

    IF n_elements(color) EQ n_elements(x) THEN $
      color = color[good1[good]]

  ENDIF ELSE BEGIN 
    Message,' There are NO Non-Zero length Vectors ',/cont
    return
  ENDELSE 
  speed =  minspeed >  speed < maxspeed


  IF TrueColor THEN BEGIN

    IF n_elements(color) EQ n_elements(speed) THEN BEGIN 
      col = color
    ENDIF ELSE BEGIN 
      IF color[0] EQ -1 THEN BEGIN 
        IF n_elements(Table) EQ 0 THEN BEGIN 
          tvlct,r,g,b,/get
          table = transpose([ [r],[g],[b]])
        ENDIF 
        veccol = BytScl( speed, min=minspeed, $
                         max=maxspeed, $
                         top=NCOLORS-1) + start_index
        col = Rgb2True(veccol, Colortable=table)
      ENDIF ELSE col=replicate( color, n_elements(speed))
    ENDELSE 

  ENDIF ELSE BEGIN 

      ; pseudo color
    IF n_elements(color) GT 1 THEN BEGIN 
      col = color
    ENDIF ELSE BEGIN 
      IF color[0] EQ -1 THEN $
        col = bytscl( speed, min=minspeed,max=maxspeed, $
                      top= ncolors-1) + byte(start_index) ELSE $
        col=replicate(color,n_elements(speed))
    ENDELSE 

  ENDELSE 

;    IF color[0] EQ -1 THEN BEGIN 
;    ENDIF ELSE BEGIN 
;      IF n_elements( color ) EQ 1 THEN BEGIN 
;        IF color EQ -1 THEN $
;           col = Bytscl( speed, min=minspeed,max=maxspeed, $
;                         top= ncolors-1) + byte( start_index)  ELSE $
;           col = replicate(color,n_elements(speed))
;      ENDIF ELSE $
;            col = color
;    ENDELSE 
;  ENDELSE 


  ;
  r = .2                          ;len of arrow head
  angle = 22.5 * !dtor            ;Angle of arrowhead
  st = r * sin(angle)             ;sin 22.5 degs * length of head
  ct = r * cos(angle)
  ;

  IF skip NE 1 THEN BEGIN 
    keep =  Lindgen( n_elements(x2)/skip )*skip
    x2 = x2[keep]
    dx = dx[keep]
    y2 = y2[keep]
    dy = dy[keep] &  keep=0
  ENDIF 

  IF Dots THEN BEGIN 
    plots,x2,y2,psym=3, color=col
  ENDIF ELSE BEGIN 

    x0 = x2-dx/2.
    x1 = x0+dx
    y0 = y2-dy/2.
    y1 = y0+dy
    nn = N_Elements(x0)


    FOR i=0l,nn-1 DO BEGIN     ;Each point
        Plots,[x0[i],x1[i],$
               x1[i]-(ct*dx[i]-st*dy[i]),$
               x1[i],x1[i]-(ct*dx[i]+st*dy[i])], $
              [y0[i],y1[i],$
               y1[i]-(ct*dy[i]+st*dx[i]),$
               y1[i],y1[i]-(ct*dy[i]-st*dx[i])], $
              color=col(i), thick= thick, noclip=0
    ENDFOR 
  ENDELSE 

RETURN 
END



