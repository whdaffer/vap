;+
; NAME:  qmodelshoe
; $Id$
; PURPOSE:  Show the input QuikSCAT interpolated field in a window
;
; AUTHOR:  whd
;
; CATEGORY:  QuikSCAT visualization
;
; CALLING SEQUENCE:  qmodelshow, filename,$
;                    lonrange=lonrange, $
;                    latrange=latrange, $
;                    minspeed=minspeed,
;                    maxspeed=maxspeed, $
;                    bottom=bottom, ncolors=ncolors
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
;  This routine assumes that the caller has arranged the display
;  environment *entirely*. It checks nothing, it corrects nothing, it
;  just makes the picture!
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
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-


PRO qmodelshow, filename, top=top, lonrange=lonrange, $
                latrange=latrange, minspeed=minspeed,maxspeed=maxspeed, $
                bottom=bottom, ncolors=ncolors

  IF n_params() LT 1 THEN BEGIN 
    r =dialog_message(["Usage: qmodelshow,filename[,$",$
                       "lonrange=longrange,latrange=latrange,$",$
                       "minspeed=minspeed,maxspeed=maxspeed,$",$
                       "bottom=bottom, ncolors=ncolors"],$
                        title='Usage: qmodelshow',/error)
    return
  ENDIF

  IF NOT isa(filename,/string,/nonempty) THEN BEGIN 
    str = "FILENAME must be non-empty string"
    r = dialog_message(str,/error)
    return
  ENDIF 

  
  q = obj_new('qmodel',file=filename)
  IF NOT obj_valid(q) THEN BEGIN 
    r = dialog_message(!error_state.msg + $
                       " Can't Read File " + filename,/error)
    return
  ENDIF 

  s = q-> getplotdata(u,v,lon,lat)
  obj_destroy,q

  top = n_elements(top) EQ 0 ? 0 : top

  nn = n_elements(lonrange)
  IF nn GT 0 THEN BEGIN 
    IF nn NE 2 THEN BEGIN 
      r = dialog_message("lonrange must be 2-vector",/error)
      return
    ENDIF 
  ENDIF ELSE lonrange = [0,360]
  nn = n_elements(latrange)
  IF nn GT 0 THEN BEGIN 
    IF nn NE 2 THEN BEGIN 
      r = dialog_message("latrange must be 2-vector",/error)
      return
    ENDIF 
  ENDIF ELSE latrange = [-90,90]

  IF n_elements(minspeed) EQ 0 THEN minspeed = 1
  IF n_elements(maxspeed) EQ 0 THEN maxspeed = 30

  s = minspeed> sqrt(u^2+v^2) < maxspeed

  minv = minspeed
  maxv = maxspeed

  loncent = mean(lonrange)
  map_set,0,loncent,lim=[latrange[0],lonrange[0],latrange[1],lonrange[1]]
  contour,s,lon,lat,$
     levels=findgen(ncolors)/(ncolors-1)*(maxv-minv)+minv,$
     c_colors=findgen(ncolors)+bottom,/cell_fill,/overplot
  map_continents,/fill,color=!d.n_colors-1

  END 
