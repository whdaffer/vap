FUNCTION uniquifyvv, verts,vv
   londiff = verts[0,vv[1:*]]-verts[0,vv]
   latdiff = verts[1,vv[1:*]]-verts[1,vv]
   diff = londiff+latdiff
   x = where(abs(diff) GT 3.e-7,nx)
   IF nx NE 0 THEN BEGIN 
     vv = vv[[x, max(x)+1]]
     nx = nx+1
   ENDIF 
   return,nx
END


PRO sl, file,data,seeds,conn, verts, dx=dx,dy=dy,title=title,skip=skip,_extra=_extra

IF n_params() NE 5 THEN BEGIN 
  Message,'Sorry, need all five',/info
  return
ENDIF 

IF n_elements(dx) EQ 0 THEN dx = 3
IF n_elements(dy) EQ 0 THEN dy = 7
nx = long(360/dx)
ny = long(121/dy) 


IF n_elements(title) EQ 0 THEN $
  title = string(dx,dy,format="('DX: ',i3,', DY: ', i3)")

window,/free, title=title,xsize=600,ysize=600
;map_set,0,180
map_set,15,200,sat_p=[20,0,0],/satellite

q=obj_new('qmodel',file=file)

S=Q->GETPLOTDATA(U,V,LON,LAT)
obj_destroy,q
data=fltarr(2,360,121)
data[0,*,*]=u
data[1,*,*]=v
seeds=fltarr(2,nx,ny)

seeds[0,*,*]=(findgen(nx)*dx)#replicate(1.,ny)
seeds[1,*,*]=replicate(1.,nx)#(findgen(ny)*dy)
seeds = reform(seeds,2,nx*ny)
particle_trace,data,seeds,verts,conn, _extra=_extra
verts[1,*] = verts[1,*]-60.
speed = 2>  sqrt(U^2+v^2) <  32
contour,speed,lon,lat,levels=findgen(30)+2,/cell_fill,/overplot

IF n_elements(skip) EQ 0 THEN skip = n_elements(verts[0,*])+1
skipper = 1
ii = 0
repeat begin
  nn=conn[ii]
  IF nn GT 1 THEN BEGIN 
    IF skipper NE 0 THEN BEGIN 
      vv = conn[ii+1:ii+nn]
      nn1 = uniquifyvv(verts,vv)
      IF nn1 GT 0 THEN BEGIN 
        plots,verts[0,vv],-90> verts[1,vv] < 90
        IF nn1 GT 301 THEN BEGIN 
          x1 = (lindgen(long((nn1-100)/200))+1)*200l
          IF n_elements(x1) GE 3 THEN BEGIN 
            x1 = x1[1:n_elements(x1)-2] 
          ENDIF ELSE BEGIN 
            x1 = n_elements(vv)/2
          ENDELSE 
          arrow,verts[0,vv[x1-1]],verts[1,vv[x1-1]],$
                verts[0,vv[x1]],verts[1,vv[x1]],/data
        ENDIF 
      ENDIF 
    ENDIF 
  ENDIF 
  skipper = (skipper+1) MOD skip 
  ii = ii+nn+1
endrep until ii ge n_elements(conn)

map_continents,/fill,color=0
END
