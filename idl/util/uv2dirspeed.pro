; $Id$
;
; Mods:
;
; $Log$
;
;
PRO UV2DirSpeed, u,v, Dir, Speed
  x = where( finite(u) AND finite(v) )
  speed = u
  speed[*] = !values.f_nan
  dir = speed
  speed[x] = sqrt( u[x]^2+v[x]^2 )
  xx = where( speed[x] gt 0.1, nxx )
  IF nxx NE 0 THEN BEGIN 
    Dir[x[xx]] = atan(u[x[xx]],v[x[xx]])/!dtor
  ENDIF 
END

  
