;
; Mods
; $Log$
;
; $Id$
;
PRO nscat_getuv, dir, speed, u, v
; the procedure takes the two parameters dir/speed and converts them
; to u (=x) and v(=y) components.
; 
  x = where( finite(dir) AND finite(speed), nx )
  u = speed
  u[*] = 0
  v = u
  IF nx NE 0 THEN BEGIN 
    u[x] =  speed[x]*sin(dir[x]*!dtor)
    v[x] =  speed[x]*cos(dir[x]*!dtor)
  ENDIF 

return
END
