FUNCTION gridplsws,node
  ;
  ;  compute and plot WVC grid points for a given 
  ;  ascending node  longitude
  ;
  ae = 6378.137	      ; Earth radius
  inc = 98.616*!dtor  ; Inclination
  cosi = cos(inc)     
  sini = sin(inc)

  P2 = 101.92	; orbit period (min)
  P1 = 1440.	; 1 mean solar day (min)
  pr = P2/P1

  ni = 1624     ; Number of Rows
  nj = 76       ; Number of columns
  res = 25.
  ;RGI = 200.

  res_i = (360./ni)*!dtor ; Angular sepration between rows
  res_j = res/ae          ; Angular seperation between cols

  ;del = RGI/ae + 0.25*res_j

  ; Set up WVC grid

  wvc_i = indgen(ni) + 1
  lonp1 =  (findgen(ni) + 0.5)*res_i - 90.*!dtor
  latp1 = -(findgen(nj)-(nj/2-0.5))*res_j

  ; for j = 0,nj-1 do latp1(j) = -(j-(nj/2 - 0.5))*res_j


  latw = fltarr(3,ni)
  lonw = fltarr(3,ni)

;  FOR i = 0,ni-1 DO BEGIN

      ascnow = node*!dtor - pr*lonp1
      x = [0,76/2,75]
      FOR j = 0,2 DO BEGIN
          jj = x[j]
          latw[j,*] = asin($
                           cosi*sin(latp1[jj]) + $
                           sini*cos(latp1[jj])*sin(lonp1) $
                          )
          lonw[j,*] = atan( $
                           (cosi*sin(lonp1) - sini*tan(latp1[jj])), $
                            cos(lonp1) $
                          ) + ascnow
      ENDFOR

      bad = where( lonw LT 0, nbad )
      IF nbad NE 0 THEN lonw[bad] =  lonw[bad] + 2*!pi


;  ENDFOR

  lonw = lonw/!dtor
  latw = latw/!dtor
       
;  lat0=-90. & lat1=90. & lon0=0. & lon1=360.
;  llim=[lat0,lon0,lat1,lon1]
;  !p.background=255
;  !p.color=0

;  map_set,0,180,0,limit=llim,color=byte(229)-!p.background,$
;   /cyl,/continents,/grid,/label

;  loadct,12,/silent

;  plots,lonw/!dtor,latw/!dtor,psym=3,color=140

;  IF node EQ 0. THEN BEGIN
;      openw,1,'wvcgridsws.dat'
;      writeu,1,lonw/!dtor
;      writeu,1,latw/!dtor
;      close,1
;  ENDIF

  return, [ [[lonw]],[[latw]] ]
END



