PRO test_animate, frametimes, changes,dd0,dd1
restore,'/tmp/files.save'
junk = {frametime: {idldt}, $
        lonpar: fltarr(2), $
        latpar: fltarr(2), $
        data: fltarr(101,61,2) }

junk.lonpar = [30,130]
junk.latpar = [-30,30]
animate=obj_new('animate',120,files,'qmodela',lonpar=[30,130],latpar=[-30,30])  
openw,olun,'testanimate.dat',/get
ptc = ReadColorTable($
                    '/usr/people/vapuser/Qscat/Resources/Color_Tables/pv.ct.2')
ct = *ptc
ncolors = n_elements(ct[0,*])
wind0 = 48
nwind = 45

;set_plot,'z'
;device,set_resolution=[960,720]
set_plot,'x'
device,pseudo=8
window,colors=ncolors,xsize=960,ysize=720,/pixmap
;loadct,39
tvlct,transpose(ct)
tvlct,r,g,b,/get

loni = (findgen(101)+30)#replicate(1.,61)
lati = replicate(1.,101)#(findgen(61)-30)
frametimes = replicate({idldt},120)
done = 0
changes = intarr(120)
FOR f=0,119 DO BEGIN 
  map_set,0,80,lim=[-30,30,30,130]

  curdat = geoanim-> getframe(f)
  geoanim-> get,frametime = frametime
  junk.frametime = frametime
  frametimes[f] = frametime
  geoanim-> get,data0 = data0, data1=data1
  IF f GT 0 THEN BEGIN 
    data0-> get,data = d0
    data1-> get,data = d1
    x = where(d0-prevd0 OR d1-prevd1,nx )
    IF nx NE 0 THEN BEGIN 
      Message,"Change at frame # " + strtrim(f,2),/cont
      changes[f] = 1
      IF NOT done THEN BEGIN 
        done = 1
        dd1 = d0
      ENDIF 
    ENDIF  
  ENDIF 
  junk.data = curdat
  writeu,olun,junk
  speed = sqrt(curdat[*,*,0]^2+curdat[*,*,1]^2) < 25
  bspeed = bytscl( speed,top=nwind-1)+wind0

  contour,speed,loni,lati,level=findgen(30),$
    c_col=findgen(30)/29*(nwind-1)+wind0,/cell_fill,/overplot
  map_continents,/fill,color=0

  im = tvrd()
  name = 'frame.' + string(f,form='(i3.3)')
  write_gif,name,im,r,g,b
  data0-> get,data=prevd0
  data1-> get,data=prevd1
  IF f EQ 0 THEN dd0 = prevd0
END

free_lun,olun
obj_destroy,geoanim
help,/heap
END

