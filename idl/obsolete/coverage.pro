pro coverage,rev1,rev2
;
;  compute and plot WVC grid points for a given ascending node
longitude
;
ni = 812
nj = 24
openr,1,'wvcgrid.dat'

; Read in nominal (node=0) WVC grid

latw = fltarr(nj,ni)
lonw = fltarr(nj,ni)
readu,1,lonw
readu,1,latw
close,1

lat0=-90. & lat1=90. & lon0=0. & lon1=360.
llim=[lat0,lon0,lat1,lon1]
!p.background=255
!p.color=0

map_set,0,180,0,limit=llim,color=byte(229)-!p.background,$
 title='NSCAT WVC Swath Coverage',/cyl,/continents,/grid,/label

loadct,12,/silent

close,2
openr,2,'revlist3.dat'
rev=0 & node=0.
;read,'Start/end rev numbers: ',rev1,rev2

if node lt 0. then node=node + 360.
if node gt 360. then node=node-360.

LOOP:
;read,'Input ascending node longitude: ',node
readf,2,rev,node

if rev lt rev1 then goto,LOOP
if rev gt rev2 then goto,STOP

lona = lonw + node
jj=where(lona gt 360.,knt)
if knt gt 0 then lona(jj) = lona(jj) - 360.

lon_edge1=fltarr(ni) & lat_edge1=lon_edge1
lon_edge2=fltarr(ni) & lat_edge2=lon_edge2
lon_edge3=fltarr(ni) & lat_edge3=lon_edge3
lon_edge4=fltarr(ni) & lat_edge4=lon_edge4

lon_edge1(*) = lona(0,*)  & lat_edge1(*) = latw(0,*)
lon_edge2(*) = lona(11,*) & lat_edge2(*) = latw(11,*)
lon_edge3(*) = lona(12,*) & lat_edge3(*) = latw(12,*)
lon_edge4(*) = lona(23,*) & lat_edge4(*) = latw(23,*)

plots,lona,latw,psym=3,color=140
oplot,lon_edge1,lat_edge1,color=0
oplot,lon_edge2,lat_edge2,color=0
oplot,lon_edge3,lat_edge3,color=0
oplot,lon_edge4,lat_edge4,color=0

goto,LOOP

STOP:
close,2

end
