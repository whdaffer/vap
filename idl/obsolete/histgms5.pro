PRO histgms5, datetime


  genv,/save  
  IF n_params() LT 1 THEN BEGIN 
    Usage,'HistoGms5, datetime'
    return
  ENDIF 

  IF NOT isa(datetime,/string,/nonempty) THEN BEGIN 
    Message,'Datetime Parameter must be a NONEMPTY STRING',/CONT
    return
  ENDIF 

  IF n_elements(datetime) GT 1 THEN BEGIN 
    Message,$
      'Parameter DATETIME should be a SCALAR: ' + $
       'Only first datetime will be read and processed',/cont
  ENDIF 


  MapLimits =  [80.,-60,200,60]
  lonlim = MapLimits[ [0,2] ]
  latlim = MapLimits[ [1,3] ]

  lonrange = lonlim[1]-lonlim[0]
  latrange = latlim[1]-latlim[0]


  xsize = 960
  aspectratio = float(latrange)/lonrange
  ysize = fix(xsize*AspectRatio)

  lonpar =  [ lonlim, ( lonlim[1]-lonlim[0] +1)/xsize ]
  latpar =  [ latlim, ( latlim[1]-latlim[0] +1)/ysize ]




  allData = gms5readall( datetime, 'ir1', lonpar=[80,200] )
  IF NOT isa(alldata,/structure) THEN BEGIN 
    Message,'Error reading data!',/cont
    return
  ENDIF 
  image = allData.imagedata.image

  xloc = allData.griddata.xloc
  yloc = allData.griddata.yloc
  minlon = allData.griddata.minlon
  
  newGrid = gms5idx(lonpar,latpar)
  loni=newGrid.loni
  lati=temporary(newGrid.lati)
  newx=interpolate(temporary(xloc),loni,lati,/grid)
  newy=interpolate(temporary(yloc),$
                   temporary(loni),$
                   temporary(lati),/grid)


  cloudmask = image[newx,newy]


  hist = histogram(cloudmask,min=0,max=255)
  hist = 100.*hist/total(hist)

  cloudmask = scale(cloudmask)*99
    ; Define the new Brightness/Saturation mappings
  xx=findgen(100)/99.

  brightmin = 0. &  brightmax=0.8 &  satmin=0. &  satmax=0.55
  bi = 0.2> interpol( [0.,1], [BrightMin, BrightMax], xx ) < 1
  si = 0> interpol( [1.,0], [SatMin,    SatMax],xx ) < 1

    ; Use 'cloudmask' to create new Brightness/Saturation values

  b2=bi[cloudmask]
  s2=si[cloudmask]

  bhist = histogram(b2,min=0,max=1,bin=0.01)
  shist = histogram(s2,min=0,max=1,bin=0.01)

  bhist = 100*bhist/total(bhist)
  shist = 100*shist/total(shist)
  
  plotfile = datetime[0] + '-hist.ps'
  savefile =  datetime[0] + '-hist.save'


  ps = { $
        XSIZE           :  9.70000,$
        XOFF            : 0.700000,$
        YSIZE           :  7.10000,$
        YOFF            :  10.3500,$
        FILENAME        :  plotfile,$
        INCHES          :   1,$
        COLOR           :   0,$
        BITS_PER_PIXEL  :   2,$
        ENCAPSULATED    :   0,$
        LANDSCAPE       :   1}
  
  set_plot,'ps'
  device,_extra=ps
  plot,hist,title=datetime, ytitle='% of data'
  timestamp
  !p.multi = [0,1,2]

  xx = findgen(n_elements(bhist))*0.01
  plot,xx,bhist< 10,title=datetime,ytit='% of data',xtit='Brightness',psym=-1
  plot,xx,shist < 10,xtit='Saturation',psym=-1,ytit='% of data'
  timestamp

  device,/close
  genv,/restore
  save,hist,bhist,shist,file=savefile

END

  

