PRO histgoes, file

  genv,/save
  IF n_params() LT 1 THEN BEGIN 
    Usage,'HistGoes, filename'
    return
  ENDIF 

  IF NOT isa(file,/string,/nonempty) THEN BEGIN 
    Message,'File Parameter must be a NONEMPTY STRING',/CONT
    return
  ENDIF 

  IF n_elements(file) GT 1 THEN BEGIN 
    Message,$
      'Parameter FILENAME should be a SCALAR: ' + $
       'Only first file will be read and processed',/cont
  ENDIF 

  Read_PCGoes, file[0], limits, goesdata, hdr=hdr, status=status

  tmp =  strsplit( basename(file[0]) ,'.',/extract) 
  outfilebase = tmp[0]
  plotfile = outfilebase  + '-hist.ps'
  savefile = outfilebase + '-hist.save'

  IF NOT status THEN BEGIN 
    Message,'Error reading goes file <' + file[0] + '>',/cont
    return
  ENDIF 

  genv,/save
  tvlct,orig_red,orig_green,orig_blue,/get
  loadct,0,/silent

  GoesFilenameStruct = ParseGoesFileName( file[0] )
  IF VarType( GoesFilenameStruct ) NE 'STRUCTURE' THEN BEGIN 
    Message," Trouble parsing " + Goesfile ,/cont
    tvlct,orig_red,orig_green,orig_blue
    genv,/restore
    return
  ENDIF ELSE BEGIN 
    sat_name = GoesFilenameStruct.SatName + " " + $
     strtrim(GoesFilenameStruct.SatNum,2 )
  ENDELSE 
  sensornum = GoesFilenameStruct.Sensornum
  sensors = ['VIS','IR2','IR3','IR4']
  sensor    =  sensors[ sensornum-1 ]

  IR =  ( sensornum GT 1 )
  IF ir THEN GoesData = 1024-temporary(GoesData)

  hist1 = histogram( goesdata, min=0, max=1023, bin=1 )

  hist1 = 100.*hist1/total(hist1)
  cloudmask = scale( GoesData,minv=0,maxv=1024)*99

  goesdata =  bytscl( goesdata, min=0, max=1024, top=255)
  hist2 = histogram( goesdata, min=0, max=255 )
  hist2 = 100.*hist2/total(hist2)  


  brightmin = 0. &  brightmax=0.8 &  satmin=0. &  satmax=0.55
  xx = findgen(100)/99.
  bi = 0> interpol( [0.,1], [BrightMin, BrightMax], xx ) < 1
  si = 0> interpol( [1.,0], [SatMin,    SatMax],xx ) < 1

     ; Use 'cloudmask' to create new Brightness/Saturation values

  b2=bi[cloudmask]
  s2=si[cloudmask]


  bhist = histogram(b2,min=0,max=1,bin=0.01)
  shist = histogram(s2,min=0,max=1,bin=0.01)

  bhist = 100*bhist/total(bhist)
  shist = 100*shist/total(shist)

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
  plot,hist1,title=outfilebase + ' (fullrange)', ytitle='% of data'
  timestamp
  plot,hist2,title=outfilebasXe + ' (byte scaled)', ytitle='% of data'
  timestamp

  !p.multi = [0,1,2]
  xx = findgen(n_elements(bhist))*0.01
  plot,xx,bhist < 10,title=outfilebase,ytit='% of data',xtit='Brightness',psym=-1
  plot,xx,shist < 10,xtit='Saturation',ytit='% of data',psym=-1
  timestamp

  device,/close

  genv,/restore
  save,hist1,hist2,file=savefile
  

END

  




