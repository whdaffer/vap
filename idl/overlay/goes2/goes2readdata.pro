FUNCTION goes2readdata, datetime, region, sensor

  IF n_params() LT 1 THEN BEGIN 
    Usage,'goesimage=goesreaddata(datetime [,region, type])'
    return,''
  ENDIF 

  IF n_elements(region) EQ 0 THEN region =  'west'
  IF NOT isa(region,/string,/nonempty) THEN BEGIN 
    Message,"Parameter REGION must be non-empty string (east|west)",/cont
    return,''
  ENDIF 

  IF n_elements(sensor) EQ 0 THEN sensor =  'ir4'
  IF NOT isa(sensor,/string,/nonempty) THEN BEGIN 
    Message,$
     "Parameter SENSOR must be non-empty string (ir{1,2,3,4}|vis)",/cont
    return,''
  ENDIF 

  goes2topdir = getenv('GOES2TOPDIR')
  IF strlen(goes2topdir) EQ 0 THEN $
    Message,"Env Var GOES2TOPDIR must be defined"

  goes2_image_template =  0
  restore,goes2topdir + '/goes2_idl_templates.save'

  f = goes2topdir + '/' + $
        region + '/' + $
         sensor + '/' + $
          datetime + '.hdf'
  file = findfile(f,count=nf)

  IF nf EQ 0 THEN $
    Message,"No such file <" + f + ">"
  
  file = file[0]
  image = 0
  data = hdf_read(file,template=goes2_image_template)
;  fileid = hdf_sd_start(file, /read )
;  IF fileid LE 0 THEN $
;    Message,"Couldn't open file <" + file + ">"
;  image = 0l

;  idx = hdf_sd_nametoindex(fileid, 'Data_Set_2')
;  id = hdf_sd_select( fileid, idx )
;  IF id GT 0 THEN $
;    hdf_sd_getdata, id, image 

;  hdf_sd_endaccess, idx
;  hdf_sd_end, fileid

  image = data.image
  return, image
END
