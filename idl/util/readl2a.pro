;+
; NAME:  readl2a
; $Id$
; PURPOSE:  read the level 2a
;
; AUTHOR:  whd
;
; CATEGORY:  trmm/qscat
;
; CALLING SEQUENCE:  
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
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.1.1.1  2001/11/30 23:57:08  vapuser
; Initial Checkin
;
;
;Jet Propulsion Laboratory
;Copyright (c) 2001, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1407 is acknowledged.
;-

FUNCTION readl2a, file

  IF n_params() LT 1 THEN BEGIN 
    Message,"Usage: data = readl2a(file)",/cont
    return,''
  ENDIF 
  catch, error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    Message,!error_state.msg,/cont
    return,''
  ENDIF 

  sd =  hdf_sd_start(file)
  l2a =  {l2a};

  index =  hdf_sd_nametoindex(sd,'row_number')
  sds =  hdf_sd_select( sd, index)
  hdf_sd_getinfo, sds, caldata=cal
  hdf_sd_getdata, sds, data
  hdf_sd_endaccess, sds

  data =  data*cal.cal + cal.cal_err
  l2a.row =  temporary(data)
  


  index =  hdf_sd_nametoindex(sd,'num_sigma0')
  sds =  hdf_sd_select( sd, index)
  hdf_sd_getinfo, sds, caldata=cal
  hdf_sd_getdata, sds, data
  hdf_sd_endaccess, sds

  data =  data*cal.cal + cal.cal_err
  l2a.num_sigma0 =  temporary(data)


  index =  hdf_sd_nametoindex(sd,'num_sigma0_per_cell')
  sds =  hdf_sd_select( sd, index)
  hdf_sd_getinfo, sds, caldata=cal
  hdf_sd_getdata, sds, data
  hdf_sd_endaccess, sds

  data =  data*cal.cal + cal.cal_err
  l2a.num_sigma0_per_cell =  temporary(data)

;   index =  hdf_sd_nametoindex(sd,'cell_lat')
;   sds =  hdf_sd_select( sd, index)
;   hdf_sd_getinfo, sds, caldata=cal
;   hdf_sd_getdata, sds, data
;   hdf_sd_endaccess, sds

;   data =  data*cal.cal + cal.cal_err
;   l2a.lat =  temporary(data)

;   index =  hdf_sd_nametoindex(sd,'cell_lon')
;   sds =  hdf_sd_select( sd, index)
;   hdf_sd_getinfo, sds, caldata=cal
;   hdf_sd_getdata, sds, data
;   hdf_sd_endaccess, sds

;   data =  data*cal.cal + cal.cal_err
;   l2a.lon =  temporary(data)


  attr=hdfgetattr( file, attribute='EquatorCrossingLongitude')
  eqxlon = float( (*attr.value)[2])
  lonlat = qswath(eqxlon)
  lon = lonlat[*,*,0]
  lat = lonlat[*,*,1] &  lonlat=0

  l2a.lon =  lon[*,l2a.row]
  l2a.lat =  lat[*,l2a.row]


  index =  hdf_sd_nametoindex(sd,'sigma0')
  sds =  hdf_sd_select( sd, index)
  hdf_sd_getinfo, sds, caldata=cal
  hdf_sd_getdata, sds, data
  hdf_sd_endaccess, sds

  data =  data*cal.cal + cal.cal_err
  l2a.sigma0 =  temporary(data)

  index =  hdf_sd_nametoindex(sd,'sigma0_qual_flag')
  sds =  hdf_sd_select( sd, index)
  hdf_sd_getinfo, sds, caldata=cal
  hdf_sd_getdata, sds, data
  hdf_sd_endaccess, sds

  data =  data*cal.cal + cal.cal_err
  l2a.qual =  temporary(data)

  index =  hdf_sd_nametoindex(sd,'sigma0_mode_flag')
  sds =  hdf_sd_select( sd, index)
  hdf_sd_getinfo, sds, caldata=cal
  hdf_sd_getdata, sds, data
  hdf_sd_endaccess, sds

  data =  data*cal.cal + cal.cal_err
  l2a.mode =  temporary(data)

  index =  hdf_sd_nametoindex(sd,'cell_index')
  sds =  hdf_sd_select( sd, index)
  hdf_sd_getinfo, sds, caldata=cal
  hdf_sd_getdata, sds, data
  hdf_sd_endaccess, sds

  data =  data*cal.cal + cal.cal_err
  l2a.index =  temporary(data)

  index =  hdf_sd_nametoindex(sd,'surface_flag')
  sds =  hdf_sd_select( sd, index)
  hdf_sd_getinfo, sds, caldata=cal
  hdf_sd_getdata, sds, data
  hdf_sd_endaccess, sds

  data =  data*cal.cal + cal.cal_err
  l2a.surface =  temporary(data)

  hdf_sd_end, sd
  return, l2a
END
