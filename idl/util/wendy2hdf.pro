;+
; NAME:   wendy2hdf
; $Id$
; PURPOSE: Convert a file from Wendy into HDF data that can be read and
;       plotted using PV and other of our software.  
;
; AUTHOR:  
;
; CATEGORY:  
;
; CALLING SEQUENCE:  wendy2hdr, file
; 
; INPUTS:  filename
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
; RESTRICTIONS:  data in file must be 720 by 301.
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.1  1999/10/06 23:01:29  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

PRO wendy2hdf, file

  IF n_params() LT 1 THEN BEGIN 
    Usage,"Wendy2HDF, file"
    return
  ENDIF 

  IF NOT isa(file, /string ) THEN BEGIN 
    Message,'File must be a STRING',/cont
    return
  ENDIF 

  IF strlen(file[0]) EQ 0 THEN BEGIN 
    Message,'File must be a String of Non-Zero length',/cont
    return
  ENDIF 

;  catch, error
;  IF error NE 0 THEN BEGIN 
;    Message,!error_state.msg,/cont
;    return
;  ENDIF 

  openr,lun, file,/get, error=err
  IF err EQ 0 THEN BEGIN 
    u = fltarr(720,301)
    v = u
    readu,lun,u,v
    free_lun,lun
    x = where( u EQ -9999.,nx)
    IF nx NE 0 THEN u(x) =  !values.f_nan

    x = where( v EQ -9999.,nx)
    IF nx NE 0 THEN v(x) =  !values.f_nan

    
    slash = strpos(file,'/',/reverse_search)+1
    base = strmid(file,slash,strlen(file)-slash)
    tmp = strsplit(base,'.',/extract) ; assumes file has extension
    time = tmp[0]
    
    year = '19' + strmid(base,0,2)
    month = strmid(base,2,2)
    day = strmid(base,4,2)
    hour = strmid(base,6,2)
    end_dt = var_to_dt( long(year), long(month), long(day), long(hour) )
    start_dt =  dt_subtract(end_dt, hour=24)
    dt_to_var, start_dt, year=year, month=month,day=day,hour=hour

    year = long(year)
    month = long(month)
    day = long(day)
    hour = long(hour)
    min = 59l
    sec = 59l
    
    start_time =  strjoin( [ strtrim(year,2), PadAndJustify( month, 2, /right ), $
                             PadAndJustify(day,2,/right), PadAndJustify(hour,2,/rig), $
                             '00','00' ], '/' )

    dt_to_var, end_dt, year=year, month=month,day=day,hour=hour

    year = long(year)
    month = long(month)
    day = long(day)
    hour = long(hour)
    min = 59l
    sec = 59l
    
    end_time =  strjoin( [ strtrim(year,2), PadAndJustify( month, 2, /right ), $
                             PadAndJustify(day,2,/right), PadAndJustify(hour,2,/rig), $
                             '59'], '/' )



    creation_time = end_time

    file_time =  strjoin( [ strtrim(year,2), PadAndJustify( month, 2, /right ), $
                             PadAndJustify(day,2,/right), PadAndJustify(hour,2,/rig), $
                             '00'],'' )

    
    filename = 'QIF-' + file_time + '.hdf'
    LONGname = 'Wendys Succor Interpolated Winds'
    Shortname =  'QSCATVAPMODEL'
    version = '1.0'
    Lonpar = [0.,359.5,0.5]
    LatPar = [-75.,75.,0.5]
    Region = [0.,-75.,359.5,75]

    status = QmodelHdfWrite( filename, u,v, ShortName=shortname, $
                             longname=longname, version=version, $
                             creationtime=creation_time, starttime=start_time, $
                             endtime=end_time, lonpar=lonpar, $
                             latpar=latpar, region=region)
    IF NOT status THEN $
      Message,'Write failed!',/cont
    
  ENDIF ELSE Message,!error_state.msg,/cont

END
