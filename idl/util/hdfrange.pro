FUNCTION hdfrange, file, doy=doy
  IF n_params() LT 1 THEN BEGIN 
    Usage,"time_range=hdfrange(file)"
    return,''
  ENDIF 
  IF NOT isa(file,/string,/nonempty) THEN BEGIN 
    Message,'File must be non-empty string!',/cont
    return,''
  ENDIF 

  IF NOT hdf_ishdf(file) THEN BEGIN 
    Message,'File must be HDF file!',/cont
    return,''
  ENDIF 
  type = hdfgetattr(file,attr='SHORTNAME')
  type =  (*(type.value))[0]
  CASE  type OF 
    'QSCATVAPMODEL': BEGIN 
      starttime = hdfgetattr(file,attr='STARTTIME')
      endtime = hdfgetattr(file,attr='ENDTIME')
      range = [ (*(starttime.value))[0], (*(endtime.value))[0] ]
      doy = [-1,-1]
    END 
    'QSCATL2B': BEGIN 
      beginningdate = hdfgetattr(file, attr='RangeBeginningDate')
      IF NOT isa(beginningdate,/structure) THEN BEGIN 
        Message,"Can't get beginning date!",/cont
        return,''
      ENDIF 
      IF NOT ptr_valid( beginningdate.value ) THEN BEGIN 
        Message,"BeginningDate Pointer NOT VALID!",/cont
        return,''
      ENDIF 

      start_date = (*(beginningdate.value))[0]
      tmp =  str_sep(start_date, '-')
      year = tmp[0]
      start_doy = tmp[1]
      date = doy2date(fix(tmp[0]),fix(tmp[1]))


      beginningtime = hdfgetattr(file, attr='RangeBeginningTime')
      IF NOT isa(beginningtime,/structure) THEN BEGIN 
        Message,"Can't get beginning time!",/cont
        return,''
      ENDIF 
      IF NOT ptr_valid( beginningtime.value ) THEN BEGIN 
        Message,"BeginningTime Pointer NOT VALID!",/cont
        return,''
      ENDIF 


      start_time =  year + '-' + date[0] + '-' + date[1] + 'T' + $
        (*(beginningtime.value))[0]

      endingdate = hdfgetattr(file, attr='RangeEndingDate')
      IF NOT isa(endingdate,/structure) THEN BEGIN 
        Message,"Can't get ending date!",/cont
        return,''
      ENDIF 
      IF NOT ptr_valid( endingdate.value ) THEN BEGIN 
        Message,"EndingDate Pointer NOT VALID!",/cont
        return,''
      ENDIF 

      start_date = (*(endingdate.value))[0]
      tmp =  str_sep(start_date, '-')
      year = tmp[0]
      end_doy = tmp[1]
      date = doy2date(fix(tmp[0]),fix(tmp[1]))


      endingtime = hdfgetattr(file, attr='RangeEndingTime')
      IF NOT isa(endingtime,/structure) THEN BEGIN 
        Message,"Can't get ending time!",/cont
        return,''
      ENDIF 
      IF NOT ptr_valid( endingtime.value ) THEN BEGIN 
        Message,"EndingTime Pointer NOT VALID!",/cont
        return,''
      ENDIF 

      end_time =  year + '-' + date[0] + '-' + date[1] + 'T' + $
        (*(endingtime.value))[0]

      doy = [ start_doy, END_doy ]
      range = [start_time, END_time]

    END 
    ELSE: BEGIN 
      Message,'Unrecognized HDF type <' + type + '>',/cont
      return,0
    END 
  ENDCASE 

  return, range
END
