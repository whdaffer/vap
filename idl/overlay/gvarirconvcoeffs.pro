FUNCTION GvarIRConvCoeffs, channel

  IF NOT exist(channel) THEN BEGIN 
    Usage,"conversionCoefficients=GvarIrConvCoeffs(channel)"
    return,''
  ENDIF 


  IF channel GT 5 OR channel LE 1 THEN BEGIN 
    Message,"channel must be >=2 and <= 5",/cont
    return,''
  ENDIF 

  ConvConFile = "/usr/people/vapuser/Qscat/Library/gvar_conversion_constants.dat"
  openr,lun, ConvConFile,/get,error=err
  IF err NE 0 THEN BEGIN 
    Message,!Error_State.msg,/cont
    return,''
  ENDIF 

  retval = ''
  found = 0
  rec = ""
  WHILE NOT eof(lun) AND NOT found DO BEGIN 
    readf, lun, rec
    rec = strcompress(strtrim(rec,2))
    IF strpos(rec,';') EQ -1 THEN BEGIN 
      tmp = str_sep(rec,' ')
      IF tmp[0] EQ channel THEN BEGIN 
        found = 1
        gain = float(tmp[1])
        bias = float(tmp[2])
        retval = [ bias, gain]
      ENDIF 
    ENDIF 
  ENDWHILE 
  free_lun,lun
  return, retval
END
