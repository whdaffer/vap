FUNCTION Goes10Constants, channel, detector
  
  IF n_params() LT 1 THEN BEGIN 
    Usage,"constants=Goes10Constants( channel [,detector])"
    return,''
  ENDIF 

  IF channel GT 5 OR channel LE 1 THEN BEGIN 
    Message,"channel must be >=2 and <= 5",/cont
    return,''
  ENDIF 

  IF n_elements(detector) EQ 0 THEN detector = 'A'
  detector = strupcase(detector)

  IF detector NE 'A' AND detector NE 'B' THEN BEGIN 
    Message,'detector must be A/B',/cont
    return,''
  ENDIF 

  constantsFile = '/usr/people/vapuser/Qscat/Library/goes10_constants.dat'
  openr, lun, constantsFile,/get_Lun, error=err
  constants = ''

  IF err EQ 0 THEN BEGIN 
    found = 0
    rec = ''
    WHILE NOT eof(lun) AND NOT found DO BEGIN 
      readf, lun, rec
      rec = strupcase( strcompress( strtrim(rec,2)))
      IF strpos(rec,';') eq -1 THEN BEGIN 
        tmp = str_sep(rec,' ')
        IF tmp[0] EQ channel AND tmp[1] EQ detector THEN BEGIN 
          found = 1
          constants = float(tmp[2:4])
        ENDIF 
      ENDIF 
    ENDWHILE 
    free_lun,lun
  ENDIF ELSE Message,!error_State.msg,/cont
  return,constants
END

  
