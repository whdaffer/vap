FUNCTION GetGoesConstants, satellite, channel, detector
  IF n_params() LT 2 THEN BEGIN 
    Usage,"constants=GetGoesConstants(sat,chan,det)"
    return,''
  ENDIF 

  IF n_Elements(detector) EQ 0 THEN detector =  'A'
  detector =  strupcase(detector)
  constants = ''
  CASE satellite OF 
    8: BEGIN 
      constants = Goes8Constants( channel, Detector)
    END
    10: BEGIN 
      constants = Goes10Constants( channel, Detector)
    END
    ELSE: Message,"Satellite must either 8 or 10",/cont
  ENDCASE 

  return,constants
END
