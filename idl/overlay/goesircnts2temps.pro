FUNCTION GoesIrCnts2Temps, Counts, satellite, channel, detector

  IF n_Params() LT 3 THEN BEGIN 
    Usage,"Temps=GoesIrCnts2Temps(Counts,satellite,channel,detector)"
    return,''
  ENDIF 

  IF n_elements(counts) EQ 0 THEN BEGIN 
    Message,'Counts must be defined',/cont
    return,''
  ENDIF 
  IF n_Elements(detector) EQ 0 THEN detector = 'A'
  detector = strupcase(detector)
  
  temps = ''
  Radiances = GvarCnt2Radiance( counts, channel )

  constants =  GetGoesConstants( satellite, channel, detector )

  IF vartype(Constants) EQ 'FLOAT' THEN BEGIN 
    nu = constants[0]
    EffTemps = EffectiveTemp(radiances, nu)
    a = constants[1]
    b = constants[2]
    temps = b*temporary(EffTemps)+a
  ENDIF 

  return,Temps

end
