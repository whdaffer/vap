FUNCTION GvarCnt2Radiance, data, channel
  IF n_params() NE 2 THEN BEGIN 
    Usage,"radiances=GvarCnt2Radiance(CountData,channel)"
    return,""
  ENDIF 

  coeffs = GvarIRConvCoeffs(channel)
  IF vartype(coeffs) NE 'FLOAT' THEN BEGIN 
    Message,'GvarConvCoeffs Failure',/cont
    return,''
  ENDIF 

  return, (data-coeffs[1])/coeffs[0]
END

  
