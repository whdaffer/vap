;+
; NAME:  IsQ2B
; $Id$
; PURPOSE:  Returns True if HDF and Q2B
;
;
; AUTHOR: William Daffer
;
;
; CATEGORY:  HDF Data I/O
;
;
;
; CALLING SEQUENCE:  
;
;        ret_val=IsQ2B(filename)
;
;
; 
; INPUTS:   
;
;     Filename: fully qualified HDF filename
;
;
;
; OPTIONAL INPUTS:  None
;
;
;	
; KEYWORD PARAMETERS:  None
;
;
;
; OUTPUTS:  
;
;   ret_val :  0 = not a Qmodel file
;              1 = Qmodel file
;             -1 = something failed that prevented us from
;                  determining wether it is or isn't.
;

;
;
; OPTIONAL OUTPUTS:  None
;
;
;
; COMMON BLOCKS:  None
;
;
;
; SIDE EFFECTS:  
;
;
;      Flakeyness in the HDF interface requires me to warn the user
;      that sometimes IDL leaves it in such a state that subsequent
;      calls to the interface leads to IDL crashing through no real
;      fault of the user.
;
;
;
; RESTRICTIONS:  
;
;
;
; PROCEDURE:  
;
;      Open file, get 'shortname' attribute. If it returns as
;      Q2B' return true. If it isn't there or is
;      different from Q2B, return 0. If there is a failure,
;      return -1, unless the failure is that the file isn't HDF, in
;      which case, return 0, since any file advertised to be HDF which
;      isn't, can't be Q2B.
;
;
;
;
;
; EXAMPLE:  
;
;
;
; MODIFICATION HISTORY:
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION IsQ2B, filename

  ; 0 means 'not a Q2B file'
  ; 1 mean 'Q2B file'

  ;
  ; -1 means some other failure
  ; 
  ;
  
  rcsid = "$Id$"
  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!Error_State.Msg,/cont
    return,-1
  ENDIF 
  IF n_Params() NE 1 THEN BEGIN 
    Message, "Usage: ret=IsQ2b(filename)",/cont
    return,-1
  ENDIF 

  IF NOT HDF_IsHdf(filename) THEN BEGIN 
    Message,'Input file ' + filename + ' is Not an HDF file!',/cont
    return,0 ; Can't be Q2b if not HDF.
  ENDIF 

    ; Must be an HDF file, Open it and look for 'shortname' attribute

  fid = Hdf_Sd_Start( filename, /Read )

  IF fid gt 0 THEN BEGIN 
    hdf_sd_fileinfo,fid,datasets,attributes
    found = 0
    ai = 0;
    shortname = ''
    WHILE ai LE attributes-1 AND NOT found DO BEGIN 
      hdf_sd_attrinfo,fid,ai,name=name,type=type,count=count,data=data
      name = strupcase(name)
      IF name EQ 'SHORTNAME' THEN BEGIN 
        found = 1
        tmp = str_sep( data, lf )
        tmp = tmp(where(strlen(tmp)))
        data =  tmp(n_elements(tmp)-1)
        shortname =  strupcase(data)
      ENDIF 
      ai = ai+1
    ENDWHILE 
    result =  (found AND (strpos( shortname,'Q2B' ) NE -1 ))
  ENDIF ELSE BEGIN 
    Message,"Error opening file " + filename
    result = -1 ; Don't know whether Q2B or not.
  ENDELSE 


  RETURN,result
END 
