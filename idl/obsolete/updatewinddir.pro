;+
; NAME:  Updatewinddit
; $Id$
; PURPOSE:  
;
;
; AUTHOR:
;
;
; CATEGORY:  Qscat VAP testing
;
;
;
; CALLING SEQUENCE:  
;
;
; 
; INPUTS:  
;
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;
;
; OUTPUTS:  
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
;
;
;
; SIDE EFFECTS:  
;
;
;
; RESTRICTIONS:  
;
;
;
; PROCEDURE:  
;
;
;
; EXAMPLE:  
;
;
;
; MODIFICATION HISTORY:
; $Log$
; Revision 1.2  1998/10/23 22:17:01  vapuser
; Moved some comments around.
;
; Revision 1.1  1998/10/22 21:19:58  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

PRO updatewinddir

  cd,current=curdir
  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    cd,curdir
    return
  ENDIF 
  cd,'$VAP_WINDS'

  f = findfile('./RMGDR/*.R')
  RNoaaReFormat, f

END


