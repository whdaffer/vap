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
; Revision 1.4  1999/10/05 17:29:19  vapuser
; who cares?
;
; Revision 1.3  1998/11/20 19:55:28  vapuser
; Changed over to using RNoaaReformat
;
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
  cd,'$VAP_DATA_TOP',cur=cur

  f = findfile('./RMGDR/*.R')
  RNoaaReFormat, f
  cd,cur

END


