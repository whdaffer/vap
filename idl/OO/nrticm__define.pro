;+
; NAME:   ntricm__define
; PURPOSE:   Defines and object of type ntricm
;
; AUTHOR; William Daffer
;
; CATEGORY:   OO
;
; CALLING SEQUENCE:   ntricm = obj_new('ntricm',...)
; 
; METHODS:
;   Init:
;   Set:
;   Get:
;   Cleanup:
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

;============================================
; Init
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  

;============================================

FUNCTION ntricm::Init, file
   self-> read, file
  return,1
END


;============================================
; Cleanup
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO ntricm::Cleanup
END


;============================================
; Set Routine
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO ntricm::Set, file = file
   IF n_elemenets(file) NE 0 THEN BEGIN 
     self-> read,file
   ENDIF 
END


PRO ntricm::read, file
   IF n_elemenets(file) NE 0 THEN BEGIN 
     openr, lun, file, error=err, /get
     IF err NE 0 THEN BEGIN 
       Message,!error_state.msg,/cont
       return
     ENDIF 
     array =  self.icemask
     readu, lun, array
     self.icemask =  temporary(array)
   ENDIF 
END


;============================================
; Get Routine
; 
;     INPUTS:  
;     OPTIONAL INPUTS:  
;     KEYWORD PARAMETERS:  
;     OUTPUTS:  
;     OPTIONAL OUTPUTS:  
;     COMMON BLOCKS:  
;
;============================================

PRO ntricm::Get, file = file, icemask=icemask
   IF arg_present(file) THEN file = self.file
   IF arg_present(icemask) THEN icemask = self.icemask
END

;============================================
; Definition Routine
;============================================

PRO ntricm__define
  junk = {ntricm, $
          icemask: bytarr(720,360), $
         file: ''}
END
