;+
; NAME:  sortidldt
; $Id$
; PURPOSE:  Sort and array of IDLDTs
;
;
; AUTHOR: William Daffer
;
;
; CATEGORY:  Time Routine/Sorting
;
;
;
; CALLING SEQUENCE:  
;
;     sorting_index = sortidldt( idldt )
;
; 
; INPUTS:  
;
;      IDLDT: An Array of IDLDts
;
;
; OPTIONAL INPUTS:   Nont
;
;
;	
; KEYWORD PARAMETERS:  None
;
;
;
; OUTPUTS:  
;
;     Sorting_index: An array of long indices giving the input array
;                    in sorted order. 
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
; SIDE EFFECTS:  None
;
;
;
; RESTRICTIONS:  None
;
;
;
; PROCEDURE:  
;
;   Convert the times to vaptimes, then call sort on that array
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

FUNCTION SortIdlDt, idldt
  rcsid = "$Id$"
  sorted_indices = 0
  IF VarType( idldt ) EQ 'STRUCTURE' THEN BEGIN 
    IF Tag_Names( idldt[0], /structure ) EQ 'IDLDT' THEN BEGIN 
      IF n_params() EQ 1 THEN BEGIN 
        vaptimes = IdlDt2Vaptime(idldt)
      ENDIF 
      sorted_indices = sort(temporary(vaptimes))
    ENDIF 
  ENDIF 
  return, sorted_indices
END





