;+
; NAME:  Destroy
; $Id$
; PURPOSE:  Cleanly  destroy a variable by recursively traversing it's
;          structure, freeing pointers and destroying objects as we go.
; 
; AUTHOR: William H. Daffer
; 
; CATEGORY:  Utility
; 
; CALLING SEQUENCE: cleanDestroy,variable 
; 
; INPUTS:  
;
;  Variable: the IDL variable you want to destroy. 
;
;      If the variable is a simple scalar, i.e. a
;      byte/int/long/float/string or a structure that has no pointers
;      or ;objects, simply set theariable to 0 and return. If the
;      variable is more complex, particularly if it is a structure
;      with pointers, traverse the variable freeing pointers and
;      calling Obj_Destroy as you go.
;
; OPTIONAL INPUTS: None  
;       
; KEYWORD PARAMETERS:  None
;
; OUTPUTS:  The original variable is returned as a scalar byte = 0.
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  all pointer/object variables within the input
;               variable are freed/destroyed.
;
; RESTRICTIONS:  none
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION LOG:
;
; $Log$
;
;
;Copyright (c) 1999, William H. Daffer
;
;-
; No Warranties
;
PRO Destroy, v, struct_element=struct_element

  
  IF n_params() LT 1 THEN return
  IF n_elements(v) EQ 0 THEN return
  struct_element =  keyword_set(struct_element)

  IF n_elements(v) EQ 1 THEN BEGIN 
    CASE vartype(v) OF 
      'POINTER': ptr_free, v
      'OBJECT': obj_destroy,v
      'STRUCTURE': BEGIN 
         tags = tag_names(v)
         ntags =  n_elements(tags)
         FOR i=0,ntags-1 DO Destroy,v.(i),/struct_element
         IF NOT struct_element THEN v = 0
      END 
      ELSE: BEGIN 
        IF NOT struct_element THEN v = 0
      end
    ENDCASE 
  ENDIF ELSE BEGIN 
    CASE vartype(v) OF 
      'POINTER': ptr_free,v
      'OBJECT': obj_destroy,v
      'STRUCTURE': BEGIN 
        tags = tag_names(v)
        FOR i=0,n_elements(v)-1 DO destroy,v[i],/struct_element
        IF NOT struct_element THEN v = 0
       END 
       ELSE: IF NOT struct_element THEN v = 0
     ENDCASE 

  ENDELSE 
END
