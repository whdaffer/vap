;+
; NAME:   Unpack_Where.pro
; $Id$
; PURPOSE:  Retrieve column/row information from a x=where() vector.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  General data munging
;
; CALLING SEQUENCE:  unpack_where, orignal_array, where_vector, col, row, ...
; 
; INPUTS:  
;
;  Original_Array: Or some other array that has the exact same
;                  dimensionality as the array on which the 'where'
;                  operation was performed.
;
;  where_vector: The result of the 'where' operation
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  There are as many outputs as there are dimensions in
;          'Original_array', with a minimum of 2.
;
;  vec1: corresponding to the first dimension of Original_array.
;  vec2: corresponding to the second dimension of Original_array.
;  vec3: corresponding to the third dimension of Original_array.
;  vec4: corresponding to the fourth dimension of Original_array.
;  
;
;
; OPTIONAL OUTPUTS:  none
; COMMON BLOCKS:  none
; SIDE EFFECTS:  none
; RESTRICTIONS:  none
;
; PROCEDURE:  
;
; EXAMPLE:  
;
;  EXAMPLE:
; 
; 	Let Array = indgen(2,2)
; 
;            Then array(0,0) = 0
;                 array(1,0) = 1
;                 array(0,1) = 2 and
; 		  array(1,1) = 3.
; 
;            Let X = where( (array mod 2) eq 0) ; find where Array is even 
;            
; 	    Then, x = (0,2) 
; 
;            Entering the following.
; 
; 	     unpack_where, array, x, col, row
; 
;  		yields Col = (0,0) and Row = (0,1)
;
;
;               So, array( col(0), row(0) ) = 0 and
;                    array( col(1), row(1) ) = 2
; 
; 
;  NOTE:	 The i-th entry in Col goes with the I-th entry in Row. 
;	         DON'T MIX AND MATCH.
; 
; 
; 
;   
; MODIFICATION HISTORY:
;
; $Log$
;
; $Id$
; -
Pro unpack_where,inarray,where_vector,vec1,vec2,vec3,vec4
On_error,2	; on error, return to caller
 
S  = size(inarray) ; Get the dimensions of inarray

IF n_params() LT 4 THEN BEGIN 
  message," Usage: Unpack_where, array, where_vector, vec1, vec2 [, vec3, vec4 ]",/cont
  message,"        Where vec1 unpacks the 1st index, vec2 the 2nd, [vec3 the 3rd ...] ",/cont
  return
ENDIF 
IF s(0) EQ 0 THEN BEGIN 
  message," Inarray is vector!",/cont
  return
ENDIF 
Case 1 of
 
  s(0) eq 2 : begin
    ncol = s(1)
    nrow = s(2)
    vec1 = where_vector mod ncol                      ; The Colomns
    vec2 = where_vector / ncol                        ; The Rows
  end
  
  s(0) eq 3 : begin
    nlev = s(1)
    ncol = s(2)
    nrow = s(3)
    vec1 = where_vector mod nlev                      ; The Levels
    vec2 = ( where_vector / nlev ) mod ncol           ; The Colomns
    vec3 = where_vector / ( nlev*ncol )               ; The Rows
  end              
 
  s(0) eq 4 :  begin
    ncube = s(1)
    nlev  = s(2)
    ncol  = s(3)
    nrow  = s(4)
    vec1  = where_vector mod ncube                    ; The Cubes
    vec2 = ( where_vector / ncube ) mod nlev          ; The Levels
    vec3 = where_vector / ( nlev*ncube ) mod ncol     ; The Colomns
    vec4 = where_vector / (ncube*nlev*ncol)	      ; The Rows
  end              
 
  else: begin
    message,'Can''t handle more than 4 dimensions yet ',/cont
  end
 Endcase
 
 
 
 Return
 End
