;+
; NAME:  isanumber
; $Id$
; PURPOSE:  return 1 if this input string can be viewed as a number
;           0 otherwise
; AUTHOR:   William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: 1|0 = isanumber(string)
; 
; INPUTS:  String: string to test for numberness. May be a vector.
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  Verbose: set this flag for verbose output
;
; OUTPUTS:  A intarr of the same dimensions at 'string' containing a 1
;           or 0, depending on whether it's a number of not.
;
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  Requires the file isanumber.so file in the linkimage subdirectory.
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.1  1999/06/17 19:14:36  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION isanumber, field,verbose=verbose
  ; Determine whether the input is an ascii representation of a
  ; number.
  ; William Daffer.
  ; 3-Dec-1997
  ; Calls Call_external routine 'isanumber'

  on_error,0
  IF NOT isa(field,/string,/nonempty) THEN BEGIN 
    message,'Input parameter must be of type STRING',/cont
    return,0
  ENDIF 
  nf = n_elements(field)
  result = intarr(nf)
  FOR f=0,nf-1 DO BEGIN 
    unwanted_alphanum = $
     "!#$%&()*,/:;<=>?@ABCDFHIJKLMNOPQRSTUVWXYZ[\]^_abcdfhijklmnopqrstuvwxyz{|}~"
    verbose =  keyword_set( verbose )
    ; Some sanity checks on the string
    test =  (strpos( field[f], '-+') NE -1) OR (strpos( field[f], '+-') NE -1) OR $
            (strpos( field[f],"'" ) NE -1) OR (strpos( field[f],'"') NE -1) OR $
            (strpos( field[f],'`') NE -1) OR $
            NOT (xchar( field[f], unwanted_alphanum ))
    IF test THEN BEGIN 
      IF verbose THEN $
        message,' field contains illegal alphanumerics: ' + field[f],/cont
      result[f] = 0
    ENDIF ELSE BEGIN 
      home = GETENV('HOME')
      env = GetENV('IDL_RELEASE_ENV')
      linkimage_file = home + '/idl/'+ env + '/linkimage/isanumber.so'
      result[f] =  call_external( linkimage_file,'isanumber',field[f])
    ENDELSE 
  ENDFOR 
RETURN, result
END

