;+
; NAME:  isa
; $Id$
; PURPOSE:  Argument type checking
;
; AUTHOR:  William Daffer
;
; CATEGORY:  See Purpose
;
; CALLING SEQUENCE:  true_false=isa(variable, /keyword )
;                     
; See 'keywords' for the explanation of the keyword
; argument
; 
; INPUTS:  
;   variable: any idl variable
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
; 
;   
;     byte : Checks to see if the variable is a byte 
;     short : Checks to see if the variable is a short integer
;     long : Checks to see if the variable is a long integer
;     float : Checks to see if the variable is a float
;     double : Checks to see if the variable is a double float
;     complex : Checks to see if the variable is a  complex
;     dcomplex : Checks to see if the variable is a double complex
;     string : Checks to see if the variable is a string
;     structure : Checks to see if the variable is a structure
;     object : Checks to see if the variable is a object
;     pointer : Checks to see if the variable is a pointer
;     type_integer : Checks to see if the variable is a byte, short
;                    or long.
;     type_float : Checks to see if the variable is a float or a double
;     type_complex : Checks to see if the variable is a complex or
;                    double complex
;     nonempty : if the check is for a string, this performs the
;                additional test of non-emptiness. NB, it will return
;                0 if *any* of the strings are empty
;     name     : if check is of 'structure' will perform the
;                additional test to see if the structure has the
;                correct name. Will only work for named structures!
;
; OUTPUTS:  1, i.e. true, if the variable is of the type specified by
;          the input keyword
;           0, i.e. false, if it isn't.
;           -1 if something went wrong.
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
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
;
; $Log$
; Revision 1.3  1999/03/04 18:51:53  vapuser
; Added name= keyword for structure tests.
;
; Revision 1.2  1999/03/04 00:28:00  vapuser
; Added 'nonempty' keyword for strings
;
; Revision 1.1  1999/02/01 20:12:39  vapuser
; Initial revision
;
;
;Copyright (c) 1999, William Daffer
;-

FUNCTION isa, variable, $
                  byte=byte, $
                  short=short, $
                  long=long, $
                  float=float, $
                  double=double, $
                  complex=complex, $
                  dcomplex=dcomplex, $
                  string=string, $
                  structure=structure, $
                  object=object, $
                  pointer=pointer, $
                  type_integer=type_integer, $
                  type_float=type_float, $
                  type_complex= type_complex, $
                  nonempty=nonempty, $
                  name=name



 
usage_msg = 'true_false=isa(variable, "followed by one of " ,/byte, /integer, /long, /float, /double, /complex, /dcomplex, /string, /structure, /object, /pointer, /type_integer, /float_type, /type_complex [,nonempty, name])'
  IF n_params() LT 1 THEN BEGIN 
    usage,usage_msg
    return,-1
  ENDIF 

  catch, error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    Message,!error_state.msg,/cont
    return,-1
  ENDIF 

  test = -1

  CASE 1 OF 
     keyword_set(byte) : test =  vartype( variable ) EQ 'BYTE'
     keyword_set(short) : test = vartype( variable ) EQ  'INTEGER'
     keyword_set(long) : test = vartype( variable ) EQ  'LONG'
     keyword_set(float) : test = vartype( variable ) EQ 'FLOAT'
     keyword_set(double) : test = vartype( variable ) EQ 'DOUBLE'
     keyword_set(complex) : test = vartype( variable ) EQ 'COMPLEX_FLOAT'
     keyword_set(dcomplex): test = vartype( variable ) EQ 'COMPLEX_DOUBLE'
     keyword_set(string) : BEGIN 
       test = vartype( variable ) EQ 'STRING'
       IF keyword_set( nonempty ) THEN BEGIN 
         ii = 0
         nn = n_elements(variable)
         REPEAT BEGIN 
           test =  test AND (strlen(variable[ii]) NE 0)
           IF test THEN ii = ii+1
         ENDREP UNTIL test EQ 0 OR ii EQ nn
       ENDIF
     END 
     keyword_set(structure) : BEGIN 
       test = vartype( variable ) EQ 'STRUCTURE'
       IF test AND exist(name) THEN $
         test = strupcase(tag_names(variable,/structure_name )) EQ strupcase(name) 
     end
     keyword_set(object) : test = vartype( variable ) EQ 'OBJECT'
     keyword_set(pointer) : test = vartype( variable ) EQ 'POINTER'
     keyword_set(TYPE_INTEGER) : test = vartype(variable) EQ 'BYTE' OR $
                            vartype(variable) EQ 'INTEGER'  OR  $
                            vartype(variable) EQ 'LONG'
     keyword_set(TYPE_FLOAT) : test = vartype(variable) EQ 'FLOAT' OR $
                     vartype(variable) EQ 'DOUBLE'
     keyword_set(TYPE_COMPLEX) : test =  vartype(variable) EQ 'COMPLEX_FLOAT' OR $
                                         vartype(variable) EQ 'COMPLEX_DOUBLE'
     ELSE: Usage,usage_msg
       
  ENDCASE 

  return,test
END
