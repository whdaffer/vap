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
;     idldt   : Checks to see if the variable[0] is an IDLDT structure.
;     type_integer : Checks to see if the variable is a byte, short
;                    or long.
;     type_float : Checks to see if the variable is a float or a double
;     type_complex : Checks to see if the variable is a complex or
;                    double complex
;     number   : True if not string, structure, object or pointer
;     nonempty : if the check is for a string, this performs the
;                additional test of non-emptiness. NB, it will return
;                0 if *any* of the strings are empty
;     name     : if check is of 'structure' will perform the
;                additional test to see if the structure has the
;                correct name. Will only work for named structures!
;     status   : returns 1 if there was no error, else returns 0.
;                Errors include: incorrect argumentation, an undefined
;                variable,  or anything caught by 'catch'
;
; OUTPUTS:  1, i.e. true, if the variable is of the type specified by
;          the input keyword
;           0, i.e. false, if it isn't, or something went wrong.
;           
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
; Revision 1.10  2000/03/17 00:41:43  vapuser
; Added IDLDT to the tests. Return 0 if input variable
; is undefined
;
; Revision 1.9  1999/07/04 16:55:28  daffer
; Added 'number' type.  True if
; not string,structure,object or pointer.
;
; Revision 1.8  1999/07/01 18:37:18  vapuser
; Fixed small bug in 'object' section
;
; Revision 1.7  1999/05/28 17:12:07  vapuser
; Put in test of classname for 'object' case
;
; Revision 1.6  1999/04/21 17:09:44  vapuser
; changed 'LONG' to 'LONGWORD'
;
; Revision 1.5  1999/03/16 18:42:44  vapuser
; Added a 'status' keyword. Changed to only return 0 or 1, error reports in
; status keyword, 1=no error, 0=error.
;
; Revision 1.4  1999/03/16 18:16:46  vapuser
; changed misspelling (.mst instead of .msg)
;
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
              idldt=idldt, $
              type_integer=type_integer, $
              type_float=type_float, $
              type_complex= type_complex, $
              number=number,$
              nonempty=nonempty, $
              name=name, $
              status=status, $
              objname=objname,$
              help=help



 
status = 1
usage_msg = 'true_false=isa(variable, "followed by one of " ,/byte, /integer, /long, /float, /double, /complex, /dcomplex, /string, /structure, /object, /pointer, /type_integer, /float_type, /type_complex, /number [,/nonempty, name=structure_name,objname=objname])'

  IF keyword_set(help) THEN BEGIN 
    usage,usage_msg
    status = 0
    return,0
  ENDIF 

  IF n_elements(variable) EQ 0 THEN BEGIN 
    status = 0
    return,0
  ENDIF 

  catch, error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    Message,!error_state.msg,/cont
    status = 0
    return,0
  ENDIF 

  IF n_elements(variable) EQ 0 THEN return,0
  test = 0

  CASE 1 OF 
     keyword_set(byte) : test =  vartype( variable ) EQ 'BYTE'
     keyword_set(short) : test = vartype( variable ) EQ  'INTEGER'
     keyword_set(long) : test = vartype( variable ) EQ  'LONGWORD'
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
         test = strupcase(tag_names(variable,/structure_name )) EQ $
           strupcase(name) 
     end
     keyword_set(object) : BEGIN 
         test = vartype( variable ) EQ 'OBJECT'
       IF test THEN BEGIN 
         IF n_elements(objname) NE 0 THEN BEGIN 
           IF obj_isa(variable,objname) EQ 0 THEN test = 0
         ENDIF 
       ENDIF 
     END 
     keyword_set(pointer) : test = vartype( variable ) EQ 'POINTER'
     keyword_set(idldt) : BEGIN 
       test = isa(variable[0],/structure)
       IF test THEN BEGIN 
         name =  tag_names(variable[0],/structure_name)
         test =  test AND name EQ 'IDLDT'
       ENDIF 
     END 
     keyword_set(TYPE_INTEGER) : $
        test = vartype(variable) EQ 'BYTE'    OR $
               vartype(variable) EQ 'INTEGER' OR  $
               vartype(variable) EQ 'LONGWORD'
     keyword_set(TYPE_FLOAT) : $
         test = vartype(variable) EQ 'FLOAT' OR $
                vartype(variable) EQ 'DOUBLE'
     keyword_set(TYPE_COMPLEX) : $
       test =  vartype(variable) EQ 'COMPLEX_FLOAT' OR $
               vartype(variable) EQ 'COMPLEX_DOUBLE'
     keyword_set(NUMBER) : $
      test= (vartype(variable) EQ 'STRING' OR $
             vartype(variable) EQ 'STRUCTURE' OR $
             vartype(variable) EQ 'OBJECT' OR $
             vartype(variable) EQ 'POINTER') EQ 0
     ELSE: BEGIN 
       Usage,usage_msg
       status = 0
     END 
       
  ENDCASE 

  return,test
END
