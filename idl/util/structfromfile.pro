;+
; NAME:  
; $Id$
; PURPOSE:  
;
;
; AUTHOR:
;
;
; CATEGORY:  
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
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION continued_line, lun, startoline, string
  continued = 1
  line = startoline
  rec = ''
  REPEAT BEGIN 
    readf, lun, rec
    IF NOT string THEN rec =  strtrim(strcompress( rec,/remove_all ),2)
    test =  strpos( rec, '$' ) EQ -1
    IF test THEN continued = 0 ELSE $ 
      rec =  strmid( rec, 0,strlen(rec)-1 )
    line = line + rec
  ENDREP UNTIL continued EQ 0
return, line
END

FUNCTION structfromfile, filename, status

    ; Parses a file that's arranged as 'keyword : value ' type
    ; constructs, just like an IDL structure definition. Continuations
    ; are indicated by a '$' as the last character in the line.  Blank
    ; lines are ignored. Everything after a ';' is ignored.  It is an
    ; error to have a $ anywhere in a non-string value except at the
    ; end of the line. Strings may be delimited by " or ' and interior
    ; quotes can be included in the usual manner, by having two
    ; adjacent or by using the other delimiter.


  on_error,2
  struct = ''
  status = 0
  openr,lun, filename, /get_lun, error=err
  array = 0
  string = 0
  IF err EQ 0 THEN BEGIN 
    rec = ''
    WHILE NOT eof(lun) DO BEGIN 
      READF,lun, rec
      test =  strpos( rec,";") 
      IF test NE -1 THEN rec =  strmid(rec,0,test-1)
        ; eliminate all white space.
      rec =  strtrim( strcompress(rec,/remove_all),2) 
      IF strlen(rec) NE 0 THEN BEGIN 
          ; -- Test for colon
        test1 =  strpos( rec, ':' ) 
        IF test1 NE -1 THEN BEGIN
            ; keyword : value (possibly continuing) pair
          keyword = strmid( rec, 0, test1 )
          rec =  strmid( rec, test1+1, strlen(rec)-1 )
            ; test for 'array' constructs
          array  =  strmid(rec,0,1) EQ '['
          start =  array EQ 1
          string = strmid(rec,start,1) EQ "'" OR $
           strmid(rec,start,1) EQ '"'
            ; Test for continued line
          test2 = strpos( rec,'$') 
          IF test2 NE -1 THEN BEGIN 
            ; there's a continuation mark. If it's at the end or
            ; beginning of the line, it's okay, but in the interior,
            ; it must be in a string.  Is it inside a string?
            IF test2 EQ strlen(rec)-1 OR test2 EQ 0 THEN  BEGIN 
              IF test2 EQ strlen(rec)-1 THEN $
               rec =  strmid( rec, 0, strlen(rec)-1) ELSE $
               rec =  ''
                value =  CONTINUED_LINE( lun, rec,string )
            ENDIF ELSE IF NOT string THEN  BEGIN 
                struct = ''
                str =  "Continuation Character '$' must be " + $
                 " last non-white character in line"
                message,str
            ENDIF 
          ENDIF ELSE value = strmid(rec,0,strlen(rec))
        ENDIF ELSE BEGIN 
            ; Contains only the value, may be part of a continued
            ; line

            ; Test for continuation sign
          test2 = strpos( rec,'$') 
          IF test2 EQ strlen(rec)-1 OR test2 EQ 0 THEN  BEGIN 
            rec =  strmid( rec, 0, strlen(rec)-1)
            value =  CONTINUED_LINE( lun, rec,string )
          ENDIF ELSE IF NOT string THEN BEGIN 
            struct = ''
            str =  "Continuation Character '$' must be " + $
             " last non-white character in line"
            message,str
          ENDIF 
        ENDELSE
        exe_string =  'value = ' + value
        IF NOT execute( exe_string ) THEN BEGIN 
          struct = ''
          message,!err_string
        ENDIF 
        IF vartype(struct) EQ 'STRING' THEN $
          struct =  create_struct( keyword, value ) ELSE $
          struct =  create_struct( struct, keyword, value )


      ENDIF  ; test on strlen(rec) ne 0
    ENDWHILE 
  ENDIF ELSE message,!err_string
RETURN, struct
END

