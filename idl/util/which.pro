;+
; NAME:  Which
; $Id$
; PURPOSE:  Like the Unix 'which' program. Tells you which source file
;          is being run.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: which,'command' 
; 
; INPUTS:  command. An IDL procedure/function
;
; OPTIONAL INPUTS:  None
;	
; KEYWORD PARAMETERS:  None
;
; OUTPUTS:  The line from help,/source that contains that module
;          name and the line with the source file in it.
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
;
;Copyright (c) 1999, William Daffer
;-

PRO which, procname

  tproc =  strupcase( strtrim( strcompress( procname,/remove_all),2) )
  help,/source,out=out
  x = where( strpos( out, tproc) NE -1, nx )
  IF nx NE 0 THEN BEGIN 
    test =  strpos(out[x],'/') 
    x1 = where( test EQ -1, nx1 )
    x2 = where( test NE -1, nx2 )
    IF nx1 NE 0 THEN BEGIN 
      x1 =  [ x[x1], x[x1]+1]
      x1 = x1[uniq(x1),sort(x1)]
    ENDIF 
    IF nx2 EQ 0 THEN x = x1 ELSE BEGIN 
      x = [x1,x2]
      x = x[uniq(x,sort(x))]
    ENDELSE 
    print,out[x] 
  ENDIF ELSE print,''

END
