;+
; NAME:   Dollar0
; $Id$
;
; PURPOSE: Return the name of the caller or the caller of the
;          caller.... This is a way to easily get the name of the
;          current routine, in case you want to use it to construct a
;          log file, for instance. The name of this routine, is by way
;          of the analogy to the $0 variable of Unix Shell scripts or
;          argv[0] of C programs.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Programming Utility
;
; CALLING SEQUENCE:  Inside the routine whose name you want to
;                   capture, say routine_name=dollar0(). If you're two
;                   levels down the stack, i.e. the routine whose name
;                   you want has called a routine which in turn calls
;                   dollar0, you say routine_name=dollar0(2)
; 
; INPUTS:  level: scalar integer. The number of levels to go down the
;         traceback stack. Default=1
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  The name of the routine at that level of the traceback
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  none
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
;
;Copyright (c) 1999, William Daffer
;-
; No Warranties

FUNCTION dollar0,level
  IF n_elements(level) EQ 0 THEN level = 1
  help,/traceback,output=output
  x = where( strpos(output ,'%') NE -1,nx)
  IF level GT nx-1 THEN BEGIN 
    Message,"Traceback isn't that deep!",/cont
    return,''
  ENDIF 
  output = strcompress(output[x[level]])
  tmp = str_sep(output,' ')
  output = tmp[1]
  return,output
END

