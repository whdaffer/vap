;+
; NAME:  MakeDep
; $Id$
; PURPOSE:  Find module dependencies. It's meant to be run as part of
;           a shell script/idl batch file to be used in determining 
;           module dependencies. See 'procedure' below.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Software Maintenance
;
; CALLING SEQUENCE:  MakeDep, sourcefile, output-from-help, outfile=outfile
; 
; INPUTS:  
;   sourcefile: The file for which these are the dependencies.
;   output-from-help: The output from a help,/source,output=output
;      i.e.                                   this part --->^^^^^^^
; 
;
; OPTIONAL INPUTS:  None
;       
; KEYWORD PARAMETERS:  None
;
; OUTPUTS: sourcefile.dep: A file in the current directory detailing
;          the dependency info.
;
; OPTIONAL OUTPUTS:  None
;
; COMMON BLOCKS:  None
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  This procedure must be run immediately after the
;               source file is compiled and 'resolve_all' is run.
;
;
; PROCEDURE:  Say you want to find the dependencies of the module
;            foo.pro. Create the following idl batch script
;
;    ; start idl batch script
;    .run foo.pro
;    resolve_all
;    help,/source,output=output
;    makedep, 'foo.pro', output
;      ;end idl batch script.
;
;    This routine will find the 'non-idl' dependencies and write them
;    to the file 'foo.dep'
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Copyright (c) 1999, William Daffer
;-


PRO makedep, sourcefile, dependencies

  IF n_params() LT 2 THEN BEGIN 
    Usage,'makedep,sourcefile,dependencies'
    return
  ENDIF 

  IF NOT isa(sourcefile,/string,/nonempty) THEN BEGIN  
    Message,'parameter SOURCEFILE must be non-empty string',/cont
    return
  ENDIF 


  
  IF NOT isa(dependencies,/string) THEN BEGIN  
    Message,'parameter DEPENDENCIES must be non-empty string',/cont
    return
  ENDIF 

  x = where(strlen(dependencies) NE 0, nx )
  IF nx EQ 0 THEN BEGIN 
    Message,'No Dependencies!',/cont
    return
  ENDIF 
  tdep = dependencies[x]

  x = where( strpos(tdep,'rsi') EQ -1, nx )
  IF nx EQ 0 THEN BEGIN 
    Message,'No NON-RSI dependencies!',/cont
    return
  ENDIF 
    
  tdep = strtrim(strcompress(tdep[x]),2)


  IF strpos(sourcefile,'.') NE -1 THEN $
    tt = str_sep(sourcefile,'.') ELSE tt = sourcefile
    outfile = tt[0] + '.dep'
  openw, lun, outfile,/get,error=err
  IF err NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return
  ENDIF 
  
  nd = n_elements(tdep)
  retdep = strarr(nd)
  ii = 0
  FOR i=0,nd-1 DO BEGIN 
    tt = str_sep(tdep[i],' ')
    IF n_elements(tt) EQ 2 THEN BEGIN 
      retdep[ii] = tdep[i]
      ii = ii+1
    ENDIF 
  ENDFOR 
  retdep = retdep[0:ii-1]
  

  printf,lun, sourcefile
  printf,lun,transpose(retdep)
  free_lun,lun

END
