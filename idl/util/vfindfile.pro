;+
; NAME:  vfindfile.pro
; $Id$
; PURPOSE:  Findfile in a Vap friendly way. Because of shell
;          limitations and the number of files we're working with, one
;          must 'cd' to the directory and do a listing of the files
;          therein, otherwise the line sent to the shell is too long
;          and findfile returns a null string. This is a kludge, I
;          know, but it's the only one I could come up with. 
;
; AUTHOR:  William Daffer
;
; CATEGORY:  VAP utility
;
; CALLING SEQUENCE:  files = vfindfile(filter [, path, count=count])
; 
; INPUTS:  
;
;    filter: scalar string: The file glob
;
; OPTIONAL INPUTS:  
;
;   path: the path. Defaults to './'
;	
; KEYWORD PARAMETERS:  count: return the number of files found
;
; OUTPUTS:  files: Vector of files matching path/filter
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
;    Cd to 'path' and do a findfile on 'filter.' The file list
;    returned contains no path information. The caller is
;    responsible for that.
;
; EXAMPLE:  
;
;  files=vfindfile('Q*','$VAP_WINDS')
;  files = deenvvar('$VAP_WINDS') + files ; put the path back in!
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION vfindfile, filter, path, count=count
  IF n_elements(filter) EQ 0 THEN filter = ''
  catch, Error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return,''
  ENDIF 
  IF n_elements(path) NE 0 THEN BEGIN 
    IF NOT isa(path,/string) THEN BEGIN 
      Message,"Parameter PATH must be a string!"
    ENDIF 
    
    cd,path,cur=curdir
    IF NOT isa(filter,/string) THEN BEGIN 
      Message,"Input Parameter FILTER must be a STRING!"
    ENDIF 
    files = findfile(filter, count=count)
    cd,curdir
  ENDIF ELSE files = findfile(filter,count=count)
  return,files
END
