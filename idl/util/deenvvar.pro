;+
; NAME: DeEnvVar  
; $Id$
; PURPOSE:  Replaces environmental variables with their expansion
;
;
; AUTHOR; William Daffer
;
;
; CATEGORY:  O/S interoperation
;
;
;
; CALLING SEQUENCE:  expanded_envars=DeEnvVar(path)
;
;
; 
; INPUTS:  path - The string containing the environmental variables.
;
;
;
; OPTIONAL INPUTS:  None
;
;
;	
; KEYWORD PARAMETERS:  None
;
;
;
; OUTPUTS:  the De Environmental Variable'd string
;
;
;
; OPTIONAL OUTPUTS:  None
;
;
;
; COMMON BLOCKS:  None
;
;
;
; SIDE EFFECTS:  None
;
;
;
; RESTRICTIONS:  Must input a legitimate path string.
;
;
;
; PROCEDURE:  
;
;
;
; EXAMPLE:  
;
;    Say ENV1 = /disk1/dir1/dir2 and ENV2=/dir3/dir4 and
;    s=DeEnvVar('$ENV1/$ENV2'), then
;
;    S = /disk1/dir1/dir2/dir3/dir4.
;
;
;
; MODIFICATION HISTORY:
; $Log$
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION DeEnvVar, path

   ; replace environmental variables with their expansion
   rcsid = "$Id$"
   IF n_elements(path) EQ 0 THEN return,''
   tpath = path
   tmp = str_sep(tpath,'/')
   fullpath = '/'
   FOR i=0,n_elements(tmp)-1 DO BEGIN 
     envpos = strpos(tmp[i],'$')
     env = getenv(strmid(tmp[i],envpos+1,strlen(tmp[i])-(envpos+1)))
     IF env ne '' THEN BEGIN 
       partial_path = DeEnvVar(env)
       fullpath = fullpath+partial_path
     ENDIF ELSE BEGIN
       fullpath = fullpath+tmp[i] 
     ENDELSE 
     IF strpos(fullpath,'/') NE strlen(fullpath)-1 THEN fullpath = fullpath+'/'

   ENDFOR 
   done = 0
   s = strpos( fullpath, '//')
   WHILE s NE -1 DO BEGIN 
     fullpath = strmid(fullpath,0,s+1) + strmid( fullpath,s+2,strlen(fullpath)-s-1)
     s = strpos( fullpath, '//')
   ENDWHILE 
   return, fullpath
END
