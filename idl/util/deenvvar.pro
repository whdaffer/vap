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
; Revision 1.1  1998/10/05 17:10:50  vapuser
; Initial revision
;
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION DeEnvVar, path

   ; replace environmental variables with their expansion
   rcsid = "$Id$"
   IF n_elements(path) EQ 0 THEN return,''
   np = n_elements(path)
   IF np EQ 1 THEN fullpath = '' ELSE fullpath = strarr(np)
   FOR p=0,np-1 DO BEGIN 
     tpath = path[p]
     tmp = str_sep(tpath,'/')
     FOR i=0,n_elements(tmp)-1 DO BEGIN 
       envpos = strpos(tmp[i],'$')
       env = getenv(strmid(tmp[i],envpos+1,strlen(tmp[i])-(envpos+1)))
       IF env ne '' THEN BEGIN 
         partial_path = DeEnvVar(env)
         fullpath[p] = fullpath[p]+partial_path
       ENDIF ELSE BEGIN
         fullpath[p] = fullpath[p]+tmp[i] 
       ENDELSE 
       IF strpos(fullpath[p],'/') NE strlen(fullpath[p])-1 THEN $
          fullpath[p] = fullpath[p]+'/'
       IF strmid(fullpath[p],0,1) NE '/' AND $
          strmid(fullpath[p],0,1) NE '.' THEN $
         fullpath[p] =  '/' + fullpath[p]
     ENDFOR 
     done = 0
     s = strpos( fullpath[p], '//')
     WHILE s NE -1 DO BEGIN 
       fullpath[p] = strmid(fullpath[p],0,s+1) + $
          strmid( fullpath[p],s+2,strlen(fullpath[p])-s-1)
       s = strpos( fullpath[p], '//')
     ENDWHILE 
   ENDFOR 
   return, fullpath
END
