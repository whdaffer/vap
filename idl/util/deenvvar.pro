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
; RESTRICTIONS:  Only works on Unices.
;                Must input a legitimate path/file string. Each of the
;                environmental variables used must entirely occupy the
;                space between two adjacent '/'s
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
; Revision 1.4  1998/10/30 22:15:21  vapuser
; Added some comments, squashed some bugs.
;
; Revision 1.3  1998/10/29 15:54:07  vapuser
; put in isFile keyword for handling files.
;
; Revision 1.2  1998/10/23 22:24:45  vapuser
; Added ability to handle vector input.
;
; Revision 1.1  1998/10/05 17:10:50  vapuser
; Initial revision
;
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION DeEnvVar, path, isFile=isFile
   ; replace environmental variables with their expansion
   rcsid = "$Id$"
   IF n_elements(path) EQ 0 THEN return,''

   IsFile = keyword_set(isFile)

   IF strpos(path, '$') EQ -1 THEN BEGIN 
     IF isFile THEN return, path ELSE BEGIN 
       IF GetChar(path,/last) NE '/' THEN return,path+'/'
     ENDELSE 
   ENDIF 

   np = n_elements(path)

   IF np EQ 1 THEN fullpath = '' ELSE fullpath = strarr(np)

   FOR p=0,np-1 DO BEGIN 
     tpath = path[p]
     IF getchar(tpath,/first) NE '/' THEN relative = 1 ELSE relative = 0

     tmp = str_sep(tpath,'/')
     FOR i=0,n_elements(tmp)-1 DO BEGIN 
       envpos = strpos(tmp[i],'$')
       IF envpos NE -1 THEN BEGIN 
         env = getenv(strmid(tmp[i],envpos+1,strlen(tmp[i])-(envpos+1)))
         IF env ne '' THEN BEGIN 
           partial_path = DeEnvVar(env,isfile=0)
           fullpath[p] = fullpath[p]+partial_path
         ENDIF ELSE BEGIN
           fullpath[p] = fullpath[p]+tmp[i] 
         ENDELSE 
       ENDIF ELSE fullpath[p] =  fullpath[p] + tmp[i]
           ; If it isn't a file, put a '/' at the end.
       IF getchar(fullpath[p],/last) NE '/' THEN $
           fullpath[p] = fullpath[p]+'/'

     ENDFOR 
     done = 0
     IF NOT relative THEN fullpath[p] = '/' + fullpath[p]
     s = strpos( fullpath[p], '//')
     WHILE s NE -1 DO BEGIN 
       fullpath[p] = strmid(fullpath[p],0,s+1) + $
          strmid( fullpath[p],s+2,strlen(fullpath[p])-s-1)
       s = strpos( fullpath[p], '//')
     ENDWHILE 
     IF isFile THEN BEGIN 
       IF GetChar(fullpath[p],/last) EQ '/' THEN $
        fullpath[p] = $
       strcompress(strmid(fullpath[p],0,strlen(fullpath[p])-1),/remove_all)
     ENDIF 
   ENDFOR 
   return, fullpath
END
