;+
; NAME: Backup
; $Id$
; PURPOSE: Create a backup of a file
;
; CATEGORY: Utility
;
; CALLING SEQUENCE: status=backup(file [,extsep = extsep]
;
; INPUTS: 
;
;   File: Scalar string. The fully qualified name of the file to backup.
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS: 
;
;    extsep: A scalar string. The character used to separate
;            extensions. (default='.')
;
; OUTPUTS: 
; 
;  Status: 1=success, 0=some error.
;  
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
;  The file named by the input parameter 'file' is copied to a new
;  file named 'file.#' where # is the next backup number. If there are,
;  as yet, no backups, the file will be copied to file.1
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
; Revision 1.1  1999/06/17 20:13:14  vapuser
; Initial revision
;
;-

FUNCTION backup, file, extsep=extsep

   catch, error
   IF error NE 0 THEN BEGIN 
     Message,!error_state.msg,/cont
     close,/all
     return,0
   ENDIF 

   IF n_elements(extsep) EQ 0 THEN sep = '.' ELSE sep = extsep

   IF fexist( file ) THEN BEGIN    
     ext =  extension(file,extsep=sep)
     newext = '1'
     test =  file + '*'
     ff = findfile(test,count=nff)
     IF nff GT 1 THEN BEGIN 
       test =  file + sep + '*'
       ff = findfile(test,count=nff)
       IF nff GE 1 THEN BEGIN 
         ext = extension( ff, extsep=sep)
         good = where(isanumber(ext),ngood)
         IF ngood NE 0 THEN $
           newext =  strtrim( max( fix(ext[good]) )+1, 2)
       ENDIF 
     ENDIF 

     backupfile = file + sep + newext

     openr,lun, file, /get, error=err
     IF err NE 0 THEN Message,!error_state.msg

     Message,'Copying file <' +file +'> to file <' + backupfile + '>',/info
     openw, lun1, backupfile, /get, error=err
     IF err NE 0 THEN Message,!error_state.msg

     rec = ''
     WHILE NOT eof(lun) DO BEGIN 
       readf,lun,rec
       printf,lun1,rec
     ENDWHILE
     close,/all

     Message,'Done ',/info
   ENDIF 
  return,1

END
