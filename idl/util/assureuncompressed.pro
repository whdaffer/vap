;+
; NAME:  assureuncompressed
; $Id$
; PURPOSE:  Find the file or it's uncompressed counterpart. If it isn't uncompressed,
;           uncompressed it. Return the name of the uncompressed file
;           or the empty string if some error occurs
;
; AUTHOR:  whd
;
; CATEGORY:  
;
; CALLING SEQUENCE: infile=assureuncompressed(infile)
; 
; INPUTS:  file -- scalar string. Name of file whose 'compressedness'
;         is to be checked. It doesn't matter whether the file has a
;         .Z or .gz ending, whether infile does or doesn't exist, this
;         routine will sort it all out. That is, if you pass something
;         like 'foo.gz' and that file doesn't exist, but 'foo' does,
;         this routine will silently return 'foo'. On the otherhand,
;         if you pass 'foo' but the file is 'foo.gz' on the disk, it
;         will uncompress it and return 'foo.'
;
;       
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  
;
;   success: The name of the uncompressed file, whether the file needed to be
;            uncompressed or not. 
;
;   failure: the empty string.
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
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1407 is acknowledged.
;-
FUNCTION assureuncompressed, file
   catch, error
   IF error NE 0 THEN BEGIN 
     catch,/cancel
     message,!error_state.msg,/noname,/cont
     return, ''
   ENDIF 

   IF n_params() LT 1 THEN Message,'Usage: file=assureuncompressed(file)'
   IF strlen(file) EQ 0 THEN Message,'zero-length filename!'
   parts = stregex(file,"(.*)\.*(Z|gz)*$",/extract) 
   IF NOT stregex(file,'(Z|gz)$',/boolean) THEN BEGIN 
     ;; uncompressed name was passed
     tfile = parts[0]
     ff = findfile(tfile,count=nf)
     IF nf EQ 0 THEN BEGIN 
       ;; no uncompress version, check 
       ;; for the compressed version
       tfile = findfile(tfile + '.[gz]*',count=nf)
       IF nf EQ 0 THEN $
         Message,"Can't find either, compressed or uncompressed! <" + file + ">"
       tfile = tfile[0]
       s = uncompress(tfile)
       IF s THEN retfile =  tfile ELSE Message,"Error uncompressing " + file
     ENDIF ELSE retfile = ff[0]
   ENDIF ELSE BEGIN 
     ;; compressed name was passed
     ff = findfile(file,count=nf)
     IF nf EQ 0 THEN BEGIN 
       ;; no compressed version, check 
       ;; for uncompressed
       tfile = parts[0]
       retfile = findfile(tfile,count=nf)
       IF nf EQ 0 THEN $
         Message,"Can't find either, compressed or uncompressed!"
       retfile = retfile[0]
     ENDIF ELSE BEGIN 
       ;; found the compressed name.
       ff = ff[0]
       s = uncompress(ff)
       retfile = ff
       IF s NE 1 THEN message,"Error uncompressing " + file
     ENDELSE 
   ENDELSE 

  return, retfile
END 
