;+
; NAME:  assurecompressed
; $Id$
; PURPOSE:  Find the file or it's compressed counterpart. If it isn't compressed,
;          compress it. Return the name of the compressed file or the
;          empty string if some error occurs.
;
; AUTHOR:  whd
;
; CATEGORY:  
;
; CALLING SEQUENCE:  
; 
; INPUTS:  file: the name of the file whose compressedness you want to
;         assure. It doesn't matter whether this file exists on disk
;         or not, this routine will sort it out. If you pass 'foo' it
;         will compress and return 'foo.gz.' If you pass 'foo.gz' and
;         it finds 'foo' it will compress and return 'foo.gz' If you
;         pass 'foo.gz' and it finds 'foo.gz' it will silently return
;         that name.
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  the name of the compressed version, regardless of the
;          state it was in at the beginning of this routine.
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  Always compressed using 'gzip.'
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
FUNCTION assurecompressed, file
   catch, error
   IF error NE 0 THEN BEGIN 
     catch,/cancel
     message,!error_state.msg,/noname,/cont
     return, ''
   ENDIF 

   IF n_params() LT 1 THEN Message,'Usage: file=assurecompressed(file)'
   IF strlen(file) EQ 0 THEN Message,'zero-length filename!'

   parts = stregex(file,"(.*)\.*(Z|gz)*$",/extract) 
   IF NOT stregex(file,'(Z|gz)$',/boolean) THEN BEGIN 
     ;; didn't pass the compressed name
     tfile = parts[0]
     ff = findfile(tfile,count=nf)
     IF nf EQ 0 THEN BEGIN 
       ;; no uncompress version, check 
       ;; for the compressed version
       tfile = findfile(tfile + '.[gz]*',count=nf)
       IF nf EQ 0 THEN $
         Message,"Can't find either, compressed or uncompressed! <" + file + ">"
       retfile = tfile[0]
     ENDIF ELSE BEGIN 
       ff = ff[0]
       s = compress(ff)
       IF s THEN retfile =  ff ELSE Message,"Error compressing " + file
     ENDELSE 
   ENDIF ELSE BEGIN 
     ;; compressed name
     ff = findfile(file,count=nf)
     IF nf EQ 0 THEN BEGIN 
       ;; no compressed version, check 
       ;; for uncompressed
       tfile = parts[0]
       retfile = findfile(tfile,count=nf)
       IF nf EQ 0 THEN $
         Message,"Can't find either, compressed or uncompressed!"
       s = compress(retfile)
       IF s NE 1 THEN Message,"Error compressing " + file
       
     ENDIF ELSE retfile = tfile
   ENDELSE 

  return, retfile
END 
