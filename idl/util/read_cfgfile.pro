;+
; NAME:  read_cfgfile
; $Id$
; PURPOSE:  Read a Configuration file for the input program, if there
;          is any.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Programming Utility
;
; CALLING SEQUENCE:  return_structure=read_cfgfile(filename)
; 
; INPUTS:  
;
;  filename: Scalar String. The fully qualified filename of the
;            configuration file.
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  
;
;  Success: A structure where each tag is a keyword and the value is
;           it's value.
;  Failure: a null string
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
;  Configuration files consist of keyword=value records. Comments are
;  signaled by a ';' and anything after the semi-colon is
;  ignored. Anything is allowed in the keyword=value records that can
;  be parsed by the IDL interpreter, which is, in fact what's
;  done. That is, the lines are passed to the IDL builtin 'execute,'
;  with the result that after the (successful) 'execute,' the 'keyword'
;  has the value 'value.' The caller of the routine is entirely
;  responsible for using the information.
;
; PROCEDURE:  
;
; EXAMPLE:  
; Say your config file is named .xxx_yyy_zzz_rc and looks like this:
; (ignore the first column of semi-colons)
;
;
;  ; --- begin Config file for routine xxx_yyy_zzz -----
;  ;
;  ;  This is a comment.
;  ;
;  comment1='this is a comment'
;  maxblaster=2
;  stupid_array=[2,3,4,5] ; this is another comment.
;  stupid_struct={ a:0, b:1, c:2, d:'d string' }
;  ; End config file for routine xxx_yyy_zzz
;
;  To parse this routine, you call.
;
;  struct=read_cfgfile('/path/to/file/.xxx_yyy_zzz_rc')
;
;  the returned structure will be:
;
;   struct = { comment1      : 'this is a comment', $
;              maxblaster    : 2, $
;              stupid_array  : [2,3,4,5], $
;              stupid_struct : { a:0, b:1, c:2, d:'d string' } }
;
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.3  2001/02/20 20:24:23  vapuser
; Took out path. Now the user must pass in fully qualified
; file name.
;
; Revision 1.3  2001/02/20 20:22:19  vapuser
; Took out path. Now the user must pass in fully qualified
; file name.
;
; Revision 1.2  1999/10/11 17:17:29  vapuser
; Make it a procedure, not a function.
; This is so it can return undefined quantities.
;
; Revision 1.1  1999/10/07 18:50:39  vapuser
; Initial revision
;
;
;
;Copyright (c) 1999, William Daffer
;-
; No Warranties
;
PRO read_cfgfile, filename, retstruct
  
  IF n_params() LT 1 THEN BEGIN 
    Usage,"return_structure = read_cfgfile(filename)"
    return
  ENDIF 
   
  IF NOT fexist(filename) THEN BEGIN 
    Message,"CFG File <" +filename +"> doesn't exist!",/info
    return
  ENDIF 

  nn = nlines(filename)
  IF nn EQ 0 THEN BEGIN 
    Message, "CFG file <" +filename +"> is EMPTY!",/info
    return
  ENDIF 

  openr,lun, filename,/get,error=err
  IF err NE 0 THEN BEGIN 
    Message,!error_state.msg,/info
    return
  ENDIF 

  rec = ''
  line = 0
  REPEAT BEGIN 
    readf, lun, rec
    line = line+1
    rec =  strtrim(strcompress( rec ),2)

    IF strpos(rec,';') ne 0  AND strlen(rec) GT 0 THEN BEGIN 
      tmp = strsplit(rec,'=',/extract)
      tag = strcompress(tmp[0],/remove_all)
      value = strcompress( tmp[1],/remove_all)
      s = execute( 'v = ' + value )
      IF NOT s THEN BEGIN 
        Message,"Error at line " + strtrim(line,2) + $
          " in CFG file <" + filename ,/cont
        return
      ENDIF 
      IF NOT exist(retstruct) THEN $
        retstruct = create_struct( tag, v ) ELSE $
        retstruct = create_struct( retstruct, tag, v ) 
    ENDIF 
  ENDREP UNTIL eof(lun)

  free_lun, lun
END

