;+
; NAME:  ChkCFG
; $Id$
; PURPOSE:  Merge keyword/parameter information with information read
;          from a config file
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Programming Utility
;
; CALLING SEQUENCE:  
;
; chkcfg, name, val, cfg_struct, rval, boolean=boolean
; 
; INPUTS:  
;  name: The 'name' of the keyword or parameter to check. 
;  val: Whatever is passed in on the command line. May be non-existant
;  cfg_struct: A structure returned by read_cfgfile. It may or may not
;              exist. If it doesn't, or it is a scalar string, the
;              routine just returns parameter 1 (val)
;
;  See 'example' for more detailed information
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  
;
;  boolean: Flag. The thing being checked is a
;                     boolean keyword, not a parameter or a
;                     keyword=value type of keyword.
;
; OUTPUTS:  rval
;
;  The nature of the output depends on whether a parameter or a
;  keyword is being checked, which depends on whether the caller sets
;  the 'keyword' keyword. The general rule is that the command line
;  over rides the config file. But in the case of boolean keywords,
;  there isn't as clear a way to distinguish between when a user
;  explicitly sets the boolean to 'false' and when it's false by
;  default (i.e. keyword_set(keyword) eq 0 because the user hasn't
;  passed it in) In the case of parameter, the basic idea is that the
;  config file only take effect when the parameter isn't present on
;  the command line. In the case of keywords, the basic idea is that
;  unless user explicitly passes keyword=0, the config file takes
;  precedence.
;
;  The basic (pseudo) code is:
;  if val doesn't exist then 
;     if name is in cfg_struct then the output = cfg_struct.name else
;     ravl is undefined
;  else
;      rval=val
;  endif 
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  none
;
; RESTRICTIONS:  none
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
;
;-
; No Warranties!

PRO chkcfg, name, val, cfg_struct, boolean=boolean
  bool = keyword_set(boolean)
  IF n_elements(cfg_struct) NE 0 THEN BEGIN 
    tags = tag_names(cfg_struct)
    x =  where( strpos(tags, strupcase(name) ) NE -1, nx )
    IF nx NE 0 THEN $
      sval = cfg_struct.(x[0])
    IF n_elements(val) eq 0 THEN BEGIN 
        IF n_elements(sval) NE 0 THEN val = sval ELSE $
          IF bool THEN val = 0 
    ENDIF 
;    IF keyword_set(boolean) THEN BEGIN 
;    ENDIF ELSE BEGIN 
;    ENDELSE 
  ENDIF ELSE BEGIN 
    IF bool AND n_elements(val) EQ 0 THEN val = 0
  ENDELSE 
END
