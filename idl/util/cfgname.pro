;+
; NAME:  cfgname
; $Id$
; PURPOSE:  Construct the name of a CFG file from the name of the
;          calling routine.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Programming Utility
;
; CALLING SEQUENCE:  name_of_cfg_file = cfgname()
; 
; INPUTS:  None
;
; OPTIONAL INPUTS:  None
;	
; KEYWORD PARAMETERS:  None
;
; OUTPUTS:  A scalar string. This string is based on the name of the
;          caller. See 'example' below.
;
; OPTIONAL OUTPUTS:  None
;
; COMMON BLOCKS:  None
;
; SIDE EFFECTS:  None
;
; RESTRICTIONS:  None
;
; PROCEDURE:  Call dollar0(2) and construct the name from the return
;
; EXAMPLE:  Say you call this routine from the routine 'foobar.' Then,
;          the returned string is 'foobar.cfg'
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Copyright (c) 1999, William Daffer
;
;-
; No Warranties!
;
FUNCTION  cfgname, level
  IF n_elements(level) EQ 0 THEN level = 0
  return, strlowcase(dollar0(level+2)) + '.cfg'
END
