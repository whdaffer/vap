PRO USAGE, message
;+
; NAME:  Usage
; $Id$
; PURPOSE:  Print a Usage statement.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Programming Utility
;
; CALLING SEQUENCE:  Usage, 'usage string'
; 
; INPUTS:  
;
;  usage_string : A string detailing the usage of the routine
;
; OPTIONAL INPUTS:  None
;
; KEYWORD PARAMETERS:  None
;
; OUTPUTS:  A message is emitted e.g. Usage: Usage, usage_string
;
; OPTIONAL OUTPUTS:  None
;
; COMMON BLOCKS:  None
;
; SIDE EFFECTS:  None
;
; RESTRICTIONS:  None
;
; PROCEDURE:  Build the string 'Usage: ' + usage_string and emit using
;            'Message'
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Copyright (c) 1998, William Daffer
;-
  lf = string(10b)
  IF n_params() LT 1 THEN return
  IF VarType(message) NE 'STRING' THEN message = string(message)
  message =  lf  + "  Usage: " + message
  Message,message, /Noname, /NoPrefix,/continue
  return
END

  
