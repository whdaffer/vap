;+
; NAME:  DB.pro
; $Id$
; PURPOSE:  Delete Breakpoints
;
;
; AUTHOR: Me
;
;
; CATEGORY: Debugging 
;
;
;
; CALLING SEQUENCE:  Db, list_of_breakpoints
;
;
; 
; INPUTS:  
;
;  DB : a list of breakpoints
;
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;
;
; OUTPUTS:  
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  
;
;
;
; SIDE EFFECTS:  
;
;
;
; RESTRICTIONS:  
;
;
;
; PROCEDURE:  
;
;
;
; EXAMPLE:  
;
;
;
; MODIFICATION HISTORY:
; $Log$
;

;Copyright (c) 1998, William Daffer
;-

PRO DB, bp, bp1, bp2, bp3, bp4, bp5, bp6, bp7
  Rcsid = "$Id$"
  catch, error
  IF error NE 0 THEN BEGIN 
    Message,!error_state.msg,/cont
    return
  ENDIF 

  IF VarType( bp ) eq 'STRING' THEN bp = ExpandColList(bp)

  IF n_elements(bp1) NE 0 THEN BEGIN 
    IF VarType( bp1 ) eq 'STRING' THEN bp1 = ExpandColList(bp1)
    bp = [bp,bp1]
  ENDIF 
  IF n_elements(bp2) NE 0 THEN BEGIN 
    IF VarType( bp2 ) eq 'STRING' THEN bp2 = ExpandColList(bp2)
    bp = [bp,bp2]
  ENDIF 
  IF n_elements(bp3) NE 0 THEN BEGIN 
    IF VarType( bp3 ) eq 'STRING' THEN bp3 = ExpandColList(bp3)
    bp = [bp,bp3]
  ENDIF 
  IF n_elements(bp4) NE 0 THEN BEGIN 
    IF VarType( bp4 ) eq 'STRING' THEN bp4 = ExpandColList(bp4)
    bp = [bp,bp4]
  ENDIF 
  IF n_elements(bp5) NE 0 THEN BEGIN 
    IF VarType( bp5 ) eq 'STRING' THEN bp5 = ExpandColList(bp5)
    bp = [bp,bp5]
  ENDIF 
  IF n_elements(bp6) NE 0 THEN BEGIN 
    IF VarType( bp6 ) eq 'STRING' THEN bp6 = ExpandColList(bp6)
    bp = [bp,bp6]
  ENDIF 
  IF n_elements(bp7) NE 0 THEN BEGIN 
    IF VarType( bp7 ) eq 'STRING' THEN bp7 = ExpandColList(bp7)
    bp = [bp,bp7]
  ENDIF 
  
      
  FOR i=0,n_elements(bp)-1 DO breakpoint,/clear,bp(i)
END
