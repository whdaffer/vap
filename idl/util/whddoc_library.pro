;+
; NAME:  whddoc_library
; $Id$
; PURPOSE:  Replace Doc Library with a widget routine that will
;          display the header of a program in a widget so that a user
;          may read the documentation while actually calling to
;          program, thereby making this routine useful.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  documentation
;
; CALLING SEQUENCE:  whddoc_library,name
; 
; INPUTS: name. Scalar String. The name of the routine whose
;         documentation we wish to peruse.
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  none
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  A widget pops up with the documentation in it.
;
; RESTRICTIONS:  Must be able to set 'X' device.
;
; PROCEDURE:  parse the file for comment lines starting with ';+' and
;            ending with ';-'
;
; EXAMPLE:  whddoc_library,'animate_wind_field'
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.1  2001/02/02 21:41:32  vapuser
; Initial revision
;
;
; Copyright (c) 1999, William Daffer
; No Warranties
;-

PRO whddoc_library, name,force=force
  IF n_params() LT 1 THEN BEGIN 
    Usage,'whddoc_library, routine_name'
    ok = dialog_message("Usage: whddoc_library, routine_name")
    return
  ENDIF 

  catch, error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    Message,!error_state.msg,/info
    return
  ENDIF 

  headerfile = "/tmp/" + name + ".doc"
  IF NOT fexist(headerfile) OR keyword_set(force) THEN $
    doc_library, name, print="cat > " + headerfile,/multi

  xdisplayfile,headerfile, height=40,title=name,width=90
END
