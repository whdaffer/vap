;+
; NAME:  ls
; $Id$
; PURPOSE: Emulate the Unix command 'ls -al' 
;
; AUTHOR:  William Daffer
;
; CATEGORY:  File utility
;
; CALLING SEQUENCE:  ls,fileglob
; 
; INPUTS:  fileglob : Scalar string. something you'd have after 
;          'ls -al' in the Unix ls command
;
; OPTIONAL INPUTS:   none
;	
; KEYWORD PARAMETERS:   none
;
; OUTPUTS:  The listing is printed to the screen
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  none
;
; RESTRICTIONS:  Must be a UNIX system
;
; PROCEDURE:  Pass the string to a spawn 'ls -al' command
;
; EXAMPLE:  ls,'*.dat'
;
; MODIFICATION HISTORY:
;
; $Log$
;
; 
; Copyright (c) 1999, William Daffer
;
;-
; No warranties!
; 
PRO ls, string
  IF !version.os_family NE 'unix' THEN BEGIN 
    Message,"The command may only be used with a 'UNIX' type OS!",/cont
    return
  ENDIF 
  IF n_elements(string) EQ 0 THEN string =  ' * '
  spawn, 'ls -al ' + string,ret
  print,ret
END
