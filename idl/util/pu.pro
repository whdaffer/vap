; NAME:
;	PU
;
; PURPOSE:
;	Replicate something of the functionality of IDL's PUSHD.pro
;	extending it so that it works something like unix's pushd.
;
; CALLING SEQUENCE:
;	PU, [Dir [,n]]
;
; INPUTS:
;	Dir:	The directory to change to. The current directory will
;		be pushed to the top of the directory stack.
; OPTIONAL INPUTS
;       N:      A number. The stack is rotated around that number of
;               positions, right if posive, left if negative.
;
; SIDE EFFECTS:
;	The current directory is pushed onto the directory stack.
;	It will be the next directory used by POPD.
;
; COMMON BLOCKS:
;	DIR_STACK:  Contains the stack.
;
; MODIFICATION HISTORY:
;	17, July, 1989, Written by AB, RSI.
;      
; $Log$
;
; $Id$
;-
;
;
PRO pu,dir,v=v
verbose = keyword_set(v)

COMMON DIR_STACK, DEPTH, STACK
IF (n_elements(DEPTH) eq 0) then depth = 0

on_error, 2		; Return to caller on error
type = vartype(dir)
IF type EQ 'STRUCTURE' OR type EQ 'COMPLEX' OR $
 type EQ 'COMPLEX_FLOAT' OR type EQ 'COMPLEX_DOUBLE' THEN $
  MESSAGE,'dir must be string, integer or float'

IF type EQ 'UNDEFINED' THEN BEGIN 
  IF depth LE 1 THEN BEGIN 
    message,' No other directory ',/cont
    return
  ENDIF ELSE BEGIN 
    tmp = stack(1)
    cd,tmp
    stack(1) =  stack(0)
    stack(0) = tmp
  ENDELSE 
ENDIF ELSE IF type NE 'STRING' THEN BEGIN 
    ; dir is a number of some sort
  
  nn =  fix(dir) MOD depth
  stack = shift( stack, -nn )
  CD, stack(depth-1)
ENDIF ELSE BEGIN 
  CD, dir, CURRENT=cwd
  IF (DEPTH eq 0) THEN BEGIN 
    STACK = [dir, CWD] 
    depth = depth+1
  ENDIF ELSE STACK = [dir, STACK]
  DEPTH = DEPTH + 1
ENDELSE 

IF verbose THEN dirs

end


