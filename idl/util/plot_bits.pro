;+
; NAME:  Plot_Bits.pro
; $Id$
; PURPOSE: Plots a array of bytes (assumed to be a flag of some sort) as
;          individual bits. The i-th bit of the flag goes between i
;          and i+0.5, depending on whether it's off or on,
;          respectively. 
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: plot_bits, byte_array 
; 
; INPUTS:  byte_array: array of bytes, assumed to be some kind of flag.
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  a plot
;
; OPTIONAL OUTPUTS:   none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  none
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
;Copyright (c) 1996, William Daffer
;-
; No Warranties

PRO plot_bits, in
s=size(in)
if s(0) +s(1) eq 0 then BEGIN 
  message,' input variable is undefined ',/cont
  return
ENDIF 


type = s( s(0) + 1 )
savesym = !psym
!psym = 3
plot,.5*( (in and 1) ne 0), yran=[0,8*(2L^(type-1)) ]
for bit=1,8*(2L^(type-1))-1 do oplot,bit + .5*( (in and 2l^bit) ne 0)
!psym = savesym

return
end
