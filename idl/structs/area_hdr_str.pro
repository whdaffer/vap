;+
; NAME:  Area_Hdr_Str.pro
; $Id$
; PURPOSE:  Defines an IDL structure to be used in reading the Headers
;          of the Noaa AREA files.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  NOAA Goes AREA data manipulation
;
; CALLING SEQUENCE:  structure=area_hdr_str() 
; 
; INPUTS:  none
;
; OPTIONAL INPUTS: none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  A structure of the type given below.
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  area_hdr_cmn - for keeping track of whether this
;                                 structure has already been defined.
;                       
;
; SIDE EFFECTS: Memory goes away  
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
;Jet Propulsion Laboratory
;Copyright (c) YYYY, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION area_hdr_str
;
COMMON area_hdr_cmn, area_hdr_defined 
;
IF n_elements( area_hdr_defined ) eq 0 THEN BEGIN
  ; This record definition taken from 
  ; http://climate-f.gsfc.nasa.gov/~chesters/text/AREADOC
  ; 1995 McIDAS area file documnetation
  ;
  area_hdr =  { area_hdr ,$
                valid     : 0l ,$ ; =0 => valid
                format    : 0l ,$ ; should always be 4
                sensrc    : 0l ,$ ; sensor source
                date      : 0l ,$ ; date (YYDDD) image data collected
                time      : 0l ,$ ; HHMMSS UTC image data collected
                lcorr     : 0l ,$ ; image line coord of area line 0, element 0
                elcorr    : 0l ,$ ; "     element coord of "  "   "   "      "
                spare1    : 0l ,$ ; unused
                nlines    : 0l ,$ ; number lines in area
                nels      : 0l ,$ ; number of elements
                nbytes    : 0l ,$ ; number of bytes per elements (1,2,4)
                lres      : 0l ,$ ; line resolution
                eres      : 0l ,$ ; element resolution
                maxbands  : 0l ,$ ; max # bands/line
                dblplen: 0l ,$    ; length of data block line prefix
                                  ; (total([dblpdoc, dblpcal, dblplmap])
                                ; + 4 if validcode is set.
                userpjct  : 0l ,$ ; McIDAS user project # of creator process
                date_crtd : 0l ,$ ; date AREA file was created
                time_crtd : 0l ,$ ; time AREA file was created
                band_map  : 0l ,$ ; filter band map for multichannel images
                satspec1  : lonarr(5) ,$ ; satellite specific info #1
                memo      : lonarr(8) ,$ ; 32 ascii char comment
                filenum   : 0L ,$ ; last 4 digits of AREA file number
                dataoffset: 0l ,$ ; byte offset of DATA block
                navoffset : 0l ,$ ; byte offset of NAV block
                validcode : 0l ,$ ; validity code. see note below or above URL
                satspec2  : lonarr(12) ,$ ; satellite specific info #2
                dblpdoc   : 0L ,$ ; Byte Length of DATA block line prefix 
                                  ; documentation region
                dblpcal   : 0L ,$ ; Byte length of DATA block line prefix 
                                  ; calibration region
                dblplmap  : 0L ,$ ; Byte length of DATA block line prefix
                                  ; level map region
                imsrctype : 0l ,$ ; image source type, VISR, VAS, AAA, ERBE, AVHR
                caltype   : 0l ,$ ; units in which the digital data is stored
                                  ; e.g. RAW, TEMP, BRIT
                inspare   : lonarr(6) ,$ ; internal use, =0
                auxoffset : 0L ,$ ; byte offset of beginning of area file's AUX block
                auxlen    : 0L ,$ ; byte length of area file's AUX block
                spare2    : 0L ,$ ; spare
                caloffset : 0L ,$ ; byte offset of area file's CAL block
                ncomments : 0L  $ ; number of comment blocks in AUDIT block
                                  }
                             
  area_hdr_defined = 1

; Note from McIDAS documentation of item 'validcode:
;   validity code; contains zeros if this area does not    
;   have validity codes; if these bytes are non-zero,      
;   they must match the first four bytes of each DATA      
;   block line prefix or the line's data is ignored;       
;   this word is usually constructed from the date         
;   and time of the Area Directory creation;               
;   YYDDDHHMMSS                                            






ENDIF

RETURN, replicate( { area_hdr })
end





