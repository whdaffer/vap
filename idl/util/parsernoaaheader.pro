;+
; NAME:  parsernoaaheader.pro
; $Id$
; PURPOSE:  
;
; Takes a header from one of RSDs Reduced Noaa NRT files (i.e. the
; native MGDR  with the sigma0s stripped off the end) and converts
; it to keyword: value format, returning a structure whose tags are
; the keywords and whose values are the keywords values. 
;
;

; AUTHOR:  William Daffer
;
; CATEGORY:  Qscat Reduced NOAA (SeaNRT) Data I/O Manipulation
;
; CALLING SEQUENCE:  parsed_header = parsernoaaheader(header)
; 
; INPUTS:  header: The first record of the Noaa SeaNRT data record
;         (doesn't matter whether it's reduced or not if there is only
;         one such header record, which Scott assures me is true.)
;
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  A structure where the keyword is the keyword part of each
;          field and the value is it's value.
;
; OPTIONAL OUTPUTS:  none
; COMMON BLOCKS:  none
; SIDE EFFECTS:  none
;
; RESTRICTIONS:  
;
; Assumptions:
;  1. Only 1 record.
;  2. Keyword/Value pairs are line-feed delimited
;  3. Time is specified as yyyy-doyThh:mm:ss.ccc
; 
;

; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.1  1999/04/09 21:43:42  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION parsernoaaheader, header
  IF n_params() NE 1 THEN BEGIN 
    Message,'Usage: retstruct=ParseRnoaaHeader(header)',/cont
    return,-1
  ENDIF 
  retstruct = -1
  ndims =  size(header,/n_dimensions )
  sz = size(header, /dimensions )
  IF ndims GT 1 THEN nrecs =  sz[1] ELSE nrecs =  1
  lf = string(10b)
  FOR nn=0l,nrecs-1 DO BEGIN 
    rec = string(header[*,nn])
    tmp = str_sep( rec, lf )
    ntags =  n_elements(tmp)
    tags = strarr(ntags)
    values = tags
    FOR ii=0,ntags-1 DO BEGIN 
      tmp2 = str_sep(tmp[ii],'=')
      tag =  strupcase(strcompress(tmp2[0],/remove_all))
      value =  strupcase(strtrim( tmp2[1],2 ))
      IF tag NE 'SPARE_METADATA_ELEMENT' THEN BEGIN 
        IF nn EQ 0 AND ii EQ 0 THEN $
          retstruct =  create_struct( tag, value ) ELSE $
          retstruct =  create_struct( retstruct, tag, value ) 
      ENDIF 
    ENDFOR 
  ENDFOR 

  tags = tag_names(retstruct)
  FOR i=0,n_elements(tags)-1 DO BEGIN 
    value = retstruct.(i)
    CASE tags[i] OF 
      'STARTTIME': BEGIN 
        value = strcompress(value,/remove_all)
        year = strmid(value,0,4)
        doy = strmid(value,5,3)
        hh = strmid(value,9,2)
        mm = strmid(value,12,2)
        date = doy2date(fix(year),fix(doy))
        month = PadAndJustify( date[0], 2, /right )
        day = PadAndJustify( date[1], 2, /right )
        new_start_time = year + '/' + month + '/' + $
             day + '/' + hh + '/' + mm
        retstruct.(i) =  new_start_time
      END
      'ENDTIME': BEGIN 
        value = strcompress(value,/remove_all)
        year = strmid(value,0,4)
        doy = strmid(value,5,3)
        hh = strmid(value,9,2)
        mm = strmid(value,12,2)
        date = doy2date(fix(year),fix(doy))
        month = PadAndJustify( date[0], 2, /right )
        day = PadAndJustify( date[1], 2, /right )
        new_end_time = year + '/' + month + '/' + $
             day + '/' + hh + '/' + mm
        retstruct.(i) =  new_end_time
      END
      ELSE:
    ENDCASE 
  ENDFOR 
  return, retstruct
END
