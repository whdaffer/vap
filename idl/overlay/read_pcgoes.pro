;+
; NAME:  Read_PCGoes
; $Id$
; PURPOSE:  Read a file created by PCs gridding software.
;
; AUTHOR:  WHD
;
; CATEGORY:  Goes imageray
;
; CALLING SEQUENCE:  Read_PCGoes, filename, limits, data, $
;                   status=status, hdr=hdr
; 
; INPUTS:  
;
;   filename : fully qualified name of file containing gridded goes file
;
; OPTIONAL INPUTS:  None
;	
; KEYWORD PARAMETERS:  
;
;    Hdr: (O) - Contains file header, should there be one.
;
; OUTPUTS:  
;
; OPTIONAL OUTPUTS:  
;  Limits: A 4-vector, [lonmin, latmin, lonmax, latmax ]
;  data:   The data from the file.
;
; COMMON BLOCKS:  None
;
; SIDE EFFECTS:  None
;
; RESTRICTIONS:  None
;
; PROCEDURE: Open file, try to determine it's type from the first few
;            bytes, then read it according to that type. See the
;            documentation below and within the procedure itself.
;
; EXAMPLE:  Read_PCGoes, '/disk1/dir1/goes-file.dat', limits, data, hdr=hdr
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.5  1999/03/03 16:55:28  vapuser
; Set initial status =0
;
; Revision 1.4  1998/12/01 18:21:44  vapuser
; Put in some more error checking and tacked a righteous header
; on the file.
;
; Revision 1.3  1998/11/17 21:02:36  vapuser
; Added a 'year' field to the goes header, which required extraction.
;
; Revision 1.2  1998/09/15 00:06:15  vapuser
; Added the hdr=hdr return keyword
;
; Revision 1.1  1998/09/09 18:29:11  vapuser
; Initial revision
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

PRO read_pcgoes, file, limits, data, image, year, jday, time, $
                 nlon, nlat, lonsize, latsize, info_string, $
                 xs= xs, ys= ys , map = map,save= save, $
                 just_header= just_header, version= version, $
                 status= status , area_file= area_file, hdr=hdr

status =  0 ; dress for failure
on_error,2 
on_ioerror, ioerror_label
IF n_params() LT 1 THEN BEGIN 
  Usage,'Read_PcGoes,file [,limits, data, status=status, hdr=hdr ]'
  return
END

file = file[0]

sensors =  ['vis','ir1','ir2','ir3','ir4']
map =  keyword_set( map )
save =  keyword_set( save )

sat     = 0L
nlon    = 0L
nlat    = nlon
jday    = nlon
time    = nlon
limits  = lonarr(4)
year    = 0l
version = 0l
lonsize = 0.
latsize = 0.
; There are three versions of the grid file floating around each 
; with it's own header. All the quantities are longwords (limits is a
; 4-vector) except [lon/lat]size which are float and areafilename
; which is a 100 element byte array.
;
; Version 1: header=nlat,nlon,jday,time,limits,lonsize,latsize
; Version 2: header=sat, nlat,nlon,jday,time,limits,lonsize,latsize
; Version 3: header = area_filename in one write, followed by the
;            version 2 header in a second.
; Version 4: header = goes_hdr_str(1). File is no longer an
;            f77_unformatted file, and the first long word, the
;            'version' is 19970730 or greater, (i.e. 30-July-1998 or
;            later)

;
; Since Versions 1 through 3 of this file is written in f77 manner,
; each individual write is preceeded and followed by the number of
; bytes in the write. Using these quantities we'll be able to
; distinguish them.
;
; The method will be to open the file without the f77 qualifier, read
; the first longword and make decisions based on it. If the file has a
; version later than 19980730, then we read it as a stream file,
; otherwise, we'll close the file and reopen it as /f77.
;

rcsid = "$Id$"

test1 = 0l &  test2=0l
openr, rlun, file, /get,error=err ; non /f77 open
IF err NE 0 THEN BEGIN
  message, !err_string, /info
  close,/all
  GOTO, ioerror_label
ENDIF 
readu,rlun,test1, test2 ; read first 'nbytes' number
close,rlun   ; close

openr,rlun, file, /f77, error= err ; now open as f77 file
IF err NE 0 THEN BEGIN
  message, !err_string, /info
  close,/all
  GOTO, ioerror_label
ENDIF 

area_file =  '' ; define area_file
CASE test1 OF 
  19980730: BEGIN 
      ; This is the newest file, no longer f77_unformatted.
    close, rlun 
    openr, rlun, file, error=err ; non f77 open
    hdr = goes_hdr_str()
    readu,rlun,hdr
    sat = hdr.type
    jday = hdr.doy
    time = hdr.hhmm
    nlon = hdr.cols
    nlat = hdr.rows
    year = hdr.year
    lonsize =  hdr.resolution[0]
    latsize = hdr.resolution[1]

    limits = hdr.limits
    version =  3
  END 
  4: BEGIN
    CASE test2 OF 
      961023: BEGIN 
        ; Version 3
        tmp =  bytarr(100)
        readu, rlun, version
        readu,rlun,tmp
        area_file =  string(tmp)
        tmp =  0
        readu,rlun,year, sat,nlat, nlon, jday, time, limits, lonsize, latsize
      END 
      ELSE : BEGIN
        message,' Unkown version ' + strtrim( test2,2 ),/info
        status = 0
        GOTO, ioerror_label
      END
    ENDCASE
  END 
  ; Version 2
  44: BEGIN 
    readu,rlun, sat,nlat, nlon, jday, time, limits, lonsize, latsize
    version = 2
  END 
  ; Version 1
  40: BEGIN 
    readu,rlun,nlat, nlon, jday, time, limits, lonsize, latsize
    version = 1
  END 

ENDCASE 
IF keyword_set( just_header ) THEN BEGIN 
  status =  1
  close,/all
  return
ENDIF 
data =  intarr( nlon, nlat )
readu, rlun, data
free_lun, rlun 
status =  1

limits =  1.0*limits

jday=strtrim(jday,2)
time=strtrim(time,2)
latsize=string(latsize,form='(f4.2)')
lonsize=string(lonsize,form='(f4.2)')

hh = strtrim(time/100,2)
mm = strtrim(time-hh*100,2)
IF strlen(hh) EQ 1 THEN hh = '0' + hh
mm = strtrim(mm)
IF strlen(mm) EQ 1 THEN mm = '0' + mm
info_string =  file +  $
 '  Jday: '+ jday+ '   time '+ hh +':'+ mm + $
 '   lat/lon preci: '+latsize+' '+lonsize


IF map THEN $
  map_pcgoes, data, limits, image, xs, ys , tit=info_string

IF save THEN BEGIN
  sensor    =  sensors( strmid( file, 5,1 ) )
  satellite =  strmid( file,0,5 )
  date_str  =  strmid( file,7,7 )
  lim_str   =  strtrim( long(limits), 2 )
  lim_str =  '[' + lim_str(0) + ',' + lim_str(1) + ',' + $
                   lim_str(2) + ',' + lim_str(3) +  ']'
  dlm =  '_'
  filename =  satellite + dlm + sensor + dlm + $
    date_str + dlm + lim_str +'.save'
  save,image,xs,ys,f=filename
ENDIF 

GOTO, endlabel
ioerror_label: 
  status = 0
  close, /all
endlabel:

return
END



