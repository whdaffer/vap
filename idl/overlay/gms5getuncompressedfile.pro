;+
; NAME:  Gms5GetUncompressedFile.pro
; $Id$
;
; PURPOSE: Out of a list of possible files, find the first one that
;          lacks a '.Z' extension and returns it. If there are none
;          such, it uncompresses the first file and returns that.
;
;
; AUTHOR:  William Daffer
;
;
; CATEGORY:  Qscat VAP GMS 5 file manipulation
;
;
;
; CALLING SEQUENCE:   
;
;    NameOfUncompressedFile = Gms5GetUncompressedFile(list_of_possible_filenames)
;
;
; 
; INPUTS:  List_of_possibles_filenames: The result of a directory
;         search on the 'datetime' that Scott Gennari's files have.
;
;
;
; OPTIONAL INPUTS:  None
; KEYWORD PARAMETERS:  None
;
; OUTPUTS: The name of the first uncompressed file it finds, or the
;          name of the first file in the list, sans the '.Z'
;          extension, after it's been uncompressed
;
; OPTIONAL OUTPUTS:  None
; COMMON BLOCKS:   None
; SIDE EFFECTS:  If there are no uncompressed files, this routine
;               uncompresses one.
;
; RESTRICTIONS:  None
;
; PROCEDURE: Search for a file lacking the '.Z' extension. If there
;            are none, uncompress the first one in the list and return
;            it's name, sans the '.Z' extension
;
;
;
; EXAMPLE:  Say you've done the following search on a directory.
;
;           files=findfile('9912311230*')
;
;   The following will return the uncompressed file matching this
;search string.
;
;    uncompressedfile = Gms5GetUncompressedFile(files)
;
;
;
; MODIFICATION LOG:
;
; $Log$
; Revision 1.1  1999/04/02 17:59:54  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION Gms5GetUncompressedFile, possibles

  ; Returns the uncompressed files contained in list 'possibles' or
  ; uncompresses the first file in the list, if there are no
  ; uncompressed files.


  IF n_Params() LT 1 THEN BEGIN 
    Usage,'UncompressedFile=Gms5GetUncompressedFile(possibles)'
    return,''
  ENDIF 

  IF NOT isa(possibles,/string,/nonempty) THEN BEGIN 
    Message,"Input parameter 'possibles' must consist of NON-EMPTY STRINGS",/cont
    return,''
  ENDIF 

  file = ''

  nf = n_elements(possibles)
  test = lonarr(nf)
  FOR f=0l,nf-1 DO $
    test[f] = rstrpos( possibles[f], '.Z') 

  x = where( test EQ -1, nx )
  IF nx NE 0 THEN BEGIN 
    file =  possibles[x[0]]
  ENDIF ELSE BEGIN 
    testfile = possibles[0]
    spawn, 'gunzip ' + testfile, return_val
    tmp = str_sep( testfile, '.' )
    testfile = strjoin( [tmp[0], tmp[1]], '.')
    file = (findfile(testfile, count=cnt))[0]
  ENDELSE 

  return, file
END
