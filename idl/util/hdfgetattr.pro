;+
; NAME:  hdfgetattr
; $Id$
; PURPOSE:  Retrieve and attribute from an HDF file by name
;
;
; AUTHOR: William Daffer
;
;
; CATEGORY:  HDF I/O
;
;
;
; CALLING SEQUENCE: retstruct=hdfgetattr( filename [,
;                   attribute=attribute, printall=0|1]) 
;
;
; 
; INPUTS:  
;    filename: string, fully qualified HDF file.
;
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;    attr:     string, the name of the attribute you want to retrieve.
;              If absent, all attributes will be returned in an array
;              of retstructs.

;    printall: flag, if set, will print all attribute names and values
;              Caveat Usor: it will blindly print all attributes. 
;
;
;
; OUTPUTS:  
;
;    retstruct: structure having form   
;               retstruct = { name:'', value:ptr_new() }
;               NB, you must free the pointer after you use it!
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
; RESTRICTIONS:  The user has to free the pointer in the returned
;                structure after use. (PTR_FREE, retstruct.attr)
;                Otherwise, the program provides an easy to use, ready
;                made memory leak.
;
;
;
;
; PROCEDURE:  
;
;
;
; EXAMPLE:  
;
;   retstruct=hdfgetattr( hdffile, attr='shortname')
;   if VarType( retstruct) eq 'STRUCTURE' then begin 
;      print, *retstruct.attr & Ptr_free, retstruct.attr
;   endif 
;
;   To get all attributes from a file
;   retstruct=hdfgetattr( hdffile )
;   if VarType( retstruct[0]) eq 'STRUCTURE' then begin 
;      for i=0,n_elements(retstruct)-1 do begin 
;          print, retstruct.name, *retstruct.attr 
;          Ptr_free, retstruct.attr
;      endfor 
;   endif 
;
;  Vartype, by the way, is a function that returns a string based on
;  IDL type code returned by the 'size' function, i.e. the type of
;  data is in the argument.
;
;
;
;
; MODIFICATION HISTORY:
; $Log$
; Revision 1.3  2001/12/08 00:02:37  vapdev
; Getting rid of obsolete RSI routines and fixing ENV vars
;
; Revision 1.2  1998/10/29 22:33:26  vapuser
; Added some code to handle the Level processors strange attributes
;
; Revision 1.1  1998/10/22 21:33:23  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION hdfgetattr, filename, attribute=attribute, printall=printall
  rcsid = "$Id$"

  printall = keyword_set(printall)
  IF n_Params() NE 1 THEN BEGIN 
    Message,'Usage: retstruct=hdfgetattr( filename, attribute = attribute | /printall )',/cont
    return,0
  ENDIF ELSE BEGIN
    IF VarType(filename) NE 'STRING' THEN BEGIN 
      Message,' FILENAME is not a STRING',/cont
      return,0
    ENDIF 
  ENDELSE 

  IF n_elements(attribute) NE 0 THEN BEGIN 
    IF VarType(attribute) NE 'STRING' THEN BEGIN 
      Message,' ATTRIBUTE is not a STRING',/cont
      return,0
    ENDIF 
  ENDIF     
  
  catch,error
  IF error NE 0 THEN BEGIN 
    catch,/cancel
    Message,!err_string,/cont
    return,0
  ENDIF 

  retstruct = { name:'', value:ptr_new() }

  IF hdf_ishdf(filename) NE 0 THEN BEGIN 

    lf =  string(10b)
    fileid = hdf_sd_start(filename,/read) 
    IF fileid gt 0 THEN BEGIN 
      hdf_sd_fileinfo,fileid,ndatasets,nattributes
      ; print,'Number of SD data sets: ',ndatasets
      ; print,'Number of attributes:   ',nattributes
      IF n_elements(attribute) eq 0 THEN $
        retstruct = replicate(retstruct, nattributes)
      FOR ai =0,nattributes-1 DO BEGIN 
        hdf_sd_attrinfo,fileid,ai,name=name,type=type,count=count,data=data
        IF printall THEN BEGIN 
          IF VarType( data ) EQ 'STRING' THEN BEGIN 
            tmp = strsplit( data, lf,/extract )
            tmp = tmp(where(strlen(tmp)))
            data =  tmp(n_elements(tmp)-1)
            name =  name + " : "
          ENDIF 
          print,name, data 
        ENDIF 
        IF n_elements(attribute) NE 0 THEN BEGIN 
          IF strupcase(attribute) EQ strupcase(name) THEN BEGIN 
            retstruct.name = name
            IF VarType(data) EQ 'STRING' THEN BEGIN 
              data = strsplit(data,lf,/extract)
              IF strupcase(data[0]) EQ 'CHAR' THEN BEGIN 
                data =  data[2:n_elements(data)-1]
                x = where(strlen(data))
                data = data[x]
              ENDIF 
            ENDIF 
            retstruct.value = Ptr_New(data,/no_copy)
          ENDIF 
        ENDIF ELSE BEGIN 
          retstruct(ai).name = name
          IF VarType(data) EQ 'STRING' THEN BEGIN 
            data = strsplit(data,lf,/extract)
            IF strupcase(data[0]) EQ 'CHAR' THEN BEGIN 
              data =  data[2:n_elements(data)-1]
                x = where(strlen(data))
                data = data[x]
            ENDIF 
          ENDIF 
          retstruct(ai).value = Ptr_New(data,/no_copy)
        ENDELSE 
      ENDFOR 
      HDF_SD_END, fileid
    ENDIF ELSE Message,"Can't open file",/cont
  ENDIF ELSE Message,'Not an HDF file ',/cont
  return,retstruct

END
