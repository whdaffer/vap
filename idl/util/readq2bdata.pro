;+
; NAME:  
; $Id$
; PURPOSE:  Reads Quikscat data. Returns either a ncells by nrecs by 4
;           array [[u],[v],[lon],[lat]] or a linked list of Q2B
;           objects (if linkedlist flag is set)
;
;
; AUTHOR; William Daffer
;
;
; CATEGORY:  I/O
;
;
;
; CALLING SEQUENCE:   return=ReadQ2BData(files,[ambig,[ /linkedlist[,decimate=decimate]]])
;
;
; 
; INPUTS:  Files - string array of fully qualified filenames
;
;
;
; OPTIONAL INPUTS:  ambig - the ambiguity to return, default=selected ambiguity.
;
;
;	
; KEYWORD PARAMETERS:  
;
;  Decimate   - take every nth vector (2=every other, 3=every
;               third...) Default = 1, take every vector.
;
;  linkedList - Flag: if set, the routine will
;                     return a pointer to a 'linkedlist' object of Q2B
;                     or QModel objects. Otherwise it will return a
;                     pointer to the data in a ncells by nrecs by 4
;                     array (e.g.  [ [ [u] ], [ [v] ],[ [lon] ], [
;                     [lat] ] ] )
;
;
;
; OUTPUTS:   Either a Pointer to a linked list object containing Q2B or Qmodel
;          objects or a pointer to the actual data as a ncells by
;          nrecs by 4 array 
;          (e.g.  [ [ [u] ], [ [v] ],[ [lon] ], [ [lat] ] ] ) Returns
;          a NULL pointer is there is a failure. User should test for
;          NULL pointer on return. N.B. It's ALWAYS faster to return
;          the linked list and extract the data as you need it, rather
;          than putting it into one big array (using array
;          concatenation) in this routine. However, it's much easier
;          to have this routine return the data, rather than a linked
;          list. 
;
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
; RESTRICTIONS:   Currently only reads HDF and Vincents 'native' format data.
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
; MODIFICATION HISTORY:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION ReadQ2BData, files, ambig,$
                      decimate=decimate ,$
                      CRDecimate=CRDecimate, $
                      ExcludeCols=ExcludeCols,$
                      mintime = mintime,$
                      maxtime = maxtime, $
                      LinkedList = LinkedList
  rcsid = "$Id$"
  retptr =  Ptr_New()
  IF n_elements(files) EQ 0 THEN BEGIN 
    Message,'Usage, retptr=ReadQ2BData(files [,ambig, [/linkedlist]])',/cont
    return, retptr
  ENDIF
  IF VarType( files(0) ) NE 'STRING' THEN BEGIN 
    Message,' Files must be of type STRING',/cont
    return, retptr
  ENDIF 
  nf = n_elements(files)
  linkedlist = keyword_set(linkedlist)
  IF N_Elements(ambig) EQ 0 THEN ambig = 0 ; selected ambiguity
  IF N_Elements(decimate) EQ 0 THEN decimate = 1 ; Take every one

  IF linkedlist THEN BEGIN 
    retdata = obj_new('LinkedList')
    IF NOT obj_valid( retdata ) THEN BEGIN 
      Message,"Can't create LinkedList!",/cont
      return, retptr
    ENDIF 
  ENDIF 

  FOR f=0,nf-1 DO BEGIN 
    IF strlen(files(f)) ne 0 THEN BEGIN 
      q = Obj_new('q2b',file=files(f))
      IF Obj_valid(q) THEN BEGIN 
        IF linkedlist THEN s = retdata -> append(q) ELSE BEGIN 
          s = q-> set(decimate = decimate, $
                      CRDecimate=CRDecimate, $
                      ExcludeCols=ExcludeCols)
          s = q-> GetPlotData( u,v,lon,lat, ambig)
          IF n_Elements(retdata) EQ 0 THEN BEGIN 
              ; First one.
            retdata =  [ [[u]], [[v]], [[lon]], [[lat]] ]
          ENDIF ELSE BEGIN
              ; Array Concatenation. SLOW!!! 
            tmp =  [ [[u]], [[v]], [[lon]], [[lat]] ]
            retdata = [ [retdata], [tmp] ]
          ENDELSE 
        ENDELSE 
        IF Obj_Valid( q ) THEN Obj_destroy,q
      ENDIF ELSE $
       Message,"Error initializing Q2B Object using file " + $
         files(f),/cont
    ENDIF ELSE $
      Message,'Filename must of non-zero length, skipping '
  ENDFOR 
  IF n_Elements( retdata ) NE 0 THEN $
    retptr = Ptr_New(retdata,/No_Copy) 
  return, retptr
END
