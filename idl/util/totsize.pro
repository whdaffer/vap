;+
; NAME:  Totsize
; $Id$
; PURPOSE:  Return total size of input object in bytes. Commonly used
;           to determine the size of a record when written to a output
;           binary file.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  I/O
;
; CALLING SEQUENCE:  size=totsize(item)
; 
; INPUTS:  
;
;   Item: Any item whose size you want to know. 
;
; OPTIONAL INPUTS:  
;       
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  
;
;   Failure: -1l
;   Success: The total size of the input item. If the item is a
;            composite object (structure or (possibly) a pointer) the
;            routine is called recursively to find the total size of the
;            constituent substructures.
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  The concept of size must make sense for all members
;               of any input item. If at any point in the recursion
;               this routine encounters a datum whose size is
;               indefinite (e.g. an Object, Null Pointer or undefined
;               variable), this routine returns a -1L.
;
; PROCEDURE: Determine the Variable type of the object. If the item
;            is simple, like an array of integers, return the product
;            of the dimensions with the size of the item. If this is a
;            (potentially) recursive structure (e.g. a structure or a
;            pointer), call totsize on the individual members of the
;            item and total up the sizes. If it encounters a element
;            for which size doesn't make any sense (e.g. an Object,
;            Null Pointer or undefined variable ) return, -1l.
;
;
;
; EXAMPLE:  
;
;  Let d={a:0, b:0b, c:0.0}
;
;  N_Tags(a,/length) will equal 8, because of the padding between the
;  b and c. But totsize(d) equals 7.
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Copyright (c) 1998, William Daffer
;-

FUNCTION totsize, item
  totsize = -1l
  vartype = VarType(item)
  CASE VarType OF 
    'STRUCTURE' : BEGIN 
      nn = n_Tags(item)
      FOR i=0L,nn-1 DO BEGIN 
        tmp = totsize(item.(i))
        IF tmp NE -1l THEN totsize =  (totsize> 0) + tmp ELSE return,-1l
      ENDFOR 
    END
    'OBJECT': BEGIN 
      Message,"Can't take size of Object ",/cont
    END
   'POINTER': BEGIN 
      IF Ptr_Valid(item) THEN BEGIN 
        Message,"Dereferencing Pointer ",/cont
        p = *item
        t = totsize(p)
        IF t NE -1 THEN totsize = 0> totsize + t ELSE return,-1l
      ENDIF ELSE Message,"Can't Take size of Null Pointer",/cont
    END
    ELSE: BEGIN 
      nn = n_elements(item)
      IF VarType EQ 'STRING' THEN $ 
          totsize = long((totsize> 0) + total(strlen(item))) $
      ELSE $
        totsize = (totsize> 0) + nn*sizeof(item)
    END
  ENDCASE 

  return, totsize
END
