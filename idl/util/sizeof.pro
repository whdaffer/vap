;+
; NAME:  Sizeof
; $Id$
; PURPOSE: Return the size of 1 of whatever makes up the input
;          item. Basically , the size of item[0], if the input is an
;          array.
;
;
; AUTHOR:  William Daffer
;
; CATEGORY:  
;
; CALLING SEQUENCE:  size=sizeof(item)
; 
; INPUTS:  
;
;   Item: The item whose size you want to know.
;
; OPTIONAL INPUTS:  None
;       
; KEYWORD PARAMETERS:  None
;
; OUTPUTS:  
;
;   The size of the item. Note, this is NOT the total size of the
;   item, but the size of the thing that makes up the item. So,
;   sizeof(bytarr(10)) = 1. The exception to this is sizeof(string)=strlen(string)
;
; OPTIONAL OUTPUTS:  None
;
; COMMON BLOCKS:  None
;
; SIDE EFFECTS:  None
;
; RESTRICTIONS:  
;
; 
; PROCEDURE:  Determine the Vartype of the item, return proper
;             number of bytes. For a String, return strlen(item[0])
;                
;
; ISSUES:
;
;   Strings and Structures; Do they have a 'size' in the way a byte
;   does.  I've decided the answer is no for structures, requiring the
;   use of the routine 'totsize' to determine thier size, while the
;   answer is a qualified yes for strings. But someone might be able to
;   argue otherwise convincingly. If you want to find out the total size
;   of a string (or an array of strings), use my routine 'totsize'.
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.2  1998/11/23 18:04:39  vapuser
; Expanded some comments
;
; Revision 1.1  1998/11/23 17:46:53  vapuser
; Initial revision
;
;
;Copyright (c) 1998, William Daffer
;
;-

FUNCTION sizeof, item
   ss = -1
   CASE vartype(item) OF 
     'UNDEFINED': Message,"UNDEFINED items have no size",/cont
     'BYTE'     : ss = 1
     'INTEGER'  : ss = 2
     'UINT'     : ss = 2
     'LONGWORD' : ss = 4
     'ULONG'    : ss = 4
     'LONG64'   : ss = 8
     'ULONG64'  : ss = 8
     'FLOAT'    : ss = 4
     'DOUBLE'   : ss = 8
     'COMPLEX_FLOAT': ss = 16
     'STRING'   : ss = strlen(item[0])
     'STRUCTURE': Message,"Can't calculate sizeof(Structure), use 'totsize(Structure)'",/cont
     'COMPLEX_DOUBLE': ss = 32
     'POINTER'  :Message,"Can't calculate sizeof(Pointer), use 'totsize(Pointer)'",/cont
     'OBJECT'   : Message,"Can't Take Sizeof(Object)",/cont
     ELSE: Message,"Unrecognized Vartype",/cont
   ENDCASE 
   return,ss

END
