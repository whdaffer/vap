;+
; NAME:  ObjTags
; $Id$
; PURPOSE:  Return the tagnames in the object
;
; AUTHOR:  William Daffer
;
; CATEGORY:  OO Utility
;
; CALLING SEQUENCE:  Tags=objTag(obj)
; 
; INPUTS:  Obj: An Object
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  tags: A vector of tag names
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
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
;
; Copyright (c) 1999, William Daffer
; No Warranties!
;-

FUNCTION objtags, obj

  IF n_params() LT 1 THEN BEGIN 
    Usage,"tags = objtags(object)"
    return,''
  ENDIF 

  IF NOT isa(obj, /object ) THEN BEGIN 
    Message,"Input must be OBJECT!",/cont
    return,''
  END

  t = 'tags = tag_names({' + obj_class(obj) + '})'
  s = execute(t)
  return,tags
END
