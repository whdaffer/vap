;
;
; $Id$
;
FUNCTION vartype, in
  s =  size(in)
  t =  s( s(0) + 1 )
  TYPES =  ['UNDEFINED',$
            'BYTE',$
            'INTEGER',$
            'LONGWORD',$
            'FLOAT',$
            'DOUBLE',$
            'COMPLEX_FLOAT',$
            'STRING',$
            'STRUCTURE',$
            'COMPLEX_DOUBLE',$
            'POINTER',$
            'OBJECT']
RETURN, types(t)
END
