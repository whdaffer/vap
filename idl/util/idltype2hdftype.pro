FUNCTION idltype2hdftype, idltype
  rcsid = "$Id$"
  CASE strupcase(idltype) OF 
    'UNDEFINED': type = 0l
    'STRING'   : type = 3L
    'BYTE'     : type = 21L
    'INTEGER'  : type = 22L
    'LONGWORD' : type = 24l
    'FLOAT'    : type = 5L
    'DOUBLE'   : type = 6L
    ELSE: type = -1L
  ENDCASE
  RETURN,type
END
