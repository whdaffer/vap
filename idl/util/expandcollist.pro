;+
; NAME:  ExpandColList.pro
; $Id$
; PURPOSE:  Given a string of form 'a,x:y,z' where y>x, this routine
;          returns the vector [a,x, x+1, ..., y-1, y,z].
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Utility
;
; CALLING SEQUENCE: expanded_list=expandColList(column_list) 
; 
; INPUTS: Column_list: a string consisting of comma and colon separated
;         elements. The comma separated elements are taken to be
;         individual elements, the colon separated ones are taken as
;         ranges, which are expanded.
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  
;
;  Success: The expanded list
;  Error: a scalar -1
;
; OPTIONAL OUTPUTS: none  
;
; COMMON BLOCKS:  none
; SIDE EFFECTS:  
; RESTRICTIONS:  
; PROCEDURE:  
; 
; EXAMPLE:  [2,4,5,6,8] = expandColList('2,4:6,8')
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Copyright (c) 1998,William Daffer
;-
; No Warranty.
;
;
FUNCTION expandColList, cols
  ColList = -1
  IF N_Params() EQ 1 THEN BEGIN 
    IF VarType(cols) EQ 'STRING' THEN BEGIN 
      Cols = strcompress(string(cols),/remove_all)
      IF strlen(cols) NE 0 THEN BEGIN 
        tmp = str_sep(cols,',')   ;
        nn = n_elements(tmp)
        FOR i=0,nn-1 DO BEGIN 
          junk = strpos( tmp[i], ':')
          IF junk EQ -1 THEN BEGIN 
            NewCols = fix(tmp[i])
          ENDIF ELSE BEGIN 
            tmp2 = str_sep( tmp[i], ':' )
            x0 = fix(tmp2[0])
            x1 = fix(tmp2[1])
            newCols = indgen( x1-x0 +1 )+x0
          ENDELSE 

          IF i EQ 0 THEN $
           ColList = newcols $
          ELSE $
           ColList =  [ColList, newCols ]
        ENDFOR 
      ENDIF 
    ENDIF 
  ENDIF 
  return, ColList
END
 
