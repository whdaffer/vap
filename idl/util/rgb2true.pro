;+
; NAME:  Rgb2True.pro 
;
;       (Renamed my color24.pro, to avoid conflict with
;       D. Fanning's Color24.pro )
;
; $Id$
; PURPOSE: Return the 24 bit color given either an index into a
;          colortable, or the actual RGB triples. 
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Color Manipulation
;
; CALLING SEQUENCE:  24bitColor=Rgb2true(colorIndex [,ColorTable=ColorTable])
; 
; INPUTS:  
;  
;  colorIndex : Index into ColorTable or RGB triples. 
;
;               The descrimination is made depending on whether the
;               array is 2 dimensional. If it is, this routine assumes
;               that whichever dimension is 3 contains the RGB
;               triples. If you want to send only 1 RGB triple, you
;               must send it as a column vector, i.e. 1-by-3 array,
;               otherwise it will see the array as 3 color indices and
;               return 3 numbers, none of which will be correct. In
;               the case that you need to send a 3-by-3 array, put the
;               RGB triple in the first dimension. If you're getting
;               weird results, try transposing the input array.
;
;
; OPTIONAL INPUTS:  None
;
; KEYWORD PARAMETERS:  
;
;   ColorTable: The colorTable to be used. Wants the array to be n by
;               3, will make a copy and transpose it if the first
;               dimension isn't *3*. 
;
; OUTPUTS:  The 24bit number corresponding to this color. 
;
;     There are 2 cases: 
;
;       Case 1, colors is a 1-d vector: 
;           Let n=color[i], the i-th entry of the input color array. Let
;               ColorTable = input Colortable or the colortable
;               obtained in the call tvlct,r,g,b,/get. Let M=number of
;               colors in colorTable. Then...
;
;           24bitColor[i] = colorTable[n<m,0] +
;                            colorTable[n<m,1]*2^8+
;                             colorTable[n<m,2]*2^16
;
;       Case 2, Colors is a 2-d array. 
;
;          
;          This routine will transpose the array IF it finds the first
;          dimension is 3. Let color=colors[i,*]. Let M=number of
;               colors in colorTable. Then...
;          
;          This case has 2 further subcases. 
;
;            SubCase 1: A colortable has been passed in via the
;                       'ColorTable' keyword...
;
;           24bitColor[i] = colorTable[color[0]<m,0] +
;                            colorTable[color[1]<m,1]*2^8+
;                             colorTable[color[2]<m,2]*2^16
;
;          SubCase 2: Otherwise...
;
;           24bitColor[i] = color[0] + color[1]*2^8 + color[2]*2^16
;
;
; OPTIONAL OUTPUTS:  None
;
; COMMON BLOCKS:  None
;
; SIDE EFFECTS:  Whenever a colortable is being used, the value in
;                color[i] (or color[i,*] in the 2-d case) is limited to
;                LE the number of colors in the color table.
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
;Copyright (c) 1998, William Daffer
;-

FUNCTION rgb2true, colors, colorTable=colorTable, transpose=transpose

  IF n_Params() LT 1 THEN BEGIN 
    Message,'usage: 24bitColors=rgb2true( colors [,colortable=colortable])',/cont
    return,''
  ENDIF 
  colorCpy = colors
  IF keyword_set(transpose) THEN colorCpy = transpose(colorCpy)
  ndims =  size( colorCpy,/n_dim )

  IF ndims EQ 1 THEN BEGIN 

      ; We're looking at color indices.

    IF n_elements(colorTable) EQ 0 THEN BEGIN 
      tvlct,r,g,b,/get
      colorTable =  [[r],[g],[b]]
    ENDIF 

    ctCpy = colorTable
    sz = size(ctCpy,/dim)
    IF sz[0] EQ 3 THEN ctCpy = transpose(ctCpy)
    sz = size(ctCpy,/dim)
    nn = sz[0]

    returnColors = ctCpy[colorCpy < nn, 0] + $
                   ctCpy[colorCpy < nn, 1]*2l^8 + $
                   ctCpy[colorCpy < nn, 2]*2l^16
  ENDIF ELSE BEGIN 
    sz = size(colorCpy,/dimensions)
    IF sz[0] EQ 3 THEN $
      colorCpy = transpose(colorCpy)
    IF n_elements(colorTable) EQ 0 THEN BEGIN 
      returnColors = colorCpy[*,0] + colorCpy[*,1]*2l^8 + colorCpy[*,2]*2l^16
    ENDIF ELSE BEGIN 
      ctCpy = colorTable
      sz = size(ctCpy)
      IF sz[0] EQ 3 THEN ctCpy = transpose(ctCpy)
      sz = size(ctCpy)
      nn = sz[0]
      returnColors = ctCpy[ colors[*,0] < nn, 0] + $
                     ctCpy[ colors[*,1] < nn, 1]*2l^8 + $
                     ctCpy[ colors[*,2] < nn, 2]*2l^16
      
    ENDELSE 
  ENDELSE 
  IF n_elements(returnColors) EQ 1 THEN returnColors = returnColors[0]
  return, returnColors
END


