;+
; NAME: Gms5ReadItem.pro 
; $Id$
; PURPOSE:  Given a time and a 'thing' read the GMS5 file that has
;           that thing. Things are  'cal','doc','grid','ir1','ir2',
;           'ir3' and 'vis'
;
; AUTHOR:  whd
;
; CATEGORY:  GMS5 data i/o
;
; CALLING SEQUENCE:  read_thing = read_GMs5Itime(datetime,thingy)
; 
; INPUTS:  
;  DateTime: a date time specified in the way the files themselves
;            are, e.g. YYYYDOYHHMM.thing.
;
;  Thing: one of the things listed above
;
; OPTIONAL INPUTS:  None
;	
; KEYWORD PARAMETERS:  None
;
; OUTPUTS:  The data read from the given file.
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  The datetime must be of the format given above and
;               the 'thing' must be one of those listed above. 
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
; Revision 1.1  1999/04/02 18:04:09  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION Gms5ReadItem, datetime, thingy

  COMMON gms5_cmn, gms5initialized, gms5_data_topdir, $
    gms5_hdftemplates_saveset_file

  IF n_params() LT 2 THEN BEGIN 
    Usage,'retStruct=Gms5ReadItem(datetime,item)'
    return,''
  ENDIF 

  forward_function Gms5GetUncompressedFile

  thingy =  strupcase(thingy)
  retThing = ''
  catch, error
  IF error NE 0 THEN BEGIN 
    message,!error_state.msg,/cont
    return, ''
  ENDIF 

  IF n_elements(Gms5Initialized) EQ 0 THEN Gms5Init

  CASE thingy OF 
    'CAL': BEGIN 
      path =  gms5_data_topdir + '/cal/'
      possibles = findfile(path + datetime + '*', count=cnt)
      IF cnt NE 0 THEN BEGIN 
          file = Gms5GetUncompressedFile(possibles)
          IF strlen(file[0]) NE 0 THEN BEGIN 
            retThing = Gms5ReadCalfile( file )
          ENDIF ELSE BEGIN 
            Message,"Can't find uncompress{ed|able} CAL file for " + $
             datetime
          ENDELSE 
      ENDIF ELSE $ 
        Message,'No Cal files found for ' +datetime
    END
    'DOC':BEGIN 
      path =  gms5_data_topdir + '/doc/'
      possibles = findfile(path + datetime + '*', count=cnt)
      IF cnt NE 0 THEN BEGIN 
          file = Gms5GetUncompressedFile(possibles)
          IF strlen(file[0]) NE 0 THEN BEGIN 
            retThing = Gms5ReadDocfile(file)
          ENDIF ELSE BEGIN 
            Message,"Can't find uncompress{ed|able} DOC file for " + $
             datetime
          ENDELSE 
      ENDIF ELSE $
        Message,'No `Doc files found for ' +datetime
    END
    'GRID':BEGIN 
      path = gms5_data_topdir + '/grid/'
      possibles = findfile(path + datetime + '*', count=cnt)
      IF cnt NE 0 THEN BEGIN 
          file = Gms5GetUncompressedFile(possibles)
          IF strlen(file[0]) NE 0 THEN BEGIN 
            grid = Gms5ReadGridfile(file)
            IF min(grid.grid,max=mx) EQ 0 AND mx EQ 0 THEN BEGIN 
              Message,"Grid is everywhere zero! -- failing!"
            ENDIF ELSE BEGIN 
              dim = size(grid.grid,/dim)
              tmp = reform(grid.grid,2,dim[0]/2,241)
              retThing = gms5Grid_str()
              retThing.xloc = ptr_new(reform(tmp[1,*,*]),/no_copy)
              retThing.yloc = ptr_new(reform(tmp[0,*,*]),/no_copy)
              tmp=0
              retThing.filename =  grid.filename
              retThing.date =  grid.date
              grid = 0
              retThing.minlon = 80.
            ENDELSE 
          ENDIF ELSE BEGIN 
            Message,"Can't find uncompress{ed|able} GRID file for " + $
             datetime
          ENDELSE 
      ENDIF ELSE $
        Message,'No Grid files found for ' +datetime
    END
    'GRIDA':BEGIN 
      path = gms5_data_topdir + '/grida/'
      possibles = findfile(path + datetime + '*', count=cnt)
      IF cnt NE 0 THEN BEGIN 
          file = Gms5GetUncompressedFile(possibles)
          IF strlen(file[0]) NE 0 THEN BEGIN 
            grid = Gms5ReadGridfile(file)
            dim = size(grid.grid,/dim)
            tmp = reform(grid.grid,2,dim[0]/2,241)
            retThing = gms5Grid_str()
            retThing.xloc = ptr_new(reform(tmp[1,*,*]),/no_copy)
            retThing.yloc = ptr_new(reform(tmp[0,*,*]),/no_copy)
            tmp=0
            retThing.filename =  grid.filename
            retThing.date =  grid.date
            grid = 0
            retThing.minlon = 70.
         ENDIF ELSE BEGIN 
           Message,"Can't find uncompress{ed|able} GRIDA file for " + $
            datetime
        ENDELSE 
      ENDIF ELSE $
        Message,'No GridA files found for ' +datetime
    END
    'IR1':BEGIN 
      path = gms5_data_topdir + '/ir1/'
      possibles = findfile(path + datetime + '*', count=cnt)
      IF cnt NE 0 THEN BEGIN 
        file = Gms5GetUncompressedFile(possibles)
        IF strlen(file[0]) NE 0 THEN BEGIN 
          image = Gms5ReadIrfile(file)
          retThing = Gms5Image_str()
          retThing.image = image.image
          retThing.filename = image.filename
          retThing.palette = image.palette
          retThing.date =  image.date
          image = 0

        ENDIF ELSE BEGIN 
          Message,"Can't find uncompress{ed|able} IR1 file for " + $
           datetime
        ENDELSE 
      ENDIF ELSE Message,'No IR1 files for ' + datetime
    END
    'IR2':BEGIN 
      path = gms5_data_topdir + '/ir2/'
      possibles = findfile(path + datetime + '*', count=cnt)
      IF cnt NE 0 THEN BEGIN 
        file = Gms5GetUncompressedFile(possibles)
        IF strlen(file[0]) NE 0 THEN BEGIN 
          image = Gms5ReadIrfile(file)
          retThing = Gms5Image_str()
          retThing.image = image.image
          retThing.filename = image.filename
          retThing.palette = image.palette
          retThing.date =  image.date
          image = 0

        ENDIF ELSE BEGIN 
          Message,"Can't find uncompress{ed|able} IR2 file for " + $
           datetime
        ENDELSE 
      ENDIF ELSE Message,'No IR2 files for ' + datetime
    END
    'IR3':BEGIN 
      path = gms5_data_topdir + '/ir3/'
      possibles = findfile(path + datetime + '*', count=cnt)
      IF cnt NE 0 THEN BEGIN 
        file = Gms5GetUncompressedFile(possibles)
        IF strlen(file[0]) NE 0 THEN BEGIN 
          image = Gms5ReadIrfile(file)
          retThing = Gms5Image_str()
          retThing.image = image.image
          retThing.filename = image.filename
          retThing.palette = image.palette
          retThing.date =  image.date
          image = 0

          ENDIF ELSE BEGIN 
            Message,"Can't find uncompress{ed|able} IR3 file for " + $
             datetime
          ENDELSE 
      ENDIF ELSE Message,'No IR3 files for ' + datetime
    END
    'VIS':BEGIN 
      path = gms5_data_topdir + '/vis/'
      possibles = findfile(path + datetime + '*', count=cnt)
      IF cnt NE 0 THEN BEGIN 
        file = Gms5GetUncompressedFile(possibles)
        IF strlen(file[0]) NE 0 THEN BEGIN 
          image = Gms5ReadVisfile(file)
          retThing = Gms5Image_str()
          retThing.image = image.image
          retThing.filename = image.filename
          retThing.palette = image.palette
          retThing.date =  image.date
          image = 0

        ENDIF ELSE BEGIN 
          Message,"Can't find uncompress{ed|able} file for " + $
           datetime
        ENDELSE 
      ENDIF ELSE Message,'No Ir3 files found for ' +datetime
    END
    ELSE: BEGIN 
      MESSAGE,'Unknown thingy! ' + thingy
      print," Thingys must be one of :  " + $
         "'cal','doc','grid','grida', 'ir1','ir2','ir3' or 'vis'"
    END 
  ENDCASE 


  return,retThing
END









