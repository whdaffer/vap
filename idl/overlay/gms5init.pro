;+
; NAME:  Gms5Init.pro
; $Id$
; PURPOSE:  Initialize Gms5 processing
; 
; AUTHOR: William Daffer
; 
; CATEGORY:  Qscat Vap/GMS 5 image processing
; 
; CALLING SEQUENCE:  gms5init [,$
;                       topdir=topdir, $
;                       templates_saveset_file = templates_saveset_file ]
; 
; INPUTS:  None
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
;   templates_saveset_file: (defaults to ~/Library/gms5templates.save
;
; OUTPUTS:  Writes the file ~/.qvapgms5rc containing initialization info.
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  gms5_cmn, gms5_data_topdir, gms5_hdftemplates_saveset_file
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION LOG:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 1999, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

PRO Gms5Init, topdir=data_topdir, $
              templates_saveset_file=_hdftemplates_saveset_file

  COMMON gms5_cmn, gms5initialized, gms5_data_topdir, $
    gms5_hdftemplates_saveset_file

;  on_error,1
  IF exist( data_topdir ) THEN BEGIN 
    IF NOT (isa(data_topdir,/string,/non_empty)) THEN BEGIN 
      Message,' TopDir must be NON-EMPTY STRING!'
    ENDIF  
  ENDIF 
  IF exist( hdf_template_saveset_file ) THEN BEGIN 
    IF NOT (isa(hdf_template_saveset_file,/string,/non_empty)) THEN BEGIN 
      Message,' TopDir must be NON-EMPTY STRING!'
    ENDIF  
  ENDIF 
  entry_errors = 0
    ; check to see if the common exists and whether its members are
    ; defined.
  IF n_elements(gms5initialized) EQ 0 THEN BEGIN 
      ; Look for ~/.qvapgms5rc. If it's there, read the info from it.
    qvapgms5rc = findfile(getenv("HOME") + '/.qvapgms5rc',count=cnt)
    IF cnt NE 0 THEN BEGIN 
      openr, lun, qvapgms5rc[0],/get,error=err
      IF err EQ 0 THEN BEGIN 
        rec = ''
        WHILE NOT eof(lun) DO BEGIN 
          readf, lun, rec
          rec = strcompress(rec,/remove_all)
          IF strpos(rec,";") ne 0 THEN $
            s = execute(rec)
        ENDWHILE 
        free_lun, lun
      ENDIF ELSE BEGIN
        Message,!error_State.msg,/cont
        Message,"Can't read ~/.qvapgms5rc file! Aborting!"
      ENDELSE 
      gms5initialized = 1

    ENDIF ELSE BEGIN
        ; No ~/.qvapgms5rc
      IF n_elements(data_topdir) eq 0  OR $
         n_elements(hdftemplates_saveset_file) EQ 0 THEN BEGIN 
        IF n_elements(data_topdir) ne 0 THEN gms5_data_topdir =  data_topdir
        IF n_elements(hdftemplates_saveset_file) ne 0 THEN $
         gms5_hdftemplates_saveset_file = hdftemplates_saveset_file
        IF !d.name EQ 'X' THEN BEGIN 
          config = gms5configurator()
          IF config[0] NE '' THEN BEGIN 
            openw, wlun, "~/.qvapgms5rc",/get,error=err
            IF err EQ 0 THEN BEGIN 
              FOR i=0,n_elements(config)-1 DO BEGIN 
                printf, wlun, config[i]
                s = execute(config[i])
              ENDFOR 
              free_lun, wlun
            ENDIF ELSE BEGIN 
              Message,"Can't open ~/.qvapgms5rc!",/cont
              Message,!error_state.msg
            ENDELSE 
          ENDIF ELSE Message,"Can't configure .qvapgms5rc file!"

        ENDIF ELSE BEGIN 
          test_saveset = ''
          print, "I need the directory at the top of the GMS 5 data "
          print, "tree and  the name of the idl saveset that has the "
          print, "HDF templates in it. The prompt will tell you what "
          print, "the default value currently is for each."
          print, "If you don't know what these are signify by pressing "
          print, "return without making any entries. " 
          print, "If there is a default, it will probably be good enough"
          print, "To change, you must enter the fully qualified filenames."
          IF n_elements(gms5_data_topdir) NE 0 THEN $
           field = gms5_data_topdir ELSE field =  ''
          prompt =  "Top of GMS 5 directory tree <" + $
                         field + "> :"
          Read,prompt, test_topdir
          test_topdir =  strcompress(test_topdir,/remove_all)
          IF strlen(test_topdir) EQ 0 THEN BEGIN 
            entry_errors = entry_errors + 1
            print,"Sorry, need that directory name!"
            print,"Once you've found it, you can put it in "
            print,"your ~/.qvapgms5rc file by addding the line..."
            print,"gms5_data_topdir=/the/fully/qualified/directory"
          ENDIF ELSE gms5_data_topdir =  test_topdir
          IF n_elements(gms5_hdftemplates_saveset_file) NE 0 THEN $
           field = gms5_hdftemplates_saveset_file ELSE field =  ''

           prompt = 'Fully qualified idlsaveset file name <' + $
            field + '>'
          Read,prompt,test_saveset
          test_saveset = strcompress(test_saveset,/remove_all)

          IF strlen(test_saveset) EQ 0 THEN BEGIN 
             entry_errors = entry_errors + 1
            print,"Sorry, need that saveset name"
            print,"Once you've found it, you can put it in "
            print,"your ~/.qvapgms5rc file by addding the line..."
            print,"gms5_hdftemplate_saveset_file=/the/fully/qualified/filename"
          ENDIF ELSE gms5_hdftemplates_saveset_file =  test_saveset
          IF entry_errors GT 0 THEN $
           Message,'Too many entry errors! Returning to main level'
        ENDELSE 
      ENDIF 
    ENDELSE 

  ENDIF

END

  




