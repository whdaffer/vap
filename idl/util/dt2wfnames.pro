;+
; NAME:  dt2wfnames
; $Id$
; PURPOSE:  Converts an array of IDLDT variables to names for
;           Qscat/SeaWinds Windfiles.
;
;
; AUTHOR: William Daffer
;
;
; CATEGORY:  Qscat/Seawinds file manipulation
;
;
;
; CALLING SEQUENCE:  
;
;    There are two ways to call this routine.
;
;     wind_file_names=dt2wfnames(array_of_wfdt_structure ) 
;
;                  - or -
;
;     wind_file_names=dt2wfnames( startdt=startdt, enddt=enddt )
;
;    That is, one can input an array of wfdt structure (which has start/stop
; info in it, see wfdt_str.pro for structure format) or input the two
; start/stop array individually through the startDt/EndDt keywords.
;
; 
; INPUTS:  
;
;    wfdt: Array of WFDT structure (see  wfdt_str.pro for structure info.)
;
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;    StartDt: An array of IDLDT structure giving the start time for
;             each file.
;
;    EndDt: An array of IDLDT structure giving the end time for
;             each file.
;
;
;
; OUTPUTS:  
;
;   Success:  A list of filename of the form 'QSyyyymmdd.Shhmm.Ehhmm'
;
;
;
; OPTIONAL OUTPUTS:  If the input is of the form wfdt, the 'name'
;                   field of that form is filled in with the names
;                   calculated by this routine.
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
; RESTRICTIONS:  
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
;
; MODIFICATION HISTORY:
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION dt2wfnames, wfdt, startDt = startDt, EndDt=EndDt

  rcsid = "$Id$"
  retarray = 0
  IF n_Params() EQ 1 THEN BEGIN 
    IF VarType( wfdt ) eq 'STRUCTURE' THEN BEGIN 
      IF Tag_Names(wfdt,/structure_name) EQ 'WFDT' THEN BEGIN 
        x = where( wfdt.start_time.julian GT wfdt.end_time.julian, nx )
        IF nx NE 0 THEN BEGIN 
          Message,'Some End Times are less than Start Times, correcting',/info
          tmp = dt_add( wfdt[x].end_time, day=1)
          wfdt[x].end_time =  tmp
        ENDIF 
        dt_to_var, wfdt.start_time, year=year, month=month, day=day, hour=hour, min=min
        year = strtrim(year,2)
        month = PadAndJustify(month,2)
        day = PadAndJustify(day,2)
        shour = PadAndJustify(hour,2)
        smin = PadAndJustify(min,2)
        dt_to_var, wfdt.end_time, hour=hour, min=min
        ehour = PadAndJustify(hour,2)
        emin  = PadAndJustify(min,2)
        basename = 'QS'+year+month+day+'.'
        retarr = basename + 'S'+shour+smin+".E"+ehour+emin
        wfdt.name = retarr
     ENDIF ELSE Message,"WFDT must be a STRUCTURE of type WFDT (see wfdt_str.pro)",/CONT
   ENDIF ELSE Message,"WFDT must be a STRUCTURE of type WFDT (see wfdt_str.pro)",/CONT

  ENDIF ELSE BEGIN 
    IF N_Elements(startDt) EQ 0 OR N_Elements(EndDt) EQ 0 THEN BEGIN 
      Message,"If you don't specify wfdt, you must specify BOTH startDt and EndDt",/cont
      return,0
    ENDIF ELSE BEGIN 

      year  = strtrim(startDt.year,2)
      month = PadAndJustify(startDt.month,2)
      day   = PadAndJustify(startDt.day,2)
      shour = PadAndJustify(startDt.hour,2)
      smin  = PadAndJustify(startDt.min,2)
      ehour = PadAndJustify(endDt.hour,2)
      emin  = PadAndJustify(endDt.min,2)
      basename = 'QS'+year+month+day+'.'
      retarr = basename + 'S'+shour+smin+".E"+ehour+emin
      wfdt.name = names

    ENDELSE 
  ENDELSE 

  return,retarr
END
