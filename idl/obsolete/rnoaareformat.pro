;+
; NAME:  RNoaaReformat
; $Id$
; PURPOSE:  given the list of file, fill the current directory with
;          simalcrums of them, structured so that they occupy the time
; range from the end of the current day back however long as their are
; files to fill.
;
; AUTHOR:  me
;
; CATEGORY:  
;
; CALLING SEQUENCE:  
; 
; INPUTS:  
;
; OPTIONAL INPUTS:  
;	
; KEYWORD PARAMETERS:  
;
; OUTPUTS:  
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
; Revision 1.1  1998/11/20 19:57:40  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

PRO RnoaaReformat, files
  COMMON q2b_rnoaa_cmn, q2b_rnoaa_nheader_recs, $
                        q2b_rnoaa_size, $
                        q2b_rnoaa_defined, $
                        q2b_rnoaa


  spawn,'rm -f QS*S*E*',ret
  lf =  string(10b);

  Dt_To_Var, Today(), year=year, month=month, day=day
  nf = n_Elements(files)
  IF nf EQ 0 THEN BEGIN 
    Message,"Usage: RnoaaReformat, files",/cont
    return
  ENDIF 
    
  totminutes = nf*101.
  minutes = totminutes MOD 60.
  hours = totminutes/60. MOD  24.
  days = fix(totminutes/(60.*24))
  start_idldt = dt_subtract( var_to_dt(year,month,day,23,59), $
                                day=days, $
                                hour=hours, $
                                minu=minutes)

  beg_idldt = start_idldt

  FOR f=0,nf-1 DO BEGIN 

    q = q2brnoaaread( files[f],/raw )
    IF VarType(q) EQ 'STRUCTURE' THEN BEGIN 
      end_idldt = Dt_Add( beg_idldt, hour=1, minute=41)

      dt_to_var, beg_idldt, year=year, month=month, day=day, $
          hour=hour, min=min
      doy = PadAndJustify(date2doy( year, month, day ),3,/right)
      year = strtrim(year,2)
      month = PadAndJustify(month,2,/right)
      day = PadAndJustify(day,2,/right)
      minute = PadAndJustify(min,2,/right)
      hour = PadAndJustify(hour,2,/right)

      OFilename =  "QS" + year + month + day

      StartTime = year + '-' + doy + 'T' + hour + ":" + $
         minute + ":"  + '00.000'

      Ext = ".S" + hour + minute


      dt_to_var, end_idldt, year=year, month=month, day=day, $
          hour=hour, min=min
      doy = PadAndJustify(date2doy( year, month, day ),3,/right)
      year = strtrim(year,2)
      month = PadAndJustify(month,2,/right)
      day = PadAndJustify(day,2,/right)
      hour = PadAndJustify(hour,2,/right)
      minute = PadAndJustify(min,2,/right)
      EndTime = year + '-' + doy + 'T' + hour + ":" + $
         minute + ":"  + '00.000'
      Ext =  ext + ".E" + hour + minute

      OFilename =  OFilename + Ext

      openw, olun, Ofilename, /get, error=err
      IF err EQ 0 THEN BEGIN 
        hdr = "Ncells=76" 
        hdr = hdr + lf + "DataStartTime=" + StartTime 
        hdr = hdr + lf + "DataEndTime=" + EndTime
        hdr = hdr + lf + "EquatorCrossingLongitude=0.0"
        t = byte(hdr)
        nt = n_elements(t)
        hdr = bytarr(q2b_rnoaa_size)
        hdr[0:nt-1] = temporary(t)

        writeu,olun,hdr
        writeu,olun,q
        free_lun,olun

      ENDIF ELSE BEGIN 
        Message,"Can't write to file " + Ofilename
        return
      ENDELSE 
     beg_idldt =  end_idldt
    ENDIF ELSE MESSAGE,"Can't read " + file[f]
  ENDFOR 
END









