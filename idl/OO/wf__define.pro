
;============================================
; Init
;============================================

FUNCTION wf::Init, filename
  self-> set,filename
  return,1
END


;============================================
; Cleanup
;============================================

PRO wf::Cleanup
END


;============================================
; Set Routin
;============================================

PRO wf::Set, $
      filename = filename
  IF n_params() EQ 0 THEN return
  ;s = rstrpos( filename,'/' ) + 1
  s = strpos(filename,'/',/reverse_search)+1
  basename =  strmid( filename, s, strlen(filename)-s)
  ;tmp = str_sep( filename, '.' );
  tmp = strsplit(filename,'.',/extract)
  IF n_elements(tmp) eq 3 THEN BEGIN 
    self.filename = filename
    yyyymmdd = strmid(tmp[0],1,8 );
    start_hhmm = strmid( tmp[1], 1, 4 )
    end_hhmm = strmid( tmp[2], 1, 4 );
    date = strmid(base_time,0,4) + '/' + $
             strmid(base_time,4,2) + '/' + $
               strmid(base_time(6,2)

    self.start_time = str_to_dt( date, start_hhmm, date_fmt=5, time_fmt=-2)
    self.end_time =  str_to_dt( date, end_hhmm,  date_fmt=5, time_fmt=-2)
    IF start_hhmm GT end_hhmm THEN self.end_time =  dt_add( self.end_time, day=1);
  ENDIF 

END


;============================================
; Get Routine
;============================================

PRO wf::Get, $
      filename = filename, $
      start_time=start_time, $
      end_time=end_time
   filename = self.filename
   start_time = self.start_time
   end_time = self.end_time
END



;============================================
; SelfHelp routine
;============================================

PRO wf::SelfHelp
   ok = Message_Dialog( "wf: No Self help available. Sorry!")
   Message,"No Self help available. Sorry!",/cont
END

;============================================
; Definition Routine
;============================================

PRO wf__define
  junk = {wf,$
         filename: '',$
         start_time: {IDLDT},$
         end_time: {IDLDT}    }
END
