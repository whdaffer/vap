
;============================================
; Init
;============================================

FUNCTION cloudoverlay::Init, AreaFile, limits = limits
  status = 0
  IF n_params() EQ 1 THEN BEGIN 
    ;r = rstrpos(AreaFile,'/')+1
    r = strpos(AreaFile,'/',/reverse_search)+1
    self.AreaFile = strmid(AreaFile, r, strlen(AreaFile)-r)
    self.AreaFilePath = strmid( AreaFile,0, r )
    
    IF n_elements(limits) EQ 4 THEN BEGIN 
      
    ENDIF ELSE BEGIN 
      IF n_elements(limits) NE 0 AND $
         n_elements(limits) NE 4 THEN $
       Message,'limits must be 4 vector',/cont
      limits = [0,0,0,0]
    ENDELSE 
    exe_string = 'grid_goes -f ' + self.AreaFile + $
       '-p ' + self.AreaFilePath + ' -o oco.dat'
    x = where(limits,nx)
    IF nx NE 0 THEN BEGIN 
      ilimits = fix(limits)
      y = where(ilimits GT 180, ny )
      IF ny NE 0 THEN ilimits[y] =  ilimits[y]-360.
      LimitStr = strtrim(ilimits,2)
      LimitString = LimitStr[0]
      FOR i=1,3 DO LimitString = ","+LimitString
      LimitString = "-l "+ LimitString
      exe_string = exe_string + LimitString
    ENDIF 
    spawn,exe_string, ret
    goesfile = ret[0]
    read_pcgoes, goesfile, limits, data, hdr=hdr
    
    print,'Finished Gridding'
  ENDIF ELSE $
   Message,"Usage: overlay=Obj_new('cloudoverlay',areafile)",/cont

  return,status
END


;============================================
; Cleanup
;============================================

PRO cloudoverlay::Cleanup
   Ptre_free, self.wholedmap.PtrToData
   Ptre_free, self.thisSection.PtrToData
END


;============================================
; Set Routin
;============================================

PRO cloudoverlay::Set
END


;============================================
; Get Routine
;============================================

PRO cloudoverlay::Get
END



;============================================
; SelfHelp routine
;============================================

PRO cloudoverlay::SelfHelp
   ok = Message_Dialog( "cloudoverlay: No Self help available. Sorry!")
   Message,"No Self help available. Sorry!",/cont
END


;============================================
; Definition Routine
;============================================

PRO cloudoverlay__define
   hdr = goes_hdr_str()
   map =  { CloudOverlayMap,$
            limits: fltarr(4),$
            GridFile: '',$
            hdr: hdr,$
            resolution: fltarr(2),$
            PtrToData: Ptr_New() }
            
  junk = {CLOUDOVERLAY,$
         AreaFile: '',$
         AreaFilePath: '',$
         wholemap: replicate( {CloudOverlayMap},1 ),$
         thisSection: replicate( {CloudOverlayMap},1 ) }
END






