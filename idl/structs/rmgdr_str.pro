;+
; NAME:  RMgdr_str.pro
; $Id$
; PURPOSE:  Used in reading the Nscat Rmgdr (Reduced MGDR) data.
;
; AUTHOR:  William Daffer
;
; CATEGORY:   Nscat Rmgdr (Reduced MGDR) data I/O
;
; CALLING SEQUENCE:  structures = Rmgdr_str(nstruct)
; 
; INPUTS:  
;
; OPTIONAL INPUTS:  nstruct: the number of structure you want (default=1)
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  an vector of 'nstruct' RMGDR structures.
;
; OPTIONAL OUTPUTS:  
;
; COMMON BLOCKS:  rmgdr_cmn - for keeping track of whether this
;                                 structure has already been defined.
;
; SIDE EFFECTS:  
;
; RESTRICTIONS:  
;
; PROCEDURE:  
;
; EXAMPLE:  
;
;  Quick and easy way to read a whole rmgdr file.
;
; Let's say the file is /a/b/rmgdr_file.bin. Then, in IDL execute the
; following code. (It's assumed that this code is in a procedure or
; function or a 'main level' procedure)
; 
; openr, rlun, '/a/b/rmgdr_file.bin',/get_lun, error=err
; if err eq 0 then begin
;   stat = fstat(rlun)       ; Get file information
;   nrecs = stat.size/1236l  ; determine the number of records you need
;   rec = rmgdr_str( nrecs ) ; create that many recrods
;   readu, rlun, rec         ; read the data in one fell swoop
;   free_lun, rlun           ; free the logical unit number
; endif else begin
;   message,!err_string,/cont
;    return
; endelse 
; 
; Of course, you still have to convert the wind quantities. See
; read_rmgdr_data.pro for that.
;
; Either See the NSCAT MGDR SIS or the code in read_rmgdr_data.pro for
; how to scale this data. Also note that lon and winddir ( and errdir if
; after Dec 30th, See Note *1 below) are unsigned integers and must be
; handled with care.

;
;
; *** Note *1.***
;  After about December 30th, 1996, the errspd and errdir fields have
;been filled instead with the u ( in errspd) and the v (in errdir)
;components of the NMC predict wind field. This is the field that is
;used in the nudging process and it's useful to have this data in the
;structure to compare with the wind processor generated output. All
;four ambiguities have the same data in them, so for these fields in
;the structure, it makes no difference which ambiguity you chose, and
;you treat them exactly as you treat the windspd and winddir
;quantities,i.e they're scaled the same as their counterparst and
;errdir is also a unsigned integer, just like winddir.  
;
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 1996, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION rmgdr_str, nstruct

COMMON rmgdr_cmn, rmgdr_size, rmgdr_defined 
RMGDR_SIZE = 1236L ; for M. Spencer's reduced MGDR format
; sizeof( rmgdr) = 12972 bytes, R.S.DUNBAR'S MGDR format

IF n_elements( rmgdr_defined ) eq 0 THEN BEGIN

  rmgdr = { rmgdr,                      $
        Time:          BYTARR(24),      $ ; row time
	Rev:			0,      $ ; rev number
        Row:                    0,      $ ; along track row # @ 50 km
        Lat:            INTARR(24),     $ ; geodectic latitude of WVC
        Lon:            INTARR(24),     $ ; East Longitude of WVC
        Col:            BYTARR(24),     $ ; Cross-track cell number @ 50km
        wvcqualflag:    BYTARR(24),     $ ; Quality flag (1=all ocean, 4=bad)
        mean_wind:      INTARR(24),     $ ; Average speed of wind solutions
        nambig:         BYTARR(24),     $ ; # of vector ambiguities
        wvc_sel:        BYTARR(24),     $ ; index of selected wind vector
        windspd:        intarr(4,24),   $ ; retrieved speeds ov WV solutions
        errspd:         intarr(4,24),   $ ; formal uncertainty in speed (see Note *1 above)
        winddir:        intarr(4,24),   $ ; retrieved directions of WV solutions
        errdir:         intarr(4,24),   $ ; formal uncertainty in direction (see Note *1)
        mle_like:       intarr(4,24),   $ ; liklihood computed for solution
        l_wind_flg:             0l,     $ ; Low speed (< 3m/s) caution flag
        h_wind_flg:             0L      $ ; hi speed (>30m/s) flag
                                         }

  rmgdr_defined = 1

ENDIF

IF n_elements( nstruct ) eq 0 THEN nstruct = 1
IF nstruct le 0 THEN nstruct = 1

RETURN, replicate( { rmgdr } , nstruct )
end





