;+
; NAME:  Rmgdr25_str.pro
; $Id$
; PURPOSE:  Used to read Nscat RMGDR (Reduced MGDR) 25 Km data.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Nscat RMGDR (Reduced MGDR) 25 Km data I/O
;
; CALLING SEQUENCE:  structs = rmgdr25_str(nstruct)
; 
; INPUTS:  
;
; OPTIONAL INPUTS:  nstruct: the number you want (default=1)
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  An vector of 'nstruct' RMGDR25 structures
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:   rmgdr25_cmn - for keeping track of whether this
;                                 structure has already been defined.
;
; SIDE EFFECTS:  none
;
; RESTRICTIONS:  none
;
; PROCEDURE:  
;
; EXAMPLE:  
;
; MODIFICATION HISTORY:
;
; $Log$
;
;Jet Propulsion Laboratory
;Copyright (c) 1996, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION rmgdr25_str, nstruct

COMMON rmgdr25_cmn, rmgdr25_size, rmgdr25_defined 
RMGDR25_SIZE = 2460L ; for M. Spencer's reduced MGDR hi res format
; sizeof( rmgdr) = 12972 bytes, R.S.DUNBAR'S MGDR format

IF n_elements( rmgdr25_defined ) eq 0 THEN BEGIN

  rmgdr25 = { rmgdr25,                      $
        Time:          BYTARR(24),     $ ; row time
	Rev:			0,      $ ; rev number
        Row:                    0,      $ ; along track row # @ 50 km
        Lat:            INTARR(48),     $ ; geodectic latitude of WVC
        Lon:            INTARR(48),     $ ; East Longitude of WVC
        Col:            BYTARR(48),     $ ; Cross-track cell number @ 50km
        wvcqualflag:    BYTARR(48),     $ ; Quality flag (1=all ocean, 4=bad)
        mean_wind:      INTARR(48),     $ ; Average speed of wind solutions
        nambig:         BYTARR(48),     $ ; # of vector ambiguities
        wvc_sel:        BYTARR(48),     $ ; index of selected wind vector
        windspd:        intarr(4,48),   $ ; retrieved speeds ov WV solutions
        errspd:         intarr(4,48),   $ ; formal uncertainty in speed
        winddir:        intarr(4,48),   $ ; retrieved directions of WV solutions
        errdir:         intarr(4,48),   $ ; formal uncertainty in direction
        mle_like:       intarr(4,48),   $ ; liklihood computed for solution
        l_wind_flg:     lonarr(2),      $ ; Low speed (< 3m/s) caution flag
        h_wind_flg:     lonarr(2)      $ ; hi speed (>30m/s) flag
;        nsigma :        bytarr(48),     $ ; number of sigma0 measurments
;        ngoodsig:       bytarr(48),     $ ; number of GOOD sigma0 measurments
;        nbeamfore:      bytarr(48),     $ ; number of fore-beam sig0
;        nbeammidv:      bytarr(48),     $ ; number of mid-beam v-pol sig0
;        nbeammidh:      bytarr(48),     $ ; number of mid-beam h-pol sig0
;        nbeamaft:       bytarr(48),     $ ; number of aft-beam sig0
;        beamptr:        bytarr(5,4,48), $ ; array of pointers to sig0 by beam
;        cenlat:         intarr(20,48),  $ ; Geodetic Latitude of sig0 cell
;        cenlon:         intarr(20,48),  $ ; East Longitude of sig0 cell
;        cellaz:         intarr(20,48),  $ ; azimuth of cell wrt north
;        incang:         intarr(20,48),  $ ; incidence angle of sig0 cell
;        sig0:           intarr(20,48),  $ ; normalized radar cross section
;        coa:            intarr(20,48),  $ ; Kp variance coefficient alpha
;        cob:            intarr(20,48),  $ ; Kp variance coefficient beta
;        coc:            intarr(20,48),  $ ; Kp variance coefficient gamma
;        pol:            bytarr(20,48),  $ ; polarization (1=vertical, 2=horizontal)
;        row25:          intarr(20,48),  $ ; along track row number @25km
;        col25:          bytarr(20,48),  $ ; cross track col number @25km
;        sig0qualflag:   intarr(20,48),  $ ; quality flags on sig0 measurements
;        sig0useflag:    bytarr(3,48),   $ ; flags denoting usability for wind
;        sflag:          bytarr(20,48)   } ; land/ice flags
                                         }

  rmgdr25_defined = 1

ENDIF

IF n_elements( nstruct ) eq 0 THEN nstruct = 1
IF nstruct le 0 THEN nstruct = 1

RETURN, replicate( { rmgdr25 } , nstruct )
end





