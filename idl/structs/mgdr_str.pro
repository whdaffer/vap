;+
; NAME:  mgdr_str.pro
; $Id$
; PURPOSE:  Define structure(s) to read Nscat 50Km MGDR data.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Nscat 50Km MGDR data I/O
;
; CALLING SEQUENCE:  structures = mgdr_str(nstruct)
; 
; INPUTS:  none
;
; OPTIONAL INPUTS:  nstruct: the number of mgdr structure you want (default=1)
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  An array of 'nstruct' mgdr structure.
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  mgdr_cmn - for keeping track of whether this
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

FUNCTION mgdr_str, nstruct,redefine=redefine


COMMON mgdr_cmn, mgdr_defined, mgdr_size, mgdr
;
mgdr_size =  12972; record length in bytes.
IF n_elements( mgdr_defined ) eq 0 OR keyword_set( redefine) THEN BEGIN

  mgdr = { $
        Time:           BYTARR(24),     $ ; row time
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
        errspd:         intarr(4,24),   $ ; formal uncertainty in speed
        winddir:        intarr(4,24),   $ ; retrieved directions of WV solutions
        errdir:         intarr(4,24),   $ ; formal uncertainty in direction
        mle_like:       intarr(4,24),   $ ; liklihood computed for solution
        l_wind_flg:     0L,             $ ; Low speed (< 3m/s) caution flag
        h_wind_flg:     0L,             $ ; hi speed (>30m/s) flag
        nsigma :        bytarr(24),     $ ; number of sigma0 measurments
        ngoodsig:       bytarr(24),     $ ; number of GOOD sigma0 measurments
        nbeamfore:      bytarr(24),     $ ; number of fore-beam sig0
        nbeammidv:      bytarr(24),     $ ; number of mid-beam v-pol sig0
        nbeammidh:      bytarr(24),     $ ; number of mid-beam h-pol sig0
        nbeamaft:       bytarr(24),     $ ; number of aft-beam sig0
        beamptr:        bytarr(5,4,24), $ ; array of pointers to sig0 by beam
        cenlat:         intarr(20,24),  $ ; Geodetic Latitude of sig0 cell
        cenlon:         intarr(20,24),  $ ; East Longitude of sig0 cell
        cellaz:         intarr(20,24),  $ ; azimuth of cell wrt north
        incang:         intarr(20,24),  $ ; incidence angle of sig0 cell
        sig0:           intarr(20,24),  $ ; normalized radar cross section
        coa:            intarr(20,24),  $ ; Kp variance coefficient alpha
        cob:            intarr(20,24),  $ ; Kp variance coefficient beta
        coc:            intarr(20,24),  $ ; Kp variance coefficient gamma
        pol:            bytarr(20,24),  $ ; polarization (1=vertical, 2=horizontal)
        wvci_25km:      intarr(20,24),  $ ; along track row number @25km
        wvcj_25km:      bytarr(20,24),  $ ; along track col number @25km
        sig0qualflag:   intarr(20,24),  $ ; quality flags on sig0 measurements
        sig0useflag:    bytarr(3,24),   $ ; flags denoting usability for wind
        sflag:          bytarr(20,24)   } ; land/ice flags

  mgdr_defined = 1

ENDIF

IF n_elements( nstruct ) eq 0 THEN nstruct = 1
IF nstruct le 0 THEN nstruct = 1

RETURN, replicate( mgdr , nstruct )
end





