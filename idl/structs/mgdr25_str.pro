;+
; NAME:  mgdr25_str.pro
; $Id$
; PURPOSE:  Creates structure of the type needed to read Nscat mgdr 25
;          km data.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Nscat MGDR 25 Km data I/O
;
; CALLING SEQUENCE:  structure = mgdr25_str(nstruct)
; 
; INPUTS:  
;
; OPTIONAL INPUTS:  nstruct : the number of structure you want. default=1
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  An array of 'nstruct' structures.
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  mgdr25_cmn - for keeping track of whether this
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

FUNCTION mgdr25_str, nstruct


COMMON mgdr25_cmn, mgdr25_defined, mgdr25_size
;
mgdr25_size =  9252; record length in bytes.
IF n_elements( mgdr25_defined ) eq 0 THEN BEGIN

  mgdr25 = { mgdr25,                      $
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
        l_wind_flg:     lonarr(2),     $ ; Low speed (< 3m/s) caution flag
        h_wind_flg:     lonarr(2),     $ ; hi speed (>30m/s) flag
        nsigma :        bytarr(48),     $ ; number of sigma0 measurments
        ngoodsig:       bytarr(48),     $ ; number of GOOD sigma0 measurments
        nbeamfore:      bytarr(48),     $ ; number of fore-beam sig0
        nbeammidv:      bytarr(48),     $ ; number of mid-beam v-pol sig0
        nbeammidh:      bytarr(48),     $ ; number of mid-beam h-pol sig0
        nbeamaft:       bytarr(48),     $ ; number of aft-beam sig0
        beamptr:        bytarr(2,4,48), $ ; array of pointers to sig0 by beam
        cenlat:         intarr(6,48),  $ ; Geodetic Latitude of sig0 cell
        cenlon:         intarr(6,48),  $ ; East Longitude of sig0 cell
        cellaz:         intarr(6,48),  $ ; azimuth of cell wrt north
        incang:         intarr(6,48),  $ ; incidence angle of sig0 cell
        sig0:           intarr(6,48),  $ ; normalized radar cross section
        coa:            intarr(6,48),  $ ; Kp variance coefficient alpha
        cob:            intarr(6,48),  $ ; Kp variance coefficient beta
        coc:            intarr(6,48),  $ ; Kp variance coefficient gamma
        pol:            bytarr(6,48),  $ ; polarization (1=vertical, 2=horizontal)
        spare:          bytarr(6,48),  $ ; along track row number @25km
        sig0qualflag:   intarr(6,48),  $ ; quality flags on sig0 measurements
        sig0useflag:    bytarr(2,48),   $ ; flags denoting usability for wind
        sflag:          bytarr(6,48)   } ; land/ice flags

  mgdr25_defined = 1

ENDIF

IF n_elements( nstruct ) eq 0 THEN nstruct = 1
IF nstruct le 0 THEN nstruct = 1

RETURN, replicate( { mgdr25 } , nstruct )
end





