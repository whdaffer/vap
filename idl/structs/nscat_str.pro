;+
; NAME:  nscat_str.pro
; $Id$
; PURPOSE:  Defines a structure to be used in reading NSCAT data.
;
; AUTHOR:  William Daffer
;
; CATEGORY:  NSCAT Data I/O
;
; CALLING SEQUENCE:  structs = nscat_str(nstruct)
; 
; INPUTS:  
;
; OPTIONAL INPUTS:  nstruct: the number of structure you want (default=1)
;	
; KEYWORD PARAMETERS:  none
;
; OUTPUTS:  A vector of 'nstruct' structures.
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  nscat_cmn - for keeping track of whether this
;                                 structure has already been defined.
;
; SIDE EFFECTS:  none
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
;
;Jet Propulsion Laboratory
;Copyright (c) 1996, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION nscat_str, nstruct

COMMON nscat_cmn, nscat_defined 
; sizeof( nscat) = 12972 bytes

IF n_elements( nscat_defined ) eq 0 THEN BEGIN

  nscat = { nscat,                      $
        Row_Time:       BYTARR(24),     $ ; row time
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
        l_wind_flg:             0l,     $ ; Low speed (< 3m/s) caution flag
        h_wind_flg:             0L,     $ ; hi speed (>30m/s) flag
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
        row25:          intarr(20,24),  $ ; along track row number @25km
        col25:          bytarr(20,24),  $ ; cross track col number @25km
        sig0qualflag:   intarr(20,24),  $ ; quality flags on sig0 measurements
        sig0useflag:    bytarr(3,24),   $ ; flags denoting usability for wind
        sflag:          bytarr(20,24)   } ; land/ice flags

  nscat_defined = 1

ENDIF

IF n_elements( nstruct ) eq 0 THEN nstruct = 1
IF nstruct le 0 THEN nstruct = 1

RETURN, replicate( { nscat } , nstruct )
end





