;+
; NAME:  q2bhdf__define.pro
; $Id$
; PURPOSE:  Defines Q2bHDF object
;
; AUTHOR:  William Daffer
;
; CATEGORY:  Qscat 2b Data manipulation
;
; CALLING SEQUENCE:  q2b=obj_new('q2bhdf' [,filename=filename])
; 
; INPUTS:  none
;
; OPTIONAL INPUTS:  none
;	
; KEYWORD PARAMETERS:  filename: A fully qualified name of a Q2b HDF file.
;
; OUTPUTS:  An object of type Q2b
;
; OPTIONAL OUTPUTS:  none
;
; COMMON BLOCKS:  none
;
; SIDE EFFECTS:  Heap variables are created. 
;
; RESTRICTIONS:  
;
;   A heap variable created. It must be destroyed in the correct
;   manner. If you don't know what that means, you ; probably
;   shouldn't be using this routine. Go read the IDL online help on
;   'object's and see the printed material in the book _Objects and
;   Object Graphics_.
;
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
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

  ; ===============================================================
  ;
  ; Q2BHDF::Init: 
  ;
  ; Initializes Q2BHDF object
  ; 
  ; Relevant Quantities
  ; 4: number of ambiguities
  ; n: number of crosstrack colums (nominally 76)
  ; m: Number of records
  ; 
  ; All keywords are Input quantities. 
  ; Returns 1 for success, 0 for failure.
  ;
  ; The routine first checks for the 'filename' keyword. If present,
  ; it tries to read that file. The status returned by the read
  ; routine is passed along by this routine.
  ; 
  ; The routine then checks for the 'data_struct' keyword. If the
  ; data_struct keyword is present, it tries to take the data from it,
  ; subject to the restriction that the datum pointed to by the
  ; keyword is a structure of type 'Q2BDATA.' (see the file
  ; q2b_str.pro for a definition) If this keyword is not present, it
  ; will check for the first positional parameter. If it is present,
  ; then ALL 14 must be present.
  ; 
  ; 
  ; Each of the postional  parameters must be the correct dimension,
  ; but will be cast to the  correct type without concern for the
  ; effect of that cast. Caveat Emptor.  
  ;
  ; The EQX structure will be constructed from the eqx_* positional
  ; parameters, if they are present. Each will be cast as needed,
  ; without regard for the effect of that cast. If these aren't
  ; present, self.eqx will not be set unless the eqx keyword is present.
  ; The Eqx quantity pointed to by the EQX keyword must be a structure
  ; of type EQX. (See the file eqx_str.pro for the definition)
  ;
  ; ===============================================================

FUNCTION q2bhdf::init, $
            u,     $ ; fltarr [4,n,m]
            v,     $ ; "         "
            lon,   $ ; fltarr [n,m]
            lat,   $ ; fltarr [n,m]
            sel,   $ ; intarr [n,m]
            sel2,  $ ; intarr [n,m]
            idx,   $ ; intarr [n,m]
            row,   $ ; intarr [n,m]
            qual,  $ ; intarr [n,m]
            nambig,$ ; intarr [n,m]
            mu,    $ ; fltarr [n,m]
            mv,    $ ; fltarr [n,m]
            su,    $ ; fltarr [n,m]
            sv,    $ ; fltarr [n,m]
            eqx_date, $ ; scalar string
            eqx_time, $ ; scalar string
            eqx_lon,  $ ; scalar string
            data_struct = data_struct, $ ; structure of type Q2BDATA
            eqx=eqx,$                    ; structure of type EQX
            filename=filename            ; filename from which to read
                                         ; the data


  status = 0
  self.hdf = 1
  IF n_elements(filename) NE 0 THEN BEGIN 
      ; First check the file to see if it's HDF
    status = self->Read(filename)
    self.nrecs = N_Elements( *self.data )
    t = self.data
    self.ncells =  N_Elements( ((*t)[0]).su )
    self.decimate = 2
  ENDIF ELSE $
     status=self->Q2B::Init( u,v,lon,lat,sel,sel2,idx,row,qual,nambig,$
                             mu,mv,su,sv,eqx_date,eqx_time,eqx_lon,svhflag = 0,$
                             data_struct=data_struc,eqx=eqx)

  RETURN,status
END


  ; ==========================================
  ;
  ; Read the input filename
  ;
  ; ==========================================

FUNCTION q2bhdf::read, filename
   status = 0
  IF Ptr_Valid( self.data ) THEN free, ptr_free, self.data 
  data =  q2bhdfread( filename, eqx=eqx )
  IF VarType(data) eq 'STRUCTURE' THEN BEGIN 
    self.eqx = eqx
    self.data =  Ptr_New(data)
    status = 1
  ENDIF ELSE Message,'Bad Read for file ' + filename,/cont
  return,status
END


  ; ==========================================
  ;
  ; Q2BHDF::Cleanup
  ;
  ; ==========================================

PRO Q2BHdf::Cleanup
   self->Q2b::Cleanup
END

  ; ==========================================
  ;
  ; Object Definition Module
  ;
  ; ==========================================


PRO q2bhdf__define
  junk = { Q2BHDF, $
           hdf:0, $
           INHERITS Q2B }

END





