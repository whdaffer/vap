;+
; NAME:   PvPlotObject__define
; $Id$
; PURPOSE:   Defines and object of type PvPlotObject
;            This object is the object maintained in the linked list
;            of objects within the PV object. It has flags and such
;            for use in manipulating and plotting the data inside PV. 
;
;
; AUTHOR: William Daffer
;
;
; CATEGORY:   QSCAT/Seawinds plotting
;
;
;
; CALLING SEQUENCE:   PvPlotObject = obj_new('PvPlotObject',data)
;
;
; 
; INPUTS:  
;
;   Data: must be an object. No restriction is made, as yet, on what
;         type.
;
;
;
; OPTIONAL INPUTS:  
;
;
;	
; KEYWORD PARAMETERS:  
;
;
;
; OUTPUTS:  
;
;
;
; OPTIONAL OUTPUTS:  
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
; Revision 1.1  1998/10/26 22:09:44  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

;============================================
; Init
;============================================

FUNCTION PvPlotObject::Init, data
  status = 1
  self.PlotFlag = 1
  self.AlreadyPlotted = 0
  self.SelectedOnly = 0
  self.InRegion = -1 ; Don't know
  IF obj_valid( data ) THEN BEGIN 
    s = data-> Get(data=tdata)
    IF s THEN BEGIN 
      name = Tag_Names(*tdata,/Structure_Name)
      test =  where( $
                    strpos( ['Q2BDATA' ,'RQ2BDATA','QMODEL'  ], name) NE -1, $
                    ntest)
      IF ntest NE 0 THEN BEGIN 
        self.data =  data
      ENDIF ELSE BEGIN 
        Message,'Unrecognized data type ' + name,/cont
        status = 0
      ENDELSE 
    ENDIF ELSE BEGIN 
      status=0
      Message,"Can't get data from input object",/cont
    ENDELSE 
  ENDIF ELSE status = 0
  return,status
END


;============================================
; Cleanup
;============================================

PRO PvPlotObject::Cleanup
  Obj_Destroy, self.data
END


;============================================
; SetData Routine
;============================================

PRO PvPlotObject::SetData, data
   IF N_Elements(data) THEN BEGIN 
     IF obj_valid( data ) THEN BEGIN 
       Obj_Destroy, self.data
       self.data = data
     ENDIF ELSE $
       Message,' Data must be an Object, No change made ',/cont
   ENDIF 
END



;============================================
; SetData Routine
;============================================

PRO PvPlotObject::SetPlotFlag, PlotFlag
   IF N_Elements(PlotFlag) THEN BEGIN 
     Self.PlotFlag = PlotFlag
   ENDIF ELSE self.PlotFlag = 0
END



;============================================
; SetAlreadyPlotted Routine
;============================================

PRO PvPlotObject::SetAlreadyPlotted, AlreadyPlotted
  IF N_Elements(AlreadyPlotted) THEN BEGIN 
    Self.AlreadyPlotted = AlreadyPlotted
  ENDIF ELSE self.AlreadyPlotted = 0
END

;============================================
; SetAlreadyPlotted Routine
;============================================

PRO PvPlotObject::Set, $
                AlreadyPlotted = AlreadyPlotted,$
                InRegion   = InRegion, $
                PlotFlag       = PlotFlag


  IF N_Elements(AlreadyPlotted) NE 0 THEN $
     self.AlreadyPlotted = AlreadyPlotted

  IF N_Elements(InRegion) NE 0 THEN $
     self.InRegion = InRegion

  IF N_Elements(Plotflag) NE 0 THEN $
     self.Plotflag = Plotflag


                
END

;============================================
; SelectedOnly - If this is Q2BDATA, convert to RQ2BDATA
;============================================

PRO PvPlotObject::SelectedOnly
   name =  Tag_Names(*self.data,/structure_Name)

   IF name EQ 'Q2BDATA' THEN BEGIN 
     self.SelectedOnly = 1
     self.data->GetAll, su = su, sv=sv, lon=lon, lat=lat, $
                   sel=sel, qual=qual, nambig=nambig, row=row
     dims = size(su, /dimensions )
     q = rq2b_str( dims[2], ncells=dims[1] )
     q.su     = temporary(su) 
     q.sv     = temporary(sv) 
     q.lon    = temporary(lon)
     q.lat    = temporary(lat)
     q.qual   = temporary(qual)
     q.nambig = temporary(nambig)
     q.row    = temporary(row)
     
     Obj_Destroy, self.data
     self.data =  Obj_New("Q2B", data=q )

   ENDIF ELSE $
     Message,name + ' Data already as reduced as possible',/info
END

;============================================
; InRegion
;============================================

PRO PvPlotObject::InRegion, InRegion
  IF N_Elements(InRegion) THEN BEGIN 
    Self.InRegion = InRegion
  ENDIF ELSE self.InRegion = 0
END


;============================================
; Get Routine
;============================================

PRO PvPlotObject::Get, $
                Data            = Data, $
                PlotFlag        = PlotFlag, $
                AlreadyPlotted  = AlreadyPlotted, $
                SelectedOnly    = SelectedOnly, $
                InRegion        = InRegion

   IF Arg_Present( Data ) THEN data = self.data
   IF Arg_Present( SelectedOnly ) THEN $
      SelectedOnly = self.SelectedOnly
   IF Arg_Present( PlotFlag ) THEN $
      PlotFlag = self.PlotFlag
   IF Arg_Present( AlreadyPlotted ) THEN $
      AlreadyPlotted = self.AlreadyPlotted
   IF Arg_Present( InRegion ) THEN $
      InRegion = self.InRegion
END


;============================================
; SelfHelp routine
;============================================

PRO PvPlotObject::SelfHelp
   ok = Message_Dialog( "PvPlotObject: No Self help available. Sorry!")
   Message,"No Self help available. Sorry!",/cont
END


;============================================
; Version
;============================================

FUNCTION PvPlotObject::Version

     ; Version number for this class

   rcsid = "$Id$"

     ; Find version number for member objects.
   s=execute( 'tags=tag_names({' + 'PvPlotObject' + '})' ) 
   n_tags = n_elements(Tags)
   i = 0
   WHILE i LE n_tags-1 DO BEGIN 

     catch, error
     IF error NE 0 THEN BEGIN 
         ; Ignore 'undefined method' errors
       IF strpos( strupcase(!Error_state.Msg), $
                  "UNDEFINED METHOD" ) NE -1 THEN BEGIN 
         error = 0
         i = i+1
       ENDIF ELSE BEGIN 
         Message,!error_state.msg,/cont
         return,''
       ENDELSE 
     ENDIF 
     
     IF VarType( self.(i) ) EQ 'OBJECT' THEN BEGIN 
       IF obj_valid( self.(i)) THEN BEGIN 
         V =  Call_Method( "VERSION", self.(i) )
         nv = N_Elements(V)
         IF exist(member_versions) THEN $
            member_versions =  [ member_versions, v ] ELSE $
            member_versions =  v
       ENDIF 
     ENDIF 
     i =  i+1
   ENDWHILE 

     ; find version number for superclasses.
   super = Obj_Class(self,/Super,count=cnt)
        
   IF cnt NE 0 THEN BEGIN 
     WHILE i LE cnt-1 DO BEGIN 
       catch, error
       IF error NE 0 THEN BEGIN 
           ; Ignore 'undefined method' errors
         IF strpos( strupcase(!Error_state.Msg), $
                    "UNDEFINED METHOD" ) NE -1 THEN BEGIN 
           error = 0
           i = i+1
         ENDIF ELSE BEGIN 
           Message,!error_state.msg,/cont
           return,''
         ENDELSE 
       ENDIF 

       V  = call_method("VERSION",super[i])

       IF exist( super_versions ) THEN $
         super_versions =  [super_versions, v ] ELSE $
         super_versions =  v 
       i = i+1

     ENDWHILE 
   ENDIF

   versions =  rcsid

   IF exist(super_versions) THEN $
      versions =  [versions, super_versions]

   IF exist( member_versions ) THEN $
      versions =  [versions, member_versions ] 

   Catch,/cancel
  return,versions(uniq(versions,sort(versions)))
END


;============================================
; Definition Routine
;============================================

PRO PvPlotObject__define
  junk = { PVPLOTOBJECT, $
          AlreadyPlotted : 0L, $
          PlotFlag       : 0L,$
          SelectedOnly   : 0l ,$
          InRegion       : 0l,$
          data           : Obj_New() }
END
