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
; Revision 1.5  1999/10/05 16:40:39  vapuser
; Added member data 'starttime','endtime' and 'creationtime' and some
; code to support them.
;
; Revision 1.4  1999/02/27 00:45:14  vapuser
; Change 'QMODEL' to 'QMODELDATA' in some tests
;
; Revision 1.3  1998/10/29 22:37:31  vapuser
; added filename member/keywords
;
; Revision 1.2  1998/10/28 23:35:37  vapuser
; ??
;
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

FUNCTION PvPlotObject::Init, data, $
                     Filename=Filename
  status = 1
  self.PlotFlag = 1
  self.UserPlotFlag = 1
  self.AlreadyPlotted = 0
  self.SelectedOnly = 0
  self.InRegion = -1 ; Don't know
  self.starttime = obj_new('eatime')
  self.endtime = obj_new('eatime')
  self.creationtime = obj_new('eatime')

  IF N_Elements(Filename) NE 0 THEN $
    self.Filename = Filename

  IF obj_valid( data ) THEN BEGIN 
    s = data-> Get(data=tdata, starttime=starttime, endtime=endtime)
    self.starttime->set,vaptime = starttime
    self.endtime->set,vaptime = endtime
    IF s THEN BEGIN 
      name = Tag_Names(*tdata,/Structure_Name)
      test =  where( $
                    strpos( ['Q2BDATA' ,'RQ2BDATA','QMODELDATA'  ], name) NE -1, $
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
  obj_destroy, self.starttime  
  obj_destroy, self.endtime
  obj_destroy, self.creationtime 
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
; SetPlotFlag Routine
;============================================

PRO PvPlotObject::SetPlotFlag, PlotFlag
   IF N_Elements(PlotFlag) THEN BEGIN 
     Self.PlotFlag = PlotFlag
   ENDIF ELSE self.PlotFlag = 0
END

;============================================
; SetUserUserPlotFlag Routine
;============================================

PRO PvPlotObject::SetUserPlotFlag, UserPlotFlag
   IF N_Elements(UserPlotFlag) THEN BEGIN 
     Self.UserPlotFlag = UserPlotFlag
   ENDIF ELSE self.UserPlotFlag = 0
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
; Set Routine
;============================================

PRO PvPlotObject::Set, $
                AlreadyPlotted = AlreadyPlotted,$
                InRegion       = InRegion, $
                PlotFlag       = PlotFlag,$
                UserPlotFlag       = UserPlotFlag,$
                Filename       = Filename


  IF N_Elements(AlreadyPlotted) NE 0 THEN $
     self.AlreadyPlotted = AlreadyPlotted

  IF N_Elements(InRegion) NE 0 THEN $
     self.InRegion = InRegion

  IF N_Elements(Plotflag) NE 0 THEN $
     self.Plotflag = Plotflag

  IF N_Elements(UserPlotFlag) NE 0 THEN $
     self.UserPlotFlag = UserPlotFlag

  IF N_Elements(Filename) NE 0 THEN $
     self.Filename = Filename


                
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
                UserPlotFlag        = UserPlotFlag, $
                AlreadyPlotted  = AlreadyPlotted, $
                SelectedOnly    = SelectedOnly, $
                InRegion        = InRegion, $
                Filename        = Filename

   IF Arg_Present( Data ) THEN data = self.data
   IF Arg_Present( SelectedOnly ) THEN $
      SelectedOnly = self.SelectedOnly
   IF Arg_Present( PlotFlag ) THEN $
      PlotFlag = self.PlotFlag

   IF Arg_Present( UserPlotFlag ) THEN $
      UserPlotFlag = self.UserPlotFlag

   IF Arg_Present( AlreadyPlotted ) THEN $
      AlreadyPlotted = self.AlreadyPlotted
   IF Arg_Present( InRegion ) THEN $
      InRegion = self.InRegion
   IF Arg_Present( Filename ) THEN $
      Filename = self.Filename
   
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
          filename       : '',$
          starttime      : Obj_new(), $
          endtime        : obj_new(), $
          creationTime   : Obj_New(),$
          AlreadyPlotted : 0L, $
          PlotFlag       : 0L,$
          UserPlotFlag   : 0l, $
          SelectedOnly   : 0l ,$
          InRegion       : 0l,$
          data           : Obj_New() }
END
