;+
; NAME:  cw_pvobj_form
; $Id$
; PURPOSE:  Compound Widget for managing info stored in the
;          PVPlotObject object.
;
;
; AUTHOR: William Daffer
;
;
; CATEGORY:  Widget/OO programming
;
;
;
; CALLING SEQUENCE:  cw_tlb=cw_pvobj_form( tlb, labels, Filenames,
;                   plotflags, SelectedOnlyFlags)
;
;
; 
; INPUTS:  
;
;   Tlb    : (I) Widget ID, The Base WidgetId in which this widget will reside
;   Labels : (I), string array, The Column labels. 
;   Names  : (I), string array, the names of the files from which this
;            data came. Now only an identifier.
;            These identifiers have suffixes which convey more
;            information about the data. They will appear in
;            parentheses after the file name. These identifiers are:
;
;            M - Model data. The 'selectedOnly' flag has no meaning
;                for this data.
;
;            S - Selected data. This data has already been reduced to
;                the 'Selected' vectors, no further reduction is possible.
;                Picking the 'selectedOnly' pick will have no effect
;                on this data 
;
;   PlotFlags: (I), Long array, Flag, 1 means plot this file, 0 means
;              don't
;
;   SelectedOnly: (I), Long Array, flag, 0 means the memory associated
;                with this identifier still has all
;                ambiguities. SelectedOnly=1 means this data has been
;                reduced as much as it can be, picking the
;            'selectedonly' pick will have no effect in this case.
;
;
;
; OPTIONAL INPUTS:   None
;
;
;	
; KEYWORD PARAMETERS:  None
;
;
; OUTPUTS:  The Widget Id of the Base of this Compound widget (or 0,
;          if there's a failure)
;
;
;
; OPTIONAL OUTPUTS:  None
;
;
;
; COMMON BLOCKS:  None
;
;
;
; SIDE EFFECTS:  None
;
;
;
; RESTRICTIONS:  None
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
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION cw_pvobj_form_events, event
  retevent = 0
  return, retevent
END

FUNCTION cw_pvobj_form, tlb, labels, Filenames, plotflag, SelectedOnly

   lf = string(10b)
   IF n_params() NE 6 THEN BEGIN 
     str = "Usage: " + lf + $
      " cw_tlb=cw_pvobj_form( tlb, labels, names, plotflags, $" + lf + $
             "plottedFlags, SelectedOnlyFlags)"
     Message,str,/cont
     return,0
   ENDIF 
   n1 = N_Elements(Filenames)
   n2 = n_Elements(plotFlag)
   n3 = n_Elements(SelectedOnly) 
   IF n1 ne n2 OR n2 NE n3 OR  n1 NE n3 BEGIN 
     Message,"All input arrays (Filenames, Plotflags, SelectedOnly) must have the same number of elements",/cont
     return,0
   ENDIF 

   IF n_elements(labels) NE 3 THEN BEGIN 
     Message,'There must be 3 labels ',cont
     return
   ENDIF 
   
   return, cw_tlb
END
