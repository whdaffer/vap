;+
; NAME:  LinkedList__Define
; PURPOSE:  Creates a Linked List
;
;
; AUTHOR; William Daffer
;
;
; CATEGORY:  OO
;
;
;
; CALLING SEQUENCE:  ll=Obj_New('LinkedList'[,data])
;
;
; 
; INPUTS:  Data - can be anything
;
;
;
; OPTIONAL INPUTS:  None
;
;
;	
; KEYWORD PARAMETERS:  None
;
;
;
; OUTPUTS:   If successful, and object of type 'linkedlist'
;
;
;
; OPTIONAL OUTPUTS:  None
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
; EXAMPLE:  ll=Obj_New('LinkedList',fltarr(10))
;
;
;
; MODIFICATION HISTORY:
; $Log$
; Revision 1.6  1999/01/22 22:20:14  vapuser
; Changed *( to (* in 'RemoveCurrent'
;
; Revision 1.5  1998/10/28 23:29:41  vapuser
; Add WhichNode method
;
; Revision 1.4  1998/10/23 22:23:59  vapuser
; Can't remember
;
; Revision 1.3  1998/10/01 17:53:09  vapuser
; Modified 'version' method so that it will report
; the versions of member classes. Put in some error handling
; so that it'll ignore calls to undefined 'version' methods.
;
; Revision 1.2  1998/10/01 15:45:54  vapuser
; Removed rcsid member, added 'version' method. Made rcsid local to version method.
;
; Revision 1.1  1998/09/30 23:44:02  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

;----------------------------------------
; Init 
;----------------------------------------
FUNCTION LinkedList::Init,data
  COMMON nodebase, nodebase

  IF n_params() EQ 1 THEN BEGIN 
    node =  nodebase
    node.data =  Ptr_New(data)
    self.head = Ptr_New( node )
    self.current = self.head
    self.tail = self.head
    self.count = 1
  ENDIF ELSE BEGIN 
    self.head =  Ptr_New()
    self.tail = self.head
    self.current = self.head
  ENDELSE 
  return,1
END
;----------------------------------------
; cleanup
;----------------------------------------
PRO LinkedList::cleanup
  current = self->GetHead()
  WHILE Ptr_Valid(current) AND self.count GE 1 DO BEGIN 
    self->DeleteCurrent
    current = self->GetHead()
  ENDWHILE 

END

;----------------------------------------
; GetCurrent
;----------------------------------------

FUNCTION LinkedList::getCurrent
  return,self.current
END

;----------------------------------------
; GetHead
;----------------------------------------
FUNCTION LinkedList::gethead
  IF ptr_valid( self.head) THEN BEGIN 
    self.current = self.head
    return, self->GetCurrent()
  ENDIF ELSE return, Ptr_New()
END


;----------------------------------------
; GetTail
;----------------------------------------

FUNCTION LinkedList::GetTail
  IF ptr_valid( self.tail) THEN BEGIN 
    self.current = self.tail
    return, self->GetCurrent()
  ENDIF ELSE return, Ptr_New();
END


;----------------------------------------
; GetNext
;----------------------------------------

FUNCTION LinkedList::GetNext
  current =  self.current
  IF Ptr_Valid( current ) THEN BEGIN 
    current = *current
    self.current = current.next
    return, self->GetCurrent()
  ENDIF ELSE return, Ptr_New();
END



;----------------------------------------
; GetPrev
;----------------------------------------

FUNCTION LinkedList::GetPrev
   current = self.current
  IF Ptr_Valid( current ) THEN BEGIN 
    current = *current
    self.current = current.prev
    return, self->GetCurrent()
  ENDIF ELSE return, Ptr_New() ;

END


;----------------------------------------
; Prepend
;----------------------------------------

FUNCTION LinkedList::Prepend, data
  COMMON nodebase, nodebase
  status = 0
  IF n_params() NE  0 THEN BEGIN 
    node = nodebase
    node.data =  Ptr_New(data)
    IF Ptr_Valid( self.head ) THEN BEGIN 
      node.next =  self.head
      self.head = Ptr_New(node)
      (*(*self.head).next).prev =  self.head
    ENDIF ELSE BEGIN 
      self.head =  Ptr_new(node)
      self.tail = self.head
    ENDELSE 
    self.current = self.head
    self.count =  self.count + 1
    status = 1
  ENDIF 
  return,status
END

;----------------------------------------
;  Append
;----------------------------------------

FUNCTION LinkedList::Append, data
  COMMON nodebase, nodebase
  status = 0
  IF n_params() NE  0 THEN BEGIN 
    node = nodebase
    node.data =  Ptr_New(data)
    
    IF Ptr_Valid(self.tail) THEN BEGIN 
      node.prev =  self.tail
      (*self.tail).next =  Ptr_New(node)
      self.tail =  (*self.tail).next
    ENDIF ELSE BEGIN 
      self.head = Ptr_New(node)
      self.tail = self.head
    ENDELSE 
    self.current = self.head
    self.count =  self.count + 1
    status = 1

  ENDIF 
  return,status
END


;----------------------------------------
; AppendList
;----------------------------------------
FUNCTION LinkedList::AppendList, added_list
  COMMON nodebase, nodebase
  status = 0
  IF n_params() NE  0 THEN BEGIN 
    IF OBJ_ISA(added_list,'LINKEDLIST' ) THEN BEGIN 
      node =  nodebase
      cc = added_list->GetHead()
      WHILE Ptr_Valid(cc) DO BEGIN 
        node.data =  Ptr_New( *(*cc).data )
        node.prev =  self.tail
        (*(self.tail)).next =  Ptr_New(node)
        self.count = self.count + 1
        self.tail =  (*(self.tail)).next 
        cc = added_list-> GetNext()
      ENDWHILE 
      status = 1
    ENDIF 
  ENDIF 
  return,status
END



;----------------------------------------
; RemoveCurrent
;----------------------------------------

FUNCTION LinkedList::RemoveCurrent
    ; Removes current node from list, repairs the gap and returns the
    ; removed node. Sets current node to what was current->next before
    ; the call to this routine. Hence, one may destroy this list by
    ; starting at the head and calling this method until there a NULL
    ; pointer is returned, then there are no nodes left.

  current = self.current
  IF ptr_valid(self.current) THEN BEGIN 
    IF ptr_valid( (*self.current).prev ) THEN BEGIN 
      prev = (*self.current).prev 
      (*prev).next =  (*self.current).next
    ENDIF 
    IF ptr_valid( (*self.current).next ) THEN BEGIN 
      next = (*self.current).next
      (*next).prev =  (*self.current).prev
    ENDIF
    head = self->GetHead()
    IF current EQ head THEN $
      head =  (*head).next
    tail = self->GetTail()
    IF current EQ tail THEN $
      tail =  (*tail).prev
  ENDIF 
  self.count = self.count-1
  return,current
END

;----------------------------------------
; GotoNode
;----------------------------------------
FUNCTION LinkedList::GoToNode, index
  return_node = Ptr_New()
  IF index LE self.count AND  $
     index GE 1 THEN BEGIN 
    CASE index OF 
      1: return_node = self->GetHead()
      self.count: return_node = self->GetTail()
      ELSE: BEGIN 
        current = self->GetHead()
        current_index = 1
        WHILE ( Ptr_Valid(current) AND $
                current_index LE self.count AND $
                current_index LT index ) DO BEGIN 
          current =  self->GetNext()
          current_index = current_index+1
        ENDWHILE
        IF current_index EQ index THEN return_node = current
      END
    ENDCASE 
  ENDIF ELSE Message, 'Requested Index must be >=1 and <= number of nodes in list',/cont
    
  return, return_node
END


;----------------------------------------
; GetCount
;----------------------------------------

FUNCTION LinkedList::GetCount
  return, self.count
END


;----------------------------------------
; NodeCount
;----------------------------------------

FUNCTION LinkedList::NodeCount, Repair = Repair
  count = 0
  c = self.head
  WHILE Ptr_Valid(c) DO BEGIN 
    count =  count+1
    c = (*c).next
  ENDWHILE
  IF keyword_set( Repair )  THEN self.count =  count
  return, count
END

;----------------------------------------
; InsertBefore
;----------------------------------------
FUNCTION LinkedList::InsertBefore, data
  COMMON nodebase, nodebase

  status = 0
  IF n_params() EQ 1 THEN BEGIN 
    node =  nodebase
    current = self->GetCurrent()
    IF NOT ptr_valid( current ) THEN $
       current = self->GetHead()

    CASE 1 OF 
      NOT ptr_valid(self.head )  : BEGIN
        ; Empty List
        node.data =  Ptr_New(data)
        self.head = Ptr_New(node)
        self.tail = self.head
        self.count =  self.count+1
        status = 1
      END
      self.current EQ self.head: BEGIN 
        ; Insert before head
        status = self->Prepend(data)
      end
      ELSE: BEGIN 
        ; Inside list proper.
        node.data =  Ptr_New(data)
        node.prev = (*current).prev
        node.next =  current
        (*(*current).prev).next = Ptr_New(node)
        (*current).prev =  (*(*current).prev).next 

        self.count =  self.count+1
        status = 1
      END
    ENDCASE 
  ENDIF 
  return, status
END


;----------------------------------------
; InsertAfter
;----------------------------------------

FUNCTION LinkedList::InsertAfter, data
  COMMON nodebase, nodebase
  status = 0
  node = nodebase
  current = self->GetCurrent()

  IF n_params() EQ 1 THEN BEGIN 
    IF NOT ptr_valid( current ) THEN $
       current = self->GetTail()
    CASE 1 OF
      NOT Ptr_valid( self.head ): BEGIN 
        ; Empty list.
        node.data =  Ptr_New(data)
        self.head = Ptr_New(node)
        self.tail = self.head
        status = 1
        self.count = self.count+1
      END 
      self.current EQ self.tail: BEGIN 
        ; Insert at end of list
        status =  self->Append( data )
      END
      ELSE: BEGIN 
        ; Insert with list proper
        node.data = Ptr_New(data)
        node.prev = current
        node.next = (*current).next
        (*(*current).next).prev =  Ptr_New(node)
        (*current).next =  (*(*current).next).prev 
        self.count =  self.count+1
        status = 1
      END
    ENDCASE 
  ENDIF 
  return, status
END 

;----------------------------------------
; IsEmpty
;----------------------------------------

FUNCTION LinkedList::IsEmpty
  ; Returns 1 if empty, 0 otherwise.
  RETURN,self.head EQ Ptr_New();
END

;---------------------------------------
; GetCurrentDataPtr
;----------------------------------------

FUNCTION LinkedList::GetCurrentDataPtr
   ; Returns pointer to data stored at 'current' node
   IF ptr_valid( self.current ) THEN $
     ret_ptr = (*self.current).data ELSE $
     ret_ptr = Ptr_New()
  return, ret_ptr
END

;---------------------------------------
; SetData
; Resets the data pointer  of the current node to the input data. If
; the keyword 'node' is set, the method first makes that the current
; node.
;
;----------------------------------------

FUNCTION LinkedList::SetData, data, node = node
  COMMON nodebase, nodebase
    ; Sets data at current node
  status = 0
  IF N_Params() EQ 1 THEN BEGIN 
    IF N_Elements(node) NE 0 THEN junk = self->GotoNode(node)
    IF Ptr_Valid( self.current ) THEN BEGIN 
       Ptr_Free, (*self.current).data 
       (*self.current).data =  Ptr_New(data)
       status = 1
    ENDIF 
  ENDIF 
  RETURN, status
END


;---------------------------------------
; Print (more like a 'help' on each node) 
;----------------------------------------

PRO LinkedList::Print, full = full
  full = keyword_set( full )
  current_save=self.current
  self.current = self.head
  IF ptr_valid( self.current ) THEN BEGIN 
    ptr = self->GetCurrentDataPtr()
    IF full THEN BEGIN 
      help, *self.current
      help, *self.current,/st
    ENDIF 
    IF Ptr_Valid(ptr) THEN help,*ptr ELSE Message,'Invalid Pointer!'
    current = self->GetNext()  
    WHILE  ptr_valid(current) DO BEGIN 
      ptr = self->GetCurrentDataPtr()
      IF full THEN BEGIN 
        help, *self.current
        help, *self.current,/st
      ENDIF 
      IF Ptr_Valid( ptr ) THEN help,*ptr ELSE Message,'Invalid Pointer!'
      current = self->GetNext()
    END 
  ENDIF 
  self.current=current_save
END



;---------------------------------------
; DeleteCurrent 
;  Like 'Removecurrent' but doesn't return
;  the current node and frees the data
;  pointer of current node.
;----------------------------------------

PRO LinkedList::DeleteCurrent

  IF ptr_valid(self.current) THEN BEGIN 

    current = self.current
    data_ptr =  (*current).data
    data = *data_ptr
    IF VarType(data[0]) EQ 'OBJECT' THEN Obj_Destroy,data
    Ptr_Free, data_ptr

    IF ptr_valid( (*self.current).prev ) THEN BEGIN 

      prev = (*self.current).prev 
      (*prev).next =  (*self.current).next
    ENDIF 
    IF ptr_valid( (*self.current).next ) THEN BEGIN 
      next = (*self.current).next
      (*next).prev =  (*self.current).prev
    ENDIF
    IF current EQ self.head THEN BEGIN 
      self.head =  (*self.head).next
      self.current = self.head
    ENDIF 
    IF current EQ self.tail THEN BEGIN 
      self.tail =  (*self.tail).prev
      self.current = self.tail
    ENDIF
    Ptr_Free,current
    self.count = self.count-1
    a = 1
  ENDIF 
END



;============================================
; Version
;============================================

FUNCTION LinkedList::Version

     ; Version number for this class
   rcsid = "$Id$"

     ; Find version number for objects in the list
   n = self-> Gethead()
   i = 0
   WHILE Ptr_Valid( n ) DO BEGIN 

     data = self->GetCurrentDataPtr()
     IF Ptr_Valid(data) THEN BEGIN 
       IF VarType(*data) EQ 'OBJECT' THEN BEGIN 
         IF Obj_Valid(*data) THEN BEGIN 
            v = Call_Method( "VERSION", *data)
            IF exist( list_versions ) THEN $
               list_versions = [list_versions,v] ELSE $
               list_versions = v
         ENDIF 
       ENDIF 
     ENDIF 

     catch, error
     IF error NE 0 THEN BEGIN 
         ; Ignore 'undefined method' errors
       IF strpos( strupcase(!Error_state.Msg), $
                  "UNDEFINED METHOD" ) NE -1 THEN BEGIN 
         error = 0
       ENDIF ELSE BEGIN 
         Message,!error_State.msg,/cont
         return,''
       ENDELSE 
     ENDIF 

     n = self->GetNext()
   ENDWHILE 

   versions =  rcsid

   IF exist(list_versions) THEN $
      versions =  [versions, list_versions]

   Catch,/cancel
  return,versions(uniq(versions,sort(versions)))
END


;----------------------------------------
; WhichNode - returns which node the list 
; is currently set to.
;----------------------------------------

FUNCTION LinkedList::WhichNode
  nodenum = 0
  IF NOT self->IsEmpty() THEN BEGIN 
    REPEAT nodenum = nodenum+1 UNTIL NOT Ptr_Valid(self->GetPrev())
    s = self-> GotoNode(nodenum)
  ENDIF 
  return,nodenum
END

;----------------------------------------
; Define
;----------------------------------------


PRO LinkedList__define
  COMMON nodebase, nodebase

  nodebase = { nodebase, $
               data: Ptr_New(), $
               prev: Ptr_New(), $
               next: Ptr_New() }


  struct =  { LinkedList,$
              count   : 0L,$
              head    : Ptr_New() ,$
              tail    : Ptr_New() ,$
              current : Ptr_New() }
END


