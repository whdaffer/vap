;+
; NAME:   CheckForLock
; $Id$
; PURPOSE:  Checks for a lock file that has the name 'dir' +
;           user+'.'+filename, see's whether is has the input pid in
;           it. If it does, it returns 1, otherwise it returns one of
;           a set of other numbers as flags indicating which failure
;           occured.
;
;
;
; AUTHOR; William Daffer
;
;
; CATEGORY:  File I/O
;
;
;
; CALLING SEQUENCE:  
;
;      locked=CheckForLock( pid, filename, user, dir=dir  )
;
;
; 
; INPUTS:  
;
;        Pid - Pid to compare with the contents of the lock file
;        Filename - The latter portion of the lock file name. The full
;                   name of the file has the name of the user
;                   prepended to whatever occurs in this variable,
;                   e.g. if user=root and filename='auto_movie.lock'
;                   then the full file is root.autom_movie.lock
;
;
;
;
; OPTIONAL INPUTS:  
;
;       User - the user whose lock file we want to check. If not
;              input, user=GetEnv("USER")
;
;          
;	
; KEYWORD PARAMETERS:  
;
;    Dir - the directory in which to lock. Default=/tmp
;
;
;
;
; OUTPUTS:  
;
;     0 - Not Locked
;     1 - Locked for this Pid
;    -1 - Can't find any lock file for this user
;    -2 - Found a lock file, but can't open it.
;
;
;
; OPTIONAL OUTPUTS:  
;
;
;
; COMMON BLOCKS:  None
;
;
;
; SIDE EFFECTS:  
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
; Revision 1.1  1998/10/22 21:17:47  vapuser
; Initial revision
;
;
;Jet Propulsion Laboratory
;Copyright (c) 1998, California Institute of Technology
;Government sponsorship under NASA Contract NASA-1260 is acknowledged.
;-

FUNCTION CheckForLock, pid, filename, user, dir=dir
  rcsid = "$Id$"
  IF n_params() ge 2 THEN BEGIN 
    IF n_elements(dir) EQ 0 THEN dir = '/tmp'
    IF N_Elements(user) EQ 0 THEN User = GetEnv("USER")
    f = findfile(dir + '/' + user + '.' + filename,count=n)
    IF n NE 0 THEN BEGIN 
      openr, lun, f[0],/get,err=err
      IF err NE 0 THEN BEGIN 
        Message,!error_state.msg,/cont
        locked = -2
      ENDIF 
      ppid = 0l
      readf, lun, ppid
      locked = (ppid EQ pid)
    ENDIF ELSE locked=0
  ENDIF ELSE locked = -1
  return,locked
END


