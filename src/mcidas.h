/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */
/**** McIDAS Revision History *** */
/* 1 MCIDAS.H 7-Sep-93,10:43:42,`RUSSD' Inital release to AIX (4069)         */
/* 2 MCIDAS.H 5-Oct-93,8:11:34,`USER' Released for McIDAS-X Only             */
/* 3 MCIDAS.H 1-Mar-94,9:59:50,`RUSSD' Standardize function names            */
/* 4 MCIDAS.H 31-Mar-94,22:52:54,`BARRYR' Add proprietary statement          */
/* 5 MCIDAS.H 2-May-94,17:04:50,`USER' Released                              */
/* 6 MCIDAS.H 3-Aug-94,18:36:48,`DWS' Modified for Solaris (4731)            */
/* 7 MCIDAS.H 22-Aug-94,6:46:52,`USER' Released for McIDAS-X Only            */
/**** McIDAS Revision History *** */


/*******************************************************************\
          Prototypes for functions in the McIDAS library.
\*******************************************************************/


#ifndef _MCIDAS_H
#define _MCIDAS_H

#include <stdio.h>

/*
 * FsLen is the size of the "string length argument" used in
 * FORTRAN interfaces.
 */
#ifndef _FsLen
#define _FsLen
    typedef int FsLen;
#endif


/* Fint2 is the C equivalent of Fortran INTEGER*2 */
#ifndef _Fint2
#define _Fint2
    typedef short Fint2;
#endif


/* Fint4 is the C equivalent of Fortran INTEGER*4 */
#ifndef _Fint4
#define _Fint4
    typedef int Fint4;
#endif


/* Freal4 is the C equivalent of Fortran REAL*4 */
#ifndef _Freal4
#define _Freal4
    typedef float Freal4;
#endif


/* Freal8 is the C equivalent of Fortran REAL*8 */
#ifndef _Freal8
#define _Freal8
    typedef double Freal8;
#endif

/* SPACE is a symbolic name for the ASCII space character */ 
#define SPACE 0x20 


/*******************************************************************
 *           GROUP: X Window System hooks and interfaces
 * 
 * askit_() - Solicits a typed-in response from the user.
 *
 * beep_() - Make an audible tone on the workstations speaker.
 *
 * curaix_() - Drives cursor for image window.
 *
 * clcsiz_() - Returns closest cursor size available.
 *
 * dfxwindow_() - Set up pixmap and Ximage for DF command.
 *
 * drawline_() - Draw a line from (x,y) to (x2,y2) at specified level.
 *
 * erasaix_() -
 *
 * eresaix_() -
 *
 * gbind_() - Flush display buffer.
 * 
 * getgrax_() -  Set a graphic color level to a specified RGB value.
 *
 * getimage_() - Returns a virtual frame (used by SVG).
 *
 * getsiz_() - Gets the minimun and maximum size of a font given the font name.
 *
 * initdfx_() - Set up pixmap and Ximage for DF command.
 *
 * initpix_() - Set up pixmap for graphics programs to write into.
 *
 * makgif() - Create a GIF file.
 *
 * putgrax_() - Put graphics on X display.
 *
 * putimage_() - Put an image out to the display.
 *
 * sendevnt_() - Send an event to X window system.
 *
 * sendtext_() - Write text to current text window.
 *
 * setfnt_() - Set which X font to use for graphics writing.
 *
 * uxsubs.c - IBM code needed for Motif, etc.  See uxxt.h
 *
 * winmsg_() -??? 
 * 
 * xtext_() - Write a string at location x,y on the text window.
 *
 * xtogif_() - Write a .gif file for SVGA command.
 */

extern void 
xtogif_(Fint4 *bframe, char *filename);

extern void 
xtext_ (Fint4 *y, Fint4 *x, char *string, Fint4 *len, Fint4 *level, Fint4 *reqheight, Fint4 *type, FsLen lenc);

extern void 
winmsg_(Fint4 *windowid);

extern long 
setfnt_(char *fontname, Fint4 *whichuc, FsLen len);

extern void 
sendtext_(char *ctext, Fint4  *window, Fint4 *color, FsLen lentext);

extern void 
sendevnt_(void);

extern void 
putimage_(char *cname, Fint4 *size, Fint4 *frame, Fint4 *point, Fint4 *wandh, Fint4 *conv_flg, FsLen len);

extern void 
putgrax_(Fint4 *red, Fint4 *green, Fint4 *blue, Fint4 *pix, Fint4 *level, Fint4 *dframe);

extern void 
makgif(void *imagedata, char *filename, int width, int height, int bitsperpixel, int *red, int *green, int *blue);

extern void 
initdfx_(Fint4 *frame);

extern void 
clcsiz_(Fint4 *lsiz, Fint4 *esiz, Fint4 *dislsiz, Fint4 *disesiz);

extern void 
curaix_(Fint4 *type, Fint4 *red, Fint4 *green, Fint4 *blue, Fint4 *height, Fint4 *width);

extern void 
dfxwindow_(Fint4 *frame, Fint4 *tvl, short *buf);

extern void
drawline_ (Fint4 *y1, Fint4 *x1, Fint4 *y2, Fint4 *x2, Fint4 *level);

extern void 
initpix_(Fint4 *frame, Fint4 *width);

extern void 
getsiz_(char *fontname, Fint4 *min , Fint4 *max, FsLen len);

extern void 
getimage_(Fint4 *bframe, Fint4 *eframe);

extern void 
getgrax_(Fint4 *pix, Fint4 *red, Fint4 *green, Fint4 *blue, Fint4 *level, Fint4 *frame);

extern void 
askit_(char *question, char *cstring, Fint4 *ret, FsLen qlen, FsLen clen);

extern void
beep_(Fint4 *freq, Fint4 *time);

extern void 
erasaix_(Fint4 *Option, Fint4 *Frame);

extern void 
eresaix_(Fint4 *table, Fint4 *frame);

extern void 
gbind_(void);




/*******************************************************************
 *            GROUP: misc byte and word manipulation routines
 *
 * blka_() - Write blanks into a buffer.
 *
 * crack_() - Unpack bytes into integer *4 words. 
 *
 * crack2_() - Unpack bytes into integer*2 words.
 *
 * filbuf_() - Fill buffer with a constant value. 
 *
 * ic_() - Extract a specified byte from a buffer.
 *
 * icw_() - Extract specified number of 2-byte groups from a buffer.
 *
 * int4_() - Return an integer*4 value from an integer*2 argument.
 *
 * ischar_() - Returns non-zero if all four characters in word are printable.
 *
 * itoa() - Convert integer to character string representation.
 *
 * maaatb_() -
 *
 * mavhtb_() -
 *
 * movb_() - Move bytes with offset.
 *
 * movblk_() - Move blocks with offsets and increments.
 *
 * movbmem_() - Move bytes from memory to memory with offset.
 *
 * movc_() - Move bytes with offsets for both source and destination.
 *
 * movpix_() - Move bytes with increments and offsets.
 *
 * movw_() - Move words (4 byte groups).
 *
 * mpixel_() - Move pixels with packing and unpacking.
 *
 * mpixtb_() - Move data with a table look-up replacement.
 *
 * mvastb_() - In-place VAS (Mode AA) pixel cracker/packer with table lookup.
 *
 * pack_() - Pack words (4 bytes hunks) into bytes in output buffer.
 *
 * pack2_() - Pack shorts (2 byte hunks) into bytes in output buffer.
 *
 * stc_() - Put a byte into a buffer.
 *
 * stcrep_() - Store a single value into consequtive bytes of buffer.
 *
 * swbyt2_() - Make a 2 byte word "big endian" order.
 * 
 * swbyt4_() - Make a 4 byte word "big endian" order.
 *
 * zeros_() - Zero out bytes in a buffer.
 */
void 
zeros_(void *buf, Fint4 *nbytes);

extern void 
swbyt2_(void *buf, Fint4 *n);

extern void 
swbyt4_(void *buf, Fint4 *n);

extern void 
stc_(Fint4 *val, void *buffer, Fint4 *offset);

extern void 
stcrep(Fint4 val, void *buffer, Fint4 *offset, Fint4 repfac);

extern void 
pack2_(Fint4 *n, Fint2 *sou, void *destination);

extern void 
pack_(Fint4 *n, Fint4 *sou, void *destination);

extern void 
mvastb_(Fint4 *n, Fint4 *isou, Fint4 *ides, unsigned char buf[], Fint4 itab[], Fint4 *ysubz);

extern void 
mpixtb_(Fint4 *n, Fint4 *isou, Fint4 *ides, void *buffer, Fint4 itab[]);

extern int 
mpixel_(Fint4 *n, Fint4 *isou, Fint4 *ides, void *buffer);

extern void 
movw_(Fint4 *num, void *inbuf, void *outbuf);

extern void 
movpix_(Fint4 *n, void *source, Fint4 *soff, Fint4 *sinc, void *destination, Fint4 *doff, Fint4 *dinc);

extern void 
movc_(Fint4 *num, unsigned char inbuf[], Fint4 *soff, unsigned char outbuf[], Fint4 *doff);

extern void 
movbmem_(Fint4 *num, Fint4 *inaddr, Fint4 *outaddr, Fint4 *offset);

extern void 
movblk_(Fint4 *n, Fint4 *ssiz, void *source, Fint4 *soff, Fint4 *sinc, void *destination, Fint4 *doff, Fint4 *dinc);

extern void 
movb_(Fint4 *number, void *inbuffer, void *outbuffer, Fint4 *offset);

extern void 
mavhtb_(Fint4 *n, Fint4 *isou, Fint4 *ides, unsigned char buf[], Fint4 itab[]);

extern void 
maaatb_(Fint4 *n, Fint4 *isou, Fint4 *ides, unsigned char buf[], Fint4 itab[]);

extern Fint4 
ischar_(void *value);

extern void 
itoa(Fint4 value, char *string);

extern Fint4 
int4_(Fint2 *val);

extern Fint4 
ic_(void *buf, Fint4 *offset);

extern Fint4 
icw_(void *buf, Fint4 *offset);

extern void 
filbuf_(Fint4 *nbytes, Fint4 *value, void *buffer, Fint4 *offset);

extern void 
blka_(Fint4 *nwords, void *buf);

extern void 
crack_(Fint4 *n, void *source, Fint4 *des);

extern void 
crack2_(Fint4 *n, void *source, Fint2 *des);

extern void 
fbyte2_(void *buffer, Fint4 *n);

extern void 
fbyte4_(void *buffer, Fint4 *n);



/*******************************************************************
 *              GROUP: Low-level file I/O
 *
 * iopn_() - Open a file for read/write.
 *
 * dirfil_() - Get a sorted list of files in a subdirectory.
 *
 * delfil_() - Delete the named file.
 *
 * leof_() - Find EOF in a file and return its location.
 *
 * ih_() - Determine the path to a file and get its handle.
 *
 * scra_() - Set the position pointer in a file for the next I/O.
 *
 * dr_() - Read a block from a file.
 *
 * dw_() - Write a block to a file.
 *
 * dc_() - Close a file.
 */

extern void 
scra_(Fint2 *fileid, Fint4 *off);

extern Fint4 
leof_(Fint2 *fileid);

extern Fint4 
iopn_(char *file, FsLen length);

extern void 
dc_(Fint2 *fileid);

extern Fint4
ih_(char *name, FsLen namelen);

extern Fint4 
delfil_(char *file, FsLen length);

extern Fint4 
dirfil_(char *cpattern, char *cpath, char *outarray, Fint4 *numfiles, char *redext, char *redfile, char *cpext, Fint4 *maxarr, Fint2 pattlen, Fint2 pathlen, Fint2 outlen, Fint2 redextlen, Fint2 redfilelen, Fint2 pattextlen);

extern Fint2 
dr_(Fint2 *fileid, void *ibuf, Fint4 *nb, Fint4 *retstat);

extern Fint2 
dw_(Fint2 *fileid, void *ibuf, Fint4 *nb, Fint4 *retstat);


/*******************************************************************
 *               GROUP: misc McIDAS system functions
 *
 * fgetline() - Returns a pointer to a character string
 * 	containing the next line from the data stream.
 *
 * fsalloc() - Returns pointer to a string with a 0 at end.
 *
 * strtofs() - Copies NUL-terminated C string to space-padded
 *	Fortran string.
 *
 * getuc() - Enables access to UC for a specified terminal number.
 *
 * asciiz_() - Puts a zero on the end of a string, in situ.
 *
 * ingroupset() - Returns true if the given gid is in the group
 * 	set of this process, false otherwise.  
 *
 * inilux_() - Give program access to UC area.
 *
 * canexec() - Test whether file is executable.
 *
 * chredo_() - Change permissions on file 'redo'
 *
 * dfflist_() - Called by who and returns a file list of maps.
 *
 * clist_() - Make a list of maps via ls and write into file maplst.
 *
 * kilpid_() - Kills a specific process ID and prints a message.
 *
 * getday_() - Returns the current day (YYDDD format).
 *
 * gettim_() - Returns the current time (HHMMSS format).
 *
 * laddr_() - Return the address of the argument.
 *
 * listpgm_() - List all PGM and MAC to file 'pgmlist' for HELP.
 *
 * lock_() - Set global system lock (semaphore).
 *
 * unlock_() - Unlock a system lock set by 'lock' (semaphore).
 *
 * luc_() - Return a value from User Common.
 *
 * lucx_() - Return a value from User Common for another 'terminal'.
 *
 * puc_() - Put an integer value into User Common.
 *
 * sentran() - Return ID for a given semaphore.
 *
 * sencmd_() - Send command to operating system and route output to
 * 	the current text window.
 *
 * aquiresem() - Attempt to acquire a semaphore.
 *
 * releasesem() - Release a semaphore.
 *
 * deletesem() - Delete a semaphore.
 *
 * sighand() - Routine handle error signals.
 *
 * setsignal() - Routine to set a signal.
 *
 * cleanup() - Clean up after signal and terminate.
 *
 * sleep_() - Sleep for milliseconds duration specified.
 *
 * sqpgm_() - Fortran callable routine to start a McIDAS command.
 *
 * systyp() - Return pointer to OS name (e.g. "AIX")
 *
 * systyp_() - Set given string to OS name (e.g. "AIX")
 *
 * upath_() - Get home directory of user.
 *
 * stralloc() - Dynamically allocate string from the concatenation
 * 	of the given strings.
 *  
 * eaccess() - Indicate whether the given pathname is accessible.
 *
 * fpaddr_() - Give caller access to REDIRECT tables in shared memory 
 *            for specific terminal (number).
 *
 * sgetsf3_() - Make a specific segment addressable by caller.
 *
 * nodename() - Get the name of the machine.
 */

extern void
asciiz_(char *string, FsLen length);

extern Fint4 
sgetsf3_(Fint4 *smidptr, Fint4 **altaddr);

extern char *
fgetline(FILE *fileptr);

extern Fint4 *
fpaddr_(void);

extern char *
stralloc(char *s, ...);

extern char *
fsalloc(char *string, FsLen length);

extern void
strtofs(char *dst, char *src, FsLen ndst);

extern void 
upath_(char *user, char *path, FsLen ulen, FsLen plen);

extern char * 
systyp(void);

extern void 
systyp_(char *ctype, FsLen len);

extern void 
sencmd_(char *cmd_arg, FsLen length);

extern Fint4 
sqpgm_(char *pgmname, char *tokens, Fint4 *sync);

extern void 
sleep_(Fint4 *millsecs);

extern void 
sighand(void);

void 
setsignal( int , void (*)() );

void 
cleanup(int);

extern int 
semtran(int key);

extern void 
aquiresem(int sid);

extern void 
releasesem(int sid);

extern int 
deletesem(int sid);

extern void 
puc_(Fint4 *value, Fint4 *index);

extern void 
lock_(char *name, FsLen length);

extern void 
unlock_(char *name, FsLen length);

extern Fint4 
luc_(Fint4 *index);

extern Fint4 
lucx_(Fint4 *index);

extern void 
listpgm_(char *path, FsLen len);

extern void 
kilpid_(Fint4 *pid);

extern Fint4 
laddr_(Fint4 variable);

extern int 
ingroupset(int gid);

extern void 
inilux_(Fint4 *init, Fint4 *term);

extern long *
getuc(int iterm);

extern void 
gettim_(Fint4 *itime);

extern int 
eaccess(char *path, int amode, int type);

extern void 
getday_(Fint4 *iday);

extern int 
canexec(char *name);

extern void 
chredo_(void);

extern void 
clist_(void);

extern void 
dfflist_(void);

extern char *
nodename(void);

/*******************************************************************
 *               GROUP: communication utilities
 *
 * connct_() - Sets up and calls function 'connect' for TCP/IP.
 *
 * discon_() - Disconnect a socket.
 *
 * opnasync_() - Open the serial port for broadcast ingest.
 *
 * recvs_() - Sets up and calls function 'recv' for TCP/IP communications.
 *
 * selects_() - Determine if any data ready for reading on socket.
 *
 * sends_() - Sets up and calls function 'send' for TCP/IP.
 */

extern Fint4 
sends_(Fint4 *ns, char *buf, Fint4 *nr);

extern Fint4 
selects_(Fint4 *ns);

extern Fint4 
recvs_(Fint4 *ns, char *inbuf, Fint4 *i);

extern Fint4 
opnasync_(char *cfile, FsLen length);

extern Fint4 
connct_(Fint4 *addr, Fint4 *s);

extern void 
discon_(Fint2 *socket);


/*******************************************************************
 *              GROUP: McPATH-related functions
 *
 * pathcat() - Return a pointer to the concatenation of the two
 * 	given strings separated by a '/'.
 * 
 * pathtok() - Parse the given colon-separated path string.  
 * 
 * pathvec() - Return the vector of strings constructed from
 * 	the given colon-separated path string.
 *
 * dirpathvec() - Return the vector of directory names constructed from
 * 	the given vector of strings.
 *
 * envdirpathvec() - Return the vector of directory names constructed
 * 	from the value of the given environment variable
 *
 * pathvecsearch() Searche for a file named 'name' of the given type
 * 	along the given path vector, accessible with the given mode.
 *
 * VecNew() - Initialize arg vector for vector of pointers to strings.
 *
 * VecAdd() - Add a char* to the end of the given vector.
 * 
 * VecOld() - Delete the vector and free allocated space.
 *
 * VecLen() - Returns number of pointers in the vector.
 *
 * MCPATHvec() - Return a vector of the directories listed in the
 * 	MCPATH environment variable.
 *
 * PATHvec() - Return a vector of the directories listed in the
 * 	PATH environment variable.
 */
extern char **
VecNew(void);

extern char ** 
VecAdd(char **vec, char *s);

extern int 
VecOld(char **vec);

extern int 
VecLen(char **vec);

extern char *
pathvecsearch(char **pathvec, char *name, int amode, int type);

extern char *
pathcat(char *prefix, char *suffix);

extern char *
pathtok(char *str);

extern char **
pathvec(char *pathstr);

extern char **
dirpathvec(char **srcvec);

extern char **
envdirpathvec(char *name);

extern char *
enhseg (long iterm);

extern char **
PATHvec(void);

extern char **
MCPATHvec(void);


#endif  /* _MCIDAS_H  */
