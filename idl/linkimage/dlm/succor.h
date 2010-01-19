#ifndef SUCCOR_H
#define SUCCOR_H
#include <stdio.h>
#include "idl_export.h"
#define ARRLEN(arr) (sizeof(arr)/sizeof(arr[0]))

#define SUCCOR_ERROR 0

static IDL_MSG_DEF msg_arr[] = {
  {"EQUIVLAT_ERROR","%N: Error: %s."}
};

static IDL_MSG_BLOCK msg_block;
extern void succor_exit(void);
extern int succor_startup(void);

int IDL_Load(void);


#define ARRLEN(arr) (sizeof(arr)/sizeof(arr[0]))


#ifdef DEBUG
#define DEBUGMSG(s) printf(s)
#else
#define DEBUGMSG(s) "/*NOTHING*/"
#endif 

#endif 
