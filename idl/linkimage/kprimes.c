/*
 *
 * Kprimes.c (an IDL linkimage routine)
 * 
 * Calculate the first n primes, where N is input to the routine
 *
 * Taken from RSIs primes.pro, which proved *way* to slow!
 *
 * Author: William Daffer
 * Copyright(c) William Daffer, 1998
 * All Rights Reserved
 * No Warranties, use at own risk
 *
 *
 * Modification Log:
 *
 * $Log$
 *
 *
 * $Id$
 *
 */

#include <stdio.h>
#include "/usr/local/rsi/idl_5.1/external/export.h"

IDL_VPTR kprimes( int argc, IDL_VPTR argv[], char *argk )
{
  IDL_VPTR nPrimesIDL,tmp, retval;
  IDL_VPTR ReturnPrimesIDL;
  IDL_VPTR nPrimes2;

  long nprimes;
  long *ReturnPrimesP;

  static char kprimes_rcsid[]="$Id$";

  int count,i,j,q,r;
  int n;
  long prm;

  IDL_LONG dim[IDL_MAX_ARRAY_DIM];

  int rtype;

  /* Start program */

  if ( argc < 1 ) 
    IDL_Message( IDL_M_GENERIC, 0, IDL_MSG_LONGJMP, 
		 "usage: firstNprimes=primes(n)" );

  /*printf("Getting nPrimesIDL\n");*/

  nPrimesIDL=argv[0];

  /*  printf("Done\n");*/

  /*  printf("Getting ReturnPrimesIDL tmp\n")  ;
  ReturnPrimesIDL=IDL_Gettmp();
  printf("Done\n")  ;
  */
  retval=IDL_Gettmp();
  retval->type=IDL_TYP_LONG;
  retval->value.l=0;



  /*printf("nprimes=%d\n", nPrimesIDL->value.i);*/

  /*  printf("Converting nPrimesIDL\n");  */
  if (nPrimesIDL->type != IDL_TYP_LONG) {
    /*printf("Converting\n");*/
    nPrimes2  = IDL_CvtLng(1,&nPrimesIDL);
  } else {
    /*printf("Copying\n");*/
    nPrimes2  = nPrimesIDL;
  }
  /*printf("Done\n");*/
  nprimes = nPrimes2->value.l;
  /*  printf("nprimes = %d\n",nprimes);
  printf("nprimes->type = %d\n",nPrimes2->type);*/

  /*  printf("Deleting nPrimes2\n");
    IDL_Deltmp(nPrimes2);
  printf("Returning\n");*/
  /*return (retval);*/
  
  dim[0]=nprimes;
  rtype=IDL_TYP_LONG;
  
  /*IDL_Deltmp(nPrimes2);
  return(retval); */

  if (nprimes<=0){
    IDL_Message(IDL_M_NAMED_GENERIC, 
		IDL_MSG_INFO || IDL_MSG_ATTR_SYS,
		"Can't allocate 'data' pointer");
    return(retval);
  }

  /*printf("Getting Scratch\n");*/
  /*  
      if ( (ReturnPrimesP = (long *) 
	IDL_GetScratch( &ReturnPrimesIDL, (IDL_LONG) nprimes,
			sizeof( IDL_LONG ) )) == NULL ) {

    IDL_Message(IDL_M_NAMED_GENERIC, 
		IDL_MSG_INFO || IDL_MSG_ATTR_SYS,
		"Can't allocate 'data' pointer");
    return(retval);
  }
  */

  if ( (ReturnPrimesP = (long *) 
	IDL_MakeTempArray(rtype,1,dim,IDL_BARR_INI_NOP, 
			  &ReturnPrimesIDL)) == NULL) {

    IDL_Message(IDL_M_NAMED_GENERIC, 
		IDL_MSG_INFO || IDL_MSG_ATTR_SYS,
		"Can't allocate 'data' pointer");
    return(retval);

  }

  /*printf("Got Scratch\n");*/

  if (nprimes==1) {
    /*    printf("nprimes=1\n");*/
    *ReturnPrimesP = 2;
    return (ReturnPrimesIDL);
  }
  

  /*printf("Starting loop\n");*/

  *ReturnPrimesP=2; 
  n=3;
  count=1;
  *(ReturnPrimesP+count)=3;

  
  case2: count += 1;

  while( count < nprimes) {
    case1:
    n += 2;
    /*printf("Outside loop, n=%d\n",n);*/
    for (i=1;i<count;i++){
        /*printf("   inside loop, i=%d\n",i);*/
      prm=*(ReturnPrimesP+i);
      r = n % prm;
      if (r==0) 
	goto case1;
      q=n/prm;
      if (q <= prm){
	/* Prime */
          /*printf("        Found prime %d\n", n);*/
	*(ReturnPrimesP+count) = n;
	goto case2;
      }
    }
  }

  /*printf("Done Loop\n");*/
  if ((nPrimes2->flags & IDL_V_TEMP) != 0) {
    /*printf("Deleting nPrimes2\n");*/
    IDL_Deltmp(nPrimes2);
  }
  /*printf("Deleting retval\n");*/
  IDL_Deltmp(retval);
  /*printf("Returning\n");*/
  return( ReturnPrimesIDL);

}

  


