/*
 * qread_rmgdr.c
 *
 * Subroutine to read one record of a SeaWinds on QuikSCAT Reduced
 * Merged GDR (RMGDR) Vap file.
 *
 * William Daffer
 * William.Daffer@jpl.nasa.gov
 * 818-354-0161
 *
 * $Id$
 *
 * Modifications:
 *
 * $Log$
 * Revision 1.1  2001/02/23 18:27:20  vapuser
 * Initial revision
 *
 *
 * 
 */
#include <stdio.h>
#include "qscat_rec.h"

int qread_rmgdr( FILE *fp, QREC *rec, int reclen ) {

  int n=0;
  if (fp == NULL) {
    fprintf(stderr,"qread_rmgdr: Missing FILE pointer\n");
    return (0);
  }
  
  if (rec == NULL) {
    fprintf(stderr,"qread_rmgdr: Missing QREC pointer\n");
    return (0);
  }

  if ( (n=fread(rec, reclen, 1, fp)) != 1) {
    if (!feof(fp)) {
      fprintf(stderr,"qread_rmgdr: Bad Read!\n");
      return(0);
    }
  }
  return (1);
}
  
