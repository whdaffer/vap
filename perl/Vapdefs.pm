#!/bin/perl -w
#
# $Id$
#
# Name: Vapdefs.pm: 
# Author: William H. Daffer
# Purpose: Put all the VAP defaults in one package.
#
# Modification Log:
# 
# $Log$
# Revision 1.2  2002/04/30 20:23:22  vapdev
# Modified the 'use lib' statement
#
# Revision 1.1  2001/02/09 19:13:52  vapuser
# Initial revision
#
#
# 
package Vapdefs;
use lib $ENV{'VAP_SFTWR_PERL'};
use Exporter ();
@ISA = qw(Exporter);
@EXPORT=qw( VAP_LIBRARY VAP_OPS_TOP VAP_WINDS VAP_ANIM 
	   VAP_OVERLAY VAP_WWW_TOP ARCHIVETOP 
	   GRIDDINGTOP IDLEXE VAP_OVERLAY_ARCHIVE 
	   VAP_WWW_TOP vap_is_batch );

use strict;

BEGIN {

  $VAP_LIBRARY=$ENV{VAP_LIBRARY} || die "Env var VAP_LIBRARY is undefined!\n";
  $VAP_OPS_TOP = $ENV{VAP_OPS_TOP} || die "Env var VAP_OPS_TOP is undefined!\n";
  $VAP_WWW_TOP = $ENV{VAP_WWW_TOP} || die "Env var VAP_WWW_TOP is undefined!\n";
  $VAP_GOES_TOP = $ENV{VAP_GOES_TOP} || die "Env var VAP_GOES_TOP is undefined!\n";

  $VAP_WINDS = $VAP_DATA_TOP unless defined($VAP_WINDS);
  $VAP_ANIM = $VAP_OPS_TOP/animate unless defined($VAP_ANIM);
  $VAP_OVERLAY = $VAP_OPS_TOP/overlay unless defined($VAP_OVERLAY);
  $VAP_OVERLAY_ARCHIVE = "$VAP_WWW_TOP/images/overlay_archive"; unless defined($VAP_OVERLAY_ARCHIVE);
  $GRIDDINGTOP = "$VAP_GOES_TOPDIR/gridded_files" unless defined($GRIDDINGTOP);
  $IDLEXE=$ENV{'IDLEXE'};


    # MOVIE DEFS
  $auto_movie_defs_file="$VAP_LIBRARY/auto_movie_defs.dat";
  require $auto_movie_defs_file;

    # Get the Overlay Defaults
  $overlay_defs_file="$VAP_LIBRARY/overlay_defs";
  require $overlay_defs_file;

    # Get generic VAP processing Defaults
  $vap_defs_file=$VAP_LIBRARY."/vap_defs";
  require $vap_defs_file;

    # Check for interactivity.
  $vap_is_batch = !defined($ENV{'TERM'});

}
1;
