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
# Revision 1.3  2002/05/07 20:40:36  vapdev
# Set -w and `use strict' and then fixing bugs. Start trying to standardize
# the methods used.
#
# Revision 1.2  2002/04/30 20:23:22  vapdev
# Modified the 'use lib' statement
#
# Revision 1.1  2001/02/09 19:13:52  vapuser
# Initial revision
#
#
# 
package Vapdefs;
use strict;
use lib $ENV{VAP_SFTWR_PERL};
use Exporter ();
use Carp;
use vars qw/@ISA @EXPORT $VAP_LIBRARY $VAP_OPS_TOP $VAP_WINDS $VAP_ANIM $VAP_OVERLAY 
	    $VAP_WWW_TOP $ARCHIVE $GRIDDINGTOP $IDLEXE $VAP_OVERLAY_ARCHIVE 
	    $VAP_WWW_TOP $vap_is_batch $VAP_GOES_TOP $VAP_DATA_TOP $VAP_GOES_GRIDDING_TOP 
	    $auto_movie_defs_file $overlay_defs_file $vap_defs_file/;
@ISA = qw(Exporter);
@EXPORT=qw( VAP_LIBRARY VAP_OPS_TOP VAP_WINDS VAP_ANIM 
	   VAP_OVERLAY VAP_WWW_TOP ARCHIVETOP 
	   GRIDDINGTOP IDLEXE VAP_OVERLAY_ARCHIVE 
	   vap_is_batch );


BEGIN {

  $VAP_LIBRARY=$ENV{VAP_LIBRARY} || 
    croak "Env var VAP_LIBRARY is undefined!\n";
  $VAP_OPS_TOP = $ENV{VAP_OPS_TOP} || 
    croak "Env var VAP_OPS_TOP is undefined!\n";
  $VAP_WWW_TOP = $ENV{VAP_WWW_TOP} || 
    croak "Env var VAP_WWW_TOP is undefined!\n";
  $VAP_GOES_TOP = $ENV{VAP_GOES_TOP} || 
    croak "Env var VAP_GOES_TOP is undefined!\n";
  $VAP_DATA_TOP = $VAP_DATA_TOP  || 
    croak "ENV var VAP_DATA_TOP is undefined\n";

  $VAP_ANIM = "$VAP_OPS_TOP/animate" unless defined($VAP_ANIM);
  $VAP_OVERLAY = "$VAP_OPS_TOP/overlay" unless defined($VAP_OVERLAY);
  $VAP_OVERLAY_ARCHIVE = "$VAP_WWW_TOP/images/overlay_archive" unless defined($VAP_OVERLAY_ARCHIVE);
  $GRIDDINGTOP = "$VAP_GOES_GRIDDING_TOP/gridded_files" unless defined($GRIDDINGTOP);
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
