#!/usr/bin/perl
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
# Revision 1.1  2001/02/09 19:13:52  vapuser
# Initial revision
#
#
# 
package Vapdefs;
use lib $ENV{'VAP_SFTWR_TOP'}."/vap/perl";
use Exporter ();
@ISA = qw(Exporter);
@EXPORT=qw( $VAP_LIB $VAP_ROOT $VAP_WINDS $VAP_ANIM 
	   $VAP_OVERLAY $VAP_WWW_TOP $ARCHIVETOP 
	   $GRIDDINGTOP $IDLEXE $VAP_OVERLAY_ARCHIVE 
	   $VAP_WWW_TOP $vap_is_batch );


BEGIN {
    # Get ENV variables
  if ($ENV{'HOME'} =~ /haifung/) {
    $LIB_TOP="/usr/people/vapuser/Qscat";
    if (-l $LIB_TOP) {
      $LIB_TOP = readlink($LIB_TOP);
      $VAP_LIB = "$LIB_TOP/Library";
    }
  } else {
    $VAP_LIB=$ENV{'VAP_LIB'} || "/usr/people/vapuser/Qscat/Library";
  }

    # MOVIE DEFS
  $auto_movie_defs_file=$VAP_LIB."/auto_movie_defs.dat";

    # Get the Overlay Defaults
  $overlay_defs_file=$VAP_LIB."/overlay_defs";
  require $overlay_defs_file;

    # Get generic VAP processing Defaults
  $vap_defs_file=$VAP_LIB."/vap_defs";
  require $vap_defs_file;

    # Check for interactivity.
  $vap_is_batch = !defined($ENV{'TERM'});

}
1;
