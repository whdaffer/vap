# $Id$
#
# $Log$
#
#
package WWW::LeftNavTable;
use strict;
use vars qw/$WESTPACIFIC $EASTPACIFIC $WESTATLANTIC %overlay_defs 
	    $DEFUNCT_SATELLITES $DEFUNCT_VIEWS/;
use lib $ENV{VAP_LIBRARY};
use Carp;
use HTML::Table;

sub new {

  my $class = shift;
  $DEFUNCT_VIEWS = "GOES.?(8|10)_?1";

  # Get the defaults for the overlays from the overlay_defs file.

  croak "Can't find overlay_defs!\n"
    unless (-e $ENV{VAP_LIBRARY}."/overlay_defs");

  require "overlay_defs" || 
    croak "Can't require overlay_defs\n";

  # Get the defaults for the tropical storms processing 
  # from the tropical_storm_defs file.

  croak "Can't find tropical_storm_defs!\n" 
    unless (-e $ENV{VAP_LIBRARY}."/tropical_storm_defs";
  require "tropical_storm_defs" or croak "Can't require tropical_storm_defs:$!\n";


  # Get the keys from this file.
  my @overlay_keys = keys %overlay_defs;

  # ====== West Atlantic, typically Goes 8 =================

  my @regions = grep /GOES.?(8|EAST)/, @overlay_keys;
  @regions = !grep /$DEFUNCT_VIEWS/,@overlay_keys;

  my $WESTATLANTIC = {SATELLITE => qw/GOES_8 GOES_EAST/,
		      REGIONS => \@regions};
  my $htmltable = HTML::Table->new(
	      -rows=>scalar(@{$WESTATLANTIC->{REGIONS}})+1,
				 -cols=>2,
				 -align=>"left",
				 -width=>"100\%",
				 -spacing=>0,
				 -padding=>0);
  $WESTATLANTIC->{HTMLTABLE} = $htmltable;

  @regions = grep /GOES.?(10|WEST)/,@overlay_keys;
  @regions = !grep /$DEFUNCT_VIEWS/,@overlay_keys;

  # ====== East Pacific, typically Goes 10 =================
  my $EASTPACIFIC = {SATELLITE =>  qw/GOES_10 GOES_WEST/,
		      REGIONS => \@regions};

  $htmltable = HTML::Table->new(
	    -rows=>scalar(@{$EASTPACIFIC->{REGIONS}})+1,
				-cols=>2,
				-align=>"left",
				-width=>"100\%",
				-spacing=>0,
				-padding=>0);
  $EASTPACIFIC->{HTMLTABLE} = $htmltable;



  # ====== West Pacific, typically GMS 5 =================
  @regions = grep /GMS_?\d?/,@overlay_keys;
  @regions = !grep /$DEFUNCT_VIEWS/,@overlay_keys;

  my $WESTPACIFIC = {SATELLITE => qw/GMS_5 GMS/,
		     REGIONS => \@regions};

  $htmltable = HTML::Table->new(
	       -rows=>scalar(@{$WESTPACIFIC->{REGIONS}})+1,
			      -cols=>2,
			      -align=>"left",
			      -width=>"100\%",
			      -spacing=>0,
			      -padding=>0);

  $WESTPACIFIC->{HTMLTABLE} = $htmltable;


  # ========= Tropical Storms ===========================

  @regions = !grep /$DEFUNCT_VIEWS/,@overlay_keys;
  my $TROPICALSTORMS = {SATELLITE => qw/GOES_10 GOES_8 GOES_EAST 
				       GOES_WEST GMS_5 GMS/,
		       REGIONS => \@regions};



  # Set the titles
  $WESTATLANTIC->{HTMLTABLE}->setCell(1,1,"West Atlantic",-colspan=>2);
  $EASTPACIFIC->{HTMLTABLE}->setCell(1,1,"East Pacific",-colspan=>2);
  $WESTPACIFIC->{HTMLTABLE}->setCell(1,1,"West Pacific",-colspan=>2);

  
  my $maintable = HTML::Table->new(-rows=>6,
			   -cols=>1,
			   -align=>"left",
			   -width=>"40\%",
			   -border=>1);

  my $self = {WESTATLANTIC => $WESTATLANTIC,
	      EASTPACIFIC => $EASTPACIFIC, 
	      WESTPACIFIC => $WESTPACIFIC,
	      MAINTABLE => $maintable,
	     @_};
  return bless $self, ref($class) || $class;
}

1;
