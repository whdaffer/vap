# $Id$
#
# $Log$
# Revision 1.7  2002/12/16 23:14:28  vapdev
# To write the body of the page
#
# Revision 1.6  2002/12/04 23:56:20  vapdev
# Ongoing work
#
# Revision 1.5  2002/12/04 01:20:31  vapdev
# Ongoing work
#
# Revision 1.4  2002/12/03 05:57:14  vapdev
# Ongoing work
#
# Revision 1.3  2002/12/02 17:24:07  vapdev
# Moved to .../perl
#
# Revision 1.2  2002/11/27 20:33:18  vapdev
# Ongoing work
#
# Revision 1.1  2002/08/08 00:20:09  vapdev
# Initial Revision
#
#
#
package LeftNavTable;
use strict;
use vars qw/$WESTPACIFIC $EASTPACIFIC $WESTATLANTIC $overlay_defs 
	    @defunct_satellites $roi_hash $tsoo_defs /;
use lib ($ENV{VAP_LIBRARY}, $ENV{VAP_SFTWR_PERL});
use Carp;
use HTML::Table;
@LeftNavTable::ISA= qw/HTML::Table/;
use CGI;
use VapUtil;

=pod

=head1

=head2

=cut


sub new {

  my $class = shift;
  my $self=$class->SUPER::new(-rows=>8,-cols=>1, @_);
  $self->setStyle( "{ font-family: Verdana, sans-serif; font-size: 500% }");
                    


 #
  # Get the defaults for the various VAP products, as well as for the
  # website itself

  #website defaults:


  croak "Can't find VapWebsite!\n"
    unless (-e $ENV{VAP_LIBRARY}."/VapWebsite");
  require "VapWebsite" || 
    croak "Can't require ".$ENV{VAP_LIBRARY} . "VapWebsite\n";

  # Overlay defaults
  croak "Can't find overlay_defs_oo!\n"
    unless (-e $ENV{VAP_LIBRARY}."/overlay_defs_oo");
  require "overlay_defs_oo" || 
    croak "Can't require ".$ENV{VAP_LIBRARY} . "overlay_defs_oo\n";

  # Tropical storm defaults
  croak "Can't find tropical_storm_defs_oo!\n" 
    unless (-e $ENV{VAP_LIBRARY}."/tropical_storm_defs_oo");
  require "tropical_storm_defs_oo" or 
    croak "Can't require tropical_storm_defs_oo:$!\n";  

  # Animation defaults.

  my $roi_hash = auto_movie_defs();

  # Start building the tables within tables within tables within
  # tables within tables . . . needed for this navbar.

  # Some useful variables

  my ($row, $col, $key, $value, $content, $link, $skip, $q, $webname);

    # The outermost table, which will contain the entire navbar. Each
    # of the subsequent tables will cells in this table.

  # my $table1=HTML::Table->new(-rows=>8,-cols=>1);




#=-*=-*=-*=-*=-*=-*=-*=-*=-* Overlays =-*=-*=-*=-*=-*=-*=-*=-*=-*=-*=-*=-*




    # Construct the table that will hold the overlay part of the nav
    # bar. This goes in cell 1,1 of the outermost table.

  my $overlay_table = HTML::Table->new(-rows=>2,-cols=>1);
  $overlay_table->setCaption("Cloud Overlays",'TOP');
  $overlay_table->setStyle("{ font: Garamond, 'Times New Roman', serif; font: 10% }");

  my ($overlay_sw_table, $overlay_qs_table);


     # The actual navigation links are constructed in these two tables
     # and will get the information on which 'regions' to use from the
     # defaults hash defined in $VAP_LIBRARY/overlay_defs_oo. The two
     # keys of interest in this instance are the WEB->{ACTIVE} key and
     # the WINDS->{INSTRUMENTS} key. The former dictates whether this
     # region is active for any wind/cloud data, i.e. if WEB->{ACTIVE}
     # = 0 this region will not appear in the navbar or the
     # appropriate page. The latter points to any array ref whose
     # values tell which wind data to use: [QS, SW] will produce a
     # navigation href and a slot in both overlay pages and one
     # individually will only produce it for the indicated SeaWinds
     # instrument.


     # The array @defunct_satellites refers to the cloud data
     # satellites. This is a quick way to eliminate webprocessing of
     # of *all* regions associated with a particular image satellite.


    # ========= SeaWinds on ADEOS-II table ============

    # first we determine whether this one is needed at all! I may be
    # that someone has decided to turn not make any SeaWinds on
    # ADEOS-II overlays.


  if ($self->anyOverlays('SW')) {
    $overlay_sw_table=HTML::Table->new(-rows=>8,-col=>1,-align=>'RIGHT');
    $overlay_sw_table->setCaption("SeaWinds",'TOP');
    $row=0;
    while ( ($key, $value) = each %{$overlay_defs}) {
	# value here is a reference to a hash, see
	# $VAP_LIBRARY/overlay_defs_oo for its definition.
      $skip=0;
      if (@defunct_satellites) {
	foreach my $s (@defunct_satellites){
	  $skip = grep (/$s/i, $key);
	  last if $skip
	}
      }
      next if $skip;
      next if !$value->{WEB}->{ACTIVE};

      $row++;
      $q=CGI->new(-no_debug=>1);
      $link = $q->a({-href=>"overlay_sw.html#$key"},
		    $value->{WEB}->{NAME} );
      $content=$q->p({-align=>'LEFT'},$link);
      $overlay_sw_table->setCell($row, 1, $content);
    }
  }


    # ========= SeaWinds on QuikSCAT table ================


  if ($self->anyOverlays('SW')) {
    $overlay_qs_table=HTML::Table->new(-rows=>8,-col=>1,-align=>'RIGHT');
    $overlay_qs_table->setCaption("QuikSCAT",'TOP');
    $row=0;
    while ( ($key, $value) = each %{$overlay_defs}) {
      $skip=0;
      if (@defunct_satellites) {
	foreach my $s (@defunct_satellites){
	  $skip = grep (/$s/i, $key);
	  last if $skip
	}
      }
      next if $skip;
      next if !$value->{WEB}->{ACTIVE};
      $row++;
      $q=CGI->new(-no_debug=>1);
      $link = $q->a({-href=>"overlay_qs.html#$key"},
		    $value->{WEB}->{NAME});
      $content=$q->p({-align=>'LEFT'},$link);
      $overlay_qs_table->setCell($row, 1, $content);
    }

  }
    # Now put the two tables into the overall `overlay' cell of the
    # Left Nav table

  $row = 1;
  $overlay_table->setCell($row++,1,$overlay_sw_table) if $overlay_sw_table;
  $overlay_table->setCell($row,1,$overlay_qs_table) if $overlay_qs_table;

    # And put the overlay table into outermost table, self
  $self->setCell(1,1,$overlay_table);




#=-*=-*=-*=-*=-*=-*=-*=-*  Animations    *=-*=-*=-*=-*=-*=-*=-*=-*=-*=-*=-*=-*=-*=-*




    # Construct the table that will hold the animation part of the nav
    # bar. This goes in cell 2,1. The animations won't split QS from
    # SW, so there's no need for the additional layer of tables as is
    # the case with the overlays.

  my $anim_table = HTML::Table->new(-rows=>9,-cols=>1);
  $anim_table->setCaption("Animations",'TOP');
  $anim_table->setStyle("{ font: Garamond, 'Times New Roman', serif; size: 500%}");
  $row=0;
  while (($key, $value) = each %{$roi_hash}){
    $skip=0;
    if (@defunct_satellites) {
      foreach my $s (@defunct_satellites){
	$skip = grep (/$s/i, $key);
	last if $skip
      }
    }
    next if $skip;
    $row++;
    $q=CGI->new(-no_debug=>1);
    $link = $q->a({-href=>"anim.html#$key"},$value);
    $content=$q->p({-align=>'LEFT'},$link);
    $anim_table->setCell($row, 1, $content);
  }


    # Now put the anim_table into self

  $self->setCell(2,1,$anim_table); 



#=-*=-*=-*=-*=-*=-*=-*=-*  Tropical Storms *=-*=-*=-*=-*=-*=-*=-*=-*=-*=-*=-*=-*




    # Construct the table that will hold the Tropical Storms. Cell
    # 3,1. this will get the information need to construct itself from
    # the tropical_storm_defs_oo hash.

  my $ts_table = HTML::Table->new(-caption=>"Tropical Storms", -rows=>2,-col=>1);
  $ts_table->setCaption("Tropical Storms",'TOP');
  $ts_table->setStyle(" { font: Garamond, 'Times New Roman', serif; size: 10%}");

  my ($ts_sw_table, $ts_qs_table);


    # ============= Seawinds on ADEOS-II =================

  if ($self->anyTSOverlays('SW')) {

    $ts_sw_table=HTML::Table->new(-rows=>8,-col=>1,-align=>'RIGHT');
    $ts_sw_table->setCaption("SeaWinds",'TOP');
    $row=0;

    foreach my $sat (@{$tsoo_defs->{SATELLITES}}) {
      $skip=0;
      if (@defunct_satellites) {
	foreach my $s (@defunct_satellites){
	  $skip = grep (/$s/i, $sat);
	  last if $skip
	}
      }
      next if $skip;
      $row++;
      $q=CGI->new(-no_debug=>1);
      $webname = $tsoo_defs->{WEB}->{$sat}->{NAME};
      $link = $q->a({-href=>"ts_sw.html#$sat"},
		    "$webname");
      $content=$q->p({-align=>'LEFT'},$link);
      $ts_sw_table->setCell($row, 1, $content);
    }

  }
    # ============= Seawinds on QuikSCAT  =================

  if ($self->anyTSOverlays('QS')) {
    $ts_qs_table=HTML::Table->new(-rows=>8,-col=>1,-align=>'RIGHT');
    $ts_qs_table->setCaption("QuikSCAT",'TOP');
    $row=0;
    foreach my $sat (@{$tsoo_defs->{SATELLITES}}) {
      $skip=0;
      if (@defunct_satellites) {
	foreach my $s (@defunct_satellites){
	  $skip = grep (/$s/i, $sat);
	  last if $skip
	}
      }
      next if $skip;
      $row++;
      $q=CGI->new(-no_debug=>1);
      $webname = $tsoo_defs->{WEB}->{$sat}->{NAME};
      $link = $q->a({-href=>"ts_qs.html#$sat"},
		    "$webname");
      $content=$q->p({-align=>'LEFT'},$link);
      $ts_qs_table->setCell($row, 1, $content);
    }
  }
    # Now put these two tables into the overlay Tropical Storm cell
    # for the Nav Bar
  $row = 1;
  $ts_table->setCell($row++,1,$ts_sw_table) if $ts_sw_table;
  $ts_table->setCell($row,1,$ts_qs_table) if $ts_qs_table;

    # And put the Tropical storm table into the proper slot in the
    # navbar.

  $self->setCell(3,1,$ts_table);


#=-*=-*=-*=-*=-* Now, for the rest of the NavBar =-*=-*=-*=-*=-*=-*=-*=-*


    # Cell 4,1 goes the 'Status' page.
  $q=CGI->new(-no_debug=>1);
  $link=$q->a({-href=>"status.html"},"Status");
  $content=$q->p({-align=>"RIGHT"},$link);
  $self->setCell(4,1,$content);

    # Cell 5,1 goes to 'Specials', if any.
  $q=CGI->new(-no_debug=>1);
  $link=$q->a({-href=>"special.html"},"Special Products");
  $content=$q->p({-align=>"RIGHT"},$link);
  $self->setCell(5,1,$content);

    # Cell 6,1 is for "information" about the website, 
  $q=CGI->new(-no_debug=>1);
  $link=$q->a({-href=>"info.html"},"Information");
  $content=$q->p({-align=>"RIGHT"},$link);
  $self->setCell(6,1,$content);

    # Cell 7,1 is the mailto url.
  $q=CGI->new(-no_debug=>1);
  $link=$q->a({-href=>"mailto: webmaster\@haifung.jpl.nasa.gov"},"Contact us!");
  $content=$q->p({-align=>"RIGHT"},$link);
  $self->setCell(7,1,$content);

  
#   # Get the keys from the overlay file.
#   my @overlay_keys = keys %{$overlay_defs};

#   # ====== West Atlantic, typically Goes 8 =================

#   my @regions = grep /GOES.?(8|EAST)/, @overlay_keys;
#   @regions = grep !/$DEFUNCT_VIEWS/,@regions;

#   my $WESTATLANTIC = {SATELLITE => ['GOES_8', 'GOES_EAST'],
# 		      REGIONS => \@regions};
#   my $htmltable = HTML::Table->new(
# 	      -rows=>scalar(@{$WESTATLANTIC->{REGIONS}})+1,
# 				 -cols=>2,
# 				 -align=>"left",
# 				 -width=>"100\%",
# 				 -spacing=>0,
# 				 -padding=>0);
#   $htmltable->setCaption("
#   $WESTATLANTIC->{HTMLTABLE} = $htmltable;

#   @regions = grep /GOES.?(10|WEST)/,@overlay_keys;
#   @regions = grep !/$DEFUNCT_VIEWS/,@regions;

#   # ====== East Pacific, typically Goes 10 =================
#   my $EASTPACIFIC = {SATELLITE =>  ['GOES_10','GOES_WEST'],
# 		      REGIONS => \@regions};

#   $htmltable = HTML::Table->new(
# 	    -rows=>scalar(@{$EASTPACIFIC->{REGIONS}})+1,
# 				-cols=>2,
# 				-align=>"left",
# 				-width=>"100\%",
# 				-spacing=>0,
# 				-padding=>0);
#   $EASTPACIFIC->{HTMLTABLE} = $htmltable;



#   # ====== West Pacific, typically GMS 5 =================
#   @regions = grep /GMS_?\d?/,@overlay_keys;
#   @regions = grep !/$DEFUNCT_VIEWS/,@regions;

#   my $WESTPACIFIC = {SATELLITE => ['GMS_5','GMS'],
# 		     REGIONS => \@regions};

#   $htmltable = HTML::Table->new(
# 	       -rows=>scalar(@{$WESTPACIFIC->{REGIONS}})+1,
# 			      -cols=>2,
# 			      -align=>"left",
# 			      -width=>"100\%",
# 			      -spacing=>0,
# 			      -padding=>0);

#   $WESTPACIFIC->{HTMLTABLE} = $htmltable;


#   # ========= Tropical Storms ===========================

#   @regions = grep !/$DEFUNCT_VIEWS/,@overlay_keys;
#   my $TROPICALSTORMS = {SATELLITE => ['GOES_10', 'GOES_8', 'GOES_EAST',
# 				       'GOES_WEST', 'GMS_5', 'GMS'],
# 		       REGIONS => \@regions};



#   # Set the titles
#   $WESTATLANTIC->{HTMLTABLE}->setCell(1,1,"West Atlantic",-colspan=>2);
#   $EASTPACIFIC->{HTMLTABLE}->setCell(1,1,"East Pacific",-colspan=>2);
#   $WESTPACIFIC->{HTMLTABLE}->setCell(1,1,"West Pacific",-colspan=>2);


# #   my $maintable = HTML::Table->new(-rows=>6,
# # 			   -cols=>1,
# # 			   -align=>"left",
# # 			   -width=>"40\%",
# # 			   -border=>1);

#   $self->{WESTATLANTIC} = $WESTATLANTIC;
#   $self->{EASTPACIFIC} = $EASTPACIFIC ;
#   $self->{WESTPACIFIC} = $WESTPACIFIC;
# #	   MAINTABLE => $maintable,
# #	     @_};


  return bless $self, ref($class) || $class;
}

=pod 

=head1 anyOverlays

=head2 Usage: 1|0 = anyOverlays{[type]};

       Checks for overlays of type `type'. If there are any, return 1,
    else return 0. The 'type' is the same as appears in the
    WINDS->{INSTRUMENT} subhash of the overlay_defs_oo hash. The
    default is to check for both QS and SW, so if that's all you want
    to check you may omit the argument. If you want to check for a
    specific one, pass either 'QS' or 'SW'.

=cut

sub anyOverlays{
  if (@_){
    while (my ($k, $v) = each %{$overlay_defs}) {
      my $i = $v->{WINDS}->{INSTRUMENTS};
      return 1 if (ref($i) == 'ARRAY') && grep(/$_[0]/,@{$i});
    }
  } else {

    # Check for both QS and SW. In this case it's sufficient that
    # WINDS->{INSTRUMENTS} is a reference to an array.

    while (my ($k, $v) = each %{$overlay_defs}) {
      my $i = $v->{WINDS}->{INSTRUMENTS};
      return 1 if (ref($i) == 'ARRAY' and $#{$i} >= 0);
    }
  }
  return 0;
}

sub anyTSOverlays{

  my $web = $tsoo_defs->{WEB};
  if (@_){
    while (my ($k, $v) = each %{$web}) {
      my $i = $v->{INSTRUMENTS};
      return 1 if (ref($i) == 'ARRAY') && grep(/$_[0]/,@{$i});
    }
  } else {

    # Check for both QS and SW. In this case it's sufficient that
    # ...->{INSTRUMENTS} is a reference to an array, assuming some
    # miscreant hasn't put [foo] in the tsoo_defs hash!

    while (my ($k, $v) = each %{$web}) {
      my $i = $v->{INSTRUMENTS};
      return 1 if (ref($i) == 'ARRAY' and $#{$i} >= 0);
    }
  }
  return 0;
}
1;
