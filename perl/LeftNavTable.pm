# $Id$
#
# $Log$
# Revision 1.10  2003/01/17 18:40:09  vapdev
# Continuing work
#
# Revision 1.9  2003/01/16 23:47:53  vapdev
# Continuing work
#
# Revision 1.8  2002/12/20 23:45:26  vapdev
# Ongoing ... well, you know.
#
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
use Carp;
use strict;
use vars qw/$WESTPACIFIC $EASTPACIFIC $WESTATLANTIC $overlay_defs 
	    $roi_hash $tsoo_defs $website_defs /;

BEGIN{

  # Get the defaults for the various VAP products, as well as for the
  # website itself

  #website defaults:

  my $defsfile = $ENV{VAP_LIBRARY} . "/VapWebsite_defs";
  croak("Can't find $defsfile!\n") unless (-e  $defsfile );
  require "$defsfile" || croak "Can't require $defsfile\n";



  # Overlay defaults
  $defsfile = $ENV{VAP_LIBRARY}."/overlay_defs_oo";
  croak "Can't find overlay_defs_oo!\n"
    unless (-e $defsfile);
  require "$defsfile" || croak "Can't require $defsfile\n";

  # Tropical storm defaults
  $defsfile=$ENV{VAP_LIBRARY}."/tropical_storm_defs_oo";
  croak "Can't find tropical_storm_defs_oo!\n" unless (-e $defsfile);
  require "$defsfile" || croak "Can't require $defsfile:$!\n";  

}
use lib ($ENV{VAP_LIBRARY}, $ENV{VAP_SFTWR_PERL});
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

  my $self=$class->SUPER::new(-rows=>8,-cols=>1, -valign=>"top",
			      @_);

  # arguments lacking the prefix '-' are considered appropriate only
  # to this object and are not used in the super class, so we go
  # through the passed array again.

  my $hash = {@_};
  my ($k,$v);
  while (($k,$v) = each %{$hash} ) {
    if ($k !~ /^-/) {
      $self->{uc($k)} = $v;
    }
  }


  $self->setStyle( "{ font-family: Verdana, sans-serif; font-size: 50% }");


  #
  # Animation defaults.
  # my $roi_hash = 
  $self->{ANIM_DEFS} = auto_movie_defs();



  # Start building the tables within tables within tables within
  # tables within tables . . . needed for this navbar.

  # Some useful variables

  my ($row, $col, $key, $value, $content, $link, $skip, $q, $webname);

    # The outermost table, which will contain the entire navbar, is
    # simply the 'self' object. Each of the subsequent tables will
    # cells in this table.

  my $outermostrow=1;

  # Generic CGI object to make html code on the fly!
  $self->{CGI} = CGI->new(-no_debug=>1);
  $q=$self->{CGI};


  # We key on $type, put it at the top of the navbar so that people
  # can navigate this page easily.


  my $overlay_table = $self->OverlayTable;
  my $anim_table = $self->AnimTable;
  my $ts_table = $self->Tropical_Storms_Table;

  my $type = $self->{TYPE};

  if ($type =~ /TROPICAL_STORM/){
    $self->setCell($outermostrow++,1,$ts_table->getTable);
    $self->setCell($outermostrow++,1,$overlay_table->getTable);
    $self->setCell($outermostrow++,1,$anim_table->getTable);
  } elsif ($type =~ /OVERLAY/){
    $self->setCell($outermostrow++,1,$overlay_table->getTable);
    $self->setCell($outermostrow++,1,$anim_table->getTable);
    $self->setCell($outermostrow++,1,$ts_table->getTable);

  } else {
    $self->setCell($outermostrow++,1,$anim_table->getTable);
    $self->setCell($outermostrow++,1,$overlay_table->getTable);
    $self->setCell($outermostrow++,1,$ts_table->getTable);
  }






#=-*=-*=-*=-*=-* Now, for the rest of the NavBar =-*=-*=-*=-*=-*=-*=-*=-*


    # Cell 4,1 goes the 'Status' page.
  #$q=CGI->new(-no_debug=>1);
  $link=$q->a({-href=>"status.html"},"Status");
  $content=$q->p({-align=>"RIGHT"},$link);
  $self->setCell($outermostrow++,1,$content);

    # Cell 5,1 goes to 'Specials', if any.
  #$q=CGI->new(-no_debug=>1);
  $link=$q->a({-href=>"special.html"},"Special Products");
  $content=$q->p({-align=>"RIGHT"},$link);
  $self->setCell($outermostrow++,1,$content);

    # Cell 6,1 is for "information" about the website, 
  #$q=CGI->new(-no_debug=>1);
  $link=$q->a({-href=>"info.html"},"Information");
  $content=$q->p({-align=>"RIGHT"},$link);
  $self->setCell($outermostrow++,1,$content);

    # Cell 7,1 is the mailto url.
  #$q=CGI->new(-no_debug=>1);
  my $webhost = $self->{WEBHOST}? $self->{WEBHOST}: "haifung.jpl.nasa.gov";
  $link=$q->a({-href=>"mailto: webmaster\@" . $webhost},"Contact us!");
  $content=$q->p({-align=>"RIGHT"},$link);
  $self->setCell($outermostrow++,1,$content);

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
  my $self=shift;
  if (@_){
    while (my ($k, $v) = each %{$overlay_defs}) {
      my $i = $v->{WINDS}->{INSTRUMENTS};
      return 1 if (ref($i) eq 'ARRAY') && grep(/$_[0]/,@{$i});
    }
  } else {

    # Check for both QS and SW. In this case it's sufficient that
    # WINDS->{INSTRUMENTS} is a reference to an array.

    while (my ($k, $v) = each %{$overlay_defs}) {
      my $i = $v->{WINDS}->{INSTRUMENTS};
      return 1 if (ref($i) eq 'ARRAY' and $#{$i} >= 0);
    }
  }
  return 0;
}

sub anyTSOverlays{
  my $self=shift;
  my $web = $tsoo_defs->{WEB};
  if (@_){
    while (my ($k, $v) = each %{$web}) {
      my $i = $v->{INSTRUMENTS};
      return 1 if (ref($i) eq 'ARRAY') && grep(/$_[0]/,@{$i});
    }
  } else {

    # Check for both QS and SW. In this case it's sufficient that
    # ...->{INSTRUMENTS} is a reference to an array, assuming some
    # miscreant hasn't put [foo] in the tsoo_defs hash!

    while (my ($k, $v) = each %{$web}) {
      my $i = $v->{INSTRUMENTS};
      return 1 if (ref($i) eq 'ARRAY' and $#{$i} >= 0);
    }
  }
  return 0;
}

#=-*=-*=-*=-*=-*=-*=-*=-*=-* Overlays =-*=-*=-*=-*=-*=-*=-*=-*=-*=-*=-*=-*


sub OverlayTable{

  my $self=shift;
  my $q=$self->{CGI};



    # Construct the table that will hold the overlay part of the nav
    # bar. 

  my $overlay_table = HTML::Table->new(-rows=>2,-cols=>1);
  $overlay_table->setCaption("Cloud Overlays",'TOP');
  $overlay_table->setStyle("{ font: Garamond, 'Times New Roman', serif; font: 50% }");
  my @order = @{$self->{ORDER}->{OVERLAY}};
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


    # ========= SeaWinds on QuikSCAT table ================

  my @defunct_satellites = @{$website_defs->{DEFUNCT_SATELLITES}};


  if ($self->anyOverlays('QS')) {
    $overlay_qs_table=HTML::Table->new(-rows=>8,-col=>1,-align=>'RIGHT');
    my $caption = $q->a({-href=>"overlay_Q.html"},
			 "QuikSCAT");
    $overlay_qs_table->setCaption($caption,'TOP');
    my $row=1;
    foreach (@order) {
      my $key = $_;
      my $value = $overlay_defs->{$key};
#       $skip=0;
#       if (@defunct_satellites) {
# 	foreach my $s (@defunct_satellites){
# 	  $skip = grep (/$s/i, $key);
# 	  last if $skip
# 	}
#       }
#       next if $skip;
      next if !$value->{WEB}->{ACTIVE};
      #$q=CGI->new(-no_debug=>1);
      my $link = $q->a({-href=>"overlay_Q.html#$key"},
		    $value->{WEB}->{NAME});
      my $content=$q->p({-align=>'LEFT'},$link);
      $overlay_qs_table->setCell($row++, 1, $content);
    }
  }

    # ========= SeaWinds on ADEOS-II table ============

    # first we determine whether this one is needed at all! I may be
    # that someone has decided to turn not make any SeaWinds on
    # ADEOS-II overlays.


  if ($self->anyOverlays('SW')) {
    $overlay_sw_table=HTML::Table->new(-rows=>8,-col=>1,-align=>'RIGHT');
    my $caption = $q->a({-href=>"overlay_S.html"},
			 "SeaWinds");
    $overlay_sw_table->setCaption($caption,'TOP');
    my $row=1;
    @order = keys %{$overlay_defs} unless @order;
    foreach (@order) {
      my $key = $_;
      my $value = $overlay_defs->{$key};
	# value here is a reference to a hash, see
	# $VAP_LIBRARY/overlay_defs_oo for its definition.
#       $skip=0;
#       if (@defunct_satellites) {
# 	foreach my $s (@defunct_satellites){
# 	  $skip = grep (/$s/i, $key);
# 	  last if $skip
# 	}
#       }
#       next if $skip;
      next if !$value->{WEB}->{ACTIVE};

      #$q=CGI->new(-no_debug=>1);
      my $link = $q->a({-href=>"overlay_S.html#$key"},
		    $value->{WEB}->{NAME} );
      my $content=$q->p({-align=>'LEFT'},$link);
      $overlay_sw_table->setCell($row++, 1, $content);
    }
  }



    # Now put the two tables into the overall `overlay' cell of the
    # Left Nav table

  my $row = 1;
  $overlay_table->setCell($row++,1,$overlay_qs_table->getTable) 
    if $overlay_qs_table;
  $overlay_table->setCell($row,1,$overlay_sw_table->getTable) 
    if $overlay_sw_table;

  $overlay_table;
}

#=-*=-*=-*=-*=-*=-*=-*=-*  Animations    *=-*=-*=-*=-*=-*=-*=-*=-*=-*=-*=-*=-*=-*=-*


sub AnimTable{

  my $self=shift;
  my $q=$self->{CGI};


    # Construct the table that will hold the animation part of the nav
    # bar. The animations won't split QS from SW, so there's no need
    # for the additional layer of tables as is the case with the
    # overlays.

  my $anim_table = HTML::Table->new(-rows=>9,-cols=>1);
  my $caption = $q->a({-href=>"anim.html"},
			 "Animations");
  $anim_table->setCaption($caption,'TOP');
  $anim_table->setStyle("{ font: Garamond, 'Times New Roman', serif; size: 50%}");
  my @order = @{$self->{ORDER}->{ANIMATION}};

  my $row=0;
  my $roi_hash = $self->{ANIM_DEFS};

  foreach (@order) {
    my $key = $_;
    my $value = $roi_hash->{$key};
    $row++;
    #$q=CGI->new(-no_debug=>1);
    my $link = $q->a({-href=>"anim.html#$key"},$value);
    my $content=$q->p({-align=>'LEFT'},$link);
    $anim_table->setCell($row, 1, $content);
  }


    # Now put the anim_table into self

  $anim_table;

}

#=-*=-*=-*=-*=-*=-*=-*=-*  Tropical Storms *=-*=-*=-*=-*=-*=-*=-*=-*=-*=-*=-*=-*

sub Tropical_Storms_Table{
  my $self=shift;
  my $q=$self->{CGI};
    # Construct the table that will hold the Tropical Storms. This
    # will get the information need to construct itself from the
    # tropical_storm_defs_oo hash.

  my $ts_table = HTML::Table->new(-caption=>"Tropical Storms", 
				  -rows=>3,-col=>1);
  my $caption = $q->a({-href=>"ts_Q.html"},
			 "QuikSCAT");
  $ts_table->setCaption("Tropical Storms",'TOP');
  $ts_table->setStyle(" { font: Garamond, 'Times New Roman', serif; size: 50%}");

  my ($ts_sw_table, $ts_qs_table);
  my @order = @{$self->{ORDER}->{TROPICAL_STORM}};

    # ============= Seawinds on QuikSCAT  =================

  if ($self->anyTSOverlays('QS')) {
    $ts_qs_table=HTML::Table->new(-rows=>8,-col=>1,-align=>'RIGHT');
    my $caption = $q->a({-href=>"ts_Q.html"},
			 "QuikSCAT");
    $ts_qs_table->setCaption($caption,'TOP');
    my $row=0;
    @order = @{$tsoo_defs->{SATELLITES}} unless @order;
    foreach my $sat (@order) {
#
#       I put this in to handle GMS5, which has some problems, but now
#       I'm going to comment it out. Even if there's no cloud data, at
#       least we can see the winds close to a storm.

#       $skip=0;
#       if (@defunct_satellites) {
# 	foreach my $s (@defunct_satellites){
# 	  $skip = grep (/$s/i, $sat);
# 	  last if $skip
# 	}
#    }
#       next if $skip;

      $row++;
      #$q=CGI->new(-no_debug=>1);
      my $webname = $tsoo_defs->{WEB}->{$sat}->{NAME};
      my $link = $q->a({-href=>"ts_Q.html#$sat"},
		       "$webname");
      my $content=$q->p({-align=>'LEFT'},$link);
      $ts_qs_table->setCell($row, 1, $content);
    }
  }

    # ============= Seawinds on ADEOS-II =================

  if ($self->anyTSOverlays('SW')) {

    $ts_sw_table=HTML::Table->new(-rows=>8,-col=>1,-align=>'RIGHT');
    my $caption = $q->a({-href=>"ts_S.html"},
			 "SeaWinds");
    $ts_sw_table->setCaption($caption,'TOP');
    my $row=0;

    @order = @{$tsoo_defs->{SATELLITES}} unless @order;
    foreach my $sat (@order) {
#       $skip=0;
#       if (@defunct_satellites) {
# 	foreach my $s (@defunct_satellites){
# 	  $skip = grep (/$s/i, $sat);
# 	  last if $skip
# 	}
#    }
#      next if $skip;
      $row++;
      #$q=CGI->new(-no_debug=>1);
      my $webname = $tsoo_defs->{WEB}->{$sat}->{NAME};
      my $link = $q->a({-href=>"ts_S.html#$sat"},
		    "$webname");
      my $content=$q->p({-align=>'LEFT'},$link);
      $ts_sw_table->setCell($row, 1, $content);
    }
    
  }

    # Now put these two tables into the overlay Tropical Storm cell
    # for the Nav Bar
  my $row = 1;
  $ts_table->setCell($row++,1,$ts_qs_table->getTable) if $ts_qs_table;
  $ts_table->setCell($row,1,$ts_sw_table->getTable) if $ts_sw_table;

  $ts_table;
}

1;
