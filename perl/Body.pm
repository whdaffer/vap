=pod 

=head1

=head2

=cut


use strict;
use Carp;
use Cwd;
use File::Copy;
use File::Basename;
use vars qw/$VERSION $usage/;

BEGIN {
  $VERSION = "0.9";

  croak "ENV var VAP_LIBRARY is undefined\n" 
    unless $ENV{VAP_LIBRARY};

  croak "ENV var VAP_SFTWR_PERL is undefined\n" 
    unless $ENV{VAP_SFTWR_PERL};

  croak "ENV var VAP_WWW_TOP is undefined\n" 
    unless $ENV{VAP_WWW_TOP};

  $usage = << EOF

    $website = Body->new(TYPE => 'type', 
			 FILE => 'file')

    'type' : the type of product. OVERLAY for standard overlays,
           TROPICAL_STORMS for tropical storms and ANIMATION for
           animations.

    'file' : the FULLY QUALIFIED FILENAME of the file, as it appears
             in the webspace!


EOF;

  

}



use lib $ENV{VAP_SFTWR_PERL};
use lib $ENV{VAP_LIBRARY};
use VapCGI;
use HTML::Table
use VapUtil;
use VapError;

@VapWebsite::ISA = qw/VapError/;

sub new{
  my $clse = shift;
  my $self = {@_};
  $self->{ERROROBJ} = VapError->new() unless $self->{ERROROBJ};
  bless $self, ref($class) || $class;
  $self->_croak("Need type!\n",
		"Body::new No type!") unless $self->{TYPE};

  $self->_croak("Need file!\n",
		"Body::new No filename!") unless $self->{FILE};

  my $type=$self->{TYPE}
  if ($type =~ /OVERLAY/i) {

    my $oodefsfile = "overlay_defs_oo";
    my $msgoodefsfile = $ENV{VAP_LIBRARY} . "/overlay_defs_oo";

    $self->_croak("Can't find defaults file $msgdefsfile!\n",
		  "CAN'T FIND DEFS FILE!") unless (! -e $msgdefsfile );
    do { require "$defsfile"; } or 
      $self->_croak("Can't `require $msgdefsfile\n",
		    "ERROR in `REQUIRE' of DEFS!");

    $self->{OVERLAY}->{DEFS} = $overlay_defs_oo;
  } elsif ($type =~ /TROPICAL_STORM/i){

    my $tropical_storm_defs_file = "tropical_storm_defs_oo";
    my $foo=$ENV{VAP_LIBRARY}. "/tropical_storm_defs_oo";
    require "$tropical_storm_defs_file" ||
      $self->_croak("Can't require $foo\n:$!",
		  "ERROR IN `REQUIRE'");

    $self->{TROPICAL_STORM}->{DEFS} = $tsoo_defs;


  } elsif (if ($type =~ /ANIMATION/i){

    my $defsfile = $ENV{VAP_LIBRARY}. "/auto_movie_defs.dat";
    $self->_croak("Body::new. Empty file:\n$defsfile",
		  "Can't find $defsfile") if (! (-e $defsfile));
    my $hash = VapUtil::auto_movie_defs($defsfile);
    $self->{ANIM}->{DEFS} = $hash;

  } else {

    $self->_croak("Body::new. Unkown type <$type>\n", 
		  "Uknown TYPE!");
  }
  $self->{CGI} = CGI->new(-nodebug=>1);
  return $self;



}


sub getBody{
  my $self=shift;
  my $type = $self->{TYPE};
  if ($type =~ /OVERLAY/i){
    $self->{HTML} = $self->updateOverlay;
  } elsif ($type =~ /TROPICAL_STORM/i) {
    $self->{HTML} = $self->updateTropicalStorm
  } else {
    $self->{HTML} = $self->Animation
  }
  1;
}

sub updateOverlay {
  my $self=shift;
  my $q = $self->{GCI};
  
}

sub update Animation {
  my $self=shift;
  my $q = $self->{GCI};
}

sub updateTropicalStorms{
  my $self=shift;
  my $q = $self->{GCI};
}

1;
