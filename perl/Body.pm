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

    'file' : the FULLY QUALIFIED FILENAME in the webspace!


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


}

sub getBody{
  my $self=shift;
  if ($self->{TYPE} =~ /OVERLAY/i){
    $self->{HTML} = $self->updateOverlay;
  } elsif ($self->{TYPE} =~ /TROPICAL_STORM/i) {
    $self->{HTML} = $self->updateTropicalStorm
  } else {
    $self->{HTML} = $self->Animation
  }
  1;
}

sub updateOverlay {
  my $self=shift;
  my $html = $self->{HTML};
}

sub update Animation {
  my $self=shift;
}

sub updateTropicalStorms{
  my $self=shift;
}

1;
