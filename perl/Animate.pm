=head1 NAME

  Animate.pm -- Object to handle the animation of QuikSCAT/SeaWinds data
                (Replaces auto_movie.pl)

=head2 SYNOPSIS

  $anim_obj = Animate->new( REGION => region,
                            TIME => time,
                            DELTA => delta);


=head2 KEYS

=over 4 

=item * REGION: The 'region'. Must be one of the regions
                defined in the IDL readible file
                $VAP_LIBRARY/auto_movie_defs.dat.

                No default!

=item * TIME: The end time of the time range in which to gather data
                 for interpolation and animation. Default = start time
                 of script run

=item * DELTA: The number of hours to go backward from TIME. If this
                 isn't passed in, it is defaulted in the IDL script
                 that this object calls.


=back
=cut

use strict;
use Carp;
use Cwd;
use vars qw/$VERSION $usage/;

# Make sure we can get the Perl code we need.
BEGIN{
  $VERSION = "0.9";
  croak "ENV var VAP_LIBRARY is undefined\n" 
    unless $ENV{VAP_LIBRARY};
  croak "ENV var VAP_SFTWR_PERL is undefined\n" 
    unless $ENV{VAP_SFTWR_PERL};
  croak "ENV var VAP_OPS_ANIM is undefined\n" 
    unless $ENV{VAP_OPS_ANIM};
  $usage = "$0: obj=Animate->new(REGION=>region [,TIME=>time,DELTA=>DELTA])\n";
}
use VapUtil;

sub new {
  my $class = shift;
  my $self={@_};
  croak $usage unless exists $self->{REGION};
  $self->{ROIS} = [auto_movie_defs()];
  $self->{TIME} = shift || systime2idltime(GetNow());
  $self->{WORKING_DIR} = $ENV{VAP_OPS_ANIM}."/". lc($self->{REGIION});
  my $working_dir = $self->{WORKING_DIR};
  croak "Can't find working directory $working_dir\n"
    unless (-e $working_dir);
  chdir $working_dir or croak "Can't CD to $working_dir";
  return bless $self, ref($class) || $class;
}


1;
