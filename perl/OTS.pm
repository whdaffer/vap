package OTS;


#
# $Id$
#
#
# Modification Log:
#
# $Log$
#



use strict;
use Carp;
use lib $ENV{VAP_SFTWR_TOP};
use lib $ENV{VAP_LIBRARY};
use vars qw/$tsoo_defs/;
use VapUtil;

=head1 NAME

  OTS.pm: Object oriented implementation of TS.pm

=head2 SYNOPSIS
 use OTS;
 $ots_obj = OTS->new(REGION=>'region',
                     TIME=>'time',
                     STORM_DELTA => delta,
                     WIND_DELTA => wind_delta);

  Where:
=over 4

=item * REGION is one of the accepted regions (currently there are
 three such regions: GMS5, GOESWESTN and GOESEASTN, corresponding,
 roughly, to the area covered by GMS5, i.e. the West Pacific; the area
 covered by GOES 10, i.e. the East Pacific and some of the Gulf of
 Mexico; and the area covered by GOES 8, i.e the Atlantic. The two
 Goes areas are only northern hemisphere, at the moment)

=item * TIME is a string in the form of yyyy/mm/dd/hh/mm/ss. This is
the time around which to search for storms, i.e. only storms close to
this (and see B<STORM_DELTA> for what `close' means) will be returned.
Default = the current time.

=item * STORM_DELTA is the number of hours around the time given in
TIME to search for storms. Default = 8

=item * WIND_DELTA is the number of hours around the time given in
TIME to search for wind data. Default = 4

=back

=head RUNTIME REQUIREMENTS

The environmental variable VAP_LIBRARY must be defined and point to an
existing, readible directory. There must be a file in that directory
named traopical_storm_defs_oo that can be 'required' into this
program.

=cut

sub new {
  my $class = shift;
  croak "ENV variable VAP_LIBRARY is not defined!\n" 
    unless exists( $ENV{VAP_LIBRARY});
  my $tropical_storm_defs_file = $ENV{VAP_LIBRARY}."/tropical_storm_defs_oo";
  croak "Can't find $tropical_storm_defs_file\n" 
    unless (-e $tropical_storm_defs_file);
  require "$tropical_storm_defs_file" ||
    croak "Can't require $tropical_storm_defs_file\n:$!";
  
  
  my $self={TIME=>VapUtil::systime2idltime(VapUtil::SysNow()),
	    STORM_DELTA => 8,
	    WIND_DELTA => 4,
	    @_, 
	   DEFAULTS => $tsoo_defs};
  croak "REGION is REQUIRED\n" unless exists $self->{REGION};
  my @allowed_regions = keys %{$self->{DEFAULTS}->{SATELLITE_REGIONS}};
  croak $self->{REGION}. " is not an allowed region!\nAllowed regions are". 
    join("\n",@allowed_regions)."\n" unless grep($self->{REGION}, @allowed_regions);
  return bless $self, ref($class) || $class
}

1;
