# $Id$
#

=head1

=head2

=cut

package TropicalStormsOverlay;
use strict;
use vars "$ENV{VAP_LIBRARY} $ENV{VAP_SFTWR_PERL}";
use Carp;

BEGIN {


  $VERSION = "0.9";

  croak "ENV var VAP_LIBRARY is undefined\n" 
    unless $ENV{VAP_LIBRARY};

  croak "ENV var VAP_SFTWR_PERL is undefined\n" 
    unless $ENV{VAP_SFTWR_PERL};

  croak "ENV var VAP_OPS_OVERLAY is undefined\n" 
    unless $ENV{VAP_OPS_OVERLAY};

  croak "ENV var VAP_WWW_TOP is undefined\n" 
    unless $ENV{VAP_WWW_TOP};

  croak "ENV var VAP_OPS_TMPFILES is undefined\n" 
    unless $ENV{VAP_OPS_TMPFILES};

  $usage = <<"EOF";

  Usage:

   obj = TopicalStormsOverlay->new(
            REGION=>region,
	    WINDFILTER=> WINDFILTER, (i.e. 'Q' or 'S' (REQUIRED!))
	    TIME=>'yyyy/mm/dd/hh/mm',(in GMT)
	    WINDPATH => 'PATH_TO_WIND_FILES',
	    WINDDELTA => n.m, #(number of hours around TIME 
                               #to look for wind data)
            STORMDELTA => f.g, #time around TIME in which to look for storms.

            ABSFLAG => 0|1,  # if 1, time range is Time+/-`delta'/2, otherwise
                             # it's time-delta (whether storms or winds.)

	    CRDECIMATE => [Col,Row],
	    EXCLUDECOLS => idl-array-submatrix-desg (as string)
            LENGTH => n # length of vectors.
            RAINFLAG => 0|1, #flag telling whether to use the rain flag.
            RF_ACTION => 0|1. #determines whether to excise the data (0) 
                              #or plot it using RF_COLOR (1)
            RF_COLOC => n # What color to use if RAINFLAG=RF_ACTION=1.
           )

    where:


      WINDFILTER: (REQUIRED) one letter switch which tells 
                  which wind data to use.

        Possible filters are: Q|q for QuikSCAT data
        S|s for SeaWinds.

        Currently there's no plan to do combined overlays.

        This is NOT a shell file glob, but a regular switch,
        so input it EXACTLY as shown!


      REGION: (REQUIRED) 
        the designation for the region as it appears in the
        tropical_storm_overlay_defs_oo_ file. 
        (See file \$VAP_LIBRARY/tropical_storm_overlay_defs_oo for
        the complete list of predefined regions.) 

      TIME: the GMT time of the `run'. This time will determine which
           storms, cloud and wind data are used and the behavior of
           the object depends on which other keys are present.

           Default = current time.


      WINDDELTA: defines the `window' around the value given in TIME
        in which to search for wind data

      PATH_TO_WIND_FILES: pretty self explanatory.
        Default given by environmental variable 'VAP_DATA_TOP'

      CRDECIMATE: the col/row decimation
        Default=[2,2]

      EXCLUDECOLS: See documentation for cloud_overlay.pro for the format
         of this string (default = '').

      LENGTH: Length fo the Vectors overplotting the cloud data

      RAINFLAG: Determines what to do with Rain Flagged wind data.
         0=ignore, 1=use (default=0)

      RF_ACTION: What to do if rainflag==1
         0=don't plot rainflagged vectors. 1=plot using rf_color

      RF_COLOR: Color to use when plotting rainflagged vectors if rf_action==1
         this color depends on the device environment. It's an index in 8-bit
         color and a true 24 bit color in 24 bit color environment.

      ABSFLAG: 0|1. If 1, time range is time +/- `delta'/2 otherwise it's 
                    time-delta (whether wind or storm delta)

EOF

}

use lib ($ENV{VAP_LIBRARY},$ENV{VAP_SFTWR_PERL});

use OTS;
use Winds;
use OGms5;
use OGoes;
use VapUtil;
use VapError;
@TropicalStormsOverlay::ISA=qw/VapError/;

=pod

=head1

=head2

=cut

sub new{

  my $class=shift;
  my $self={TIME=>VapUtil::systime2idltime(),
	    WINDDELTA => 4,
	    STORMDELTA => 8,
	    WINDPATH=>$ENV{VAP_DATA_TOP},
	    ABSFLAG => 0,
	    CRDECIMATE => [0,0], 
	    EXCLUDECOLS => "",
	    RAINFLAG => 0,
	    RF_ACTION => 0,
	    RF_COLOR => 0,
	    @_};
  bless $self, ref($class) || $class;
  $self->{ERROROBJ} = VapError->new() unless $self->{ERROROBJ};
  $self->_croak("TS: WINDFILTER undefined!\n",
		"TS: Bad new (windfilter)") unless $self->{WINDFILTER};
  $self->_croak("TS: REGION undefined!\n",
		"TS: Bad new(region)") unless $self->{REGION};

  $self->{OTS} = OTS->new(TIME => $self->{TIME},
			  STORMDELTA => $self->{STORMDELTA},
			  ABSFLAG => $self->{ABSFLAGS});
			  
  return $self;
}

1;
