
=head1 NAME

  Overlay.pm -- Object to handle creation of the overlays of QuikSCAT/SeaWinds data
                Replaces most of the code in cloud_overlay.

=head2 SYNOPSIS

  $overly_obj = Overlay->new( REGION=>region, 
                              TIME => 'yyyy/mm/dd/hh/mm',
                              WINDPATH => 'path-to-wind-data',
                              WINDFILTER=> 'filter',
                              DELTA => n,
                              SATNAME => 'name',
                              SATNUM => 'num',
                              SENSORNUM => 'num',
                              LONLIM=>[lonmin,lonmax],
                              LATLIM=>[latmin,latmax],
                              CRDECIMATE=>[x,y],
                              EXCLUDCOLS=>"exclude_string",
                              RAINFLAG=>0|1,
                              RF_ACTION=>0|1,
                              RF_COLOC=>color,
                              LENGTH=>n,
                              TELME=>0|1,
                              HELP=>0|1);

  These keywords control the type of overlay done, see the
  documentation of the IDL routine 'cloud_overlay.pro' for a full
  description of each key.

  Almost all of the time this object will be `used' by the script
  called by cron to make the standard VAP overlay products. As such it
  will generally only use the keys `REGION,' and 'TIME.' The object
  then uses REGION as a key into the hash defined in
  $VAP_LIBRARY/overlay_defs. The other keys (WINDPATH, WINDFILTER,
  SATNAME, SATNUM, SENSORNUM, LONLIM, LATLIM, CRDECIMATE, EXCLUDECOLS,
  RAINFLAG, RF_ACTION, RF_COLOR, DELTA and LENGTH) exists to allow the
  user to specify the information contained in overlay_defs. TELLME
  writes out what processing would be undertaken and then exits
  without actually doing the processing; HELP emits a usage message.

=cut 
#
# $Id$
#
# Modifications:
#
# $Log$
#
use strict;
use vars qw/%overlay_defs/;
use Carp;
use Cwd;
use File::Basename;
use File::Copy;
use lib $ENV{VAP_SFTWR_PERL};

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

  my $defsfile = $ENV{VAP_LIBRARY} . "/overlay_defs_oo";
  croak "Can't find defaults file $defsfile!\n" unless (! -e $defsfile );
  do { require "$defsfile"; } or croak "Can't `require $defsfile\n";

  my $usage =  "\nUsage:\n";
  $usage .= " overlyobj = Overlay->new(REGION=>region,\n";
  $usage .= "  TIME=>'yyyy/mm/dd/hh/mm',(in GMT)\n";
  $usage .= "  WINDPATH => 'PATH_TO_WIND_FILES',\n";
  $usage .= "  WINDFILTER=> WINDFILTER, (i.e. QS or SW or (QS|SW)) (REQUIRED!)\n";
  $usage .= "  DELTA => n, (number of hours around TIME to look for wind data)\n";
  $usage .= "  SATNAME => SATELLITE_NAME, (.e.g. 'goes')\n";
  $usage .= "  SATNUM =>  SATELLITE_NUMBER, (e.g. 10 for goes10)\n";
  $usage .= "  SENSORNUM => SENSOR_NUMBER, (e.g. 1=vis, 2=ir2, 3=ir3, 4=ir4)\n";
  $usage .= "  LONLIM => [lonmin, lonmax],\n";
  $usage .= "  LATLIM => [latmin,latmax], \n";
  $usage .= "  CRDECIMATE => [Col,Row],\n";
  $usage .= "  EXCLUDECOLS => idl-array-submatrix-desg (as string)\n";
  $usage .= "  TELLME =>: Calculates, reports variable values and exits\n\n";
  $usage .= "  where: \n";


  $usage .= "    WINDFILTER: (REQUIRED) filter to use for wind data\n";
  $usage .= "      Possible filters are: Q for QuikSCAT data\n";
  $usage .= "      S for SeaWinds or B for Both\n";
  $usage .= "      This is NOT a shell file glob, but a regular expression,\n";
  $usage .= "      so input it EXACTLY as shown!\n";
  $usage .= "      This switch is *REQUIRED*\n";


  $usage .= "    REGION is the designation for the \n";
  $usage .= "      region as it appears in the _overlay_defs_ file\n";
  $usage .= "      See file $VAP_LIBRARY/overlay_defs for the complete list of \n";
  $usage .= "      predefined regions. If absent, the program falls over to using the \n";
  $usage .= "      information given (some of which can be defaulted)\n";
  $usage .= "      in the combination of the satname/satnum/sensornum/lon/lat\n";
  $usage .= "      options.\n\n";

  $usage .= "    If REGION is absent the required minimal set of keys is:\n\n";
  $usage .= "       WINDFILTER, SATNUM, SATNUM, SENSORNUM, LONLIM, LATLIM\n";



$usage .=   "    TIME: the GMT time of the `run'. This time will determine which\n";
$usage .=   "       cloud and wind data are used and the behavior of the object\n";
$usage .=   "       depends on which other keys are present.\n";

$usage .=   "       If REGION is present but TIME is not, the data used in the\n";	  
$usage .=   "       run will be determined using the ASCTIME/DESCTIME in the hash\n"; 
$usage .=   "       defined in the $VAP_LIBRARY/overlay_defs_oo. The object will\n";  
$usage .=   "       choose the time closest but not exceeding to the current\n";      
$usage .=   "       time.\n";
  
$usage .=   "       If TIME is present at object creation, the object will use\n";
$usage .=   "       that time whether REGION (and hence the ASCTIME/DESCTIME\n";
$usage .=   "       implied) is present or not.\n\n";

  $usage .= "    DELTA: defines the `window' around the value given in TIME\n";
  $usage .= "      in which to search for wind data\n"
  $usage .= "    PATH_TO_WIND_FILES: pretty self explanatory.\n";
  $usage .= "      Default given by environmental variable 'VAP_DATA_TOP'\n";

  $usage .= "    SATNAME: Name of satellite, Currently only 'goes' works\n";
  $usage .= "      Maybe someday we'll be able to use GMS\n";

  $usage .= "    SATNUM: Satellite Number (Currently only 10 and 8 work, since \n";
  $usage .= "      we can only do 'GOES' satellites\n";

  $usage .= "    SENSORNUM: 1=vis, 2=ir2, 3=ir3, 4=ir4\n";

  $usage .= "    LONLIM: Followed by two values: e.g. '--lonlim [minlon, maxlon]'\n";
  $usage .= "      Defaults are dependent on the satellite\n";

  $usage .= "    LATLIM: Similar to --lon, but in latitude\n";
  $usage .= "    CRDECIMATE: the col/row decimation\n";
  $usage .= "      Default=[2,2]\n";
  $usage .= "    EXCLUDECOLS: See documentation for cloud_overlay.pro for the format\n";
  $usage .= "       of this string (default = '')\n";
  $usage .= "    LENGTH: Length fo the Vectors overplotting the cloud data\n";
  $usage .= "    RAINFLAG: Determines what to do with Rain Flagged wind data.\n";
  $usage .= "       0=ignore, 1=use (default=0)\n";
  $usage .= "    RF_ACTION: What to do if rainflag==1\n";
  $usage .= "       0=don't plot rainflagged vectors. 1=plot using rf_color\n";
  $usage .= "    RF_COLOR: Color to use when plotting rainflagged vectors if rf_action==1\n";
  $usage .= "       this color depends on the device environment. It's an index in 8-bit \n";
  $usage .= "       color and a true 24 bit color in 24 bit color environment.\n";
  $usage .= "    TELLME: Calculates, reports the values for all the variables and exits\n\n";
  $usage .= "      Use this option to find out what the defaults are in any given situation\n";

  $usage .= "    If one wishes to grid and overlay an arbitrary region not predefined\n";
  $usage .= "    in the overlay_defs file, one must use the combination of \n";
  $usage .= "    satname/satnum/sensornum/lon/lat to do so.\n\n";
}
use VapUtil;
use VapError;
use OGoes;
use OGms5;

#=============================================================
#
#=============================================================

sub new {
  my $class = shift;
  my $self={@_};
  $self->{OVERLAY_DEFAULTS} = \%overlay_defs;
  %overlay_defs = undef;
  if (defined $self->{REGION}) {
    my $region = $self->{REGION};
    my $hash= $self->{DEFAULTS}->{$region};
    $self->{SATNAME}     = $hash-> {CLOUDS}->{Satellite};
    $self->{LONLIM}      = $hash-> { CLOUDS }->{ LONLIM      };
    $self->{LATLIM}      = $hash-> { CLOUDS }->{ LATLIM      };
    $self->{SATNUM}      = $hash-> { CLOUDS }->{ SATNUM      };
    $self->{SENSORNUM}   = $hash-> { CLOUDS }->{ SENSORNUM   };
    $self->{ASCTIME}     = $hash-> { CLOUDS }->{ ASCTIME     };
    $self->{DESCTIME}    = $hash-> { CLOUDS }->{ DESCTIME    };
    $self->{NVECTORS}    = $hash-> { WINDS  }->{ NVECTORS    };
    $self->{CRDECIMATE}  = $hash-> { WINDS  }->{ CRDECIMATE  };
    $self->{LENGTH}      = $hash-> { WINDS  }->{ LENGTH      };
    $self->{DELTA}       = $hash-> { WINDS  }->{ DELTA       };
    $self->{RAINFLAG}    = $hash-> { WINDS  }->{ RAINFLAG    } unless $self->{RAINFLAG};
    $self->{RF_ACTION}   = $hash-> { WINDS  }->{ RF_ACTION   } unless $self->{RF_ACTION};
    $self->{RF_COLOR}    = $hash-> { WINDS  }->{ RF_COLOR    } unless $self->{RF_COLOR};
    $self->{EXCLUDECOLS} = $hash-> { WINDS  }->{ EXCLUDECOLS } unless $self->{EXCLUDECOLS};

    if (! $self->{TIME} ){

      # Figure out which is closer to current time: the ASCTIME or
      # DESCTIME that's defined in the overlay_defs_oo hash.

      use Time::Local;
      my $nowsecs = $^T;
      my ($ss, $mm, $hh, $mday, $mon, $year, $wday, $yday, $isdst) = 
	gmtime(time());
      my ($ahh,$amm) = split /:/,$self->{ASCTIME};
      my $atime = timegm(0,$amm,$ahh,$mday,mon,$year);
      my ($dhh,$dmm) = split /:/,$self->{DESCTIME};
      my $dtime = timegm(0,$dmm,$dhh,$mday,mon,$year);
      my $time = $atime<=$nowsecs? $atime: $dtime;
      $self->{TIME} = systime2idltime($time);
    }
  } else {
    print "$usage\n";
    croak "*** Need, minimally, SATTIME, SATNUM, SENSORNUM, WINDFILTER, LON and LAT\n";
    unless (defined($self->{WINDFILTER}) && 
	    defined($self->{SATNAME}) && 
	    defined($self->{SENSORNUM}) && 
	    defined($self->{LONLIM}) && 
	    defined($self->{LATLIM}) );
    $self->{TIME} = $systime2idltime(SysNow()) 
      unless $self->{TIME};

  }
  bless $self, ref($class) || $class;
  $self->{ERROROBJ} = VapError->new() or 
    croak "Error creating VapError Object!\n";
  $self->{DELTA} = 4 unless $self->{DELTA};
  $self->{SECS} = idltime2systime($self->{TIME});
  $self->ReportStatus();

  return $self;
}
#=============================================================
# ReportStatus
#=============================================================
sub ReportStatus{
  my $self=shift;
  1;
}
#=============================================================
# _croak
#  Wrapper for errorobject->ReportAndDie
#==================================================================

sub _croak {
  my $self=shift;
  my $msg=shift || "NULL MESSAGE\n";
  my $subject =shift || "NULL SUBJECT";
  my $errobj=$self->{ERROROBJ};
  $errobj->ReportAndDie($subject, $msg);
  1;
}


#=============================================================
#
#=============================================================



#=============================================================
#
#=============================================================

1;
