
=head1 NAME

  Overlay.pm -- Object to handle creation of the overlays of
                QuikSCAT/SeaWinds data. Replaces most of the code in
                cloud_overlay.

=head2 SYNOPSIS


   overlyobj = Overlay->new(REGION=>region,
	    TIME=>'yyyy/mm/dd/hh/mm',(in GMT)
	    WINDPATH => 'PATH_TO_WIND_FILES',
	    WINDFILTER=> WINDFILTER, (i.e. 'Q' or 'S' (REQUIRED!)
	    WINDDELTA => n.m, (number of hours around TIME to look for wind data)
            IMAGEDELTA => f.g, image data closest to TIME must be 
                          no more than this delta prior. if ABS flag is set, 
                          this interval extends on both sides of TIME, i.e. 
                          the image data may be later than TIME.
            ABSFLAG => 0|1. (determines whether the check for image data extends 
                             back from TIME or in an interval around TIME)
	    SATNAME => SATELLITE_NAME, (.e.g. 'goes')
	    SATNUM =>  SATELLITE_NUMBER, (e.g. 10 for goes10)
	    SENSORNUM => SENSOR_NUMBER, (e.g. 1=vis, 2=ir2, 3=ir3, 4=ir4)
	    LONLIM => [lonmin, lonmax],
	    LATLIM => [latmin,latmax],
	    CRDECIMATE => [Col,Row],
	    EXCLUDECOLS => idl-array-submatrix-desg (as string),
            LENGTH => n
            RAINFLAG => 0|1,
            RF_ACTION => 0|1,
            RF_COLOR => n,
            ERROROBJ => An object of type VapError, (a attempt at a unified logging mechanism)
                        One is created if not passed in, but this object should really be created 
                        by the caller and then passed to all objects used in processing some Vap product.
	    TELLME => 1, (Calculates, reports and exits)
            GET_REGIONS => 1, (returns the current valid regions)
                           )

    where:


      WINDFILTER: (REQUIRED) one letter switch which tells 
                  which wind data to use.

        Possible filters are: Q|q for QuikSCAT data
        S|s for SeaWinds.

        Currently there's no plan to do combined overlays.

        This is NOT a shell file glob, but a regular switch,
        so input it EXACTLY as shown!



      REGION: the designation for the region as it appears in the
        _overlay_defs_oo_ file. (See file \$VAP_LIBRARY/overlay_defs_oo for
        the complete list of predefined regions.) If absent, the
        program falls over to using the information given (some of
        which can be defaulted) in the combination of the
        satname/satnum/sensornum/lon/lat options.

          If REGION is absent the required minimal set of keys is:
          WINDFILTER, SATNUM, SATNUM, SENSORNUM, LONLIM, LATLIM



      TIME: the GMT time of the `run'. This time will determine which
           cloud and wind data are used and the behavior of the object
           depends on which other keys are present.

           If REGION is present but TIME is not, the data used in the
           run will be determined using the ASCTIME/DESCTIME in the hash
           defined in the \$VAP_LIBRARY/overlay_defs_oo. The object will
           choose the time closest but not exceeding to the current
           time.

           If TIME is present at object creation, the object will use
           that time whether REGION (and hence the ASCTIME/DESCTIME
           implied) is present or not.

      WINDDELTA: defines the `window' around the value given in TIME
        in which to search for wind data

      IMAGEDELTA: A float value. Determines (along with ABSFLAG) what
                  time interval to search for image data. If ABSFLAG
                  == 0, the interval is [TIME-IMAGEDELTA, TIME]. If
                  ABSFLAG==1 then the interval is 
                  [TIME-IMAGEDELTA/2,TIME+IMAGEDELTA/2]

      ABSFLAG: 0|1. Determines the sort of interval to search in. See
               IMAGEDELTA.

      PATH_TO_WIND_FILES: pretty self explanatory.
        Default given by environmental variable 'VAP_DATA_TOP'

      SATNAME: Name of satellite, Currently only 'goes' works
        Maybe someday we'll be able to use GMS

      SATNUM: Satellite Number (Currently only 10 and 8 work, since 
        we can only do 'GOES' satellites

      SENSORNUM: 1=vis, 2=ir2, 3=ir3, 4=ir4

      LONLIM: Followed by two values: e.g. '--lonlim [minlon, maxlon]'
        Defaults are dependent on the satellite

      LATLIM: Similar to --lon, but in latitude

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

      ERROROBJ: Used in error reporing. It should be created and
                passed by the caller, but it will be created by this
                object if that hasn't happened.

      GET_REGIONS: returns the current valid regions, then exits.

      TELLME: Calculates, reports the values for all the variables and exits
        Use this option to find out what the defaults are in any given situation

      If one wishes to grid and overlay an arbitrary region not predefined
      in the overlay_defs file, one must use the combination of
      satname/satnum/sensornum/lon/lat to do so.

  The minimal set of keywords required by this object are REGION and
  WINDFILTER. The rest will take appropriate defaults.

=cut 

#
# $Id$
#
# Modifications:
#
# $Log$
# Revision 1.9  2003/01/16 23:47:53  vapdev
# Continuing work
#
# Revision 1.8  2003/01/04 00:16:21  vapdev
# Continuing work
#
# Revision 1.7  2002/12/20 23:45:26  vapdev
# Ongoing ... well, you know.
#
# Revision 1.6  2002/12/10 19:57:12  vapdev
# Ongoing work
#
# Revision 1.5  2002/12/09 23:39:23  vapdev
# Continuing work
#
# Revision 1.4  2002/12/06 22:54:03  vapdev
# Continuing work
#
# Revision 1.3  2002/12/06 00:39:22  vapdev
# Continuing work
#
# Revision 1.2  2002/12/03 00:13:28  vapdev
# Ongoing work
#
# Revision 1.1  2002/08/21 18:28:54  vapdev
# initial revision
#
#
package Overlay;
use strict;
use vars qw/$overlay_defs $gms5_defs $VERSION $VAP_LIBRARY $usage/;
use Carp;
use Cwd;
use File::Basename;
use File::Copy;


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

   overlyobj = Overlay->new(REGION=>region,
	    TIME=>'yyyy/mm/dd/hh/mm',(in GMT)
	    WINDPATH => 'PATH_TO_WIND_FILES',
	    WINDFILTER=> WINDFILTER, (i.e. 'Q' or 'S' (REQUIRED!))
	    WINDDELTA => n.m, (number of hours around TIME to look for wind data)
            IMAGEDELTA => f.g, (image data closest to TIME must be 
                          no more than this delta prior. if ABS flag is set, 
                          this interval extends on both sides of TIME, i.e. 
                          the image data may be later than TIME.)
	    SATNAME => SATELLITE_NAME, (.e.g. 'goes')
	    SATNUM =>  SATELLITE_NUMBER, (e.g. 10 for goes10)
	    SENSORNUM => SENSOR_NUMBER, (e.g. 1=vis, 2=ir2, 3=ir3, 4=ir4)
	    LONLIM => [lonmin, lonmax], (longitude limits for overlay)
	    LATLIM => [latmin,latmax],  (latitude limits for overlay)
	    CRDECIMATE => [Col,Row],  (decimate the wind data by col/row)
	    EXCLUDECOLS => idl-array-submatrix-desg (as string)
            LENGTH => n, (length of vectors)
            RAINFLAG => 0|1, (what to do about rainflagged data)
            RF_ACTION => 0|1, (what to do if doing something about rainflagged data)
            RF_COLOR => N,    (color to use if RF_ACTION=1)
            ERROROBJ => VapError_object, (unified logging)
	    TELLME =>1, (Calculates, reports and exits)
            get_regions => 1) (returns current valid regions, then exits)

    where:


      WINDFILTER: (REQUIRED) one letter switch which tells 
                  which wind data to use.

        Possible filters are: Q|q for QuikSCAT data
        S|s for SeaWinds.

        Currently there's no plan to do combined overlays.

        This is NOT a shell file glob, but a regular switch,
        so input it EXACTLY as shown!



      REGION: the designation for the region as it appears in the
        _overlay_defs_oo_ file. (See file \$VAP_LIBRARY/overlay_defs_oo for
        the complete list of predefined regions.) If absent, the
        program falls over to using the information given (some of
        which can be defaulted) in the combination of the
        satname/satnum/sensornum/lon/lat options.

          If REGION is absent the required minimal set of keys is:
          WINDFILTER, SATNUM, SATNUM, SENSORNUM, LONLIM, LATLIM



      TIME: the GMT time of the `run'. This time will determine which
           cloud and wind data are used and the behavior of the object
           depends on which other keys are present.

           If REGION is present but TIME is not, the data used in the
           run will be determined using the ASCTIME/DESCTIME in the hash
           defined in the \$VAP_LIBRARY/overlay_defs_oo. The object will
           choose the time closest but not exceeding to the current
           time.

           If TIME is present at object creation, the object will use
           that time whether REGION (and hence the ASCTIME/DESCTIME
           implied) is present or not.

      WINDDELTA: defines the `window' around the value given in TIME
        in which to search for wind data

      PATH_TO_WIND_FILES: pretty self explanatory.
        Default given by environmental variable 'VAP_DATA_TOP'

      SATNAME: Name of satellite, Currently only 'goes' works
        Maybe someday we'll be able to use GMS

      SATNUM: Satellite Number (Currently only 10 and 8 work, since 
        we can only do 'GOES' satellites

      SENSORNUM: 1=vis, 2=ir2, 3=ir3, 4=ir4

      LONLIM: Followed by two values: e.g. '--lonlim [minlon, maxlon]'
        Defaults are dependent on the satellite

      LATLIM: Similar to --lon, but in latitude

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

      GET_REGIONS: returns valid regions, then exits.

      TELLME: Calculates, reports the values for all the variables and
                exits Use this option to find out what the defaults
                are in any given situation

      If one wishes to grid and overlay an arbitrary region not predefined
      in the overlay_defs file, one must use the combination of
      satname/satnum/sensornum/lon/lat to do so.

EOF


}

use lib $ENV{VAP_SFTWR_PERL};
use lib $ENV{VAP_LIBRARY};
use OGoes;
use OGms5;
use VapUtil;
use VapError;


#=============================================================
#
#=============================================================

sub new {
  my $class = shift;
  my $self = {@_};

  my $defsfile = "overlay_defs_oo";
  my $msgdefsfile = $ENV{VAP_LIBRARY} . "/overlay_defs_oo";
  $self->{ERROROBJ} = VapError->new() unless $self->{ERROROBJ};
  bless $self, ref($class) || $class;


  $self->{ERROROBJ}->_croak("Can't find defaults file $msgdefsfile!\n",
		"CAN'T FIND DEFS FILE!") unless ( -e $msgdefsfile );
  do { require "$defsfile"; } || 
    $self->{ERROROBJ}->_croak("Can't `require $msgdefsfile\n",
		  "ERROR in `REQUIRE' of DEFS!");
  $self->{OVERLAY_DEFAULTS} = $overlay_defs;
  if ($self->{GET_REGIONS}){
    my @regions = $self->getRegions;
    return @regions;
  }


  if ($self->{GET_DEFS}){
    return $self->{OVERLAY_DEFAULTS};
  }

  $self->{TMPDIR} = $ENV{VAP_OPS_TMPFILES};
  $self->{USER} = $ENV{USER};
  $self->{PID} = $$;
  $self->{WINDPATH} = $ENV{VAP_DATA_TOP};
  $self->{ABSFLAG} = 0 unless $self->{ABSFLAG};

  my $minusage = "*** Minimally, I need either (REGION,WINDFILTER)\n";
  $minusage .= "or TIME, SATNUM, SENSORNUM, WINDFILTER, LONLIM and LATLIM\n";
  if ($self->{HELP}) {
    print $usage;
    print $minusage;
    exit;
  }
  if (@_ < 2){
    print "Too few arguments!\n";
    print "$usage\n";
    croak $minusage;
  }

  @{$self->{GOES_NUM2NAME}} = qw/null vis ir2 ir3 ir4/; 
  @{$self->{GMS_NUM2NAME}} = qw/null ir1 ir2 ir3 vis/;

     # `null' is just a placeholder to make the indexing work out.


  if ($self->{REGION} && $self->{WINDFILTER}) {
    my $region = $self->{REGION};
    my $hash= $self->{OVERLAY_DEFAULTS}->{$region};
    $self->{SATNAME}     = $hash->{CLOUDS}->{Satellite};
    $self->{LONLIM}      = $hash->{ CLOUDS }->{ LONLIM      };
    $self->{LATLIM}      = $hash->{ CLOUDS }->{ LATLIM      };
    $self->{SATNUM}      = $hash->{ CLOUDS }->{ SATNUM      };
    $self->{SENSORNUM}   = $hash->{ CLOUDS }->{ SENSORNUM   };

    $self->{NVECTORS}    = $hash->{ WINDS  }->{ NVECTORS    };
    $self->{CRDECIMATE}  = $hash->{ WINDS  }->{ CRDECIMATE  };
    $self->{LENGTH}      = $hash->{ WINDS  }->{ LENGTH      };
    $self->{WINDDELTA}   = $hash->{ WINDS  }->{ DELTA       } unless $self->{WINDDELTA};
    $self->{RAINFLAG}    = $hash->{ WINDS  }->{ RAINFLAG    } unless $self->{RAINFLAG};
    $self->{RF_ACTION}   = $hash->{ WINDS  }->{ RF_ACTION   } unless $self->{RF_ACTION};
    $self->{RF_COLOR}    = $hash->{ WINDS  }->{ RF_COLOR    } unless $self->{RF_COLOR};
    $self->{EXCLUDECOLS} = $hash->{ WINDS  }->{ EXCLUDECOLS } unless $self->{EXCLUDECOLS};

    if (! $self->{TIME} ){

      # Figure out which of ASCTIME or DESCTIME, defined in the
      # overlay_defs_oo hash, is closest to, but doesn't exceed, the
      # current time.

      use Time::Local;
      my $nowsecs = $^T;
      my ($ss, $mm, $hh, $mday, $mon, $year, $wday, $yday, $isdst) = 
	gmtime(time());
      my $timehash;
      $timehash = ($self->{WINDFILTER} =~ /^S/i)? 
	$hash->{CLOUDS}->{TIME}->{SW}:$hash->{CLOUDS}->{TIME}->{QS};

    $self->{ASCTIME}     = $timehash->{ASCTIME};
    $self->{DESCTIME}    = $timehash->{DESCTIME};

      my ($thh,$tmm,$atime,$dtime, $time);
      ($thh,$tmm) = split /:/,$timehash->{ASCTIME};
      $atime = timegm(0,$tmm,$thh,$mday,$mon,$year);
      ($thh,$tmm) = split /:/,$timehash->{DESCTIME};
      $dtime = timegm(0,$tmm,$thh,$mday,$mon,$year);

      if ($atime > $nowsecs && $dtime > $nowsecs) {
	if ($atime > $dtime){
	  $atime -= 86400;
	} else {
	  $dtime -= 86400;
	} 
      } elsif ($atime < $nowsecs && $dtime < $nowsecs) {
	if ($atime < $dtime){
	  $atime += 86400;
	} else {
	  $dtime += 86400;
	} 
      } 

      if (!$self->{ABSFLAG}){
	$time = $atime<=$nowsecs? $atime: $dtime;
      } else {
	$time = abs($nowsecs-$atime) < abs($nowsecs-$dtime)? $atime: $dtime
      }


      $self->{TIME} = systime2idltime($time);
    }
  } else {
    print "$usage\n";
    croak $minusage
      unless (defined($self->{WINDFILTER}) && 
	      defined($self->{SATNAME}) && 
	      defined($self->{SENSORNUM}) && 
	      defined($self->{LONLIM}) && 
	      defined($self->{LATLIM}) );

    $self->{TIME} = systime2idltime(SysNow()) 
      unless $self->{TIME};

  }
  $self->{IMAGEDELTA} = 3 unless $self->{IMAGEDELTA};
  $self->{DELTA} = 4 unless $self->{DELTA};
  $self->{SECS} = idltime2systime($self->{TIME});
  $self->{LIMITS} = [${$self->{LONLIM}}[0], 
		     ${$self->{LATLIM}}[0],
		     ${$self->{LONLIM}}[1], 
		     ${$self->{LATLIM}}[1]];
  return $self;
}

#=============================================================
# setupProcessing
#=============================================================


sub setupProcessing{
  my $self=shift;

  # is this a GMS overlay?
  my $GMS = $self->{REGION}? 
    ($self->{REGION} =~ /GMS/) : 
      ($self->{SATNAME} =~ /GMS/i);

  my ($gmstype, $gridded_file, $delta);
  if (!$GMS){
    # Goes!
    my $sat = $self->{SATNUM};
    my $sensor = ${$self->{GOES_NUM2NAME}}[$self->{SENSORNUM}];
    my @lonlim = @{$self->{LONLIM}};
    my @latlim = @{$self->{LATLIM}};
    my $limits = [$lonlim[0], $latlim[0], $lonlim[1], $latlim[1]];
    my $goes = OGoes->new(SAT => $sat,
			  SENSOR => $sensor,
			  LIMITS => $limits,
			  TIME => $self->{TIME}, 
			  DELTA => $self->{IMAGEDELTA},
			  ABSFLAG => $self->{ABSFLAG},
			  ERROROBJ => $self->{ERROROBJ});
    $self->{ERROROBJ}->_croak("Error creating OGoes object",
		  "OGoes initialization failure!") unless $goes;
    my $t = time();
    $gridded_file = $goes->gag();
    print "Error gridding file -- Continuing without one!\n" 
      unless ($gridded_file);
    $self->{GRIDDED_FILE} = $gridded_file;
    $t=time() - $t;
    print "Gridding took $t seconds!\n";
  } else {
    # GMS!
    my $sensor = ${$self->{GMS_NUM2NAME}}[$self->{SENSORNUM}];
    my @lonlim = @{$self->{LONLIM}};
    my @latlim = @{$self->{LATLIM}};
    my $limits = [$lonlim[0], $latlim[0], $lonlim[1], $latlim[1]];
    my $gms = OGms5->new(DATETIME => $self->{TIME},
			 LIMITS=>$limits,
			 DELTA => $self->{IMAGEDELTA},
			 ERROROBJ=>$self->{ERROROBJ});
    my ($datetime, $mindiff) = $gms->GetClosest();
    $self->{ERROROBJ}->_croak("No GMS files within $delta of $datetime",
		"No GMS files") unless $datetime;
    $gmstype = $sensor;
    $gridded_file=$datetime;

  }
  $self->{GRIDDED_FILE} = $gridded_file;
  $self->{GMSTYPE} = $gmstype || "";

  1;
}

#=============================================================
# _createLockfile
#
#   Construct lockfile name, then create the file.
#==================================================================
sub _createLockfile{
  my $self=shift;
  my $lockfile = $self->{TMPDIR} . "/" . $self->{USER} . 
    ".overlay." . $self->{PID};
  $self->{LOCKFILE} = $lockfile;
  open LOCKFILE, ">$lockfile" or 
    $self->{ERROROBJ}->_croak("Open failure for $lockfile\n", "LOCKFILE OPENERROR");
  $lockfile;
}


#=============================================================
# runIDL
#
#  Contruct the various temporary files and populate them with the
#  required lines of IDL code, then execute the temporary file while
#  parsing the output for errors. Report back any problems.
#
#==================================================================
sub runIDL{
  my ($self,$gridded_file,$gmstype) = @_;

    # build the IDL batch file.
  my $idl_tmp_file = $self->_buildTmpfile();

  my $exe_str="idl $idl_tmp_file";
  my $r=system( $exe_str )/256;

  $self->{ERROROBJ}->_croak("Error in IDL\n",
		"ERROR CALLING IDL") if ($r != 0);
  # check for errors

  $self->CheckForErrors;

  my ($outputname, $thumbnail) =$self->_getOutputname();

  # since we've let the IDL name the file, the output should have 
  # the following format. sat_sensor_date_a,b,c,d.jpg
  $self->{OUTPUTNAME} = $self->_constructFinalname($outputname,
						   ".gif",".jpg",".jpeg",".ps",
						   ".GIF",".JPG",".JPEG",".PS"
						  );
  $self->{THUMBNAIL} = $self->_constructFinalname($thumbnail,
						  ".gif.TN",".GIF.TN",".GIF.tn",".gif.tn",
						  ".jpg.tn",".jpg.TN",".JPG.tn",".JPG.TN",
						  ".jpeg.tn",".jpeg.TN",".JPEG.tn",".JPEG.TN",
						  ".ps.tn", ".PS.tn",".ps.TN", ".PS.TN" 
						 );

  1;
}


#=============================================================
# _buildTmpfile
#    Construct the tmpfile that will contain the IDL commands.
#==================================================================

sub _buildTmpfile{

  my $self=shift;
  # The IDL routines like time as yyyy/mm/dd/hh/mm. 
  # So, construct this time string.

  my $lock_file = $self->_createLockfile();
  my $gridded_file = $self->{GRIDDED_FILE};
  my $delta = $self->{WINDDELTA};
  my ($idl_time_string, $idl_tmp_file, $exe_str);
  $idl_time_string=$self->{TIME};
  my ($name0, $path0) = fileparse($0);
  $idl_tmp_file=$self->{TMPDIR} ."/$name0" . "_idl_called_by_perl_$$.pro";
  print "Writing IDL tmp file $idl_tmp_file\n";

  open(TMPFILE,">$idl_tmp_file") || 
    $self->{ERROROBJ}->_croak("Can't open IDL tmpfile\n$idl_tmp_file\n",
		  "$name0: Bad Open on IDL tmpfile\n");

  my $idl_windfilter = $self->{WINDFILTER} =~ /^Q/i? "QS*":"SW*";

  print TMPFILE "gridded_file = '". $self->{GRIDDED_FILE} . "'\n";
  print TMPFILE "idl_windfilter = '" . $idl_windfilter . "'\n";
  print TMPFILE "idl_time_string = '" . $idl_time_string . "'\n";
  print TMPFILE "delta = $delta\n";
  print TMPFILE "wpath = '" . $self->{WINDPATH} . "'\n";
  print TMPFILE "lockfile = '". $lock_file . "'\n";
  print TMPFILE "length = ". $self->{LENGTH} . "\n";
  print TMPFILE "CRDecimate = [" . 
    join( ",", @{$self->{CRDECIMATE}} ) . "]\n";
  print TMPFILE "ExcludeCols = '" . $self->{EXCLUDECOLS} . "'\n";
  my @limits = @{$self->{LIMITS}};
  my $maplimits = "[ $limits[0], $limits[1], $limits[2], $limits[3]]";

  print TMPFILE "mapLimits = $maplimits\n";

  my $tmpfile_exe_str = << "EOF";

  cloud_overlay, gridded_file, idl_windfilter, idl_time_string, delta,\$
    wpath = wpath, lockfile=lockfile, length=length, \$
    crdecimate=crdecimate, excludecols=excludecols

EOF

  $tmpfile_exe_str .= ",\$\nrainflag = " . $self->{RAINFLAG} 
    if $self->{RAINFLAG};
  $tmpfile_exe_str .= ",\$\n rf_action=" . $self->{RF_ACTION} 
    if $self->{RF_ACTION};
  $tmpfile_exe_str .= ",\$\n rf_color=". $self->{RF_COLOR} 
    if $self->{RF_COLOR};
  $tmpfile_exe_str .= ",\$\n gmsType='" . $self->{GMSTYPE} . "'"
    if $self->{GMSTYPE};

  print TMPFILE "\n$tmpfile_exe_str\n";
  print TMPFILE "\nexit\n";

  close TMPFILE;
  $self->{IDLTMPFILE} = $idl_tmp_file;

}
#=============================================================
# _constructFinalname
#
#    Construct the name this file will have in the overlay archive
#    portion of the WWW area, That name will include the region and
#    windfilter info, which isn't included in the filename constructed
#    by the IDL routine cloud_overlay
#
#==================================================================

sub _constructFinalname{
  my $self = shift;
  my $outputname = shift;
  my @exts = @_;
  my ($name, $path, $ext) = fileparse($outputname, @exts);
  my @tmp=split(/_/,$self->{REGION});
  my $tmp=join("_",(@tmp[$#tmp-1,$#tmp],$self->{WINDFILTER}));
  $name .= "_$tmp";
  $name .= $ext;
  $name = "$path/$name";
  rename $outputname, $name;
  $name;
}


#=============================================================
# _getOutputname
#  Get the name of the output file.
#==================================================================

sub _getOutputname{
  my $self = shift;
  my $file_with_name = $ENV{VAP_OPS_OVERLAY} . 
    "/auto_cloud_overlay_output_file.". $self->{PID};
  open FILE,"<$file_with_name" or 
    $self->{ERROROBJ}->_croak("Couldn't open $file_with_name: $!\n",
		  "CLOUD_OVERLAY: OPENERROR, FILE_WITH_NAME");
  my $outputfile = <FILE>;
  my $thumbnail = <FILE>;
  chomp $outputfile;
  chomp $thumbnail;
  close FILE;
  $outputfile = $self->{OUTPUTFILE} = $ENV{VAP_OPS_OVERLAY} . "/$outputfile";
  $thumbnail = $self->{THUMBNAIL} = $ENV{VAP_OPS_OVERLAY} . "/$thumbnail";
  ($outputfile, $thumbnail);
}


#=============================================================
# CheckForErrors
#  Replaces VapUtil::CheckForErrors
#==================================================================
sub CheckForErrors{
  my $self=shift;
  my $lockfile = shift || $self->{LOCKFILE};
  open FILE, "$lockfile" or 
    $self->{ERROROBJ}->_croak("Error opening $lockfile for reading\n",
		  "LOCKFILE OPENERROR (READ)");
  my @lines = <FILE>;
  close FILE;
  my @errors = grep /ERROR/, @lines;
  if (@errors) {
    my $string = "cloud_overlay had errors!\n Printing\n$lockfile:\n\n";
    $string .=  join("\n", @lines ) . "\n";
    $self->{ERROROBJ}->_croak($string, "ERRORS IN LOCKFILE!");
  }
}


#=============================================================
# ReportStatus
# not used for anything, at the moment.
#=============================================================
sub _reportStatus{
  my $self=shift;
  my $subject = shift || carp "Need subject line!\n";
  my $message = shift || "NULL MESSAGE\n";;
  1;
}



#=============================================================
# {ERROROBJ}->_croak
#  Wrapper for errorobject->ReportAndDie
#==================================================================

# sub {ERROROBJ}->_croak {
#   my $self=shift;
#   my $msg=shift || "NULL MESSAGE\n";
#   my $subject =shift || "NULL SUBJECT";
#   my $errobj=$self->{ERROROBJ};
#   $errobj->ReportAndDie($subject, $msg);
#   1;
# }

sub getOutputname{
  my $self=shift;
  return $self->{OUTPUTNAME};
}

sub getThumbnail{
  my $self=shift;
  return $self->{THUMBNAIL};
}

sub getDefs{
  my $self=shift;
  return $self->{OVERLAY_DEFAULTS};
}

sub getRegions{
  my $self=shift;
  my @regions;
  while (my ($k,$v) = each %{$self->{OVERLAY_DEFAULTS}}){
    push @regions, $k if $v->{WEB}->{ACTIVE};
  }
  if (wantarray) {
    return @regions;
  } else {
    my $string = "Current valid regions:\n". join("\n",@regions) ."\n";
    $self->{ERROROBJ}->Report($string,'INFO');
  }
  
  1;
}
1;
