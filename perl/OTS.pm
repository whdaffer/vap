package OTS;


#
# $Id$
#
#
# Modification Log:
#
# $Log$
# Revision 1.8  2003/01/28 19:02:49  vapdev
# Ongoing work
#
# Revision 1.7  2003/01/25 00:38:08  vapdev
# Continuing work
#
# Revision 1.6  2003/01/16 23:47:53  vapdev
# Continuing work
#
# Revision 1.5  2003/01/04 00:16:21  vapdev
# Continuing work
#
# Revision 1.4  2002/12/30 22:08:58  vapdev
# ongoing
#
# Revision 1.3  2002/12/23 19:18:50  vapdev
# more work
#
# Revision 1.2  2002/12/10 19:57:13  vapdev
# Ongoing work
#
# Revision 1.1  2002/08/08 00:16:47  vapdev
# Initial Revision
#
#



use strict;
use Carp;
use LWP::Simple;
use Time::Local;
use Cwd;
use vars qw/$tsoo_defs $VERSION/;

=head1 NAME

  OTS.pm: Object oriented implementation of TS.pm

=head2 SYNOPSIS

 use OTS;

 $ots_obj = OTS->new(REGION=>'region',
                     WINDFILTER => 'Q|S',
                     [TIME=>'time',
                     STORMDELTA => delta,
                     WINDDELTA => wind_delta]);

  Where:
=over 4

=item * REGION : The region in which we'll search for storms.
                 currently, those regions are: GMS5, GOESEAST and GOESWEST
	         GMS5 has 70<=longitude<=190 and -60<=latitude<=60
		 GOESEAST is roughly the Northern Atlantic and 
		 GOESWEST is roughly the North Eastern Pacific, 
                 about as far west as the Hawaiian Islands.

                 No Default. If not passed the object will die.

=item * WINDFILTER: Which data to make the overlay with. Q for
                    QuikSCAT or S for SeaWinds. NO DEFAULT!


=item * REGION: one of the accepted regions (currently there are three
                such regions: GMS5, GOESWESTN and GOESEASTN,
                corresponding, roughly, to the area covered by GMS5,
                i.e. the West Pacific; the area covered by GOES 10,
                i.e. the East Pacific and some of the Gulf of Mexico;
                and the area covered by GOES 8, i.e the Atlantic. The
                two Goes areas are only northern hemisphere, at the
                moment)


=item * TIME: a string in the form of yyyy/mm/dd/hh/mm/ss. This is the
	      time around which to search for storms, i.e. only storms
	      close to this (and see B<STORMDELTA> for what `close'
	      means) will be returned.  Default = the current time.

=item * STORMDELTA: the number of hours around the time given in
                    TIME to search for storms. Default = 8

=item * WINDDELTA: the number of hours around the time given in
                   TIME to search for wind data. Default = 4

=item * ABSFLAG: 0|1, if 1, time range is TIME +/- `delta'/2
                 (for *both* deltas!) Otherwise it's
                 TIME-`delta'

=back

=head2 RUNTIME REQUIREMENTS

The environmental variable VAP_LIBRARY must be defined and point to an
existing, readible directory. There must be a file in that directory
named traopical_storm_defs_oo that can be 'required' into this
program.

Also, the environmental variable VAP_SFTWR_PERL is required, so that
other modules may be found.

=cut

BEGIN {
  $VERSION = "0.9";

  croak "ENV var VAP_LIBRARY is undefined\n" 
    unless $ENV{VAP_LIBRARY};

  croak "ENV var VAP_SFTWR_PERL is undefined\n" 
    unless $ENV{VAP_SFTWR_PERL};

  croak "ENV var VAP_WWW_TOP is undefined\n" 
    unless $ENV{VAP_WWW_TOP};

  my $tropical_storm_defs_file = "tropical_storm_defs_oo";
  my $foo=$ENV{VAP_LIBRARY}. "/tropical_storm_defs_oo";
  croak "Can't find $tropical_storm_defs_file\n" 
    unless (-e $foo);
}

use lib $ENV{VAP_SFTWR_PERL};
use lib $ENV{VAP_LIBRARY};
require "VapWebsite_defs";
use Winds;
use OGms5;
use OGoes;
use VapUtil;
use VapError;
#use Net::FTP;

=pod

=head1 new (REGION=>'region,
            WINDFILTER=>'Q|S',
            [TIME=>yyyy/mm/dd/hh/mm, 
             STORMDELTA => f.g, 
             WINDDELTA=> f.g,
             ABSFLAG => 0|1]);

=head2 Synopsis:

       create an object of type OTS, [Object, Tropical Storm], This
       object will be used in parsing the Tropical storm data file
       http://www.solar.ifa.hawaii.edu/Tropical/Data/tropicalXX (where
       XX = last to digits of the year) and returning information
       about the contents of that file.

       The object limits itself to finding storms in a region around
       TIME, the form and size of this region is determined by the
       values of ABSFLAG and STORMDELTA.

=head2 Arguments

  Same as above.

=cut

sub new {
  my $class = shift;

  my $self={TIME=>systime2idltime(SysNow()),
	    STORMDELTA => 8,
	    WINDDELTA => 4,
	    ABSFLAG => 0,
	    @_};
  $self->{SYSTIME} = idltime2systime($self->{TIME});
  bless $self, ref($class) || $class;
  $self->{ERROROBJ} = VapError->new() unless $self->{ERROROBJ};

  my $tropical_storm_defs_file = "tropical_storm_defs_oo";
  my $foo=$ENV{VAP_LIBRARY}. "/tropical_storm_defs_oo";
  require "tropical_storm_defs_oo" ||
    $self->{ERROROBJ}->_croak("Can't require $foo\n:$!",
		  "ERROR IN `REQUIRE'");
  $self->{DEFAULTS} = $tsoo_defs; # Load the tropical storm defaults.
  if ($self->{GET_DEFS}){
    return $self->{DEFAULTS};
  }

  $self->{ERROROBJ}->_croak("No WINDFILTER!\n",
			    "Error OTS::new. No WINDFILTER!\n") unless
			      $self->{WINDFILTER};
  $self->{ERROROBJ}->_croak("REGION is REQUIRED\n",
		"NO REGION!\n") unless exists $self->{REGION};



  chdir $self->{DEFAULTS}->{TS_OVERLAY_DIR} || 
    $self->{ERROROBJ}->_croak(["Can't CD to working dir",
			       $self->{DEFAULTS}->{TS_OVERLAY_DIR}], 
			      "OTS::new. Can't CD to working dir!");

  my @allowed_regions = keys %{$self->{DEFAULTS}->{SATELLITE_REGIONS}};
  $self->{ERROROBJ}->_croak( $self->{REGION}. 
		 " is not an allowed region!\nAllowed regions are". 
                 join("\n",@allowed_regions)."\n" ,
		 "DISALLOWED REGION!\n") 
    unless grep($self->{REGION}, @allowed_regions);

  if ($self->{ABSFLAG}) {
    $self->{SYSENDTIME} = $self->{SYSTIME} + 
      ($self->{STORMDELTA}*3600)/2.;
    $self->{ENDTIME} = 
      systime2idltime($self->{SYSENDTIME});
    $self->{SYSSTARTTIME} = $self->{SYSTIME} - 
      ($self->{STORMDELTA}*3600)/2.;
    $self->{STARTTIME} = 
      systime2idltime($self->{SYSSTARTTIME});
  } else {
    $self->{SYSENDTIME} = $self->{SYSTIME};
    $self->{ENDTIME} = $self->{TIME};
    $self->{SYSSTARTTIME} = $self->{SYSTIME} - $self->{STORMDELTA}*3600;
    $self->{STARTTIME} = 
      systime2idltime($self->{SYSSTARTTIME});
  }

  $self->{DECSTARTTIME} = systime2decyear($self->{SYSSTARTTIME});
  $self->{DECENDTIME} = systime2decyear($self->{SYSENDTIME});
  $self->{DECTIME} = systime2decyear($self->{SYSTIME});



  $self->getTropicalStormsDatafile;
  my @storms = $self->parseTropicalStorms;
  @storms = $self->findCloseEnough;
  if (!@storms) {
    my $region = $self->{REGION};
    my $starttime = $self->{STARTTIME};
    my $endtime = $self->{ENDTIME};
    $self->{ERROROBJ}->_croak(
		       ["No storms found for $region  in time range ",
			"starttime: $starttime",
			"end time: $endtime\n"],
			      "No storms in $region for timerange");
  }
  return $self;
}

=pod

=head1 getTropicalStormsDataFile

=head2 Synopsis: FTPs the file containing the current information
       about the tropical storms. Stores the entire file in
       @{$self->{FILE_CONTENTS}}

=cut


sub getTropicalStormsDatafile {

  my $self = shift;
  my $localfile=$ENV{VAP_LIBRARY} . "/" . $self->{DEFAULTS}->{LOCAL_URL};
  my $fetch=0;
  my @in=();
  if (-e $localfile ) {

    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,
	  $size,$atime,$mtime,$ctime,$junk)=stat($localfile);
    my $lasttime;

    if ($^T-$ctime < 3*3600. ) {
      # local file is less than 3 hours old.

      # Look for the last ACT time in the local file. If it is more
      # than 7 hours away from the current time, re-fetch the
      # remote file.

      if (open FILE,"<$localfile") {
	@in=<FILE>;
	close FILE;
	my @lastfew = ();
	if (@in < 100) {
	  @lastfew = @in;
	} else {
	  @lastfew = @in[ $#in-100 .. $#in];
	}
	my $lasttime;
	foreach (@lastfew) {
	  last if /^\s+$/;
	  my ($time,$type)=$self->_getTimeAndType($_);
	  next if $type =~ /FOR/;
	  $lasttime=$time;
	}
	if (!$lasttime) {
	  $fetch=1;
	} else {
	  my $testtime=systime2decyear($^T);
	  my $diff=($testtime-$lasttime)*365*24;
	  # The reports are about every 6 hours.
	  $fetch=1 if ($diff > 7 );
	}
      } else { $fetch=1;}
    } else {$fetch=1;}
  } else {$fetch=1;}

  my $content;
  my @content;

  if ($fetch) {
#     my $user = $ENV{USER};
#     my $host = $ENV{HOSTNAME} || `hostname`;
#     my $pw = $user . "\@" . $host;
#     my $self->{FTPOBJ} = FTP->new($self->{DEFAULTS}->{STORMS_HOST},
# 				  "anonymous",
# 				  "$pw");
#     $self->{ERROROBJ}->_croak("Can't create FTP object\n",
# 			      "OTS; Error creating FTP object") 
#       unless $self->{FTPOBJ};
#     my $ftp = $self->{FTPOBJ};
#     my $dir=$self->{DEFAULTS}->{STORMS_DIR};
#     my $file=$self->{DEFAULTS}->{STORMS_FILE}
#     $ftp->cd($dir) || 
# 	     $self->{ERROROBJ}->_croak("FTP: Can't CD to $dir\n",
# 				       "FTP: CD error!\n");

    my $remote = $self->{DEFAULTS}->{REMOTE_URL};

    $content = get($remote) or 
      $self->{ERROROBJ}->_croak("Can't get($remote)\n",
		    "OTS:ERROR GETTING remote TS data file");
    open FILE, ">$localfile" or 
      $self->{ERROROBJ}->_croak("Can't open $localfile\n",
		    "OTS:ERROR OPENING LOCAL FILE!");
    print FILE $content;
    close FILE;
    @content=split /\n/, $content;
  }  else {
    @content=@in;
  }
  $self->{FILE_CONTENTS} = \@content;

  1;
}

=pod

=head1 parseTropicalStorms

=head2 Synopsis: $obj->parseTropicalStorms([type, startime, endtime])

  Parse the Tropical Storms data file (which is stored internally in
  the object, and look for storms having at least type `type' (which
  defaults to DEP)that are 'ACTual' reports, as opposed to FORecasts
  between $self->{START_TIME} and $self->{ENDTIME};
  The user may, alternately, pass in type,starttime,endtime and the
  object will reset all of the associated quantities.

  Stores all such storms in the internal hash $self->{STORMS}

=cut

sub parseTropicalStorms{ 

# A typical record might look like:

# 2000 1 05 18.00 UT 12.2 S 82.8 E BABIOLA-00 235 T 09 kt \
# ??? mb 035 045 kt CYC ACT 2000.01571038251 WTXS31 2000.01642190346
#
# Where I've wrapped the line for readibility.
#

  my $self=shift;
  my @content=@{$self->{FILE_CONTENTS}};

  my $stormtype=shift @_ || "DEP";
  my $stormnum=$self->{DEFAULTS}->{STORMRANKING}->{$stormtype} || 
      $self->{ERROROBJ}->_croak(["Unknown Storm type of $stormtype!",
		     "Allowed values are DEP, STO, CYC, HUR, TYP\n"],
		     "OTS: unknown stormtype!");
  
  my $starttime=$self->{DECSTARTTIME};
  my $endtime=$self->{DECENDTIME};

  my ($hour, $min, $vaptime, $time1, $lat, $lon, $name, $yy, $type);

  my %stormranking = %{$self->{DEFAULTS}->{STORMRANKING}};
  for (@content){
    chomp;
    my ($year,$month,$day,$hhmm,$tlat,$tlon,$tname,$course,$speed,$press,
     $winds,$gusts,$storm_type,$rpt_type,$time2,$station,$time3) = /^\s*(\d+)\s+(\d+)\s+(\d+)\s+(\d+\.\d+)\s+UT\s+(\d+\.\d\s*[SN])\s+(\d+\.\d\s*[EW])\s+(\w+-\d+)\s+(\d+|\?+)\s+T\s+(\d+|\?+)\s+kt\s+(\d+|\?+)\s+mb\s+(\d+|\?+)\s+(\d+|\?+)\s+kt\s+(\w+|\?+)\s+(\w+|\?+)\s+(\d+\.\d+)\s+(\w+)\s+(\d+\.\d+).*/;
    ($hour,$min)=split(/\./,$hhmm);
    $vaptime=sprintf("%04d/%02d/%02d/%02d/%02d",$year,$month,$day,$hour,$min);
    $time1=VapUtil::parts2decyear($year,$month,$day,$hour,$min,0);
    ($lat,$type) = $tlat=~/(\d+\.\d)\s*([SN])/;
    $lat *= -1 if $type=~/S/;
    ($lon,$type)=$tlon=~/(\d+\.\d)\s*([EW])/;
    $lon = 360-$lon  if $type=~/W/;
    ($name,$yy)=$tname=~/(\w+)-(\d+)/;
    
    my $test_satellite = $self->{DEFAULTS}->{REGION2SAT}->{$self->{REGION}};
    next if $stormranking{$storm_type} < $stormnum;
    next if $rpt_type !~ /ACT/;
    next if $starttime && ($time1 < $starttime);
    next if $endtime && ($time1 > $endtime);
    my $sat = $self->whichSatellite($lon, $lat);
    next unless $sat && $test_satellite eq $sat;


    push @{$self->{STORMS}->{$name}->{VAPTIME}},    $vaptime;
    push @{$self->{STORMS}->{$name}->{TIME1}},      $time1;
    push @{$self->{STORMS}->{$name}->{ LAT}},       $lat;
    push @{$self->{STORMS}->{$name}->{ LON}},       $lon;
    push @{$self->{STORMS}->{$name}->{ COURSE}},    $course;
    push @{$self->{STORMS}->{$name}->{ SPEED}},     $speed;
    push @{$self->{STORMS}->{$name}->{ PRESS}},     $press;
    push @{$self->{STORMS}->{$name}->{ WINDS}},     $winds;
    push @{$self->{STORMS}->{$name}->{ GUSTS}},     $gusts;
    push @{$self->{STORMS}->{$name}->{ STORM_TYPE}},$storm_type;
    push @{$self->{STORMS}->{$name}->{ RPT_TYPE}},  $rpt_type;
    push @{$self->{STORMS}->{$name}->{ TIME2}},	    $time2;
    push @{$self->{STORMS}->{$name}->{ TIME3}},	    $time3;

  }
  delete $self->{FILE_CONTENTS};
  my @storms = $self->stormsFound;
  foreach (@storms){
    $self->getClosestObservation($_);
  }
  @storms;
}


=pod

=head1 _getTimeAndType

=head2 Synopsis: $obj->_getTimeAndType(record)

  Return the time and type of the storm for this record.

=cut




sub _getTimeAndType{
  my $self=shift;
  shift; # shifts next argument into $_
  chomp;
  my ($year,$month,$day,$hhmm,$tlat,$tlon,$tname,$course,$speed,$press,
   $winds,$gusts,$storm_type,$rpt_type,$time2,$station,$time3) = /^\s*(\d+)\s+(\d+)\s+(\d+)\s+(\d+\.\d+)\s+UT\s+(\d+\.\d\s*[SN])\s+(\d+\.\d\s*[EW])\s+(\w+-\d+)\s+(\d+|\?+)\s+T\s+(\d+|\?+)\s+kt\s+(\d+|\?+)\s+mb\s+(\d+|\?+)\s+(\d+|\?+)\s+kt\s+(\w+|\?+)\s+(\w+|\?+)\s+(\d+\.\d+)\s+(\w+)\s+(\d+\.\d+).*/;

  my ($hour,$min)=split(/\./,$hhmm);
  my $time=parts2decyear($year,$month,$day,$hour,$min,0);
  ($time, $rpt_type);
}



# =pod

# =head1 _getSatelliteInfo

# =head2 


# =cut


# sub getSatelliteInfo{

#   my $self = shift;

#   my $lon=shift || carp "Need a longitude here!\n";
#   my $lat=shift || carp "Need a latitude here!\n";
#   my %info=();

#   my %SatelliteRegions = $self->{DEFAULT}->{SATELLITE_REGIONS};

#   foreach my $k (keys %SatelliteRegions) {
#     my @region=@{$SatelliteRegions{$k}{Region}};
#     if ( $lon >= $region[0] && $lat>= $region[1] && 
# 	$lon <= $region[2] && $lat<= $region[3]) {
#       %info = $SatelliteRegions{$k}{Info};
#       last;
#     }
#   }
#   %info;
# }



=pod

=head1 whichSatellite

=head2 usage: $sat = $obj->whichSatellite($lon, $lat);

   Returns the satellite in whose region the input lon/lat falls.

=cut

sub whichSatellite{

  my $self=shift;

  my $lon=shift || croak "Need a longitude here!\n";
  my $lat=shift || croak "Need a latitude here!\n";

  my $satellite;
  my $SatelliteRegions = $self->{DEFAULTS}->{SATELLITE_REGIONS};

  while (my ($k,$v) = each %{$SatelliteRegions}) {
    my @region=@{$v->{Region}};
    if ( $lon >= $region[0] && $lat>= $region[1] && 
	$lon <= $region[2] && $lat<= $region[3]) {
      $satellite = $k;
      last;
    }
  }
  $satellite;
}

=pod

=head1 getClosestObservation

=head2 usage:  ($ob,lon,lat,time1, vaptime, storm_type,
                   winds,gusts,press,course,speed,
                        rpt_type,satellite ) = 
                         getClosestObservation($storm);

      Returns all the information for the closest observation of a
      particular storm.

=cut

sub getClosestObservation{
  # 1 = getClosestObservation($storm)
  #

  my $self=shift;
  my $storm=shift;
  my ($ref,$hash,$time,$absflag,
	@obtimes,$mindiff,$ob, $diff,$closest);


  $hash = $self->{STORMS}->{$storm};
  $time=$self->{DECTIME};
  $absflag=$self->{ABSFLAG};

  @obtimes=@{$hash->{TIME1}};
  $mindiff=1.e8;
  for ($ob=0;$ob<=$#obtimes;$ob++){
    my $diff=$time-$obtimes[$ob];
    $diff=abs($diff) if ($absflag);
    if ($diff >= 0 && $diff < $mindiff) {
      $mindiff=$diff;
      $closest=$ob;
    }
  }

  $ob=$closest;

  my @closest = ($ob,${$hash->{LON}}[$ob], 
		 ${$hash->{LAT} }[$ob], 
		 ${$hash->{TIME1}}[$ob],
		 ${$hash->{VAPTIME}}[$ob], 
		 ${$hash->{STORM_TYPE}}[$ob], 
		 ${$hash->{WINDS}}[$ob], 
		 ${$hash->{GUSTS}}[$ob], 
		 ${$hash->{PRESS}}[$ob], 
		 ${$hash->{COURSE}}[$ob], 
		 ${$hash->{SPEED}}[$ob], 
		 ${$hash->{RPT_TYPE}}[$ob]);
  $self->{STORMS}->{$storm}->{CLOSEST} = \@closest;
  @closest;

}



=pod

=head1 

=head2 

OBSOLETE


=cut

# sub getClosestInRegion{

#   my ($ref, %hash, $regions, $region, $time, 
# 	 @retdata, @match_data, $obregion);

#   $ref=shift || croak "Need the observations hash!\n";
#   %hash=%{$ref};
#   $regions=join ",", getRegions();
#   $region=shift || croak "Need `region' [$regions]!\n"; 
#   $time=shift || croak "Need `time' parameter!\n";

#   @retdata = ();
#   @match_data=getClosestObservation(\%hash, $time);
#   if ($#match_data > 0) {
#     $obregion=whichSatellite($match_data[1], $match_data[2] );
#     @retdata = @match_data if $region =~/$obregion/;
#   }
#   @retdata;
# }



=pod

=head1 getClosestBySatellite ([region,time,absflag])

=head2 Synopsis:

       Get the storms closest to 'time' subject to the absflag for the
       given `region', all of which default to the values in the
       object if not passed.

      If the user passes any of these values in, the corresponding
       value in the object is set. This may cause some delay, since it
       will require several internal routines to research for storms.


=cut

sub getClosestBySatellite{

  my (@match_data, %match_data, $k, %hash, 
	$sats, $ref);
  my $self=shift;
  my $research = scalar(@_);
  my $satellite=shift || $self->{REGION};
  my $time=shift || $self->{TIME};
  my $absflag=shift || $self->{ABSFLAG};
  $self->parseTropicalStorms() if $research;
  my $storms=$self->{STORMS};

  while (my ($k,$v) = each %{$storms}) {
    my $closest=-1;
    my @time=@{$storms->{$k}->{TIME1}};
    my @sat=@{$storms->{$k}->{SATELLITE}};
    my $mindiff=1.e10;
    for (my $i=0;$i<=$#time;$i++){
      next if !$sat[$i];
      my $diff=$time-$time[$i];
      $diff=abs($diff) if $absflag;
      if ($diff >=0 && $diff < $mindiff && $sat[$i] =~ /$satellite/ ) {
	$mindiff=$diff;
	$closest=$i;
      }
    }
    my $ob;
    if ($closest > -1) {
      $ob=$closest;
      @match_data = 
	  ($ob,
	   ${$storms->{$k}->{LON}        }[$ob],
           ${$storms->{$k}->{LAT}        }[$ob],
 	   ${$storms->{$k}->{TIME1}      }[$ob],
           ${$storms->{$k}->{VAPTIME}    }[$ob],
           ${$storms->{$k}->{STORM_TYPE} }[$ob],
           ${$storms->{$k}->{WINDS}      }[$ob],
           ${$storms->{$k}->{GUSTS} 	 }[$ob],
           ${$storms->{$k}->{PRESS} 	 }[$ob],
           ${$storms->{$k}->{COURSE}	 }[$ob],
           ${$storms->{$k}->{SPEED} 	 }[$ob],
           ${$storms->{$k}->{RPT_TYPE}   }[$ob]);
      @{$self->{STORMS}->{$k}} = \@match_data;
    }

  }
  %{$self->{STORMS}}
}




=pod

=head1 

=head2 


=cut


sub getRegions{
  my $self=shift;
  my @regions = keys %{$self->{DEFAULTS}->{SATELLITE_REGIONS}};
}




=pod

=head1 

=head2 


=cut

sub getSatellites{
  my @Satellites;
}


=pod

=head1 

=head2 


=cut

sub stormsFound{
  my $self=shift;
  my @storms = keys %{$self->{STORMS}};
}


=pod

=head1  processStorm

=head2 Usage: makeOverlay("storm name");

  This routine is creates the overlay of the storm and leaves it in $VAP_TS_OVERLAY

=cut


sub makeOverlay{

  my $self=shift;
  my $storm = shift;
  my $region = $self->{REGION};
  my $wind_delta = $self->{WINDDELTA};

  my ($obnum, $lon, $lat, $time1, $vaptime,
      $storm_type, $winds, $gusts, $press, $course,
      $speed, $rpt_type)  = @{$self->{STORMS}->{$storm}->{CLOSEST}};

  #my $msg= "\n\n --- Storm $storm: $lon, $lat, $time1, $vaptime, $storm_type, $winds, $press\n\n";

  #$self->{ERROROBJ}->Report($msg, "INFO");
  
  my $stormtime=$vaptime;
  my $year = substr($vaptime,0,4);
  my $type = $self->{DEFAULTS}->{STORMTYPES}->{$rpt_type};
  $type = $storm_type =~ /\?+/? 'UNK': $storm_type;

  my $subtitle = 
    "Location: Lon: $lon, Lat: $lat, Time: $stormtime, Sus. Winds: $winds (kts)";

  my $filter = $self->{WINDFILTER} eq 'Q' ? "QS":"SW";
  my $windobj = Winds->new(FILTER => $filter,
			  ERROROBJ=> $self->{ERROROBJ});
  $self->{WINDOBJ} = $windobj;
  
  my ($path,$modidltime,@windfiles)= 
    $windobj -> FindClosestInTimeAndDistance($lon,
					     $lat,
					     5,
					     $stormtime,
					     $self->{WINDDELTA},1);

  $modidltime = $self->{TIME} if (!$modidltime);
  $self->{ERROROBJ}->Report("Time of closest wind data (modidltime): $modidltime\n",
		"INFO");
  my @tmp=split("/",$modidltime);
  my $idltimestring=join("",@tmp[0 .. 4]);
  my $idltmpfilestr;
  my $windfilter =   ($self->{WINDFILTER} eq 'Q')? "Q": "S";
  my $outputname="$region-$type-$storm-$idltimestring" . "-" . $windfilter . ".jpeg";


  my @maplimits=($lon-10,$lat-10,$lon+10,$lat+10);
  my $maplimits="[".join(",",@maplimits)."]";


    ## Determine whether it's a GMS5 or GOES region. If the latter,
    ## find and/or grid the appropriate GOES file, if the former, FTP
    ## the appropriate GMS5 files here.

  if ($region eq 'GMS5') {

      # Make sure out maplimits are acceptable. Only need to check
      # long, since lat is +/-60

    my @limits = @{$self->{DEFAULTS}->{SATELLITE_REGIONS}->{GMS5}->{Region}};
    $maplimits[0] = $maplimits[0] < $limits[0]? $limits[0]:$maplimits[0];
    $maplimits[2] = $maplimits[2] > $limits[2]? $limits[2]:$maplimits[2];
    if (abs($maplimits[0]-$maplimits[2]) < 10) {
      $self->{ERROROBJ}->_croak(
				["Longitude range is too small for",
				 "storm, $storm",
				 "at lon: $lon, lat: $lat",
				 "minlon: $maplimits[0]",
				 "maxlon: $maplimits[2]\n"],
				"OTS: MAPLIMITS PROBLEM!"
			       );
    }
    $maplimits="[".join(",",@maplimits)."]";
    my $gms=OGms5->new( DATETIME => $modidltime, 
		      ERROROBJ => $self->{ERROROBJ},
		      TYPE => 'ir1');
    my ($gms5datetime, $gms5mindiff)= $gms->GetClosest;
    if ($gms5mindiff) {
      if (abs($gms5mindiff) < $wind_delta*3600) {
	my $test=$gms->GetAll($gms5datetime);
	$gms5datetime = undef unless $test;
      }
    }
    $self->{ERROROBJ}->_croak("ERROR: Both GMS cloud data and Qscat data are missing!\n",
		  "OTS: Cloud/Wind data missing!")
      unless ($gms5datetime || @windfiles );

    $self->{ERROROBJ}->Report(["  Wind Files time: $vaptime +/- $wind_delta hours",
		    "  For a stormtime of $stormtime\n"],'INFO');
    if ($gms5datetime){
      $self->{ERROROBJ}->Report("\n  GMS5 time: $gms5datetime\n",'INFO') if $gms5datetime;
      $idltmpfilestr = "tropical_storms_overlay,\'$gms5datetime\'";
    } else {
      $self->{ERROROBJ}->Report("\n  GMS5 time: <no GMS5 data\n",'INFO');
      $idltmpfilestr = "tropical_storms_overlay,''";
    }
    $idltmpfilestr .= ",gmstype=\'ir1\'";

  } elsif ($region eq 'GOESWEST') {

    my $t0=time();
    $self->{ERROROBJ}->Report("Starting GAG at ".localtime($t0),'INFO');
    my $ogoes = OGoes->new(REGION => "GOESWEST", DATETIME => $modidltime, 
			ABSFLAG=>0, 
			LIMITS => \@maplimits);
    my $goesfile=$ogoes->gag;
    my $t1=time();
    $self->{ERROROBJ}->Report("Finished GAG at ".localtime($t1)."\n",'INFO');
    $t0 = $t1-$t0;
    $self->REPORT("GAG took $t0 seconds\n",'INFO');
    $self->{ERROROBJ}->_croak("Both GoesWest data and QuikSCAT data is missing!\n",
		  "Missing Cloud/Wind data!") 
	unless ($goesfile || @windfiles);
    $self->{ERROROBJ}->Report("Goesfile: $goesfile\n",'INFO');
    $idltmpfilestr="tropical_storms_overlay,\'$goesfile\'";

  } elsif ($region eq 'GOESEAST') {

    my $t0=time();
    $self->{ERROROBJ}->Report("Starting GAG at ".localtime($t0),'INFO');
    my $ogoes = OGoes->new("GOESEEST", "4", $modidltime, 0, @maplimits);
    my $goesfile=$ogoes->gag;
    my $t1=time();
    $self->{ERROROBJ}->Report("Finished GAG at ".localtime($t1)."\n",'INFO');
    $t0 = $t1-$t0;
    $self->REPORT("GAG took $t0 seconds\n",'INFO');
    $self->{ERROROBJ}->_croak("Both GoesEest data and QuikSCAT data is missing!\n",
		  "Missing Cloud/Wind data!") 
	unless ($goesfile || @windfiles);
    $self->{ERROROBJ}->Report("Goesfile: $goesfile\n",'INFO');
    $idltmpfilestr="tropical_storms_overlay,\'$goesfile\'";
  } else {
    $self->{ERROROBJ}->_croak(["How the heck did I get here?\n",
		   "region != GMS5 or GOESEAST/GOESWEST",
		   "region = $region"],
		   "OTS: error, undefined region!");
  }

  $idltmpfilestr .= ",crd=[0,0],len=1,maplimits=maplimits";
  $idltmpfilestr .= ",outfile=outfile";

  my $windfiles;
  if (@windfiles) {
    $path .= "/" if $path !~ /.*\/$/;
    my $str="\n";
    $str .= join("\n", @windfiles);
    $str .= "\n";
    $self->{ERROROBJ}->Report("Windfiles to be used: $str", 'INFO');
    $windfiles="\'$path\' + [\'$windfiles[0]\'";

    for (my $i=1;$i<=$#windfiles;$i++){
      $windfiles .= ",\'$windfiles[$i]\'";
    }
    $windfiles .= "]";
    $idltmpfilestr .= ",windfiles=windfiles";
    # "Wind files: $windfiles\n";
  }

  my $oplotstring=makeIDLOplotString($lon, $lat);
  $idltmpfilestr .= ",oplot=oplot";

  
    ## Open the lockfile and put the current unix time in there.  This
    ## is the only way we have of gathering output from the IDL
    ## process. We also use it to communcate that this run is
    ## 'non-interactive' in the sense that IDL is being started by the
    ## perl script. This sort of 'non-interactivty' applies both when
    ## the script is called by the user as well as when it is called
    ## by cron, an ambiguity which I haven't been able to eliminate.

  my $time=timegm(gmtime(time));
   #   chdir $self->{DEFAULTS}->{TS_OVERLAY_DIR}  || 
   #    $self->{ERROROBJ}->_croak("Can't CD to $TS::TS_OVERLAY_DIR",
   # 		 "OTS::CD error!");
  my $lockfile="ts_cronjob.$time.lock";
  open FILE,">$lockfile" || 
   $self->{ERROROBJ}->_croak("Can't open $lockfile",
		 "OTS: Open error on lockfile!");
  print FILE $time;
  close FILE;

  $idltmpfilestr .= ",lockfile=lockfile, title=title, subtitle=subtitle\n";
  $idltmpfilestr .= "exit\n";

    ## Open the tmp file that will be executed by IDL and write the
    ## command string to it 

  my $tmpfile="ts_tmpfile.$time.pro";
  $self->{ERROROBJ}->Report("Tmpfile: $tmpfile\n", 'INFO');

  open TMPFILE, ">$tmpfile" || 
    $self->{ERROROBJ}->_croak("Can't open IDL TMPFILE\n",
		  "OTS: Open error on tmpfile!");

  print TMPFILE "title=\'$storm_type $storm :\'\n";
  print TMPFILE "subtitle=\'$subtitle\'\n";
  print TMPFILE "maplimits=$maplimits\n";
  print TMPFILE "windfiles=$windfiles\n" if $windfiles;
  print TMPFILE "oplot=$oplotstring\n";
  print TMPFILE "outfile=\'$outputname\'\n";
  print TMPFILE  "lockfile=\'$lockfile\'\n";

  print TMPFILE $idltmpfilestr;
  print TMPFILE "exit\n";
  close TMPFILE;

  my $r=system( "$VapUtil::IDLEXE  $tmpfile")/256;
  $self->{ERROROBJ}->_croak("Bad return from system( $IDLEXE $tmpfile)\n",
		"OTS: Bad return from SYSTEM!") if $r != 0;

  unlink $tmpfile || $self->{ERROROBJ}->Report("Couldn't unlink($tmpfile)\n",'INFO');
  open FILE, "<$lockfile" || 
    $self->{ERROROBJ}->_croak("Can't reopen $lockfile\n",
		  "OTS: Failure reopening locfile!");
  my $errcnt = 0;
  while (<FILE>){
    $self->{ERROROBJ}->Report($_,'INFO') && $errcnt++ if /.*ERROR.*/;
  }
  close FILE;

  $self->{ERROROBJ}->_croak("Found $errcnt Errors -- Exiting\n",
		"OTS: ERRCNT > 0!") if $errcnt;
  $self->{ERROROBJ}->_croak("Can't find output file $outputname\n",
		"Can't find OUTPUTNAME") if (! -e $outputname);

  unlink $lockfile || $self->{ERROROBJ}->Report("Couldn't unlink($lockfile)\n",'INFO');

  # return the *fully* qualified output name!
  $self->{OUTPUTNAME} = $outputname = $self->{DEFAULTS}->{TS_OVERLAY_DIR} . "/$outputname";
  $outputname;
}


sub outputname{
  my $self = shift;
  return $self->{OUTPUTNAME};
}

sub getDefs{
  my $self=shift;
  return $self->{DEFAULTS};
}

sub PrettyPrint{
  my $self=shift;
  my $storm=shift;
  my $region=$self->{REGION};

  my @msg=();
  push @msg,"\n\n\t =========== Working on $_ ========\n";


  my @match_data= @{$self->{STORMS}->{$storm}->{CLOSEST}};

  # Match data looks like:
#       @match_data = 
# 	  ($ob,
# 	   $storms->{$k}->{LON},
#          $storms->{$k}->{LAT},
#  	   $storms->{$k}->{TIME1},
#          $storms->{$k}->{VAPTIME},
#          $storms->{$k}->{STORM_TYPE},
#          $storms->{$k}->{WINDS},
#          $storms->{$k}->{GUSTS},
#          $storms->{$k}->{PRESS},
#          $storms->{$k}->{COURSE},
#          $storms->{$k}->{SPEED},
#          $storms->{$k}->{RPT_TYPE})

  my ($lat, $lon) = @match_data[1 .. 2];
  my $time = $match_data[4];
  my $type = $match_data[5];
  my $sustained = $match_data[6];
  my $msg = "$region: $storm, $time, $lat, $lon, $type, $sustained\n";
  push @msg,$msg;
  push @msg,"\n";
  $self->{ERROROBJ}->Report(\@msg,'INFO');

}

sub findCloseEnough{
  my $self=shift;
  my @storms = $self->stormsFound;
  foreach (@storms){
    my @closest = @{$self->{STORMS}->{$_}->{CLOSEST}};
    my $time = $closest[3];
    if ($time < $self->{DECSTARTTIME} ||
	$time > $self->{DECENDTIME} ) {
      delete $self->{STORMS}->{$_};
    }
  }
  @storms = $self->stormsFound;
}
1;
