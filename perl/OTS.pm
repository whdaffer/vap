package OTS;


#
# $Id$
#
#
# Modification Log:
#
# $Log$
# Revision 1.2  2002/12/10 19:57:13  vapdev
# Ongoing work
#
# Revision 1.1  2002/08/08 00:16:47  vapdev
# Initial Revision
#
#



use strict;
use Carp;
use vars qw/$tsoo_defs/;

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

BEGIN {
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

use lib $ENV{VAP_SFTWR_TOP};
use lib $ENV{VAP_LIBRARY};
require "VapWebsite_defs";
use VapUtil;
use VapError;

@OTS::ISA = qw/VapError/;


=pod

=head1 new ([TIME=>yyyy/mm/dd/hh/mm, DELTA => f.g, ABSFLAG => 0|1]);

=head2 Synopsis:

       create an object of type OTS, [Object, Tropical Storm], This
       object will be used in parsing the Tropical storm data file
       http://www.solar.ifa.hawaii.edu/Tropical/Data/tropicalXX (where
       XX = last to digits of the year) and returning information
       about the contents of that file.

       The object limits itself to finding storms in a region around
       TIME, the form and size of this region is determined by the
       values of ABSFLAG and DELTA.

=head2 Arguments

=over 4

=item * TIME : The 'time' we're interested in. Nominally this time,
               along with the two deltas, defines the interval where
               we look for storms and/or winds. It defaults to the
               current time.

=item * ABSFLAG: If set, use the TIME +/- DELTA/2, otherwise, just
                 use TIME-DELTA. Default=0 (i.e. OFF)


=item * DELTA: defines the time window in which we search for
               storms. Default = 8

=back

=cut

sub new {
  my $class = shift;

  my $self={TIME=>VapUtil::systime2idltime(VapUtil::SysNow()),
	    STORM_DELTA => 8,
	    ABSFLAG => 0,
	    @_};
  $self->{SYSTIME} = VapUtil::idltime2systime($self->{TIME});
  bless $self, ref($class) || $class;
  $self->{ERROROBJ} = VapError->new() unless $self->{ERROROBJ};

  my $tropical_storm_defs_file = "tropical_storm_defs_oo";
  my $foo=$ENV{VAP_LIBRARY}. "/tropical_storm_defs_oo";
  require "$tropical_storm_defs_file" ||
    $self->_croak("Can't require $foo\n:$!",
		  "ERROR IN `REQUIRE'");

  $self->{DEFAULTS} = $tsoo_defs; # Load the tropical storm defaults.

  $self->_croak("REGION is REQUIRED\n",
		"NO REGION!\n") unless exists $self->{REGION};
  my @allowed_regions = keys %{$self->{DEFAULTS}->{SATELLITE_REGIONS}};
  $self->_croak( $self->{REGION}. 
		 " is not an allowed region!\nAllowed regions are". 
                 join("\n",@allowed_regions)."\n" ,
		 "DISALLOWED REGION!\n") 
    unless grep($self->{REGION}, @allowed_regions);
  if ($self->{ABSFLAG}) {
    $self->{ENDTIME} = 
      VapUtil::systime2idltime($self->{SYSTIME} + ($self->{DELTA}*3600)/2.);
    $self->{STARTTIME} = 
      VapUtil::systime2idltime($self->{SYSTIME} - ($self->{DELTA}*3600)/2.);
  } else {
    $self->{ENDTIME} = $self->{TIME};
    $self->{STARTTIME} = 
      VapUtil::systime2idltime($self->{SYSTIME} - $self->{DELTA}*3600);

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
      # than three hours away from the current time, re-fetch the
      # remote file.

      if (open FILE,"<$localfile") {
	@in=<FILE>;
	close FILE;
	my @last100=@in[ $#in-99 .. $#in ];
	for (@last100) {
	  last if /^\s+$/;
	  my ($time,$type)=$self->_getTimeAndType($_);
	  next if $type =~ /FOR/;
	  my $lasttime=$time;
	}
	if (!$lasttime) {
	  $fetch=1;
	} else {
	  my $testtime=Vap_Util::systime2decyear($^T);
	  my $diff=($testtime-$lasttime)*365*24;
	  $fetch=1 if ($diff > 3 );
	}
      } else { $fetch=1;}
    } else {$fetch=1;}
  } else {$fetch=1;}

  my $content;
  my @content;

  if ($fetch) {
    my $remote = $self->{DEFAULTS}->{REMOTE_URL};
    $content = get($remote) or 
      $self->_croak("Can't get($remote)\n",
		    "OTS:ERROR GETTING remote TS data file");
    open FILE, ">$localfile" or 
      $self->_croak("Can't open $localfile\n",
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

  my $self=shift
  my @content=@{$self->{FILE_CONTENTS}};

  my $stormtype=shift @_ || "DEP";
  my $stormnum=$stormranking{$stormtype} || 
      $self->_croak(["Unknown Storm type of $stormtype!",
		     "Allowed values are DEP, STO, CYC, HUR, TYP\n",
		     "OTS: unknown stormtype!");
  
  my $starttime=shift @_ || $self->{STARTTIME};
  my $endtime=shift @_ || $self->{ENDTIME};

  $self->{STARTTIME} = $starttime if ($starttime ne $self->{STARTTIME});
  $self->{ENDTIME} = $endtime if ($endtime ne $self->{ENDTIME});
  delete $self->{SYSTIME};

  my ($hour, $min, $vaptime, $time1, $lat, $lon, $name, $yy);

  my %stormranking = %{$self->{DEFAULTS}->{STORMRANKING}};

  for (@content){
    chop;
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
    
    next if $stormranking{$storm_type} < $stormnum;
    next if $rpt_type !~ /ACT/;
    next if $starttime && ($time1 < $starttime);
    next if $endtime && ($time1 > $endtime);


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
    push @{$self->{STORMS}->{$name}->{SATELLITE}}, 
      $self->_whichSatellite($lon, $lat)
  }
  1;
}


=pod

=head1 _getTimeAndType

=head2 Synopsis: $obj->_getTimeAndType(record)

  Return the time and type of the storm for this record.

=cut




sub _getTimeAndType{
  $self->shift;
  shift; # shifts next argument into $_
  chomp;
  my ($year,$month,$day,$hhmm,$tlat,$tlon,$tname,$course,$speed,$press,
   $winds,$gusts,$storm_type,$rpt_type,$time2,$station,$time3) = /^\s*(\d+)\s+(\d+)\s+(\d+)\s+(\d+\.\d+)\s+UT\s+(\d+\.\d\s*[SN])\s+(\d+\.\d\s*[EW])\s+(\w+-\d+)\s+(\d+|\?+)\s+T\s+(\d+|\?+)\s+kt\s+(\d+|\?+)\s+mb\s+(\d+|\?+)\s+(\d+|\?+)\s+kt\s+(\w+|\?+)\s+(\w+|\?+)\s+(\d+\.\d+)\s+(\w+)\s+(\d+\.\d+).*/;

  my ($hour,$min)=split(/\./,$hhmm);
  my $time=Vap_Util::parts2decyear($year,$month,$day,$hour,$min,0);
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
  my %SatelliteRegions = $self->{DEFAULT}->{SATELLITE_REGIONS};

  while (my ($k,$v) = each %SatelliteRegions) {
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
                         getClosestObservation($time [,$absflag]);

      Returns all the information for the closest observation of 

=cut

sub getClosestObservation{
  # Usage @retarray=getClosestObservation(%hash, $time)
  #

  my $self=shift
  my ($ref,%hash,$time,$absflag,
	@obtimes,$mindiff,$ob, $diff,$closest);

  $ref=shift; # reference to hash
  %hash = %{$ref};
  $time=shift;
  $absflag=shift;

  @obtimes=@{$hash{'time1'}};
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

  ($ob,${hash{lon}}[$ob], 
   ${hash{lat} }[$ob], 
    ${hash{time1}}[$ob],
     ${hash{vaptime}}[$ob], 
      ${hash{storm_type}}[$ob], 
       ${hash{winds}}[$ob], 
        ${hash{gusts}}[$ob], 
         ${hash{press}}[$ob], 
          ${hash{course}}[$ob], 
            ${hash{speed}}[$ob], 
             ${hash{rpt_type}}[$ob], 
              ${hash{satellite}}[$ob] );
}



=pod

=head1 

=head2 


=cut

sub getClosestInRegion{

  my ($ref, %hash, $regions, $region, $time, 
	 @retdata, @match_data, $obregion);

  $ref=shift || croak "Need the observations hash!\n";
  %hash=%{$ref};
  $regions=join ",", getRegions();
  $region=shift || croak "Need `region' [$regions]!\n"; 
  $time=shift || croak "Need `time' parameter!\n";

  @retdata = ();
  @match_data=getClosestObservation(\%hash, $time);
  if ($#match_data > 0) {
    $obregion=whichSatellite($match_data[1], $match_data[2] );
    @retdata = @match_data if $region =~/$obregion/;
  }
  @retdata;
}



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
	$sats, $satellite, $time, $ref);
  $self=shift;
  my $research = scalar(@_);
  $satellite=shift $self->{REGION}
  $time=shift || $self->{TIME}
  $absflag=shift || $self->{ABSFLAG};
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
           ${$storms->{$k}->{RPT_TYPE}   }[$ob],
           ${$storms->{$k}->{SATELLITE}  }[$ob] );
      @{$self->{MATCH_DATA}->{$k}} = \@match_data;
    }

  }
  %{$self->{MATCH_DATA}}
}




=pod

=head1 

=head2 


=cut


sub getRegions{
  my @regions = keys %SatelliteRegions;
}




=pod

=head1 

=head2 


=cut

sub getSatellites{
  my @Satellites
}







=pod

=head1 

=head2 


=cut

sub stormsFound{
  my $self=shift;
  my @storms = keys %{$self->{MATCH_DATA}};
}


sub processStorm{

  my $self=shift;
  my $storm = shift;

  my ($obnum, $lon, $lat, $time1, $vaptime,
      $storm_type, $winds, $gusts, $press, $course,
      $speed, $rpt_type, $satellite)  = @{$self->{MATCH_DATA}->{$storm}};

  my $msg= "\n\n --- Storm $storm: $lon, $lat, $time1, $vaptime, $storm_type, $winds, $press\n\n";

  $self->Report($msg, "INFO");
  
  $stormtime=$vaptime
  $stormtype = $stormtypes{$stormtype};
  $type = 'UNK' if /\?+/;

  $subtitle = 
    "Location: Lon: $lon, Lat: $lat, Time: $stormtime, Sus. Winds: $winds (kts)";

  $stormname = $k;

  ($path,$modidltime,@windfiles)= 
    Qs::FindClosestInTimeAndDistance($match[1], 
				     $match[2], 
				     $stormtime,$wind_delta,5, 1);
1;
