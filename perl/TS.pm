#!/bin/perl -w
#
#
# $Id$
#
# This module provides code for retrieving and parsing the list of
# tropical depressions/storms/{hurricanes,typhoons,cyclones}
# maintained at www.soloar.ifa.hawaii.edu.
#
# Currently, this file is
#
#  http://www.solar.ifa.hawaii.edu/Tropical/Data/tropicalXX 
#
#  where XX is the two digit year.
#
# But I will be putting this into a config file to be 'required' by
# this module
#
# Modification Log:
#
# $Log$
# Revision 1.1  2001/02/09 19:07:16  vapuser
# Initial revision
#
#
#
package TS;
require Exporter;
@ISA=qw(Exporter);
@EXPORT=qw(getTropicalStormsDatafile parseTropicalStorms getClosestObservation 
	   whichSatellite getClosestInRegion getSatelliteInfo 
	   getRegions getSatellites getClosestBySatellite);


  # for debugging purposes

use Carp;
use LWP::Simple;
use lib $ENV{VAP_SFTWR_PERL};
use VapUtil;
#use Gms5;

BEGIN {
  $TS::VAP_LIBRARY=$ENV{'VAP_LIBRARY'}  || die "VAP_LIBRARY is UNDEFINED!\n";
  $TS::defs_file=$ENV{'TS_DEFS_FILE'} || 
      "$VAP_LIBRARY/tropical_storm_defs";
  require $defs_file || die "Can't require $defs_file!\n";
  
  %storms=();
  1;
};

sub getTropicalStormsDatafile {

  #use vap_util;

  my $localfile="$VAP_LIBRARY/$LOCAL_URL";
  my $fetch=0;
  my @in=();
  if (-e $localfile ) {

    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,
	  $size,$atime,$mtime,$ctime,$junk)=stat($localfile);
    my $lasttime;

    if ($^T-$ctime < 3*3600. ) {
      if (open FILE,"<$localfile") {
	@in=<FILE>;
	close FILE;
	my @last100=@in[ $#in-99 .. $#in ];
	for (@last100) {
	  last if /^\s+$/;
	  my ($time,$type)=getTimeAndType($_);
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
    $content = get($REMOTE_URL) || die "Can't get($REMOTE_URL)\n";
    open FILE, ">$localfile" || die "Can't open $localfile\n";
    print FILE $content;
    close FILE;
    @content=split /\n/, $content;
  }  else {
    @content=@in;
  }
  
  @content;
}

sub parseTropicalStorms{ 
# A typical record might look like:
# 2000 1 05 18.00 UT 12.2 S 82.8 E BABIOLA-00 235 T 09 kt ??? mb 035 045 kt CYC ACT 2000.01571038251 WTXS31 2000.01642190346

  my $ref=shift; # Passed a reference to an array.

  my @content=@{$ref};

  my $stormtype=shift @_ || "DEP";
  my $stormnum=$stormranking{$stormtype} || 
      die "Unknown Storm type of $stormtype!\nAllowed values are DEP, STO, CYC, HUR, TYP\n";
  
  my $starttime=shift @_;
  my $endtime=shift @_;

  my ($hour, $min, $vaptime, $time1, $lat, $lon, $name, $yy);

  for (@content){
    chop;
    my ($year,$month,$day,$hhmm,$tlat,$tlon,$tname,$course,$speed,$press,
     $winds,$gusts,$storm_type,$rpt_type,$time2,$station,$time3) = /^\s*(\d+)\s+(\d+)\s+(\d+)\s+(\d+\.\d+)\s+UT\s+(\d+\.\d\s*[SN])\s+(\d+\.\d\s*[EW])\s+(\w+-\d+)\s+(\d+|\?+)\s+T\s+(\d+|\?+)\s+kt\s+(\d+|\?+)\s+mb\s+(\d+|\?+)\s+(\d+|\?+)\s+kt\s+(\w+|\?+)\s+(\w+|\?+)\s+(\d+\.\d+)\s+(\w+)\s+(\d+\.\d+).*/;
    ($hour,$min)=split(/\./,$hhmm);
    $vaptime=sprintf("%04d/%02d/%02d/%02d/%02d",$year,$month,$day,$hour,$min);
    $time1=Vap_Util::parts2decyear($year,$month,$day,$hour,$min,0);
    ($lat,$type) = $tlat=~/(\d+\.\d)\s*([SN])/;
    $lat *= -1 if $type=~/S/;
    ($lon,$type)=$tlon=~/(\d+\.\d)\s*([EW])/;
    $lon = 360-$lon  if $type=~/W/;
    ($name,$yy)=$tname=~/(\w+)-(\d+)/;

    next if $stormranking{$storm_type} < $stormnum;
    next if $rpt_type !~ /ACT/;
    if ($starttime) {
      next if $time1 < $starttime;
    }
    if ($endtime) {
      next if ($time1 > $endtime);
    }
    push @{$storms{$name}{vaptime}},    $vaptime;    
    push @{$storms{$name}{time1}},      $time1;    
    push @{$storms{$name}{ lat}},    	 $lat;        
    push @{$storms{$name}{ lon}},    	 $lon;        
    push @{$storms{$name}{ course}}, 	 $course;     
    push @{$storms{$name}{ speed}},  	 $speed;      
    push @{$storms{$name}{ press}},  	 $press;      
    push @{$storms{$name}{ winds}},  	 $winds;      
    push @{$storms{$name}{ gusts}},  	 $gusts;      
    push @{$storms{$name}{ storm_type}},$storm_type; 
    push @{$storms{$name}{ rpt_type}},	 $rpt_type;   
    push @{$storms{$name}{ time2}},	 $time2;      
    push @{$storms{$name}{ time3}},	 $time3;      
    push @{$storms{$name}{satellite}},	 whichSatellite($lon, $lat)
  }
  %storms;
}





sub getTimeAndType{
  shift;
  chomp;
  my ($year,$month,$day,$hhmm,$tlat,$tlon,$tname,$course,$speed,$press,
   $winds,$gusts,$storm_type,$rpt_type,$time2,$station,$time3) = /^\s*(\d+)\s+(\d+)\s+(\d+)\s+(\d+\.\d+)\s+UT\s+(\d+\.\d\s*[SN])\s+(\d+\.\d\s*[EW])\s+(\w+-\d+)\s+(\d+|\?+)\s+T\s+(\d+|\?+)\s+kt\s+(\d+|\?+)\s+mb\s+(\d+|\?+)\s+(\d+|\?+)\s+kt\s+(\w+|\?+)\s+(\w+|\?+)\s+(\d+\.\d+)\s+(\w+)\s+(\d+\.\d+).*/;

  my ($hour,$min)=split(/\./,$hhmm);
  my $time=Vap_Util::parts2decyear($year,$month,$day,$hour,$min,0);
  ($time, $rpt_type);
}


sub getSatelliteInfo{

  my $lon=shift || carp "Need a longitude here!\n";
  my $lat=shift || carp "Need a latitude here!\n";
  my %info=();

  foreach my $k (keys %SatelliteRegions) {
    my @region=@{$SatelliteRegions{$k}{Region}};
    if ( $lon >= $region[0] && $lat>= $region[1] && 
	$lon <= $region[2] && $lat<= $region[3]) {
      %info = $SatelliteRegions{$k}{Info};
      last;
    }
  }
  %info;
}

sub whichSatellite{

  my $lon=shift || croak "Need a longitude here!\n";
  my $lat=shift || croak "Need a latitude here!\n";

  my $satellite;

  foreach my $k (keys %SatelliteRegions) {
    my @region=@{$SatelliteRegions{$k}{Region}};
    if ( $lon >= $region[0] && $lat>= $region[1] && 
	$lon <= $region[2] && $lat<= $region[3]) {
      $satellite = $k;
      last;
    }
  }
  $satellite;
}


sub getClosestObservation{
  # Usage @retarray=getClosestObservation(%hash, $time)
  #

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

sub getClosestBySatellite{

  my (@match_data, %match_data, $k, %hash, 
	$sats, $satellite, $time, $ref);

  $ref=shift || croak "Need the observations hash!\n";
  %hash=%{$ref};
  $sats = join ",",getSatellites;
  $satellite=shift || croak "Need `satellite' [$sats]\n";
  $time=shift || croak "Need `time' parameter!\n";
  $absflag=shift || 0;

  foreach my $k (keys %hash) {
    my $closest=-1;
    my @time=@{$hash{$k}{time1}};
    my @sat=@{$hash{$k}{satellite}};
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
	   ${$hash{$k}{lon}        }[$ob], 
           ${$hash{$k}{lat}        }[$ob], 
 	   ${$hash{$k}{time1}      }[$ob],
           ${$hash{$k}{vaptime}    }[$ob], 
           ${$hash{$k}{storm_type} }[$ob], 
           ${$hash{$k}{winds}      }[$ob], 
           ${$hash{$k}{gusts} 	   }[$ob], 
           ${$hash{$k}{press} 	   }[$ob], 
           ${$hash{$k}{course}	   }[$ob], 
           ${$hash{$k}{speed} 	   }[$ob], 
           ${$hash{$k}{rpt_type}   }[$ob], 
           ${$hash{$k}{satellite}  }[$ob] );
      @{$match_data{$k}{match_data}} = @match_data;
    }

  }
  %match_data;
}


sub getRegions{
  my @regions = keys %SatelliteRegions;
}


sub getSatellites{
  my @Satellites
}


# sub oogleWWW{
#   open HTML, "$VAP_WWW_TOP/storms/index.html" || 
#       die "Can't open storms/index.html\n";
#   my @lines=<HTML>;
#   close HTML;
#   @lines
# }

1;
