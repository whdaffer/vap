#!/usr/bin/perl
#
# $Id$
#
# Qs.pm
#
# Perl Module for finding and manipulating 
# Qscat Wind data files.
#
#  For use with the TS and ET module and any other module which needs
#  to be able to find Qscat wind files or get their times.
#
# Author: William H. Daffer
#
# Modification Log:
#
# $Log$
# Revision 1.1  2001/02/09 18:52:57  vapuser
# Initial revision
#
#
#
package Qs;

use lib getenv('VAP_SFTWR_TOP')."/vap/perl";
use Cwd 'chdir', 'getcwd';
use Time::Local;
use Net::FTP;
use Carp;
use File::Basename;
use VapUtil;
use Vapdefs;

@ISA = qw(Exporter);
@EXPORT=qw(getWindFiles getFileTimes);


#BEGIN {

#   $VAP_LIB=$ENV{'VAP_LIB'}   || "/usr/people/vapuser/Qscat/Library";

#     # Get the Overlay Defaults
#   $overlay_defs_file=$VAP_LIB."/overlay_defs";
#   require $overlay_defs_file;

#     # Get generic VAP processing Defaults
#   $vap_defs_file=$VAP_LIB."/vap_defs";
#   require $vap_defs_file;

#     # Check for interactivity.
#   $_is_batch = !defined($ENV{'TERM'});

#}


sub getWindFiles{

  local ($startime, $endtime, @files, $st, $et, @in_range);

  # usage: @files=getWindFiles(yyyy/mm/dd/hh/mm, yyyy/mm/dd/hh/mm)
  if (!$_[0]) {
    $starttime = timegm(gmtime(time()-3*3600));
  } else {
    $starttime = idltime2systime($_[0]);
  }
  if (!$_[1]) {
    $endtime = timegm(gmtime(time()));
  } else {
    $endtime = idltime2systime($_[1]);
  }

  ($path,@files)=getFileList();
  croak "Can't get file list!\n" unless $#files> -1;
  for (@files) {
    ($st,$et) = getFileTimes($_);
    push @in_range, $_ unless ($st > $endtime || $et < $starttime);
  }  
  ($path,@in_range);
  
}


sub getFileList{
  local($dir,@files);
  $dir=shift || "$VAP_WINDS";
  opendir DIR, "$dir" || croak "Can't open directory $dir\n";
  @files=grep /^QS\d+\.S\d+\.E\d+/, readdir(DIR);
  closedir DIR;
  @files = sort @files;
  ($dir,@files);
}

sub getFileTimes{
  local($file,$name,$path,$year,$month,$day,$hour,
	$min,$start,$end,$starttime,$endtime);

  $file=shift || 
      croak "Param 1 </path/to/file/QSYYYYMMDD.SHHMM.EHHMM> is REQUIRED!\n";
  ($name,$path) = fileparse($file);
  ($year,$month,$day,$start,$end) = 
      $name =~ /^QS(\d{4,4})(\d{2,2})(\d{2,2})\.S(\d+)\.E(\d+)$/;
  
  ($hour,$min) = $start =~ /(\d{2,2})(\d{2,2})/;
  $starttime=timegm(0, $min, $hour, $day, $month-1, $year-1900);
  ($hour,$min) = $end =~ /(\d{2,2})(\d{2,2})/;  
  $endtime=timegm(0, $min, $hour, $day, $month-1, $year-1900);
  $endtime += 24*3600 if $endtime < $starttime;
  ($starttime, $endtime);
}

sub FindClosestInTimeAndDistance{
  
  local($lon, $lat, $time, $starttime, $endtime, $tolerance, $ofile, $delflag,
	$randomtag, $idltmpfile, $ofile, $t0, $time_delta, 
	$year, $month, $day, $hour, $min, $k, $kk, $v, $t, @t, 
	$name, $value, $path);
  local(%hash) = ();
  
  $lon=shift || croak "Param 1 <LON> is required\n";
  $lat=shift || croak "Param 2 <LAT> is required\n";
  $time=shift || croak "Param 3 <TIME> is required\n";
  $time_delta=shift || 2;
  $tolerance=shift || 5;
  $delflag = shift;
  $randomtag= makeRandomTag();
  $ofile= shift || "$VAP_ROOT/tmpfiles/nearto.$randomtag.dat";
  $idltmpfile="$VAP_ROOT/tmpfiles/runnearto.$randomtag.pro";
  
  ($year,$month,$day,$hour,$min) = split /\//, $time;
  $t0=timegm(0,$min,$hour,$day,$month-1,$year-1900);
  $starttime=systime2idltime($t0-$time_delta*3600);
  $endtime=systime2idltime($t0+$time_delta*3600);
  
  
  open IDLTMPFILE, ">$idltmpfile" || die "Can't open $idltmpfile\n";
  print IDLTMPFILE "lon=$lon\n";
  print IDLTMPFILE "lat = $lat\n";
  print IDLTMPFILE "starttime = \'$starttime\'\n";
  print IDLTMPFILE "endtime = \'$endtime\'\n";
  print IDLTMPFILE "tolerance = $tolerance\n";
  print IDLTMPFILE "ofile=\'$ofile\'\n";
  print IDLTMPFILE "ret=nearto(lon,lat,starttime,endtime,ofile=ofile)\n";
  print IDLTMPFILE "exit\n";
  close IDLTMPFILE;
  
  $r=system( "$Qs::IDLEXE $idltmpfile")/256;
  die "Bad return from system( $IDLEXE $tmpfile)\n" if $r != 0;
  
  unlink $idltmpfile || warn "Couldn't unlink($idltmpfile)\n";
  die "Can't find $ofile!\n" if (! -e $ofile) ;
  
  open OFILE, "<$ofile" || croak"Can't reopen $ofile\n";
  $first=1;
  while (<OFILE>){
    chop;
    last if /^-+\s+ERROR\s+-+.*$/;
    ($k,$v) = split /:/;
    $k =~ s/\s+//g;
    $v =~ s/\s+//g;
    
      # Gonna use a little trick here. The format of this file is:


#      FILE    : /disk5/winds/qscat/Rnoaa/QS20000911.S1139.E1340
#      ROWTIME : 2000/09/11/13/37
#      LOCATION:  234.89    30.91
#      DISTANCE:    0.13
#      INSWATH:        1
#      ------------------------ 
#      FILE    : /disk5/winds/qscat/Rnoaa/QS20000911.S1300.E1520
#      ROWTIME : 2000/09/11/13/37
#      LOCATION:  234.91    30.90
#      DISTANCE:    0.13
#      INSWATH:        1
#      ------------------------ 


    # This code will split on the ':' and then assign $$k = $v.
    # but $k will be one of the keywords 
    # FILE, ROWTIME, LOCATION, DISTANCE OR INSWATH so the effect is to assign
    # $FILE = $v or $ROWTIME = $v...
    # these are then put into the hash using $FILE as the key.
    #
    # Pretty slick, eh?

    if (!$v) {
      if (!$first){
	$hash{$FILE}{rowtime}  = $ROWTIME;
	$hash{$FILE}{location} = $LOCATION;
	$hash{$FILE}{distance} = $DISTANCE;
	$hash{$FILE}{inswath}  = $INSWATH;
      }
    } else {
      (($v,$path, $suffix) = fileparse($v)) if /^FILE.*:.*/;
      $$k = $v;
    }
    $first=0;
  }
  close OFILE;

  unlink $ofile if $delflag;


  @keys=keys %hash;
  if ($#keys==0) {
    @files=Bracket($path, $keys[0]);
    @ret=($path, $hash{$keys[0]}{rowtime}, @files);
  } elsif ($#keys > 0) {

    foreach $key (keys %hash){
      ($year,$month,$day,$hour,$min)=split "/", $hash{$key}{rowtime};
      push @times, 
	join("|", $key,timegm( 0, $min, $hour, $day, $month-1, $year-1900));
    }

    @times = sort @times;

    foreach $f (@times){
      ($file,$t)=split(/\|/, $f);
      push @files, $file;
      push @t, $t;
    }

    @files=Bracket($path, @files);
    $time=systime2idltime(($t[0]+$t[$#t])/2);
    @ret=($path, $time, @files);

  }
  @ret;
    
}

sub Bracket{
    

    # given the input path and files, find the one before the first in
    # the list and the one after the last.
    # requires that getFileList returns the files sorted.

  local( $name, $path, @files, @keepfiles, $i, $first, $last);
  local($path) = shift || croak "Param 1, the path, I need it!\n";
  @keepfiles = @_;
  $first = shift || croak "Param 2, The FIRST file, I need it!\n";

  $last = shift || $first;

  ($path, @files) = getFileList($path);

  $i=-1;
  for (@files) {
    if (/$first/ && $i){
      push @keepfiles, $files[$i] ;
    }
    if (/$last/ && $i+2 <= $#files) {
      push @keepfiles, $files[$i+2];
      last;
    }
    $i++;
  }
  @keepfiles = sort @keepfiles;
     
}

sub getBefore{
  @files = BrackFile(@_);
  $ret=$files[0];
}

sub getAfter{
  @files = BrackFile(@_);
  $ret=$files[1];
}

sub bydate{
  @aa=split(/\|/, $a);
  @bb=split(/\|/, $b);
  $bb[1] <=> $aa[1];
}
1;

