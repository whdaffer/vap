#!/bin/perl -W
#
# $Id$
#
# ET.pm 
#
#
#  Module for use with the Earth Today program and the National Air
#  and Space Museum.
#
#  
#  Author: Willam H. Daffer
#
# Modification Log:
#
# $Log$
# Revision 1.3  2002/05/07 20:40:36  vapdev
# Set -w and `use strict' and then fixing bugs. Start trying to standardize
# the methods used.
#
# Revision 1.2  2002/04/30 20:23:22  vapdev
# Modified the 'use lib' statement
#
# Revision 1.1  2001/02/09 18:45:28  vapuser
# Initial revision
#
#
#

package ET;
use strict;
use lib $ENV{VAP_SFTWR_PERL};
use VapUtil;
#use Vapdefs;
use Cwd;
use Time::Local;
BEGIN{
  die "ENV var VAP_OPS_TS_OVERLAY undefined!\n" 
    unless $ENV{VAP_OPS_TS_OVERLAY};
  die "ENV var VAP_OPS_LOGFILES undefined!\n"
    unless $ENV{VAP_OPS_LOGFILES};
}
sub new {

  my $class=shift;
  my $self={};


  $self->{PROCESSING_TOPDIR} = $ENV{VAP_OPS_TS_OVERLAY};
  $self->{PROCESSING_TOPDIR} = "/disk7/vap/earth-today/testbed";
  $self->{PICKUP_TOPDIR} = $self->{PROCESSING_TOPDIR}."/pickup";
  $self->{PID} = $$;
  $self->{FILETIME}=timegm(gmtime($^T));
  $self->{STARTTIME} = $^T;
  $self->{ENDTIME} = 0;
  $self->{STATUS} = 0;
  $self->{DIR} =  "";
  $self->{WINDFILES} = "";
  $self->{INTERPFILE} = "";
  $self->{STARTDIR} = getcwd();
  chdir $self->{PROCESSING_TOPDIR};

  my (@time, $year, $yday, $hour, $min);

  @time = gmtime($^T);
  $year = $time[5]+1900;

    ## The 'yday' field is the delta between the current day and Jan 1. 
    ## It's not the actual Day of the year! (yday for Jan 1 == 0!)

  $yday = $time[7]+1;
  $hour = $time[2];
  $min  = $time[1];
  $self->{DIRNAME} = sprintf("%04d%03d%02d",$year ,$yday,$hour);
  my $logname= sprintf("%04d%03d%02d%02d",$year ,$yday,$hour,$min);
  $self->{DIR} = $self->{PROCESSING_TOPDIR}."/".$self->{DIRNAME};
  $self->{PICKUPDIR} = $self->{PICKUP_TOPDIR}."/".$self->{DIRNAME};
  $self->{LOGFILE} =  $ENV{VAP_OPS_LOGFILES}."/et.$logname.log";
  return   bless $self, ref($class) || $class;
}

sub _croak {
  use Carp;
  my $self=shift;
  my $msg=shift;
  chdir $self->{STARTDIR};
  croak "$msg\n";
  1;
}


sub _carp {
  my $self=shift;
  my $msg=shift;
  chdir $self->{STARTDIR};
  croak "$msg\n";
  1;
}

sub _createDir{
  my $self =shift;
  my $dir = $self->{DIR};
  if (! -e  $dir ) {
    mkdir $dir, 0755 or  $self->_croak( "Can't mkdir <$dir>\n");
  }

    # all of the processing is done in the target directory.
  chdir $dir or $self->_croak( "Can't CHDIR to <$dir>\n");
  1;
}

sub makeAnimation{
  my $self=shift;
  $self->_createDir();
  my $tmpfile=$self->{PROCESSING_TOPDIR}."/tmpfiles/et.".
    $self->{FILETIME}.".".$self->{pid}.".pro";
  my $lockfile=$self->{PROCESSING_TOPDIR}."/tmpfiles/et.".
    $self->{FILETIME}.".".$self->{PID}.".lock";

  open LOCKFILE, ">$lockfile";
  print LOCKFILE $self->{PID}."\n";
  close LOCKFILE ;

  open TMPFILE, ">$tmpfile"  or 
     $self->_croak("Can't open IDL TMPFILE\n<$tmpfile>\n");

  print TMPFILE "lockfile=\'$lockfile\'\n";
  print TMPFILE "makeetanim,lockfile=lockfile,/gif\n";
  print TMPFILE "exit\n";
  close TMPFILE;



  my $r=system("idl $tmpfile\n")/256;
  $self->_croak("Error calling idl <$r>\n") if $r; 
  open LOCKFILE, "<$lockfile";
  my @lines=<LOCKFILE>;
  close LOCKFILE;
  my @errors=grep(/.*ERROR:.*/, @lines);
  if ($#errors >= 0) {
    my $errors = join "\n", @lines;
    $self->_croak("ERRORS in IDL! ($errors)\n");
  } 
  my @elapsedtime=grep(/.*ELAPSED_TIME=.*/,@lines);
  $self->_carp("Can't get Elapsed Time\n") if $#elapsedtime<0;
  my @junk=split /=/, $elapsedtime[0];
  my $elapsed_time=$junk[1];
  chop $elapsed_time;
  $self->{IDL_ELAPSED_TIME} = $elapsed_time;
  
  my @interpfilename = grep(/.*INTERPFILE=.*/,@lines);
  $self->_carp("Can't get name of Interpolated File\n") if $#interpfilename<0;
  @junk=split /=/, $interpfilename[0];
  my $interpfile = $junk[1];
  chop $interpfile; 
  $self->{INTERPFILE}=$interpfile || "<don't know>";

  my @windfiles=grep(/.*WFILES=.*/,@lines);
  my $windfiles=$windfiles[0];
  chop $windfiles;
  @junk=split /=/, $windfiles;
  $windfiles=$junk[1];

  $self->{WINDFILES} = $windfiles;

  unlink $lockfile;
  unlink $tmpfile;
  1;
}

sub moveToPickupLocation{
  use File::Copy;
  my $self=shift;
  my $startcopy = time();
  mkdir($self->{PICKUPDIR}, 0755) or  
    $self->_croak("Can't create ".$self->{PICKUPDIR}."\n");
  opendir DIR, $self->{dIR} or 
    $self->_croak("Can't open ".$self->{dir}."\n");
  my @files= grep !/^\.\.?$/, readdir(DIR);
  closedir DIR;
  $self->_croak("No Files in ".$self->{DIRNAME}."\n") if $#files < 0;
  for (@files) {
    copy $_, $self->{PICKUPDIR}."./$_" or 
	die $self->_croak("Can't copy $_ to ".$self->{PICKUPDIR}."\n");
  }
  $self->{COPY_TOOK} = (time()-$startcopy)/60.0;
  
  1;
}

sub makeQTMovie{
  #!/bin/csh -f
  my $self=shift;
  my $start=time();
  chdir $self->{PICKUPDIR} or $self->_croak("Can't cd to ".$self->{PICKUPDIR}."\n");
  $self->{OUTPUTQT} = $self->{PICKUPDIR}."/qt.mov";
  my $exestring = "dmconvert -f qt -p video,comp=qt_cvid,squal=0.9,tqual=0.9,rate=30 ";
  $exestring .= "-n gwind.0\#\#,start=1,end=60,step=1 gwind.0\#\# ";
  $exestring .= $self->{outputqt}." >/tmp/DMCONVERT.LOG 2>&1";
  my $r=system("$exestring")/256;
  $self->{QT_TOOK} = (time()-$start)/60.0;

  $self->movetoWWW();
  1;
}

sub movetoWWW{
  use Time::Local;

  my $self=shift;
  my $qt=shift || $self->{OUTPUTQT};
  $self->_croak("need QT file") if !$qt;

  my ($size,$atime) = (stat($qt))[7,8];
  $size /= 1000;
  $size = "$size Kb";
  my $ctime = localtime($atime);
  my ($y, $m, $d, @rest) = gmtime($atime-86400);
  $y+=1900;
  $m+=1;
  my $timestring= sprintf("%04d%02d%02d",$y,$m,$d);
  rename "/usr/freeware/apache/share/htdocs/earth-today/qt.mov", 
    "/usr/freeware/apache/share/htdocs/earth-today/old/qt-$timestring.mov" or 
     unlink "/usr/freeware/apache/share/htdocs/earth-today/qt.mov";

  copy $qt, "/usr/freeware/apache/share/htdocs/earth-today/qt.mov";
  chdir "/usr/freeware/apache/share/htdocs/" or 
      $self->_croak("Can't cd to /usr/freeware/apache/share/htdocs");
  open HTML, "<index.html" or 
      $self->_croak("Can't open index.html file");
  my @in=<HTML>;
  close HTML;
  open OUT, ">index.html.new" or 
      $self->_croak("Can't open index.html.new");
  for (@in){
    s/^\s*\(\s*Created:.*/(Created: $ctime,/;
    s/^\s*Size:.*/Size: $size\)/;
    print OUT;
  }
  close OUT;
  rename "index.html", "index.html.old" or 
      $self->_croak("Can't rename index.html");

  rename "index.html.new", "index.html" or 
      $self->_croak("Can't rename index.html.new");

}

sub logit{

  my $self = shift;
  print "In logit\n";
  $self->{ENDTIME} = time();
  $self->{ELAPSED} = ($self->{ENDTIME}-$self->{STARTTIME})/60.;
  open LOGFILE, "> ".$self->{LOGFILE} or 
      $self->_croak("Can't open ".$self->{LOGFILE}.".\n");


  print LOGFILE "-------------------------------------------------------------\n";
  print LOGFILE "interpfile     = ".$self->{INTERPFILE}."\n";
  my $st=localtime($self->{STARTTIME});
  my $et=localtime($self->{ENDTIME});
  print LOGFILE "starttime      = $st\n";
  print LOGFILE "endtime        = $et\n";
  print LOGFILE "elapsedtime    = ".$self->{ELAPSED}."  minutes \n";
  print LOGFILE "idlelapsedtime = ".$self->{IDL_ELAPSED_TIME} ." minutes\n";
  print LOGFILE "status         = ".$self->{STATUS}."\n";
  print LOGFILE "filetime       = ".$self->{FILETIME}."\n";
  print LOGFILE "dir            = ".$self->{DIR}."\n";
  print LOGFILE "logfile        = ".$self->{LOGFILE}."\n";
  print LOGFILE "pid            = ".$self->{PID}."\n";
  print LOGFILE "copy took      = ".$self->{COPY_TOOK}."\n";
  print LOGFILE "QT took        = ".$self->{QT_TOOK}."\n";
  print LOGFILE "QT file        = ".$self->{OUTPUTQT}."\n";
  print LOGFILE "windfiles      = ".$self->{WINDFILES}."\n\n";

#  while (($k,$v) = each %self) {
#    print LOGFILE "$k = $v\n";
# }

  close LOGFILE;
  print "(logit): done!\n";
}

sub cleanDirs{
  my $self=shift;
  my $keeptime=60*60*24*3; # keep stuff for three days.

    ## Since the structure under the 'processing' and the 'pickup' is
    ## the same, we'll read the subdirectories of the processing
    ## directory and then apply the results to both processing and
    ## pickup areas.

  chdir $self->{PROCESSING_TOPDIR} or 
      $self->_croak("Can't cd to ".$self->{PROCESSING_TOPDIR}."\n");
  opendir DIR, "." or 
      $self->_croak("Can't open ".$self->{PROCESSING_TOPDIR}." to read");
  my @files=grep !/^\.\.?$/, readdir(DIR);
  closedir DIR;

  for (@files){

    next unless (/^\d{9}/ && -d $_);

    my ($year, $doy, $hour) = /^(\d{4})(\d{3})(\d{2})$/;
    my ($mday, $mon) = doy2mday_mon( $doy, $year);

      ## The 'doy' field is the delta between the current day and Jan 1. 
      ## It's not the actual Day of the year! (doy for Jan 1 == 0!)
    my $time=timegm( 0, 0, $hour, $mday, $mon-1, $year-1900, 0, $doy-1,0, 0);

    
    if ( ($self->{FILETIME} - $time) > $keeptime) {
      chdir $_ or $self->_croak("Can't cd to $_");
      opendir DIR, "." or $self->_croak("Can't open $_ to read");
      my @files=grep !/^\.\.?$/, readdir(DIR);
      my $f;
      closedir DIR;
      foreach $f (@files){
	unlink $f;
      }
      chdir "..";
      rmdir;

        ## Now go to the processing directory and delete the current
        ## subdirectory.

      chdir $self->{PICKUP_TOPDIR}."/$_" or 
	  $self->_croak("Can't cd to ".$self->{PICKUP_TOPDIR}."/$_\n");
      opendir DIR, "." or $self->_croak("Can't open $_ to read");
      @files=grep !/^\.\.?$/, readdir(DIR);
      closedir DIR;
      foreach $f (@files){
	unlink $f;
      }
      chdir "..";
      rmdir;
      chdir $self->{PROCESSING_TOPDIR} or 
	  $self->_croak("Can't cd to ".$self->{PROCESSING_TOPDIR}."\n");
    }
  }
  
}
sub DESTROY {
  my $self=shift;
  chdir $self->{STARTDIR};
  1;
}


sub dbifyLog{
  use DBI;
  use File::Basename;

  my $self=shift;
  my $log=shift || $self->_croak( "Need name of Log file");
  chdir "logs" or $self->_croak( "Can't CD to ./logs");
  
  open LOG, "<$log" or $self->_croak( "Can't open Log file");
  my @lines=<LOG>;
  close LOG;
  copy $log, "./dbified/$log" or $self->_croak("Can't copy $log to ./dbified");
  unlink $log;
  @lines=@lines[1 .. $#lines];


  my %mhash=(Jan=>1,
	  Feb=>2,
	  Mar=>3,
	  Apr=>4,
	  May=>5,
	  Jun=>6,
	  Jul=>7,
	  Aug=>8,
	  Sep=>9,
	  Oct=>10,
	  Nov=>11,
	  Dec=>12);


  my $dbh=DBI->connect('DBI:mysql:ET','vapuser','vapuser@mysql',
		    {RaiseError=>1, AutoCommit=>1} );
  my %hash=();
  my ($field, $val, $junk);
  for (@lines) {
    next unless /^\s*(.*)\s+=\s+(.*)\s+$/;
    $field=$1;
    $val=$2;
    $field =~ s/\s+//g;
    if (/^interpfile/){
      $val="'$val'";
    } elsif (/^starttime/){
      $field="start";
      my ($dayname,$monname,$mday,$hh,$mm,$ss,$year) = 
	  ($val =~ /^(\w+) (\w+) (\d{2}) (\d{2}):(\d{2}):(\d{2}) (\d{4})/);
      my $mon=$mhash{$monname};
      $val=sprintf("%04d-%02d-%02d %02d:%02d:%02d", 
		   $year, $mon, $mday, $hh, $mm, $ss);
      $val= "'$val'";      

    } elsif(/^endtime/){
      $field="stop";
      my ($day,$monname,$mday,$hh,$mm,$ss,$year) = 
	  ($val =~ /^(\w+) (\w+) (\d{2}) (\d{2}):(\d{2}):(\d{2}) (\d{4})/);
      my $mon=$mhash{$monname};
      $val=sprintf("%04d-%02d-%02d %02d:%02d:%02d", 
		   $year, $mon, $mday, $hh, $mm, $ss)      ;
      $val= "'$val'";
    } elsif(/^elapsedtime/){
      $field="elapsed";
      ($val,$junk)= ($val =~/(\d+\.\d+) (\w+)/);
    } elsif(/^idlelapsedtime/){
      $field="idlelapsed";
      ($val,$junk)= ($val =~/(\d+\.\d+) (\w+)/);
    } elsif(/^status/){
    } elsif(/^filetime/){
    } elsif(/^dir/){
      $field = "dirnum";
      my @tmp=split("/", $val);
      $val=$tmp[$#tmp];
    } elsif(/^logfile/){
      $field="log";
      $val = "'$val'";
    } elsif(/^pid/){
    } elsif(/^copy took/){
    } elsif(/^windfiles/){
      my @tmp=split /,/, $val;
      my $windpath="";
      my $windfiles="";
      use File::Basename;
      for (@tmp) {
	my ($name,$path,$suffix) = fileparse($_);
	$windpath .= $path if ($windpath ne $path);
	$windfiles .= "$name,";
      }
      $windfiles =~ s/(.*),/$1/;
      $hash{windpath} = "'$windpath'";
      $val="'$windfiles'";
    } else {
      next;
    }

    $hash{$field}=$val;

  }  
  my @processing= ("dirnum", "start", "stop", "elapsed", "status");
  my @files=("dirnum", "interpfile", "log", "windfiles", "windpath");


    ## Load the PROCESSING table
  my $cols="";
  my $vals="";
  for (@processing){
    $cols .= "$_,";
    $vals .= "$hash{$_},";
  }
  $cols =~ s/(.*),$/$1/;
  $vals =~ s/(.*),$/$1/;
  my $sql = "INSERT INTO processing ($cols) VALUES ($vals)";
  $dbh->do($sql);


    ## Load the FILES table
  $cols="";
  $vals="";
  for (@files){
    $cols .= "$_,";
    $vals .= "$hash{$_},";
  }
  $cols =~ s/(.*),$/$1/;
  $vals =~ s/(.*),$/$1/;
  $sql = "INSERT INTO files ($cols) VALUES ($vals)";
  $dbh->do($sql);

  $dbh->disconnect;
}

sub dbit{
  use DBI;
  use File::Basename;
  use Time::Local;
  my $self = shift;

  $self->logit();

  print "In dbit\n";
  $self->{ENDTIME} = time();
  $self->{ELAPSED} = ($self->{ENDTIME}-$self->{STARTTIME})/60.;


  my $dbh=DBI->connect('DBI:mysql:ET','vapuser','vapuser@mysql',
		    {RaiseError=>1, AutoCommit=>1} );

  my ($ss,$mm,$hh,$mday,$mon,$year,@junk)=localtime($self->{STARTTIME});
  $year += 1900;
  $mon += 1;
  my $st=sprintf("%04d-%02d-%02d %02d:%02d:%02d", 
		   $year, $mon, $mday, $hh, $mm, $ss)      ;

  ($ss,$mm,$hh,$mday,$mon,$year,@junk)=localtime($self->{ENDTIME});
  $year += 1900;
  $mon += 1;
  my $et=sprintf("%04d-%02d-%02d %02d:%02d:%02d", 
		   $year, $mon, $mday, $hh, $mm, $ss)      ;

  my @tmp=split("/", $self->{DIR});
  my $dirnum=$tmp[$#tmp];


    ## create the two SQL statements and load these results into the
    ## database

    ## the 'processing' table
  my $sql="INSERT INTO processing (dirnum, start, stop, elapsed, status) ";
  $sql .= "VALUES ($dirnum, '$st', '$et', ";
  $sql .= $self->{ELAPSED}.",". $self->{STATUS}.")";
  $dbh->do($sql);
  
  @tmp=split /,/, $self->{windfiles};

  my $windpath="";
  my $windfiles="";
  for (@tmp) {
    my ($name,$path,$suffix) = fileparse($_);
    $windpath .= $path if ($windpath ne $path);
    $windfiles .= "$name,";
  }
  $windfiles =~ s/(.*),$/$1/;

    ## the 'files' table
  $sql  ="INSERT INTO files (dirnum, interpfile, log, windfiles, windpath) ";
  $sql.= "VALUES( $dirnum, ";
  $sql.= "'".$self->{INTERPFILE}."', '";
  $sql.= $self->{LOGFILE}."', '$windfiles', '$windpath')";
  $dbh->do($sql);

  $dbh->disconnect;
  print "done at ".scalar(localtime(time))."!\n";
}

1;
  

