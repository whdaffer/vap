#!/usr/bin/perl
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
# Revision 1.1  2001/02/09 18:45:28  vapuser
# Initial revision
#
#
#
use lib $ENV{'VAP_SFTWR_TOP'}."/vap/perl";
package ET;
use VapUtil;
use Vapdefs;
use Cwd;

sub new {

  my $class=shift;
  $self={};
  bless $self, $class;
  $self->_init();
  return $self;
}

sub _init{
  use Time::Local;

  my $self=shift;
  my $class=shift;
  $self{processing_topdir} = "$VAP_ROOT/earth-today";
  $self{processing_topdir} = "/disk7/vap/earth-today/testbed";
  $self{pickup_topdir} = "/disk7/vap/earth-today/testbed/pickup";
  $self{pid} = $$;
  $self{filetime}=timegm(gmtime($^T));
  $self{starttime} = $^T;
  $self{endtime} = 0;
  $self{status} = 0;
  $self{dir} =  "";
  $self{windfiles} = "";
  $self{interpfile} = "";
  $self{startdir} = getcwd();
  chdir $self{processing_topdir};

  my (@time, $year, $yday, $hour, $min);

  @time = gmtime($^T);
  $year = $time[5]+1900;

    ## The 'yday' field is the delta between the current day and Jan 1. 
    ## It's not the actual Day of the year! (yday for Jan 1 == 0!)

  $yday = $time[7]+1;
  $hour = $time[2];
  $min  = $time[1];
  $self{dirname} = sprintf("%04d%03d%02d",$year ,$yday,$hour);
  my $logname= sprintf("%04d%03d%02d%02d",$year ,$yday,$hour,$min);
  $self{dir} = "$self{processing_topdir}/$self{dirname}";
  $self{pickupdir} = "$self{pickup_topdir}/$self{dirname}";
  $self{logfile} =  "$VAP_ROOT/logfiles/et.$logname.log";
  $self{logfile} =  "/disk7/vap/earth-today/testbed/logs/et.$logname.log";
  1;
}

sub _croak {
  use Carp;
  my $self=shift;
  my $msg=shift;
  chdir $self{startdir};
  croak $msg;
  1;
}


sub _carp {
  my $self=shift;
  my $msg=shift;
  chdir $self{startdir};
  croak $msg;
  1;
}

sub _createDir{
  my $self =shift;
  my $dir = $self{dir};
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
  my $tmpfile="$self{processing_topdir}/tmpfiles/et.$self{filetime}.$self{pid}.pro";
  my $lockfile="$self{processing_topdir}/tmpfiles/et.$self{filetime}.$self{pid}.lock";

  open LOCKFILE, ">$lockfile";
  print LOCKFILE "$self{pid}\n";
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
  $elapsed_time=$junk[1];
  chop $elapsed_time;
  $self{idl_elapsed_time} = $elapsed_time;
  
  my @interpfilename = grep(/.*INTERPFILE=.*/,@lines);
  $self->_carp("Can't get name of Interpolated File\n") if $#interpfilename<0;
  @junk=split /=/, $interpfilename[0];
  my $interpfile = $junk[1];
  chop $interpfile; 
  $self{interpfile}=$interpfile || "<don't know>";

  my @windfiles=grep(/.*WFILES=.*/,@lines);
  my $windfiles=$windfiles[0];
  chop $windfiles;
  @junk=split /=/, $windfiles;
  $windfiles=$junk[1];

  $self{windfiles} = $windfiles;

  unlink $lockfile;
  unlink $tmpfile;
  1;
}

sub moveToPickupLocation{
  use File::Copy;
  my $self=shift;
  $startcopy = time();
  mkdir($self{pickupdir}, 0755) or  $self->_croak("Can't create $self{pickupdir}\n");
  opendir DIR, $self{dir} or $self->_croak("Can't open $self{dir}\n");
  @files= grep !/^\.\.?$/, readdir(DIR);
  closedir DIR;
  $self->_croak("No Files in $self{dirname}\n") if $#files < 0;
  for (@files) {
    copy $_, "$self{pickupdir}/$_" or 
	die $self->_croak("Can't copy $_ to $self{pickupdir}\n");
  }
  $self{copy_took} = (time()-$startcopy)/60.0;
  
  1;
}

sub makeQTMovie{
  #!/bin/csh -f
  my $self=shift;
  my $start=time();
  chdir $self{pickupdir} or $self->_croak("Can't cd to $self{pickupdir}");
  $self{outputqt} = "$self{pickupdir}/qt.mov";
  $r=system("dmconvert -f qt -p video,comp=qt_cvid,squal=0.9,tqual=0.9,rate=30 -n gwind.0\#\#,start=1,end=60,step=1 gwind.0\#\# $self{outputqt} >/tmp/DMCONVERT.LOG 2>&1")/256;
  $self{qt_took} = (time()-$start)/60.0;

  $self->movetoWWW();
  1;
}

sub movetoWWW{
  use Time::Local;

  my $self=shift;
  my $qt=shift || $self{outputqt};
  $self->_croak("need QT file") if !$qt;

  my ($size,$atime) = (stat($qt))[7,8];
  my $size /= 1000;
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
  @in=<HTML>;
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
  $self{endtime} = time();
  $self{elapsed} = ($self{endtime}-$self{starttime})/60.;
  open LOGFILE, "> $self{logfile}" or 
      $self->_croak("Can't open $self{logfile}\n");


  print LOGFILE "-------------------------------------------------------------\n";
  print LOGFILE "interpfile     = $self{interpfile}\n";
  $st=localtime($self{starttime});
  $et=localtime($self{endtime});
  print LOGFILE "starttime      = $st\n";
  print LOGFILE "endtime        = $et\n";
  print LOGFILE "elapsedtime    = $self{elapsed} minutes \n";
  print LOGFILE "idlelapsedtime = $self{idl_elapsed_time} minutes\n";
  print LOGFILE "status         = $self{status}\n";
  print LOGFILE "filetime       = $self{filetime}\n";
  print LOGFILE "dir            = $self{dir}\n";
  print LOGFILE "logfile        = $self{logfile}\n";
  print LOGFILE "pid            = $self{pid}\n";
  print LOGFILE "copy took      = $self{copy_took}\n";
  print LOGFILE "QT took        = $self{qt_took}\n";
  print LOGFILE "QT file        = $self{outputqt}\n";
  print LOGFILE "windfiles      = $self{windfiles}\n\n";

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

  chdir $self{processing_topdir} or 
      $self->_croak("Can't cd to $self{processing_topdir}");
  opendir DIR, "." or 
      $self->_croak("Can't open $self{processing_topdir} to read");
  @files=grep !/^\.\.?$/, readdir(DIR);
  closedir DIR;

  for (@files){

    next unless (/^\d{9}/ && -d $_);

    my ($year, $doy, $hour) = /^(\d{4})(\d{3})(\d{2})$/;
    my ($mday, $mon) = doy2mday_mon( $doy, $year);

      ## The 'doy' field is the delta between the current day and Jan 1. 
      ## It's not the actual Day of the year! (doy for Jan 1 == 0!)
    my $time=timegm( 0, 0, $hour, $mday, $mon-1, $year-1900, 0, $doy-1,0, 0);

    
    if ( ($self{filetime} - $time) > $keeptime) {
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

      chdir "$self{pickup_topdir}/$_" or 
	  $self->_croak("Can't cd to $self{pickup_topdir}/$_");
      opendir DIR, "." or $self->_croak("Can't open $_ to read");
      my @files=grep !/^\.\.?$/, readdir(DIR);
      my $f;
      closedir DIR;
      foreach $f (@files){
	unlink $f;
      }
      chdir "..";
      rmdir;
      chdir $self{processing_topdir} or 
	  $self->_croak("Can't cd to $self{processing_topdir}");
    }
  }
  
}
sub DESTROY {
  chdir $self{startdir};
  1;
}


sub dbifyLog{
  use DBI;
  use File::Basename;

  my $self=shift;
  my $log=shift || $self->_croak( "Need name of Log file");
  chdir "logs" or $self->_croak( "Can't CD to ./logs");
  
  open LOG, "<$log" or $self->_croak( "Can't open Log file");
  @lines=<LOG>;
  close LOG;
  copy $log, "./dbified/$log" or $self->_croak("Can't copy $log to ./dbified");
  unlink $log;
  @lines=@lines[1 .. $#lines];


  %mhash=(Jan=>1,
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


  $dbh=DBI->connect('DBI:mysql:ET','vapuser','vapuser@mysql',
		    {RaiseError=>1, AutoCommit=>1} );
  %hash=();
  for (@lines) {
    next unless /^\s*(.*)\s+=\s+(.*)\s+$/;
    $field=$1;
    $val=$2;
    $field =~ s/\s+//g;
    if (/^interpfile/){
      $val="'$val'";
    } elsif (/^starttime/){
      $field="start";
      ($dayname,$monname,$mday,$hh,$mm,$ss,$year) = 
	  ($val =~ /^(\w+) (\w+) (\d{2}) (\d{2}):(\d{2}):(\d{2}) (\d{4})/);
      $mon=$mhash{$monname};
      $val=sprintf("%04d-%02d-%02d %02d:%02d:%02d", 
		   $year, $mon, $mday, $hh, $mm, $ss);
      $val= "'$val'";      

    } elsif(/^endtime/){
      $field="stop";
      ($day,$monname,$mday,$hh,$mm,$ss,$year) = 
	  ($val =~ /^(\w+) (\w+) (\d{2}) (\d{2}):(\d{2}):(\d{2}) (\d{4})/);
      $mon=$mhash{$monname};
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
      @tmp=split("/", $val);
      $val=$tmp[$#tmp];
    } elsif(/^logfile/){
      $field="log";
      $val = "'$val'";
    } elsif(/^pid/){
    } elsif(/^copy took/){
    } elsif(/^windfiles/){
      @tmp=split /,/, $val;
      $windpath="";
      $windfiles="";
      use File::Basename;
      for (@tmp) {
	($name,$path,$suffix) = fileparse($_);
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
  @processing= ("dirnum", "start", "stop", "elapsed", "status");
  @files=("dirnum", "interpfile", "log", "windfiles", "windpath");


    ## Load the PROCESSING table
  $cols="";
  $vals="";
  for (@processing){
    $cols .= "$_,";
    $vals .= "$hash{$_},";
  }
  $cols =~ s/(.*),$/$1/;
  $vals =~ s/(.*),$/$1/;
  $sql = "INSERT INTO processing ($cols) VALUES ($vals)";
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

  $self->logit();

  my $self = shift;
  print "In dbit\n";
  $self{endtime} = time();
  $self{elapsed} = ($self{endtime}-$self{starttime})/60.;


  $dbh=DBI->connect('DBI:mysql:ET','vapuser','vapuser@mysql',
		    {RaiseError=>1, AutoCommit=>1} );

  ($ss,$mm,$hh,$mday,$mon,$year,@junk)=localtime($self{starttime});
  $year += 1900;
  $mon += 1;
  $st=sprintf("%04d-%02d-%02d %02d:%02d:%02d", 
		   $year, $mon, $mday, $hh, $mm, $ss)      ;

  ($ss,$mm,$hh,$mday,$mon,$year,@junk)=localtime($self{endtime});
  $year += 1900;
  $mon += 1;
  $et=sprintf("%04d-%02d-%02d %02d:%02d:%02d", 
		   $year, $mon, $mday, $hh, $mm, $ss)      ;

  @tmp=split("/", $self{dir});
  $dirnum=$tmp[$#tmp];


    ## create the two SQL statements and load these results into the
    ## database

    ## the 'processing' table
  $sql="INSERT INTO processing (dirnum, start, stop, elapsed, status)  ";
  $sql .= "VALUES ($dirnum, '$st', '$et', $self{elapsed}, $self{status})";
  $dbh->do($sql);
  
  @tmp=split /,/, $self{windfiles};

  $windpath="";
  $windfiles="";
  for (@tmp) {
    ($name,$path,$suffix) = fileparse($_);
    $windpath .= $path if ($windpath ne $path);
    $windfiles .= "$name,";
  }
  $windfiles =~ s/(.*),$/$1/;

    ## the 'files' table
  $sql="INSERT INTO files (dirnum, interpfile, log, windfiles, windpath) ";
  $sql.="VALUES( $dirnum, '$self{interpfile}', '$self{logfile}', '$windfiles', '$windpath')";
  $dbh->do($sql);

  $dbh->disconnect;    
  print "done!\n";
}

1;
  

