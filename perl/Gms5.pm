#
#
# Perl Package: Gms5
#
#
# This package provides basic functionality in GMS5 image processing
# is to be used whenever this functionality is required. It has
# modules that check for and retrieve any of the various GMS5 files
# needed in the Qscat/Seawinds GMS5 image processing.
#
# To gain access to this package's functions,  *use* the package, i.e. put
#
#  use Gms5; 
#
#  somewhere close to the beginning of the program. 
#
# See the perl programs 'cloud_overlay' or 'gms5getall' for examples of its use.
#
#
# This package assumes that all the gms data is in a series of
# subdirectories of a common parent, given in the $LOCAL_TOPDIR
# variable. That is, the structure of the data tree is 
#
#  $LOCAL_TOPDIR/{cal,doc,grid,grida,ir1,ir2,ir3,vis}
#

# Modifications:
# $Log$
# Revision 1.6  2002/05/07 20:40:36  vapdev
# Set -w and `use strict' and then fixing bugs. Start trying to standardize
# the methods used.
#
# Revision 1.5  2001/02/09 18:20:23  vapuser
# Changed all 'die's to 'croak' Took out 'grida' from the vet
# check. Added sub Getclosest.
#
# Revision 1.4  2000/03/09 16:20:55  vapuser
# Took Vis out of 'GETALL' and 'CHECKALL'.
# Put archive info in 'required' file
# $VAP_LIB/gms5_archive. Oh, and a time kludge.
#
# Revision 1.3  1999/04/02 22:29:38  vapuser
# Many many changes
#
# Revision 1.2  1999/04/02 18:35:10  vapuser
# Removed 'doc' and 'cal' processing. We really don't need it.
#
# Revision 1.1  1999/04/02 18:29:10  vapuser
# Initial revision
#
#
# $Id$
#
#
#
package Gms5;
require Exporter;
use strict;
use vars qw/@ISA @EXPORT $startdir $user $local_host $REMOTE_HOST 
	    $LOCAL_TOPDIR $REMOTE_TOPDIR $ftp/;

use subs qw/Open, Close, List, Get, Pwd,
	   CdTop, CdDoc, CdCal, CdGrid, CdGrida, 
	   CdIr1, CdIr2, CdIr3, CdVis, 
	   GetCal, GetGrid, GetGrida,
	   GetDoc, GetIr1, GetIr2, GetIr3, GetVis,
	   GetAllFileLists, GetAll, GetClosest, CheckLocal/;

@ISA =qw(Exporter);
@EXPORT= ($REMOTE_TOPDIR, $LOCAL_TOPDIR,
	   &Open, &Close, &List, &Get, &Pwd,
	   &CdTop, &CdDoc, &CdCal, &CdGrid, &CdGrida, 
	   &CdIr1, &CdIr2, &CdIr3, &CdVis, 
	   &GetCal, &GetGrid, &GetGrida,
	   &GetDoc, &GetIr1, &GetIr2, &GetIr3, &GetVis,
	   &GetAllFileLists, &GetAll, &GetClosest, &CheckLocal );

use Net::FTP;
use Cwd 'chdir', 'getcwd';
use Carp;
use Time::Local;
use VapUtil;

BEGIN {
  $REMOTE_HOST = "";
  $REMOTE_TOPDIR="";
  $startdir=getcwd();
  $user=$ENV{'USER'};
  $local_host=$ENV{'HOST'} . ".jpl.nasa.gov";

  $Gms5::VAP_LIB=$ENV{'VAP_LIBRARY'} || croak "ENV var VAP_LIBRARY is undefined!\n";

  require $Gms5::VAP_LIB."/gms5_archive";

  $LOCAL_TOPDIR= $ENV{'VAP_GMS_TOP'} || croak "ENV var VAP_GMS_TOPDIR is undefined\n";
}

sub GetIntersection {

  # Given a gms type (which defaults to Ir1) get the intersection of
  # all the archive lists, i.e. all the files mentioned in
  # 'archive.filelist' file in each of the directories searched. Type
  # must be specified as 'ir1','ir2','ir3' or 'vis', or the routine
  # fails.

  # It is advisable that the user call 'getallfilelists' prior to
  # calling this routine as this routine assumes the 'archive.filelist' 
  # files are current.

  
  my $type = shift || "ir1";
  chdir $LOCAL_TOPDIR || croak "Can't cd to $LOCAL_TOPDIR\n";
  chdir $type || croak "Can't CD to $type\n";
  open ARCHIVE, "<archive.filelist";
  my @files= <ARCHIVE>;
  close ARCHIVE;
  chdir "..";

  my %filecnt=();
  my $dircnt=1;
  foreach my $file (@files) {
    my $tmp=substr($file,0,10);
    $filecnt{$tmp}++;
  }

  my @dirs=('grid', 'grida'); #removed cal and doc from list
  foreach my $dir (@dirs) {
    chdir $dir || croak "Can't CD to $dir\n";
    open ARCHIVE, "<archive.filelist";
    @files= <ARCHIVE>;
    close ARCHIVE;
    chdir "..";
    $dircnt++;
    foreach my $file (@files) {
      my $tmp=substr($file,0,10);
      $filecnt{$tmp}++;
    }
  }

  my @datetimes;
  foreach my $datetime ( keys %filecnt ){
    push @datetimes, $datetime if ($filecnt{$datetime} == $dircnt);
  }  

  @datetimes;

}


sub GetAllFileLists {

  Open(); # unless defined ($ftp);
  my @list=();
#   CdDoc();
#   @list=List("*.Z");
#   open FILE, ">archive.filelist";
#   print FILE @list;
#   close FILE;

#   CdCal();
#   @list=List("*.Z");
#   open FILE, ">archive.filelist";
#   print FILE @list;
#   close FILE;


  CdGrid();
  @list=List("*.Z");
  open FILE, ">archive.filelist";
  print FILE @list;
  close FILE;


  CdGrida();
  @list=List("*.Z");
  open FILE, ">archive.filelist";
  print FILE @list;
  close FILE;


  CdIr1();
  @list=List("*.Z");
  open FILE, ">archive.filelist";
  print FILE @list;
  close FILE;

  CdIr2();
  @list=List("*.Z");
  open FILE, ">archive.filelist";
  print FILE @list;
  close FILE;

  CdIr3();
  @list=List("*.Z");
  open FILE, ">archive.filelist";
  print FILE @list;
  close FILE;


  CdVis();
  @list=List("*.Z");
  open FILE, ">archive.filelist";
  print FILE @list;
  close FILE;


  Close();
  1;

}
sub GetAll {

  my $datetime=shift;
  croak "Need datetime!\n" unless $datetime;

  my $test = CheckAll($datetime);
  my @list;
#  if ($test) {

    Open(); #unless defined ($ftp);
#     CdDoc();
#     @list=List("$datetime*");
#     carp "No Doc files found for $datetime\n" if $#list<0;
#     $file="$datetime.txt.Z";
#     GetDoc( $file);


#     CdCal();
#     @list=List("$datetime*");
#     carp "No Cal files found for $datetime\n" if $#list<0;
#     $file="$datetime.cal.Z";
#     GetCal( $file);


    CdGrid();
    @list=List("$datetime*");
    carp "No Grid files found for $datetime\n" if $#list<0;
    my $file="$datetime.hdf.Z";
    GetGrid( $file);


  CdIr1();
  @list=List("$datetime*");
  carp "No Ir1 files found for $datetime\n" if $#list<0;
  $file="$datetime.hdf.Z";
  GetIr1( $file);

  #   CdIr2();
  #   @list=List("$datetime*");
  #   carp "No Ir2 files found for $datetime\n" if $#list<0;
  #   $file="$datetime.hdf.Z";
  #   GetIr2( $file);

  #   CdIr3();
  #   @list=List("$datetime*");
  #   carp "No Ir3 files found for $datetime\n" if $#list<0;
  #   $file="$datetime.hdf.Z";
  #   GetIr3( $file);

#     CdVis();
#     @list=List("$datetime*");
#     carp "No Vis files found for $datetime\n" if $#list<0;
#     $file="$datetime.hdf.Z";
#     GetVis( $file);

  CdGrida();
  @list=List("$datetime*");
  carp"No Grida files found for $datetime\n" if $#list<0;
  $file="$datetime.hdf.Z";
  GetGrida( $file);

  Close();
#  }
  $test;
  
} # GetAll


sub Open  {
  # Open remote connection
  my $machine= shift || $REMOTE_HOST;
  $ftp = Net::FTP->new( $machine ) ||
    croak "Can't create ftp object!\n";
  $ftp->login("anonymous","$user\@catspaw.jpl.nasa.gov") || 
    croak "Can't log on to $machine\n";
  # CD to topdir 
  $ftp->binary;
}

sub Close {
  $ftp->quit || croak "Can't close connection to $REMOTE_HOST\n";
}


sub List {
  my $glob= shift || "*";
  my @ls = $ftp->ls( $glob );
  carp "No such file" unless $ls[0];
  @ls = join("\n", @ls);
}

sub Get {
  my $file = shift;
  croak "Gms5::Get No file!\n" unless $file;
  $ftp->get( $file );

}

sub CdDoc   { $ftp->cwd("$REMOTE_TOPDIR/doc") || 
		    croak "Can't CD to $REMOTE_TOPDIR/doc\n";  
		     chdir "$LOCAL_TOPDIR/doc" || 
                       croak "Cant CD to $LOCAL_TOPDIR/doc\n"; }
sub CdCal   { $ftp->cwd("$REMOTE_TOPDIR/calib") ||
		    croak "Can't CD to $REMOTE_TOPDIR/calib\n";
		chdir "$LOCAL_TOPDIR/cal" || 
		  croak "Can't CD to $LOCAL_TOPDIR/cal\n"; }
sub CdGrid  { $ftp->cwd("$REMOTE_TOPDIR/hdf/grid" ) ||
		    croak "Can't CD to $REMOTE_TOPDIR/hdf/grid\n" ;
		    chdir "$LOCAL_TOPDIR/grid" ||
		      croak "Can't CD to $LOCAL_TOPDIR/grid\n";}
sub CdGrida { $ftp->cwd("$REMOTE_TOPDIR/hdf/grida") ||
		    croak "Can't CD to $REMOTE_TOPDIR/hdf/grida\n";
		chdir "$LOCAL_TOPDIR/grida" ||
		    croak "Can't CD to $LOCAL_TOPDIR/grida\n";}
sub CdIr1   { $ftp->cwd("$REMOTE_TOPDIR/hdf/ir1/4km"  ) ||
		    croak "Can't CD to $REMOTE_TOPDIR/hdf/ir1/4km\n" ;
		    chdir "$LOCAL_TOPDIR/ir1" ||
		      croak "Can't CD to $LOCAL_TOPDIR/ir1\n";}
sub CdIr2   { $ftp->cwd("$REMOTE_TOPDIR/hdf/ir2/4km"  ) ||
		    croak "Can't CD to $REMOTE_TOPDIR/hdf/ir2/4km\n" ;
		    chdir "$LOCAL_TOPDIR/ir2" ||
		      croak "Can't CD to $LOCAL_TOPDIR/ir2\n";}
sub CdIr3   { $ftp->cwd("$REMOTE_TOPDIR/hdf/ir3/4km"  ) ||
		    croak "Can't CD to $REMOTE_TOPDIR/hdf/ir3/4km\n" ;
		    chdir "$LOCAL_TOPDIR/ir3" ||
		      croak "Can't CD to $LOCAL_TOPDIR/ir3\n";}
sub CdVis   { $ftp->cwd("$REMOTE_TOPDIR/hdf/vis/4km"  ) ||
		    croak "Can't CD to $REMOTE_TOPDIR/hdf/vis/4km\n" ;
		    chdir "$LOCAL_TOPDIR/vis" ||
		      croak "Can't CD to $LOCAL_TOPDIR/vis\n";}


sub GetDoc { 
  my $file=shift;
  croak "No File\n" unless $file;
  CdDoc();
  Get $file unless (-e $file);
  
}


sub GetCal {
  my $file=shift;
  croak "No File\n" unless $file;
  CdCal();
  Get $file unless (-e $file);
}


sub GetGrid {
  my $file=shift;
  croak "No File\n" unless $file;
  CdGrid();
  Get $file unless (-e $file);
}

sub GetGrida {
  my $file=shift;
  croak "No File\n" unless $file;
  CdGrida();
  Get $file unless (-e $file);
}

sub GetIr1 {
  my $file=shift;
  croak "No File\n" unless $file;
  CdIr1();
  Get $file unless (-e $file);
}

sub GetIr2 {
  my $file=shift;
  croak "No File\n" unless $file;
  CdIr2();
  Get $file unless (-e $file);
}
sub GetIr3 {
  my $file=shift;
  croak "No File\n" unless $file;
  CdIr3;
  Get $file unless (-e $file);
}

sub GetVis {
  my $file=shift;
  croak "No File\n" unless $file;
  CdVis;
  Get $file unless (-e $file);
}

sub CheckAll {

  # returns 1 if all the default files for a particular datetime are
  # there, 0 otherwise currently, this include doc/cal/grid/grida/ir1
  # and vis.  It' checks the local archive first, then the remote.

  my $datetime=shift; croak "No datetime\n" unless $datetime;

  return 1 if CheckLocal($datetime);
  my $ret=1;
  my @list=();
  Open(); # unless defined ($ftp);


#   CdDoc();
#   @list=List("$datetime*");
#   if (!$list[0]) {
#     carp "No Doc files found for $datetime\n";
#     $ret=0;
#   }


#   CdCal();
#   @list=List("$datetime*");
#   if (!$list[0]) {
#     carp "No Cal files found for $datetime\n";
#     $ret=0;
#   }



  CdGrid();
  @list=List("$datetime*");
  if (!$list[0]) {
    carp "No Grid files found for $datetime\n";
    $ret=0 ;
  }


   CdGrida();
   @list=List("$datetime*");
   if (!$list[0]) {
     carp"No Grida files found for $datetime\n";
     $ret=0 ;
   }
 

  CdIr1();
  @list=List("$datetime*");
  if (!$list[0]) {
    carp "No Ir1 files found for $datetime\n";
    $ret=0 ;
  }
 
#   CdIr2();
#   @list=List("$datetime*");
#   if (!$list[0]) {
#    carp "No Ir2 files found for $datetime\n";
#    $ret=0 ;
#  }

#   CdIr3();
#   @list=List("$datetime*");
#   if (!$list[0]) {
#    carp "No Ir3 files found for $datetime\n";
#    $ret=0 ;
#  }

#   CdVis();
#   @list=List("$datetime*");
#   if (!$list[0]) {
#     carp "No Vis files found for $datetime\n";
#     $ret=0 ;
#  }
  Close();
  $ret;
} # CheckAll

sub Gms5DateTime2SysTime{

  croak "Need datetime!\n" if !$_[0];
  my ($year,$month,$day,$hour,$min,$timegm);
  $year=substr($_[0],0,2);
  $year += 2000 if $year < 90; # kludge
  $month=substr($_[0],2,2);
  $day=substr($_[0],4,2);
  $hour=substr($_[0],6,2);
  $min=substr($_[0],8,2);
  my $timegm=timegm( 0, $min, $hour, $day, $month-1, $year);
  $timegm;
}

sub GetClosest{
  my $idltime = shift || croak "<param1> yyyy/mm/dd/hh/mm is REQUIRED\n";
  my $absflag=shift || 0;
  my $time=idltime2systime($idltime);
  GetAllFileLists();
  my @datetimes=GetIntersection();
  my ($diff,$datetime,$mindiff);
  if (@datetimes) {
    $mindiff=1.e10;
    for (@datetimes) {
      my $gmstime=Gms5DateTime2SysTime($_);
      $diff=$time-$gmstime;
      $diff = abs($diff) if $absflag;
      if ($diff > 0 && $diff < $mindiff ) {
	$mindiff = $diff;
	$datetime=$_;
      }
    }
  }
  ($datetime, $mindiff);
}

sub CheckLocal {
  my( $datetime, $absflag, @datetimes, $gmstime, $diff, $mindiff, @there);

  $datetime = shift || croak "<param1> yymmddhhmm is REQUIRED\n";
  @datetimes = GetIntersection();
  @there = grep/$datetime/,@datetimes;
  return 1 if @there;
  0;

  }
1;

