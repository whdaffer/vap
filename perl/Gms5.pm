#!/usr/bin/perl
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
@ISA =qw(Exporter);
@EXPORT=qw( $REMOTE_TOPDIR, $LOCAL_TOPDIR, 
	   Open, Close, List, Get, Cd, Pwd, 
	   CdTop, CdDoc, CdCal, CdGrid, CdGrida, 
	   CdIr1, CdIr2, CdIr3, CdVis, 
	   GetCal, GetGrid, GetGrida,
	   GetDoc, GetIr1, GetIr2, GetIr3, GetVis,
	   GetAllFileLists, GetAll );

use Net::FTP;
use Cwd 'chdir', 'getcwd';
use Carp;
use Time::Local;
    

BEGIN {
  $startdir=Cwd::getcwd();
  $user=$ENV{'USER'};
  $local_host="$ENV{'HOST'}.jpl.nasa.gov";

  $VAP_LIB=$ENV{'VAP_LIB'}                  || "/usr/people/vapuser/Qscat/Library";

#   Put these in $VAP_LIB/gms5_archive, so 
#   that we could change quickly. (whd 2000/03/09)
#   
#  $remote_host="explorer.arc.nasa.gov";
#  $remote_host="rsd.gsfc.nasa.gov";
#  $REMOTE_TOPDIR="/pub/Weather/GMS-5";

  require $VAP_LIB."/gms5_archive";

  $LOCAL_TOPDIR= $ENV{'VAP_GMS_TOPDIR'} || $ENV{'VAP_ROOT'}."/gms5" || "/disk5/vap/gms5";
  #$LOCAL_TOPDIR="/disk4/vap/gms5";
  
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

  
  $type=$_[0] || "ir1";
  chdir $LOCAL_TOPDIR || die "Can't cd to $LOCAL_TOPDIR\n";
  chdir $type || die "Can't CD to $type\n";
  open ARCHIVE, "<archive.filelist";
  @files= <ARCHIVE>;
  close ARCHIVE;
  chdir "..";

  $dircnt=1;
  foreach $file (@files) {
    $tmp=substr($file,0,10);
    $filecnt{$tmp}++;
  }

  @dirs=('grid'); #removed cal and doc from list
  foreach $dir (@dirs) {
    chdir $dir || die "Can't CD to $dir\n";
    open ARCHIVE, "<archive.filelist";
    @files= <ARCHIVE>;
    close ARCHIVE;
    chdir "..";
    $dircnt++;
    foreach $file (@files) {
      $tmp=substr($file,0,10);
      $filecnt{$tmp}++;
    }
  }

  foreach $datetime ( keys %filecnt ){
    push @datetimes, $datetime if ($filecnt{$datetime} == $dircnt);
  }  

  @datetimes;

}


sub GetAllFileLists {

  Open(); # unless defined ($ftp);

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

  $datetime=shift;
  die "Need datetime!\n" unless $datetime;

  $test = CheckAll($datetime);
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
    $file="$datetime.hdf.Z";
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

#      CdGrida();
#      @list=List("$datetime*");
#      carp"No Grida files found for $datetime\n" if $#list<0;
#      $file="$datetime.hdr.Z";
#      GetGrida( $file);

    Close();
#  }
  $test;
  
} # GetAll


sub Open  {
  # Open remote connection
  $machine= shift || $remote_host;
  $ftp = Net::FTP->new( $machine );
  die "Can't create ftp object!\n" unless $ftp;
  $ftp->login("anonymous","$user\@catspaw.jpl.nasa.gov") || 
      die "Can't open connection to $machine\n";
  # CD to topdir 
  $ftp->binary;
  $ftp->cwd($remote_top_gms_dir) || die "Can't remote cd to $remote_top_cms_dir\n" ;
}

sub Close {
  $ftp->quit || die "Can't close connection to $remote_host\n";
}


sub List {
  $glob= shift || "*";
  @ls = $ftp->ls( $glob );
  carp "No such file" unless $ls[0];
  @ls = join("\n", @ls);
}

sub Get {
  $file = shift;
  die "Gms5::Get No file!\n" unless $file;
  $ftp->get( $file );

}

sub CdDoc   { $ftp->cwd("$REMOTE_TOPDIR/doc") || 
		    die "Can't CD to $REMOTE_TOPDIR/doc\n";  
		     chdir "$LOCAL_TOPDIR/doc" || 
                       die "Cant CD to $LOCAL_TOPDIR/doc\n"; }
sub CdCal   { $ftp->cwd("$REMOTE_TOPDIR/calib") ||
		    die "Can't CD to $REMOTE_TOPDIR/calib\n";
		chdir "$LOCAL_TOPDIR/cal" || 
		  die "Can't CD to $LOCAL_TOPDIR/cal\n"; }
sub CdGrid  { $ftp->cwd("$REMOTE_TOPDIR/hdf/grid" ) ||
		    die "Can't CD to $REMOTE_TOPDIR/hdf/grid\n" ;
		    chdir "$LOCAL_TOPDIR/grid" ||
		      die "Can't CD to $LOCAL_TOPDIR/grid\n";}
sub CdGrida { $ftp->cwd("$REMOTE_TOPDIR/hdf/grida") ||
		    die "Can't CD to $REMOTE_TOPDIR/hdf/grida\n";
		chdir "$LOCAL_TOPDIR/grida" ||
		    die "Can't CD to $LOCAL_TOPDIR/grida\n";}
sub CdIr1   { $ftp->cwd("$REMOTE_TOPDIR/hdf/ir1/4km"  ) ||
		    die "Can't CD to $REMOTE_TOPDIR/hdf/ir1/4km\n" ;
		    chdir "$LOCAL_TOPDIR/ir1" ||
		      die "Can't CD to $LOCAL_TOPDIR/ir1\n";}
sub CdIr2   { $ftp->cwd("$REMOTE_TOPDIR/hdf/ir2/4km"  ) ||
		    die "Can't CD to $REMOTE_TOPDIR/hdf/ir2/4km\n" ;
		    chdir "$LOCAL_TOPDIR/ir2" ||
		      die "Can't CD to $LOCAL_TOPDIR/ir2\n";}
sub CdIr3   { $ftp->cwd("$REMOTE_TOPDIR/hdf/ir3/4km"  ) ||
		    die "Can't CD to $REMOTE_TOPDIR/hdf/ir3/4km\n" ;
		    chdir "$LOCAL_TOPDIR/ir3" ||
		      die "Can't CD to $LOCAL_TOPDIR/ir3\n";}
sub CdVis   { $ftp->cwd("$REMOTE_TOPDIR/hdf/vis/4km"  ) ||
		    die "Can't CD to $REMOTE_TOPDIR/hdf/vis/4km\n" ;
		    chdir "$LOCAL_TOPDIR/vis" ||
		      die "Can't CD to $LOCAL_TOPDIR/vis\n";}


sub GetDoc { 
  $file=shift;
  die "No File\n" unless $file;
  CdDoc();
  Get $file unless (-e $file);
  
}


sub GetCal {
  $file=shift;
  die "No File\n" unless $file;
  CdCal();
  Get $file unless (-e $file);
}


sub GetGrid {
  $file=shift;
  die "No File\n" unless $file;
  CdGrid();
  Get $file unless (-e $file);
}

sub GetGrida {
  $file=shift;
  die "No File\n" unless $file;
  CdGrida();
  Get $file unless (-e $file);
}

sub GetIr1 {
  $file=shift;
  die "No File\n" unless $file;
  CdIr1();
  Get $file unless (-e $file);
}

sub GetIr2 {
  $file=shift;
  die "No File\n" unless $file;
  CdIr2();
  Get $file unless (-e $file);
}
sub GetIr3 {
  $file=shift;
  die "No File\n" unless $file;
  CdIr3;
  Get $file unless (-e $file);
}

sub GetVis {
  $file=shift;
  die "No File\n" unless $file;
  CdVis;
  Get $file unless (-e $file);
}

sub CheckAll {

  # returns 1 if all the default files for a particular datetime are
  # there, 0 otherwise currently, this include doc/cal/grid/grida/ir1
  # and vis.  

  $datetime=shift; die "No datetime\n" unless $datetime;

  $ret=1;

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


#   CdGrida();
#   @list=List("$datetime*");
#   if (!$list[0]) {
#     carp"No Grida files found for $datetime\n";
#     $ret=0 ;
#   }
 

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

  die "Need datetime!\n" if !$_[0];
  $year=substr($_[0],0,2);
  $year += 2000 if $year < 90; # kludge
  $month=substr($_[0],2,2);
  $day=substr($_[0],4,2);
  $hour=substr($_[0],6,2);
  $min=substr($_[0],8,2);
  $timegm=timegm( 0, $min, $hour, $day, $month-1, $year);
  $timegm;
}
1;

