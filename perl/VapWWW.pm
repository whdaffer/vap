#!/usr/bin/perl  
#
#
# $Id$
#
# Name  : VapWWW.pm: 
# Author: William H. Daffer
# Purpose: 
#
#   Package for Qscat/SeaWinds Vap to interface with the Web side of
#   life.  This package contains many subroutines used to move files to
#   the website and modify the web page. It also contains the code used
#   when interfacing with the IDL software used to create web products!
#
#
#
# Note on unfortunate historical accident.
#
#
# When I first started writing the idl/perl code to run VAP, I was
# using the time format YYYYMMDDThh:mm:ss.ccc, which I took to calling
# 'vaptime' in the Perl code. Unfortunately, the IDL code is much
# easier to write if I separate all the fields in whatever time format
# I use by the same separator, instead of the 3 (one a null) that this
# format requires. So I made 'yyyy/mm/dd/hh/mm/ss' the default in the
# IDL code and took to calling that 'vaptime' as well. Now, I have two
# 'vaptimes', one for the perl code and one for the IDL code. To make
# matters infinitely worse, in the perl code that has to construct the
# IDL 'vaptimes' I call that 'vaptime' 'idltime,' easily confused with
# idldt time, the native IDL structure used to manipulate times.
#
# I'm going to continue this usage here, until I have the desire (not
# likely) to go around changing nomenclature. So, to recap:
#
#  fmt('vaptime' in perl) = yyyymmddThh:mm:ss.ccc
#  fmt('vaptime' in idl) = fmt('idltime' in perl) = yyyy/mm/dd/hh/mm/ss
#
#
#
# Modification History:
#
# $Log$
# Revision 1.2  2002/04/30 20:23:22  vapdev
# Modified the 'use lib' statement
#
# Revision 1.1  2001/02/09 19:09:25  vapuser
# Initial revision
#
#
#
package VapWWW;
use strict;
use vars qw(@ISA @EXPORT $_is_batch @wwwfiles %wwwhash 
	    %sat_num %sensor_dir $_secs_in_one_week @satregions 
	    $ts_template_re $ts_template %during_hash );

use lib $ENV{VAP_SFTWR_PERL};
use Exporter ();
use Cwd 'chdir', 'getcwd';
use Time::Local;
use Carp;
#use Vapdefs;

@ISA = qw(Exporter);
@EXPORT=qw( &auto_movie_defs &doy2mday_mon 
	   &date2doy &date_string &make_yyyymmdd &gag &grid_goes 
	   &getgoesfile &fixlonrange &vaptime2systime &systime2vaptime 
	   &ParseWindFileNames &GetWindFiles &GetNow &DeltaTime
	   &ParseVapTime &vaptime2idltime &VapMailErrorMsg 
	   &rewriteStormsPage &redoStormsPageLinks &redoStormsPage );


BEGIN {

  $_is_batch = !defined($ENV{'TERM'});

  @wwwfiles = ('daily_nepac.mov',
	       'daily_nwpac.mov',
	       'daily_npac.mov',
	       'daily_nwatl.mov',
	       'daily_indian.mov',
	       'daily_atlhurr.mov',
	       'daily_pachurr.mov',
	       'GOES104NEPAC1.jpeg',
	       'GOES104PACHURR1.jpeg',
	       'GOES84NWATL1.jpeg',
	       'GOES84ATLHURR1.jpeg',
	       'GMS51WPAC1.jpeg',
	       'GMS51JAPAN1.jpeg',
	       'GMS51FAREAST1.jpeg'
	       );
  
  @wwwnames = ('nepac_anim',
	       'nwpac_anim',
	       'npac_anim',
	       'nwatl_anim',
	       'indian_anim',
	       'atlhurr_anim',
	       'pachurr_anim',
	       #	       'npac_evol_anim',
	       'goes104nepac1',
	       'goes104pachurr1',
	       'goes84nwatl1',
	       'goes84atlhurr1',
	       'gms51wpac1',
	       'gms51japan1',
	       'gms51fareast1'
	       
	       );
  
  %wwwhash=('nepac_anim'     => 'daily_nepac.mov',
	    'nwpac_anim'     => 'daily_nwpac.mov',
	    'npac_anim'      => 'daily_npac.mov', 
	    'nwatl_anim'     => 'daily_nwatl.mov',
	    'indian_anim'    => 'daily_indian.mov',
	    'atlhurr_anim'   => 'daily_atlhurr.mov',
	    'pachurr_anim'   => 'daily_pachurr.mov',
	    #	    'npac_evol_anim' => 'daily_npac_evol.mov',
	    'goes104nepac1'  => 'GOES104NEPAC1.jpeg',
	    'goes104pachurr1'  => 'GOES104PACHURR1.jpeg',
	    'goes84nwatl1'   => 'GOES84NWATL1.jpeg',
	    'goes84atlhurr1'   => 'GOES84ATLHURR1.jpeg',
	    'gms51wpac1'     => 'GMS51WPAC1.jpeg',
	    'gms51japan1'    => 'GMS51JAPAN1.jpeg', 
	    'gms51fareast1'  => 'GMS51FAREAST1.jpeg'
	    );
  
  %sat_num= ('8','9' , # AREA08* are goes 9 files !
	     '9','8' , # AREA09* are goes 8 files !
	     '10','8' , # AREA10* are goes 8 files !
	     );
  %sensor_dir = ('1','vis',
		 '2','ir2',
		 '3','ir3',
		 '4','ir4',
		 '5','ir5'
		 );
  
  
  $_secs_in_one_week = 7*24*60*60;
  
  @satregions = ("GOESWEST", "GOESEAST", "GMS5");
  
  $ts_template_re="<a href=\"((\w+)-(\w+)-(\w+)-(\d+)\.jpeg)\">(.*)<p><img\s+src=\".*\.jpeg\".*></a>.*\(Processed: (.*),\s+Size:\s+(\d+\s*[KkBb]*)";
  
  
  
#  $ts_template="<p><a href=\"$file\">$linkstring<p><img src=\"$file\" width=320 height=240 align=top></a><p>(Processed: $processtime, Size: $size )\n";
  
  %during_hash = ();
  
  require "$VAP_LIB/StormNames";
}

sub auto_movie_defs {
  # parses the file $VAP_ROOT/auto_movie_defs.dat returns array
  # containing the ROI designations
  # (i.e. 'nepac','nwpac','npac','nwatl' and whatever other regions of
  # interest we may decide to do.
  

  # open the file 
  open (DEFS,"<$auto_movie_defs_file") || die "Can't open $auto_movie_defs_file\n";
  @defs =<DEFS>;
  close DEFS;

  # loop over records in the file taking the 'value' of the 'desig'
  # field in each record. Each record is a string suitable for
  # defining an IDL structure in an IDL 'execute' call, i.e. it looks
  # like { F1:X1, F2:X2, F3:X3 ... }.  So we split on the ',' then
  # find 'desig' and split on the ":"


  foreach $r ( @defs) {
    next if $r =~ /^;.*$/; # next if IDL comment line
    next if $r =~ /^\s*$/; # next if empty line
    @tmp = split(/,/, $r);
    $tmp = $tmp[0];
    @tmp = split(/:/, $tmp);
    push @desigs, $tmp[1];
  }
  @desigs;
}




sub date_index{ 
  # Looks for strings demarcating the 'created at' time
  # for the four files listed below in the file index.html (in the
  # current directory) and and puts in the ctime of each of the four
  # files listed below and the current time in the place of 'Last
  # modified:'.  time. It does this for the files daily_pac.mov,
  # pac_daily_a.gif, pac_daily_nn_a.gif, pac_daily_d.gif and
  # pac_daily_nn_d.gif. Also looks for the string "Last modified: "
  # When it finds this string it chops the last part off the string
  # and appends the current date/time.  
  # 
  # It also looks for strings of the form 'xxx_size' where xxx =
  # anim,pgoa or pgod and substitutes the file size (in Kbs) into the
  # file.
  # 
  # No arguments are required. It know's what it needs to do.

  local( $start_dir)=Cwd::cwd();
  local( %file_size ) = ();
  local( %file_date ) = ();

  $umask = umask;
  print "Current umask = $umask\n";
  print "Setting umask to 023 \n";

  umask( 023 ) || die "Couldn't reset umask to 023\n";
  $umask = umask;
  print "New umask = $umask\n";
  $tmp = oct(023);
  print "Should equal $tmp\n";

  @months=("Jan","Feb","Mar","Apr","May","Jun","Jul",
	   "Aug","Sep","Oct","Nov","Dec");
  @days=("Sun","Mon","Tue","Wed","Thu","Fri","Sat");

  $VAPIM = $vap_perl::VAP_WWW_TOP."/images/";
  $DOCROOT=$vap_perl::VAP_WWW_TOP;
  

  do {
    print "Couldn't chdir to $DOCROOT";
    return undef;
  } unless chdir $DOCROOT;
  
  
  while( ($key,$value) = each %wwwhash ) {
    $file = "$VAPIM$value";
    
    if (-e $file) { 

     if (! ( ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
       $atime,$mtime,$ctime,$junk)=stat($file)))
     {
       print "Couldn't stat $file\n";
       return undef;
     }

      ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
       $atime,$mtime,$ctime,$junk)=stat($file) if !$nlink;
      
      $file_size{$key}= $tmp = sprintf( "%d Kb", int( $size*1.e-3+0.5) );
      # convert  mod time
      ($sec,$min,$hour,$mday,$mon, $year,$wday,$yday,$isdst) = localtime($mtime); 
      $day=$days[$wday];
      $month=$months[$mon];
      $year += 1900;

      # construct the file date 
      $file_date{$key} = sprintf( "%3s %3s %02d %02d:%02d:%02d %04d PST",
				 $day, $month, $mday, $hour,$min,$sec, $year);

    } else {
      $file_size{$key} = "???Kb";
      $file_date{$key} = "???";
    }
  }

  #
  # Get the current time for the 'Last modified' string
  #  
  ($sec,$min,$hour,$mday,$mon,
   $year,$wday,$yday,$isdst) = localtime(time);
  
  $day=$days[$wday];
  $month=$months[$mon];
  $mod_date_str = sprintf( "%03s %3s %02d %02d:%02d:%02d %04d PST",
			  $day, $month, $mday, $hour,$min,$sec, $year);

  open(IN ,"index.html.blank");
  @in=<IN>;
  close(IN);
  do { print "Couldn't rename index.html\n";
       undef;} unless rename ("index.html", "index.html.old");
  do {
    print "Can't open index.html\n";
    undef; } unless open (OUT, ">index.html");
  $search_string1 = "(hhmts|";
  $search_string1 .= join("|",@wwwnames);
  $search_string1 .= ")";

  for ($i=0;$i<=$#in;$i++) {
    $rec=$in[$i];
    if ($rec !~ /^\s*<!--\s*<!--/) {
#      if ($rec=~/(anim|size|goes104|goes84|gms51|hhmts){       
#      if ($rec =~ /(anim|size|goes104nepac1|goes84nwatl1|hhmts)/) {
      if ($rec =~ /$search_string1/) {
#       print "Found something, rec = $rec\n";
	if ($rec =~ /hhmts start/) {
	    # Handle the 'Last Modified' at the end of the page 
	  print OUT $rec;
	  print OUT "Last Modified: $mod_date_str\n";
	  $i += 2;
	} else {
	  foreach $k (@wwwnames) {
	    if ( $rec =~ /$k/ ) {
#	    print "key = $k\n";
	      $search_string2=$k."_size";
	      if ($rec =~ /.*$search_string2.*/ ) {
		$rec =~ s/$search_string/$file_size{$k}/;
		$in[$i] = $rec;
		last ;
#	      print "After sub, rec = \n";
	      } elsif ($rec =~ /.*$k start/ ) {
#	      print "file mod time \n";
		print OUT $rec;
		print OUT "Created: $file_date{$k} </br></br>\n";
		$i += 2;
		last;
	      } 
	    }
	  }
	}
      }
    }
    print OUT $in[$i];
  }
  do {print "Couldn't go back to $start_dir\n";
	  undef;} unless chdir $start_dir;
  close OUT;
  1;
}

sub VapMailErrorMsg{
  if ($vap_is_batch){
    $errmsg=$_[0] || "Generic Error\n";
    $subject=$_[1] || "<Generic Vap Error>\n";
    $addresses=join ", ", @{$vap_perl::vap_defs{'Error_Mail_Address'}};
    open MAIL_MESSAGE, "|mailx -s \'$subject\' $addresses";
    print MAIL_MESSAGE $errmsg;
    close MAIL_MESSAGE;
  }
  1;
}

sub rewriteStormsPage{

  use File::Basename;
  use POSIX;

  

  local ($_, $file, $junk, $region, $storm, $type,$time, $dir, 
	 @lines, @newlines, $linkstring, $processtime, $tz, 
	 $size, $sizestring, %stormtype, @timeparts, $after,
	 @quicknavlinks );

  local(@pre, @during, @post);

  $dir = getcwd();
  chdir $VAP_WWW_TOP || croak "Can't CD to $VAP_WWW_TOP!\n";
  
  %stormtype = ( 'UNK' => "Unknown", 
		'DEP' => "Tropical Depression",
		'STO' => "Tropical Storm",
		'CYL' => "Cyclone",
		'HUR' => "Hurricane",
		'TYP' => "Typhoon" );
  
  $fqfile = shift || 
      croak "Param 1, name of new image file, I need it!\n";

  @relfile= $fqfile =~ /\/(.*)/;
  $relfile=$relfile[0];
  ($file,$path) = fileparse($fqfile);
  ($region,$type,$storm,$time) = split "-", $file ;
  ($time, $junk) = split /\./, $time;
  
  local @timeparts = $time =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/;
  $systime=timegm(0,$timeparts[4], $timeparts[3], 
		  $timeparts[2], $timeparts[1]-1, $timeparts[0]-1900);
  $time=join "/",@timeparts;
  $region =~ tr/[a-z]/[A-Z]/;
  
  $linkstring =  "$storm ($stormtype{$type}): $time (approx.)";
  local $tz=POSIX::tzname();
  $processtime=localtime(time);
  $processtime .= " ($tz)";
  local($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,
	$size,$atime,$mtime,$ctime,$junk)=stat($relfile);
  $size = int( $size/1000);
  $size = "$size Kb";

  ## must go into hash/array as 'file,region,type,name,date,link,process,size'

  $key=join( "|",
	    ($fqfile,$region,$storm,$type,$datetime,
	     $linkstring,$processtime,$size) );

  push @during, $key;
  $during_hash{$key} = $systime;

  open STORMSFILE, "< storms.html" || croak "Can't open storms.html\n";
  
  local($test)="REGION_$region";
  foreach $line (<STORMSFILE>) {
    $_ = $line;
    if (/^.*<.*NAME\s*=\s*$test.*>.*$/
        ... 
	/^\s*<!--\s+END\s+$test\s*-->/ ){
      
      local($this_file,$this_region, $this_type, $this_name, 
	    $this_datetime, $this_size);
      
      if (!$after) {
	push @pre, $_ ;
	$after=1;
	next;
      }

      if (m|^.*<a\s+.*\s*href=\"(.*/(\w+)-(\w+)-(\w+)-(\d+)\.jpeg)\">(.*)<p><img src=.*></a>.*\(\s*Processed:\s*(.*)\s*,\s*Size:\s*(\d+\s*[KkBb]*\s*)\).*|) {
	
	## file,region,type,name,date,link,process,size
	$this_file=$1;
	$this_region=$2;
	$this_type=$3;
	$this_name=$4;
	$this_datetime=$5;
	$this_linkstring=$6;
	$this_processtime=$7;
	$this_size=$8;
	
	
	local($year,$month,$day,$hour,$min,@rest)= 
	    $this_datetime =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/;
	local($systime)=timegm(0,$min,$hour,$day,$month-1,$year-1900);
	
	next if ( ($^T-$systime) >= $_secs_in_one_week );
	
	$key=join( "|",($this_file,$this_region,$this_name,$this_type,
			$this_datetime,$this_linkstring,$this_processtime,
			$this_size) );
	push @during, $key;

	$during_hash{$key} = $systime;
      } 

      push @post, $_ if (/^\s*<!--\s+END\s+$test\s*-->/)

	  
    } else {
      if (/^$/) {
	$oneblank +=1;
	next if ($oneblank > 1);
      } else {
	$oneblank=0;
      }

      if ($after) {
	push @post, $_;
      } else {
	if (/^.*<a\s+href=#\w+\s*>.*/) {
	    next unless /.*=#REGION_.*/;
	}
	push @pre, $_;
      }
    }
    
  }
  close FILE;

  @during = sort bysystime @during;
  unlink "storms.html.old" if -e "storms.html.old";
  
  for (@during) {
    ($file,$region,$name,$type,$datetime,
     $linkstring,$processtime,$size) = split /\|/;;
    $_="<p><a href=\"$file\">$linkstring<p><img src=\"$file.TN\" align=top></a><p>(Processed: $processtime, Size: $size)\n";
  }
  

  $processtime=localtime(time)."($tz)";  
  for (@post) {
    $_ = "Last Modified: $processtime\n" if /^Last Modified:.*$/;
  }



   # Concatenate all the lines together.
  @lines=(@pre, "\n", @during, @post);    

  local(@newlines) = redoStormsPageLinks(\@lines);
  undef @lines;
  open NEWFILE, ">storms.html.new" || 
      croak "Can't open new storms.html file!\n";


    # for some strange reason which I've been unable to suss out,
    # redoStormPageLinks sometimes returns a scalar, so here I just
    # make sure it's one long ine.

  if ($#newlines == 0) {
    $newlines=$newlines[0];
  } else {
    $newlines = join "", @newlines;
  }

  print NEWFILE $newlines;
  close NEWFILE;
  
  ## move the old file to a safe place.
  rename "storms.html", "storms.html.old" || 
      carp "Can't rename storms.html to storms.html.old\n";
  unlink "storms.html";
  ## move the new file into it's place.
  rename "storms.html.new", "storms.html" || 
      carp "Can't rename storms.html.new to storms.html\n";
  
  chdir $dir || carp "Can't CD to $dir\n";
  
  1;
}

sub redoStormsPageLinks{

    ## Given the lines of the storms.html page, delete old links,
    ## rework the nav menu and return the modified array.

  use File::Basename;
  use POSIX;

  local ($ref,$_, $file, $junk, $region, $storm, $type,$time, $dir, 
	 @lines, @newlines, $linkstring, $processtime, $tz, 
	 $size, $sizestring, %stormtype, @timeparts, $after,
	 @quicknavlinks  );
  
  $ref=shift || croak "Need reference to `lines' array!\n";
  @lines= @{$ref};

  # go through the lines, find latest first picture of each
  # storm in each region and rewrite it's html tag so that we can
  # refer to it from the 'quiknav menu.
  
  for (@lines) {
    if (/^.*<.*NAME\s*=\s*REGION_(GOESWEST|GOESEAST|GMS5).*>.*$/
 	... 
	
 	/^\s*<!--\s+END\s+REGION_(GOESWEST|GOESEAST|GMS5)\s*-->/){
      $region=$1;
      
      if (m|^.*<a\s+.*\s*href=\"(.*/(\w+)-(\w+)-(\w+)-(\d+)\.jpeg)\">(.*)<p><img src=.*></a>.*\(\s*Processed:\s*(.*)\s*,\s*Size:\s*(\d+\s*[KkBb]*\s*)\).*|) {
	
 	## file,region,type,name,date,link,process,size
 	local ($file,$region,$type,$name,$datetime,
 	       $linkstring,$processtime,$size,$link);


	$file=$1;
 	$region=$2;
 	$type=$3;
 	$name=$4;
 	$datetime=$5;
 	$linkstring=$6;
 	$processtime=$7;
 	$size=$8;


	local($year,$month,$day,$hour,$min,@rest)= 
	    $datetime =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/;
	local($systime)=timegm(0,$min,$hour,$day,$month-1,$year-1900);
	

	if ( ($^T-$systime) >= $_secs_in_one_week ){
	  $_ = "";
	  next;
	}
	
	# If it's a new name, it's the first in this series, so tag
	# it!
	
 	if ($name ne $oldname) {
 	  push @quiknavlinks, "$region.$name";
 	  $_ ="<p><a name=$name href=\"$file\">$linkstring<p><img src=\"$file.TN\" align=top></a><p>(Processed: $processtime, Size: $size)\n";
 	}
	
 	$oldname=$name;
      } 
      
    }
    
    
  }
  
  ## Now rewrite the quiknav menu!
  @newlines=();
  while (@lines) {
    $_=shift @lines;
    if (/^.*<!--\s*(BEGIN)\s+QUICKNAV_(GOESWEST|GOESEAST|GMS5)\s*-->.*$/
 	...
 	/^.*<!--\s*(END)\s+QUICKNAV_(GOESWEST|GOESEAST|GMS5)\s*-->.*$/){
      push @newlines, $_
       if /^.*<!--\s*(BEGIN|END)\s+QUICKNAV_(GOESWEST|GOESEAST|GMS5)\s*-->.*$/;
      local($quiknavregion)=$2;
      local($test)="^.*<a href=#REGION_$quiknavregion.*";
      next unless /$test/;

      foreach $link (@quiknavlinks) {
 	($region,$link1) = split /\./, $link;
 	$_ .= "\n<p><a href=#$link1><font size =-3>$link1</font></a>\n" if ($region eq $quiknavregion);
      }
      
    }
    push @newlines, $_;
    if (/^\s*<!--\s*END QUICKNAV\s*-->.*$/){
      last;
    }
  }
  
  
  push @newlines, @lines;

  return wantarray? @newlines: join "", @newlines;


#   undef @lines;
#   unlink "storms.html.old" if -e "storms.html.old";
#   open NEWFILE, ">storms.html.new" || 
#       croak "Can't open new storms.html file!\n";
#   print NEWFILE @newlines;
#   close NEWFILE;
  
#   ## move the old file to a safe place.
#   rename "storms.html", "storms.html.old" || 
#       carp "Can't rename storms.html to storms.html.old\n";
#   unlink "storms.html";
#   ## move the new file into it's place.
#   rename "storms.html.new", "storms.html" || 
#       carp "Can't rename storms.html.new to storms.html\n";
  
#   chdir $dir || carp "Can't CD to $dir\n";
#   1;
  
}


sub redoStormsPage{
    ## Freshen up the page!
  $dir=getcwd();
  chdir "/disk2/vap/www/htdocs";
  open FILE, "<storms.html";
  @lines=<FILE>;
  close FILE;
  @newlines=redoStormsPageLinks(\@lines) || die "error\n";
  undef @lines;
  open NEWFILE, ">storms.html.new" || 
      croak "Can't open new storms.html file!\n";
    # for some strange reason which I've been unable to suss out,
    # redoStormPageLinks sometimes returns a scalar, so here I just
    # make sure it's one long ine.

  if ($#newlines == 0) {
    $newlines=$newlines[0];
  } else {
    $newlines = join "", @newlines;
  }
  print NEWFILE $newlines;
  close NEWFILE;
  
  ## move the old file to a safe place.
  rename "storms.html", "storms.html.old" || 
      carp "Can't rename storms.html to storms.html.old\n";
  unlink "storms.html";
  ## move the new file into it's place.
  rename "storms.html.new", "storms.html" || 
      carp "Can't rename storms.html.new to storms.html\n";
  
  chdir $dir || carp "Can't CD to $dir\n";
  
  1;

}


sub bysystime{
  ## A typical key will look like:
# /images/storms/GMS5-TYP-BILIS-20000823153639.jpeg|GMS5|BILIS|TYP|20000823153639|Tropical Typhoon Bilis, 8/23 ~15:36<p>|8/23 21:00 (PDT) |180 Kb

  local(@ak,$ab,$aa,$bb,$test1);
  @ak=split(/\|/,$a);
  @bk=split(/\|/,$b);
  $aa=$ak[2];
  $bb=$bk[2];
  if ($ak[1] =~ /GMS5/) {
    $test1 = $westpacnames{$bb} <=> $westpacnames{$aa};
  } elsif ($ak[1] =~ /GOESWEST/) {
    $test1 = $eastpacnames{$bb} <=>  $eastpacnames{$aa};
  } elsif ($ak[1] =~ /GOESEAST/) {
    $test1 = $atlanticnames{$bb} <=> $atlanticnames{$aa};
  } else {
      # job security!
    croak "Unknown region! $ak[1]\n";
  }
  if ($test1 == 0) {
    $during_hash{$b} <=> $during_hash{$a};
  } else {
    $test1;
  }

}

1;
