#!/bin/perl5 
#
# Modification history:
#
# $Log$
#
#
#
# $Id$
#


use Cwd 'chdir';

($sec,$min,$hour,$mday,$mon,
    $year,$wday,$yday,@isdst)=localtime($^T);
$year += 1900;
$mon+= 1;

$mon  = ($mon  <10) ?  "0$mon"  : "$mon";
$mday = ($mday <10) ?  "0$mday" : "$mday";

$tmp="QS$year$mon$mday";

($sec,$min,$hour,$mday,$mon,
    $year,$wday,$yday,@isdst)=localtime($^T-86400);
$year += 1900;
$mon+= 1;

$mon  = ($mon  <10) ?  "0$mon"  : "$mon";
$mday = ($mday <10) ?  "0$mday" : "$mday";

  # yesterday and today.
@QSbasename=("QS$year$mon$mday",$tmp);

@filetimes = (
	      "S0000.E0140",
	      "S0141.E0321",
	      "S0322.E0503",
	      "S0504.E0643",
	      "S0644.E0825",
	      "S0826.E1006",
	      "S1007.E1147",
	      "S1148.E1327",
	      "S1328.E1508",
	      "S1509.E1650",
	      "S1651.E1830",
	      "S1831.E2012",
	      "S2013.E2153",
	      "S2153.E2359");



$windDir=$ENV{"VAP_WINDS"};
die "VAP_WINDS is undefined!\n" if !$windDir;
chdir $windDir;
opendir WINDDIR, "./" || die "Can' Open dir $windDir\n";
@windDir=readdir(WINDDIR);
closedir WINDDIR;

foreach $dirEntry (@windDir) {
  next if $dirEntry =~ /^\.+$/;
  if (-l $dirEntry) {
    print "Unlinking $dirEntry\n";
    unlink($dirEntry);
  } else {
    if ($dirEntry =~ /^L2B_rev.*DAT$/) {
      $junk=substr($dirEntry,7,2);
      if ($junk >= 10 ) {
	$number=$junk;
      } else {
	$number=substr($junk,0,1);
      }
      push @numbers, $number;
    }
  }
}

@sortednumbers = sort  {$a<=>$b;} @numbers;
for ($i=0; $i<=$#filetimes; $i++) {
  for ($j=0;$j<2;$j++) {
    $ii=$i % ($#sortednumbers+1);
    $number=$sortednumbers[$ii];
    if ($number>9) {
      $oldfile="L2B_rev".$number."_0att.DAT";
    } else {
      $oldfile="L2B_rev".$number."_n.DAT";
    }
    $newfile="$QSbasename[$j].$filetimes[$i]";
    print "linking $oldfile to $newfile\n";
    symlink( $oldfile, $newfile ) || die "Can't link $oldfile to $newfile\n";
  }
}



