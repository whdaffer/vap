#!/bin/perl -w

# Usage: succorf.pl infile outfile
#
# Wrap the call to succorf, send all output (std{out,err} to a log
# file) then echo that logfile to stdout.
#
# This way and IDL routine can spawn this script and have the errors
# (all of which are strings containing the word ERROR in it) passed
# back to the IDL caller. We have to do this because of the way RSI
# implemented spawn, it only captures stdout but leaves stderr alone.

use strict;
my $infile = shift || "succorf.in";
my $outfile = shift || "succorf.out";
my $ret;
my $log= "succorf.$$.log";

if (-e $log) {
  unlink $log or die "Can't delete $log!\n";
}

$ret=system("succorf $infile $outfile > $log 2>&1")/255;
open FILE, "$log" or die "Can't open $log: $!\n";
while (<FILE>){
  print
}
if ($ret) {
  print "Bad return ($ret) from succorf\n" 
} else {
  unlink $log;
}
close FILE;
exit;
