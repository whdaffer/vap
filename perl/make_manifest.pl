#!/usr/bin/perl -w
# $Id$
#
# $Log$
#
# Usage: make_manifest
#
# Requires: The Environmental variable VAP_TS_ARCHIVE point to the
# directory containing the `year' subdirectories.
#
# This routine doesn't set the ACTIVE keyword in the manifest, but the
# first time through VapWebsite, these will be set for each storm and
# year.


use Cwd;
use Time::Local;
use Data::Dumper;

my ($sata, $storm_typea, $namea, $datea,$yeara, $typea,
    $satb, $storm_typeb, $nameb, $dateb, $yearb, $typeb);

my $topdir=$ENV{VAP_TS_ARCHIVE};
$thirty_days = 3600*24*30; # thirty days, in seconds

chdir $topdir or die "$!\n";
opendir DIR, "." or die "$!\n";
my @dirs=grep /(?:\d{4})/, readdir(DIR);
closedir DIR;
my $manifest=();
foreach my $d (@dirs) {
  chdir $d or die "$!\n";
  @files=myreaddir();
  foreach (@files){
    my ($sat, $storm_type, $name, $date,$year, $type) = parsename($_);
    my $modtime = (stat($_))[9];
    push @{$manifest->{$d}->{$name}->{FILES}}, $_;
    $manifest->{$d}->{$name}->{LAST_SEEN} = $modtime 
      if 
	!($manifest->{$d}->{$name}->{LAST_SEEN}) ||
	  $manifest->{$d}->{$name}->{LAST_SEEN} < $modtime;
  }
  while (my ($name, $hash) = each %{$manifest->{$d}}){
    $hash->{LAST_SEEN_DATETIME} = 
      scalar(localtime($hash->{LAST_SEEN}));
    $hash->{ACTIVE} = (($^T-$hash->{LAST_SEEN}) > $thirty_days)?0:1;
    #@files = @{$manifest->{$d}->{$name}->{FILES}};
    #@files = sort bydate @files;
    #@{$manifest->{$d}->{$name}->{FILES}} = @files
    @{$hash->{FILES}} = sort bydate 
      @{$hash->{FILES}};
  }
  chdir $topdir;
}

my $manifest_file = $ENV{VAP_LIBRARY} . "/tropical_storms_manifest";
open MANIFEST, ">$manifest_file" or die "$!\n";
my $dumper = Data::Dumper->new([$manifest],[qw(manifest)]);
print MANIFEST $dumper->Dump;
close MANIFEST;

exit;


sub myreaddir{
  opendir DIR, "." or die "$!\n";
  my @files = grep /\w+-\w+-\w+-\w.*\.jpeg/,readdir (DIR);
  closedir DIR;
  @files=sort bydate @files;
}


sub parsename{
  # The files look like GMS5-CYC-01B-200010161251-X.jpeg
  my $file=shift;
  die unless $file;
  my @tmp=split /\./, $file;
  my ($sat, $storm_type, $name, $date,$type)  = split /-/, $tmp[0];
  my ($year, $month, $day, $hour, $min) = $date =~ 
    /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/;
  ($sat, $storm_type, $name, $date,$year, $type);
}

sub bydate{
  ($sata, $storm_typea, $namea, $datea,$yeara,$typea) = parsename($a);
  ($satb, $storm_typeb, $nameb, $dateb, $yearb,$typeb) = parsename($b);
  $datea <=> $dateb;
}
