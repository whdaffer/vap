#!/usr/bin/perl
#
#
#
# This package aids in finding dependencies between IDL routines.  It
# requires that all IDL code is contained in directores descended from
# a common parent and the prescence of a subdirectory named
# 'dependencies' in each directory containing IDL code, in which
# reside files having the same basename as each of the .pro file in
# the parent directory but with the extension .dep, these files
# containing a list of the modules on which that module depends. See
# the idl routine "makedep.pro" and the shell script
# ~/scr/{makedepindir, makealldep,makeidldep} 
#
#
# It is possible to have more than one directory tree containing IDL
# code, but this code will only traverse one at a time, as it requires
# the top of the directory try structure as an argument. I could make
# it smarter by making it aware of the IDL_PATH environmental
# variable, but I have all my code in one tree, so I'll do that later,
# when I need to.  
#
# Currently one can count the dependencies (countallidldep) which
# returns the number of times each routine is required by another, and
# one can find all routines that depend on a given
# routine. (whatdependson). Each of these subroutines is called via a
# pseudonynous routine in the ~/perl directory
#
# Author: William Daffer
# 
#
#
# $Log$
#
# $Id$
package makeidldep;

require Exporter;
@ISA = qw(Exporter);
@EXPORT=qw( countdep getdirs countallidldep depindir %functions %procedures );

use Cwd 'chdir', 'getcwd';
#use Time::Local;
BEGIN { my $topdir;
	my %functions={};
	my %procedures={}; }


sub countdep {
  # Usage: countdep file.dep
  #
  #

  print "Usage: countdep idl_dep_file.dep\n" if !$_[0];

  open DEPFILE, $_[0] or die "Can't open dep file $_[0]\n";
  
  @in=<DEPFILE>;
  close DEPFILE;
  #%functions = $_[1];
  #%procedures=$_[2];

  $file=shift(@in);
  chop $file;
  $functions=0;

  foreach $line (@in) {
    chop $line;
    if ($line =~ /^Compiled.*Functions:$/){
      $functions=1;
    };
    next if $line =~ /^Compiled/;
    @tmp=split(" ", $line);
    if ($functions)  {
      $functions{$tmp[1]}++;
    } else {
      $procedures{$tmp[1]}++;
    }
  }
  (%functions,%procedures );
}

sub getdirs{
  local( $dir, @directories);
  $dir= shift @_ || getcwd();
  opendir DIR, "$dir" || die "Can't open directory $dir\n";
  @directories = grep !/^\./, readdir DIR;
  @directories = grep -d, map "$dir/$_", @directories;
  @directories;
};

sub countallidldep{ 
  local($dir);
  local( $topdir ) =shift @_ || die "Need directory!\n";
  local( @directories) =makeidldep::getdirs($topdir);
  foreach $dir (@directories) {
    if ($dir =~ /.*dependencies$/) {
      \@deps = depindir( $dir );
    } else {
      countallidldep($dir);
    }
  }
  1;
}

sub depindir{
  local ($dir, @depfiles, $depfile );
  #die "Usage: depindir dependency-directory\n" if !$_[0];
  
  $dir=shift @_;
  opendir DEPDIR, $dir || die "Can't open dependency directory $dir\n";
  @depfiles = grep /.*\.dep$/, map "$dir/$_", readdir DEPDIR;
  
  foreach $depfile (@depfiles) {
    \@deps = countdep( $depfile );	
  }
  1;
}

sub whatdependson{
  die "Need input file\n" if !$_[0];
  die "Need input dir\n" if !$_[1];
  local($file)=shift @_;
  local($dir)=shift @_;
  #print "whatdependson: Working on directory $dir\n";
  local ($d, @dirs, $depfile, @depfiles, @deps, $dd );
  @dirs = getdirs($dir);
  foreach $d ( @dirs ){
    if ($d =~ m%.*/dependencies$%){
      @deps = dependantsindir($file, $d) ;
    } else {
      @deps = whatdependson( $file, $d );
    }
  }

  while ($dd = pop @deps) {
    $dependants{$dd}++;
  }

  keys dependants;
}

sub dependantsindir {
  die "Need input file\n" if !$_[0];
  die "Need input dir\n" if !$_[1];
  local($file) = $_[0];
  local($dir) = $_[1];
  local(@dependants);
  #print "   dependantsindir: Working on directory $dir\n";
  opendir DEPDIR, $dir || die "Can't open dependency directory $dir\n";
  @depfiles = grep /.*\.dep$/, map "$dir/$_", readdir DEPDIR;
  
  foreach $depfile (@depfiles) {
    $tmp=isdependent( $file, $depfile );
    push @dependants, $tmp if $tmp;
  }
  @dependants;
}

sub isdependent {

  local($test_file)= shift @_ || die "Need test file\n";
  local($depfile) = shift @_ || "Need dependency file\n";
  open DEPFILE, $depfile or die "Can't open dep file $depfile\n";
  
  @in=<DEPFILE>;
  close DEPFILE;

  local($file)=shift(@in);
  chop $file;
  $_=$depfile;
  s|^(.*)/dependencies/.*dep|$1|;
  $file="$_/$file";

  foreach $line (@in) {
    chop $line;
    next if $line =~ /^Compiled/;
    return $file if $line =~ /$test_file/;
  }
  return undef;

}
1;

