# $Id$
#
# Purpose: Some very simple extensions of the GGI object.
#
# Modifications:
#
# $Log$
#
package VapCGI;
use strict;
use CGI;
@VapCGI::ISA = qw(CGI);

sub new {
  my $class = shift;
  my $self=$class->SUPER::new(@_);
  return bless $self, ref($class) || $class;
}

sub map {
  my $self=shift;
  my $hash = shift;
  my @keys = keys %{$hash};
  while (my ($k,$v) = each %{$hash} ) {
    $k =~ s/^-(.*)/$1/;
    delete $hash->{"-$k"};
    $k = uc $k;
    $hash->{$k} = $v;
    if ($k eq "AREA") {
      my @arr=@{$v};
      foreach my $ref (@arr){
	while (my ($kk, $vv) = each %{$ref}){
	  $kk =~ s/^-(.*)/$1/;
	  delete $ref->{"-$kk"};
	  $kk = uc $kk;
	  $ref->{$kk} = $vv;
	}
      }
    }
  }

  my $str="<MAP NAME = \"". $hash->{NAME}. "\">\n";
  delete $hash->{'NAME'};
  my @areas = @{$hash->{AREA}};
  foreach my $ref (@areas){
    $str .= "<AREA";
    while (my ($k,$v) = each %{$ref} ) {
      $str .= " $k = \"$v\"";
    }
    $str .= ">\n";
  }

  $str .= "\n</MAP>\n";

  return $str;
}

1;
