=head1 VapWebsite

=head2 SYNOPSIS

       Object encapsulating code to maintain VAP website.

=head2 USAGE

       my $vapwww = VapWebsite->new()

       No further initialization is necessary, all such information is
       maintained in a defaults file: $VAP_LIBRARY/website_defs.

       All further manipulation of the website is done through method calls.

=head2 METHODS

=over 4

=item * new: Constructor

=item * UpdateWebsite(file=>`file' [,item=>item] )

        Move `file' to the correct location in the webspace and update
        the correct webpage. If further specification is necessary,
        the input argument `item' will give it. This is to cover
        future, unforeseen eventualities, generally this capability
        will be unnecessary, since the name of the file should make
        clear where to put the file and what page to update.


=back


=cut

use strict;
use Carp;
use Cwd;
use File::Copy;
use File::Basename;
use vars qw/$VERSION $usage/;

BEGIN {
  $VERSION = "0.9";

  croak "ENV var VAP_LIBRARY is undefined\n" 
    unless $ENV{VAP_LIBRARY};

  croak "ENV var VAP_SFTWR_PERL is undefined\n" 
    unless $ENV{VAP_SFTWR_PERL};

  croak "ENV var VAP_WWW_TOP is undefined\n" 
    unless $ENV{VAP_WWW_TOP};

}

use lib $ENV{VAP_SFTWR_PERL};
use lib $ENV{VAP_LIBRARY};
require "VapWebsite";
use VapUtil;
use VapError;


sub new{
  my $class = shift;
  my $self={@_};
  return bless $self, ref($class) || $class;
}

sub UpdateWebsite{
  my $self=shift;
  my $file=shift or croak "Need name of file to copy to webspace!\n";
  my $item=shift;

  my ($name, $path, $ext) = fileparse($file,@allowed_extensions);
  my @fields=();
  if ($ext =~ /(jpeg|jpg)/i){
    if ($name =~ /-(TYP|DEP|STO)-/) {
      # It's a tropical storm image.
      my ($region, $type, $name, $date) = 
	($name =~ /(\w+)-(\w+)-(\w+)-(\d+)\..*/);
      $item = $region unless $item;
    } else {
      # A regular overlay!
      $item = $region unless $item;
    }
  } else {
    # It's a movie: 
    my $destdir = "$VAP_WWW_TOP/mov_archive";
    @fields=split /_/,$name;
    $item = $fields[0] unless ($item);
    
  }
}

1;
