#
# $Id$
#      
# Modification Log:
#
# $Log$
# Revision 1.1  2002/05/07 20:51:53  vapdev
# Error handling/reporting object
#
#
#

=head1 NAME VapError.pm
=head2 USE

   Simple object to handle all loggin *and* error reporting.

=head2 SYNOPSIS

 USAGE: 

  To create the object

    $error_object = VapError->new(key=>value)


=over 4
=head2 KEYS
=item * INFO_HANDLE the filehandle that Report(...,INFO) sends the
        message to, by default this equals *STDOUT

=item * ERROR_HANDLE: the filehandle the Report(...,ERROR) sends the
        message to, by default *STDERR

=item * MAIL_ADDRESSES: The list of addresses to send mail to. This
        can be a reference to an array or a space separated list of
        addresses passed as a single string. The important think to
        remember is that it must be suitable for use in a call to
        `mailx'.

=back

  When the user wants to send a message:

    $error_object -> Report(message,SEVERITY);

    Here SEVERITY is either "INFO" or "ERROR"

  When the user wants to send a message and terminate:

    $error_object-> ReportAndDie(subject,message[,address])




 FAILURE MODES: The object requires that the environmental variables
                $VAP_LIBRARY and $VAP_SFTWR_PERL exist.

 METHODS:

  new:

    The constructor reads the file $VAP_LIBRARY/vap_error_defs which
    contains a reference to a hash having default values to be used in
    error reporting.  The most important of which is the MAIL_ADDRESSES
    key. The user may override any element of that hash on the
    commandline by passing the appropriate fields. The field
    MAIL_ADDRESSES is checked at the end of the constructor to make
    sure that it has been set. If it hasn't, it is defaulted to
    $user@$host where $user and $host are read from the %ENV.

    Arguments. None required, although the user may override any of the
               values in the default hash contained in the file
               $VAP_LIBRARY/vap_error_defs by passing them in keyword
               => value format, They get placed  directly into the
               object.  Any arguments are acceptable but the  only
               fields being used right now (Thu Aug  8 11:08:36 2002)
               are INFO_HANDLE, ERROR_HANDLE and MAIL_ADDRESSES.

               Stay tuned for any changes.  

  Report(message,severity):

    Send a <message> to the $self->{INFO_HANDLE} if <severity> ==
    INFO otherwise sent it to $self->{ERROR_HANDLE}

  
  ReportAndDie(subject, message, additional_addresses):

    Send an email message to each address in
    $self->{MAIL_ADDRESSES} (as well as any contained in the
    optional argument `address' bearing the subject `subject' and the
    message body `message' (or NULL BODY) if `message' isn't passed.
    Arguments:

     subject: <Required>: The subject line of the email message.
     message: (optional) The message body
     address: (optional) Additional addresses. May be passed as a
                         reference to an array, or a comma separated
                         list of addresses.

=cut
package VapError;
use strict;
use Carp;
use vars qw($vap_error_defs $VAP_LIBRARY *STDOUT *STDERR);
BEGIN {
  croak  "ENV variable VAP_LIBRARY is undefined!\n"
    unless $ENV{VAP_LIBRARY};
  $VAP_LIBRARY = $ENV{VAP_LIBRARY};
  croak  "ENV variable VAP_SFTWR_PERL is undefined!\n"
    unless $ENV{VAP_SFTWR_PERL};
}

use lib $ENV{VAP_SFTWR_PERL};
use lib $ENV{VAP_LIBRARY};
require "vap_error_defs";


sub new {
  my $class = shift;
  my $h={@_};
  my $self={};
  my ($k, $v);
  while (($k,$v) = each %{$vap_error_defs}){
    $self->{$k} = $v;
    delete $vap_error_defs->{$k};
  }
  undef $vap_error_defs;
  while (($k,$v) = each %{$h}){
    $self->{$k} = $v;
    delete $h->{$k};
  }
  undef $h;

  $self->{IS_BATCH}  = !defined($ENV{'TERM'});

  croak "MAIL_ADDRESSES hasn't been set!\n" 
    unless defined ($self->{MAIL_ADDRESSES});

  return bless $self, ref($class) || $class;

}

#---------------------------------------------
# Report
# Usage: $obj->Report(message,severity)
#
# Send message to $self->{INFO_HANDLE} (by default STDOUT unless
# overridden in the call to `new') if `severity' == info|INFO,
# otherwise send it to ERROR_HANDLE (by default 

sub Report{
  my $self=shift;
  my $message=shift or carp "Need Message!\n";
  my $severity = shift || "ERROR";
  $severity = uc $severity;
  my $handle= $severity."_HANDLE";
  my $fh=$self->{$handle};
  print $fh $message;
  1;
}

#---------------------------------------------
#ReportAndDie -- 
# Usage: $obj->ReportAndDie(subject [,message, address]);
#
#  Should never return, as it calls "croak" at the end.  Write an
#  email having subject `subject' and optional message `message' to
#  every email contained in the array $self->{MAIL_ADDRESSES} and,
#  optionally, to any addtional ones contained in the *comma separated
#  string* `address.'



#
sub ReportAndDie{
  my $self=shift;
  my $subject = shift or 
      carp "usage: \$obj->ReportAndDie(subject [,message, address])\n"; 
  my $message=shift || "NULL MESSAGE";

  $self->Report($message,"ERROR");

  my $address = shift;
  my $addresses;
  if (ref($self->{MAIL_ADDRESSES}) eq 'ARRAY') {
    $addresses = join " ", $self->{MAIL_ADDRESSES};
  } else {
    $addresses = $self->{MAIL_ADDRESSES};
  }
  if (ref($address) eq 'ARRAY'){
    $address = join " ",$address;
  }

  $addresses = "$address $addresses"; 
  if ($self->{IS_BATCH}) {
    open PIPE, "|mailx -s'$subject' $addresses" or 
      croak "Can't open `mailx':$!\n";
    print PIPE "$message\n";
    close PIPE;
    my $final_message = "Message: $message\nsent to $addresses\n\n";
    my $fh=$self->{ERROR_HANDLE} ;
    print $fh $final_message;
  }
  confess "And now, I die at ".scalar(localtime(time))."!\n";
  0;
}
1;
