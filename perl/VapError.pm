#!/bin/perl -w
#
# $Id$
#
# Simple object to handle all error reporting.
#
# USAGE: 
#
#  To create the object
#
#    $error_object = VapError->new(key=>value)
#
#  When the user wants to send a message:
#
#    $error_object -> Report(message,SEVERITY);
#
#  When the user wants to send a message and terminate:
#
#    $error_object-> ReportAndDie(subject,message,address)
#    
#
#
#
# FAILURE MODES: The object requires that the environmental variable
#                $VAP_LIBRARY exist.
#
# METHODS:
#
#  new:
#
#    The constructor reads the file $VAP_LIBRARY/vap_error_defs which
#    contains a reference to a hash having default values to be used in	
#    error reporting, the most important of which is the MAIL_ADDRESSES	
#    key. The user may override any element of that hash on the		
#    commandline by passing the appropriate fields. The field		
#    MAIL_ADDRESSES is checked at the end of the constructor to make	
#    sure that it has been set. If it hasn't, it is defaulted to		
#    $user@$host where $user and $host are read from the %ENV.            
#
#    Arguments. None required, although the user may override any of the
#               values in the default hash contained in the file
#               $VAP_LIBRARY/vap_error_defs by passing them in keyword
#               => value format, They get placed  directly into the
#               object.  Any arguments are acceptable but the  only
#               field being used right now (Mon May 6 11:51:34 2002)
#               is  MAIL_ADDRESSES. 
#
#               Stay tuned for any changes.  
#
#  Report:
#
#    Send an error message to the 
#  
#  ReportAndDie:
#
#    Send an email message to each address in $self->{MAIL_ADDRESSES}
#    (as well as any contained in the optional argument `address'
#    bearing the subject `subject' and the message body `message' (or
#    NULL BODY) if `message' isn't passed.
#    Arguments: 
#
#     subject: <Required>: The subject line of the email message.
#     message: (optional) The message body
#     address: (optional) Additional addresses. May be passed as a
#                         reference to an array, or a comma separated
#                         list of addresses.
#
#      
# Modification Log:
#
# $Log$
#
#
use strict;
use Carp;
sub new {
  my $class = shift;
  my $self1 = {@_};
  my $VAP_LIBRARY = $ENV{VAP_LIBRARY} or 
      die "ENV variable VAP_LIBRARY is undefined!\n";
  my $defs_file = "$VAP_LIBRARY/vap_error_defs";
  if (-e $defs_file ) {
    open FILE, "$defs_file" or die "Can't open $defs_file: $!\n";
    local $/=undef;
    my $file=<FILE>; close FILE;
    if ($file) {
      if (eval $file)
	  $self = $vap_error_defs;
      else {
	carp "Can't `eval' $defs_files: behavior may be erratic!\n"
      }
    }
  } else {
    carp "Can't find $defs_file! Behavior may be erratic!\n";
  }


  if (ref($self1) eq 'HASH') 
      while (my ($k,$v) = each $self1) {
	$self->{$k} = $v;
	delete $self1{$k}
      }

  if (!exist($self->{MAIL_ADDRESSES})) {
    $self->{MAIL_ADDRESSES} = $ENV{USER}. "\@" . $ENV{HOSTNAME}
  }
  return bless $self, $ref($class) || $class;

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
      carp "usage: $obj->ReportAndDie(subject [,message, address])\n"; 

  my $message=shift || "NULL MESSAGE";
  my $address = shift;
  my $addresses;
  if (ref($self->{MAIL_ADDRESSES} == 'array'))
      $addresses = join ", ", $self->{MAIL_ADDRESSES};
  else
      $addresses = $self->{MAIL_ADDRESSES};
  if (ref($address) == 'ARRAY')
      $address = join ", ",$address;

  $addresses = "$address,$addresses" 
  open PIPE, "|mailx -s$subject $addresses" or croak "Can't open `mailx':$!\n";
  print PIPE "$message\n";
  close PIPE;
  croak "And now, I die!\n";
  0;
}
1;
