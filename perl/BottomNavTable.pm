package BottomNavTable;
use Carp;
use strict;

BEGIN{

  # Get the defaults for the various VAP products, as well as for the
  # website itself

  #website defaults:

  my $defsfile = $ENV{VAP_LIBRARY} . "/VapWebsite_defs";
  croak("Can't find $defsfile!\n") unless (-e  $defsfile );
  require "$defsfile" || croak "Can't require $defsfile\n";



  # Overlay defaults
  $defsfile = $ENV{VAP_LIBRARY}."/overlay_defs_oo";
  croak "Can't find overlay_defs_oo!\n"
    unless (-e $defsfile);
  require "$defsfile" || croak "Can't require $defsfile\n";

  # Tropical storm defaults
  $defsfile=$ENV{VAP_LIBRARY}."/tropical_storm_defs_oo";
  croak "Can't find tropical_storm_defs_oo!\n" unless (-e $defsfile);
  require "$defsfile" || croak "Can't require $defsfile:$!\n";  

}
use lib ($ENV{VAP_LIBRARY}, $ENV{VAP_SFTWR_PERL});
use HTML::Table;
@BottomNavTable::ISA= qw/HTML::Table/;
use CGI;
use VapUtil;

sub new {
  my $class=shift;
  my $self=$class->SUPER::new(-rows=>3,-cols=>4, -valign=>"top",
			      @_);
  # arguments lacking the prefix '-' are considered appropriate only
  # to this object and are not used in the super class, so we go
  # through the passed array again.

  my $hash = {@_};
  my ($k,$v);
  while (($k,$v) = each %{$hash} ) {
    if ($k !~ /^-/) {
      $self->{uc($k)} = $v;
    }
  }


  $self->setStyle( "{ font-family: Verdana, sans-serif; font-size: 25% }");

  $self->{CGI} = CGI->new(-no_debug=>1);
  my $q=$self->{CGI};
  my ($link,$content);


    # ========= OVERLAYS ================

  my $overlay_table=HTML::Table->new(-cols=>1,-rows=>2,
				     -align=>'left');
  $overlay_table->setCaption("Overlays",'LEFT');
  $link=$q->a({-href=>"/overlay_Q.html"},"QuikSCAT");
  $overlay_table->setCell(1,1,$link);
  $link=$q->a({-href=>"/overlay_S.html"},"SeaWinds");
  $overlay_table->setCell(1,2,$link);
  $self->setCell(1,1,$overlay_table->getTable);


    # ========= Animations ================

  my $anim_table=HTML::Table->new(-cols=>1,-rows=>2,
				     -align=>'left');
  $link=$q->a({-href=>"/anim.html"},"Animations");
  $anim_table->setCell(1,1,$link);
  $self->setCell(1,2,$anim_table->getTable);


    # ========= Tropical Storms ================

  my $ts_table=HTML::Table->new(-cols=>1,-rows=>2,
				     -align=>'left');
  $ts_table->setCaption("Tropical Storms",'LEFT');
  $link=$q->a({-href=>"/ts_Q.html"},"QuikSCAT");
  $ts_table->setCell(1,1,$link);
  $link=$q->a({-href=>"/ts_S.html"},"SeaWinds");
  $ts_table->setCell(1,2,$link);
  $self->setCell(1,3,$ts_table->getTable);


  my $column=1;
  $link=$q->a({-href=>"status.html"},"Status");
  $content=$q->p({-align=>"RIGHT"},$link);
  $self->setCell(2,$column++,$content);

    # Cell 5,1 goes to 'Specials', if any.
  $link=$q->a({-href=>"special.html"},"Special Products");
  $content=$q->p({-align=>"RIGHT"},$link);
  $self->setCell(2,$column++,$content);

    # Cell 6,1 is for "information" about the website, 
  $link=$q->a({-href=>"info.html"},"Information");
  $content=$q->p({-align=>"RIGHT"},$link);
  $self->setCell(2,$column++,$content);

    # Cell 7,1 is the mailto url.
  my $webhost = $self->{WEBHOST}? $self->{WEBHOST}: "haifung.jpl.nasa.gov";
  $link=$q->a({-href=>"mailto: webmaster\@" . $webhost},"Contact");
  $content=$q->p({-align=>"RIGHT"},$link);
  $self->setCell(2,$column++,$content);

  $link=$q->a({-href=>"#TOP"},"Top");
  $self->setCell(3,1,$link);
  $self->setCellAlign(3,1,"CENTER");
  $self->setCellColSpan(3,1,4);
  
  return bless $self, ref($class) || $class;


}

1;
