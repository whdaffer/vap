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
  my $nrows=2;
  my $self=$class->SUPER::new(-valign=>"BOTTOM",@_);

# -rows=>$nrows,-cols=>1, 
# 			      -valign=>"BOTTOM", @_);

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


  $self->setStyle( "{ font-family: Arial, hevetica, Verdana, sans-serif;}");

  $self->{CGI} = CGI->new(-no_debug=>1);
  my $q=$self->{CGI};
  my ($link,$content);


  my $row=1;
  my $col=1;
    # ========= OVERLAYS ================

  my $overlay_table=HTML::Table->new();

#-cols=>2,-rows=>2,
#				     -align=>'center',
#				    -valign=>'BOTTOM'

  my $font=$q->font({-size=>'-1'},"Overlays");
  $overlay_table->setCell(1,1,$font);
  $overlay_table->setCellHead(1,1);
  $overlay_table->setCellAlign(1,1,'CENTER');
  $overlay_table->setCellVAlign(1,1,'CENTER');
  $overlay_table->setCellColSpan(1,1);

  $font=$q->font({-size=>'-3'},"QuikSCAT");
  $link=$q->a({-href=>"/overlay_Q.html"},$font);
  $overlay_table->setCell(1,2,$link);

  $font=$q->font({-size=>'-3'},"SeaWinds");
  $link=$q->a({-href=>"/overlay_S.html"},$font);
  $overlay_table->setCell(1,3,$link);

  $self->setCell($row,$col++,$overlay_table->getTable);



    # ========= Tropical Storms ================

  my $ts_table=HTML::Table->new();
# -cols=>2,-rows=>2,
# 				-align=>'CENTER'
  $font=$q->font({-size=>'-1'},"Tropical Storms");
  $ts_table->setCell(1,1,$font);
  $ts_table->setCellHead(1,1);
  $overlay_table->setCellAlign(1,1,'CENTER');
  $overlay_table->setCellVAlign(1,1,'CENTER');
#  $overlay_table->setCellColSpan(1,1,2);
  #$overlay_table->setCellNoWrap(1,1,2);

  $font = $q->font({-size=>'-3'},"QuikSCAT");
  $link=$q->a({-href=>"/ts_Q.html"},$font);
  $ts_table->setCell(1,2,$link);

  $font = $q->font({-size=>'-3'},"SeaWinds");
  $link=$q->a({-href=>"/ts_S.html",},$font);
  $ts_table->setCell(1,3,$link);

  $self->setCell($row, $col++,$ts_table->getTable);


    # ========= Animations ================

  #$col=1;
  #my $anim_table=HTML::Table->new(-cols=>1,-rows=>2,
  # -align=>'center');
  
  
  $link=$q->a({-href=>"/anim.html"},"Animations");
  #$anim_table->setCell(1,2,$link);
  #$self->setCell($row++, $col++,$anim_table->getTable);
  $self->setCell($row, $col++,$link);


    # The rest of the nav bar!
  #$col=1;
  $link=$q->a({-href=>"status.html"},"Status");
  $content=$q->p({-align=>"CENTER"},$link);
  $self->setCell($row,$col++,$content);

    # 'Specials', if any.
  $link=$q->a({-href=>"special.html"},"Specials");
  $content=$q->p({-align=>"CENTER"},$link);
  $self->setCell($row,$col++,$content);

    # "information" about the website, 
  $link=$q->a({-href=>"info.html"},"Info");
  $content=$q->p({-align=>"CENTER"},$link);
  $self->setCell($row,$col++,$content);

    #


#   my $webhost = $self->{WEBHOST}? $self->{WEBHOST}: "haifung.jpl.nasa.gov";
#   $link=$q->a({-href=>"mailto: webmaster\@" . $webhost},$q->font({-size=>'-1'},"Contact"));
#   $content=$q->p({-align=>"CENTER"},$link);
#   $self->setCell($row,$col,$content);

  $link=$q->a({-href=>"#TOP"},"Top");
  $self->setCell($row,$col,$link);

  #$self->setCellAlign($row,1,"CENTER");

  return bless $self, ref($class) || $class;


}

1;
