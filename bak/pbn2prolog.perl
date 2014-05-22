#!/usr/bin/perl

open my $pbnfile, $ARGV[0] ;

$output = $ARGV[0] ;
$output =~ s/bn$/l/ ;

open my $db, '>', $output ; 

process_pbnfile ($pbnfile);

close $pbnfile ;
close $db ;

sub process_pbnfile {
  my ($pbnfile) = @_ ;
  my $comment = 0 ;

  print $db "deal([" ;

  for my $line (<$pbnfile>) {

    $line =~ s/^%.*$/ / ;

    if ( $line =~ /{.*}/ ) {
    }
    elsif ( $comment == 0  and $line =~ /^\s*{/ ) 
      { $comment = 1 ; 
      }
    elsif ( $comment == 1 and $line =~ /}/ ) 
      { $comment = 0 ;
      }
    elsif ( $comment == 0 ) 
      { unless ( $line =~ /^\*\s*$/ ) {
	  process_pbnline($line) ;
	} 
      } 
  }

  print $db "end_of_deal ]).\n" ;
}

sub process_pbnline {
  my ($line) = @_ ;

  $line =~ s/^\s*$/end_of_deal]).\ndeal([\n/ ;
  $line =~ s/^\[(\w+)\s+(.*)\]$/tag('$1', $2),/ ;
  $line =~ s/"(.*)"(.*)"(.*)"/"$1'$2'$3"/ ;
  
  $line = pbn_date( $line ) ;
  $line = pbn_number( $line ) ;
  $line = pbn_nsew( $line ) ;
  $line = pbn_contract( $line ) ;
  $line = pbn_scoring( $line ) ;
  $line = pbn_vulnerability ( $line ); 
   
  $line = pbn_deal( $line ) ;

  $line = pbn_bidround( $line ) ;
  $line = pbn_playround( $line ) ; 


  # final cleanup

  $line =~ s/'(\w)'/lc($1)/eg ;

  print $db $line ;
}
 
sub pbn_deal {
  my ($line) = @_ ; 

  $line =~ s/tag\('Deal',\s+"(\w):(\w*)\.(\w*)\.(\w*)\.(\w*)\s+(\w*)\.(\w*)\.(\w*)\.(\w*)\s+(\w*)\.(\w*)\.(\w*)\.(\w*)\s+(\w*)\.(\w*)\.(\w*)\.(\w*)"/tag('Deal', ['$1',[[$2],[$3],[$4],[$5]],[[$6],[$7],[$8],[$9]],[[$10],[$11],[$12],[$13]],[[$14],[$15],[$16],[$17]]]/ ;

  if ( $line =~ /tag\('Deal'/ ) {
    $line =~ s/([AKQJT98765432])([AKQJT98765432])/$1,$2/g ;
    $line =~ s/([AKQJT98765432])([AKQJT98765432])/$1,$2/g ;
    $line =~ s/A/a/g ;
    $line =~ s/K/k/g ;
    $line =~ s/Q/q/g ;
    $line =~ s/J/j/g ;
    $line =~ s/T/t/g ;
  }

  return $line;
}

sub pbn_bidround {
  my ($line) = @_ ;
  
  $line =~ s/^[Aa][Pp].*/br([ap / ;
  $line =~ s/^[Pp]ass([\!\?][\!\?]?)\s+/br([pass $1 / ;
  $line =~ s/^[Pp]ass\s+/br([pass / ;
  $line =~ s/^[Xx][Xx]([\!\?][\!\?]?)\s+/br([xx $1 / ;
  $line =~ s/^[Xx][Xx]\s+/br([xx / ;
  $line =~ s/^[Xx]([\!\?][\!\?]?)\s+/br([x $1 / ;
  $line =~ s/^[Xx]\s+/br([x / ;
  $line =~ s/^([1-7])([SHDCN][T]?)([\!\?][\!\?]?)\s+/br([[$1,'$2'] $3 / ;
  $line =~ s/^([1-7])([SHDCN][T]?)\s+/br([[$1,'$2'] / ;
  $line =~ s/^(br\(\S*) (.*)$/$1 $2]),/ ;
  
  if ( $line =~ /br\(/ ) {
    $line =~ s/\s+[Pp]ass/ ,pass/g ;
    $line =~ s/\s+[Aa][Pp]/ ,ap/ ;
    $line =~ s/\s+\*/ ,ap/ ;
    $line =~ s/([\!\?][\!\?]?)/, note('$1')/g ;
    $line =~ s/\$(\d+)/,note(nag\/$1)/g ;
    $line =~ s/\s+=(\d+)=/ ,note($1)/g ;
    $line =~ s/\s+[Xx][Xx]/ ,xx/g ;
    $line =~ s/\s+[Xx]/ ,x/g ;
    $line =~ s/\s+([1-7])([SHDCN][T]?)/ ,[$1,'$2']/g ;
    $line =~ s/'NT'/'N'/g ;
  } 

  return $line;
}

sub pbn_playround {
  my ($line) = @_ ;

  if ( $line =~ /^[-SHDC]/ ) {
    $line =~ s/^[-]/pr(nocard / ;
    $line =~ s/^([SHDC])([AKQJT98765432])/pr(['$1','$2'] / ;
    $line =~ s/[-]/, nocard/g ;
    $line =~ s/([SHDC])([AKQJT98765432])/, ['$1','$2']/g ;
    $line =~ s/([\!\?][\!\?]?)/, note('$1')/g ;
    $line =~ s/\$(\d+)/, note(nag\/$1)/g ;
    $line =~ s/=(\d+)=/, note($1)/g ;

    $line =~ s/pr\((.*$)/pr($1),/ ;
  }
  return $line ;
}
 
sub pbn_contract {
  my ($line) = @_ ;
  if ( $line =~ /tag\('Contract'/ ) {
    $line =~ s/"[Pp]ass"/pass/ ;
    $line =~ s/"(\d)([NnSsHhDdCc][Tt]?)([Xx][Xx]?)"/[$1,'$2','$3']/ ;
    $line =~ s/"(\d)([NnSsHhDdCc][Tt]?)"/[$1,'$2']/ ;
    $line =~ s/'NT'/'N'/g ;  
    $line =~ s/XX/xx/g ;  
    $line =~ s/X/x/g ;    
  }
  return $line ;
}


sub pbn_date {
  my ($line) = @_ ;
  if ( $line =~ /tag\('Date'/ ) {
    $line =~ s/\?/0/g ;
    $line =~ s/"(\d{4})\.(\d{2})\.(\d{2})"/[$1,$2,$3]/ ;
  }
  return $line ;
}

sub pbn_number {
  my ($line) = @_ ;
  my @tags = ("Board", "Result");

  if ( ($line =~ /tag\('(\w+)'/) and ($1 ~~ @tags )) {
    $line =~ s/"(\d+)"/$1/ ;
    $line =~ s/"\D+"/void/ ;
  } 
  return $line ;
}

sub pbn_nsew {
  my ($line) = @_ ;
  my @tags = ("Dealer", "Deal", "Declarer", "Auction", "Play");

  if ( ($line =~ /tag\('(\w+)'/) and ($1 ~~ @tags )) {
    $line =~ s/"(\w)"/'$1'/ ;
  } 
  return $line ; 
}

sub pbn_scoring {
  my ($line) = @_ ;

  if ( $line =~ /tag\('Scoring'/ ) {
    $line =~ s/"(\w+)"/'$1'/ ;
  } 
  return $line ; 
}

sub pbn_vulnerability {
  my ($line) = @_ ;

  if ( $line =~ /tag\('Vulnerable'/ ) {
    $line =~ s/"(\w+)"/'$1'/ ;
  } 
  return $line ; 
}

