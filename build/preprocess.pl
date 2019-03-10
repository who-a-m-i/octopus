#!/usr/bin/env perl

my $fin         = $ARGV[0];
my $ndebug      = $ARGV[1] eq "no";
my $nline_num   = $ARGV[2] eq "no";
my $nf90_forall = $ARGV[3] eq "no";

#regular expression to match opening and closing parentesis
my $NestedGuts = qr{
  (?{ local $d=0 }) # Set depth to 0
  (?>
    (?:                  
      [^()]+        # you could see some non-parenthesis text
    | \( (?{$d++})  #  ...increment the depth
    | \) (?(?{ $d!=0 }) (?{ $d-- }) | (?!))     #  ...in which case decrement the depth...
    )*
  )
  (?(?{$d!=0})(?!))
}x;

open(IN,  "<$fin");

my $ncount_forall;

while($_ = <IN>)
{
  # First, eliminate the push_sub and pop_sub if the code is compiled in non-debug mode.
  # We replace it by an empty line as not to mess up the line numbers.
  if($ndebug){
      if (/push_sub/) {
          print "\n";
          next;
      }
      if (/pop_sub/) {
          print "\n";
          next;
      }
  }

  # Substitute "\newline" by a real new line.
  s/\\newline/\n/g;

  # Substitute "\cardinal" by "#".
  s/\\cardinal/#/g;

  if($nline_num){
      next if /^#/;
  }

  if($nf90_forall){
      if(m/^\s*forall\s*\(($NestedGuts)\)\s*(.*)$/xi){
	  print "! preprocessed forall\n";
	  my $i;
	  my $str  = $1;
	  my $body = $2;

	  # get rid of commas that are inside parenthesis
	  my $newstr = "";
	  my $dd = 0;
	  for($i=0; $i<length($str); $i++){
	      my $c = substr($str, $i, 1);
	      $dd++ if($c eq '(');
	      $dd-- if($c eq ')');
	      $newstr .= ($c eq ',' && $dd > 0) ? '#comma#' : $c;
	  }

	  my @loops = split(/,/x, $newstr);
	  $ncount_forall = $#loops + 1;
	  for($i=0; $i<$ncount_forall; $i++){
	      $loops[$i] =~ s/:/,/g;
	      $loops[$i] =~ s/#comma#/,/;
	      print "do ", $loops[$i], "\n";
	  }
	  if($body){
	      print "$body\n";
	      for($i=0; $i<$ncount_forall; $i++){
		  print "end do\n";
	      }
	  }
	  next;
      }
      if(m/^\s*end\s+forall/i){
	  my $i;
	  for($i=0; $i<$ncount_forall; $i++){
	      print "end do\n";
	  }
	  next;
      }
  }
  print;
}

close(IN);
