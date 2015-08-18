#!/usr/bin/env perl

################################################
##   Main function that invokes the subfunctions
################################################
{
    my $coderFile = $ARGV[0];
    convert_cdr_to_m($coderFile,'');
}
##################################################
## End of Main Function and start of subfunctions
##################################################

sub convert_cdr_to_m {

  my $coderFile = shift;
  my $forceRebuildAll = shift;
  
  my $mFile = $coderFile;
  $mFile =~ s/\.cdr/\.m/ ;
  my $pFile = $coderFile;
  $pFile =~ s/\.cdr/\.p/ ;


  my $coderTime = (stat $coderFile)[9];
  my $mTime     = (stat $mFile)[9];
  my $pTime     = (stat $pFile)[9];

  if ( (not defined($mTime)) or ($mTime eq '') ) {
    $mTime = 0;
  }

  if ( (not defined($pTime)) or ($pTime eq '') ) {
    $pTime = 0;
  }


  if($forceRebuildAll ne 'force') {
    if($coderTime < $mTime) {
      return;
    }
  }

  # Delete P and M files associated with the coder file
  unlink($pFile);
  unlink($mFile);

  open(coderFileH, "<$coderFile") or die "Cannot open $coderFile: $!\n";
  open(mFileH, ">$mFile") or die "Cannot create $mFile: $!\n";

  while ($line = <coderFileH>) {
     # Check for code template lines. These start with special chars
     # a) "..." fprintf template line
     # b) "$$$" sprintf string concat template line
     # c) "###" reset the current string template
      
     $params = "";
     if ($line =~ /^\.\.\./){
        ## Line started with "..." so this is template line for Coder fprintf
        ## substitute all string variables


        transform_line();
        $line = "fprintf(file,\'$line\\n\'$params);\n";

        print mFileH "$line";
     } elsif ($line =~ /^\$\$\$/){
        ## Line started with "$$$" so this is template line for Coder sprintf

        transform_line();
        print mFileH "SF_CODER_STR=[SF_CODER_STR,sprintf(\'$line\\n\'$params)];\n";
     } elsif ($line =~ /^\#\#\#/){
        ## Line started with "###" so this is a reset Coder sprintf line
        print mFileH "SF_CODER_STR='';\n";
     } else {
        print mFileH $line
     }
  }
  close mFileH;
  close coderFileH;

  # Change the permission of the generated mFile to read only
  chmod 0555, $mFile;

  print "Created $mFile\n";
}

sub transform_line {

    $line =~ s/^...//; # get rid of ... at the start of the line
    $line =~ s/[\r\n]+$//; # get rid of cr/lf chars at the end of the line

    $params = "";


     # IMPORTANT: we first fill the locations of %.17g with a string token named
     # SFDOUBLEZZ, and the locations of %s with a string token named SFSTRINGZZ.
     # since we need to double up ONLY the legitimate % chars in $line. After we
     # double up % chars in line, we replace SFDOUBLEZZ and SFSTRINGZZ by appropriate
     # format specifiers (i.e. %.17g and %s)
     # This means that the coder file must not use variables named
     # SFSTRINGZZ and SFDOUBLEZZ

    while ($line =~ /\$([^\$]+)\$/) {
       # a string variable is detected
       if ($1=~'^\#') {
         # it should be regarded as a numeric string $#numericVar$
         $line =~ s/\$\#([^\$]+?)\$/SFDOUBLEZZ/;
         $params = "$params,$1";
       } else {
          # a normal string variable $stringVar$
         $line =~ s/\$([^\$]+?)\$/SFSTRINGZZ/;
         $params = "$params,$1";
       }

    }

    # we double up all the legitimate % chars
    $line =~ s/\%/\%\%/g;

    # we replace our placeholder tokens by appropriate
    # format specifiers
    $line =~ s/SFDOUBLEZZ/\%\.17g/g;
    $line =~ s/SFSTRINGZZ/\%s/g;

    # double up all single quote chars
    $line =~ s/\'/\'\'/g;

    # double up all back-slash chars
    $line =~ s/\\/\\\\/g;

}
