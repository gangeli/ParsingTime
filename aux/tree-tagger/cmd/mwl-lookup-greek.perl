#!/usr/bin/perl

$month{"Ιανουαρίου"} = 1;
$month{"Φεβρουαρίου"} = 1;
$month{"Μαρτίου"} = 1;
$month{"Απριλίου"} = 1;
$month{"Μαΐου"} = 1;
$month{"Μαϊου"} = 1;
$month{"Μαίου"} = 1;
$month{"Ιουνίου"} = 1;
$month{"Ιουλίου"} = 1;
$month{"Αυγούστου"} = 1;
$month{"Σεπτεμβρίου"} = 1;
$month{"Οκτωβρίου"} = 1;
$month{"Νοεμβρίου"} = 1;
$month{"Δεκεμβρίου"} = 1;

$month{"Ιανουάριο"} = 1;
$month{"Φεβρουάριο"} = 1;
$month{"Μάρτιο"} = 1;
$month{"Απρίλιο"} = 1;
$month{"Μάιο"} = 1;
$month{"Ιούνιο"} = 1;
$month{"Ιούλιο"} = 1;
$month{"Αυγούστο"} = 1;
$month{"Αύγουστο"} = 1;
$month{"Σεπτέμβριο"} = 1;
$month{"Οκτώβριο"} = 1;
$month{"Νοέμβριο"} = 1;
$month{"Δεκέμβριο"} = 1;

while (<>) {
    chomp;
    if ($_ eq '') {
	print_sentence();
    }
    else {
	push @token, $_;
    }
}
print_sentence();

sub print_sentence {
    for( $i=0; $i<=$#token; $i++ ) {
	if (exists $month{$token[$i]}) {
	    $start = $end = $i;
	    if ($token[$start-1] =~ /^[1-9][0-9]?([ηαο]ς?)?(-[1-9][0-9]?([ηαο]ς?)?)?$/){
		$start--;
	    }
	    if ($token[$start-1] eq 'Πέμπτη') {
		$start--;
	    }
	    if ($token[$end+1] eq 'του') {
		$end++;
	    }
	    if ($token[$end+1] =~ /^(1[0-9][0-9][0-9]|20[0-9][0-9]|'[0-9][0-9])$/) {
		$end++;
	    }
	    for( $k=$start; $k<$end; $k++) {
		$join[$k] = 1;
	    }
	}
	elsif (($token[$i] eq "απ'" && $token[$i+1] eq 'όλα') ||
	       ($token[$i] eq 'εν' && 
		($token[$i+1] eq 'λόγω' || $token[$i+1] eq 'όψει')))
	{
	    $join[$i] = 1;
	}
	       
    }
    for( $i=0; $i<=$#token; $i++ ) {
	if ($join[$i] == 1) {
	    print "$token[$i] "
	}
	else {
	    print "$token[$i]\n"
	}
    }
    undef @token;
    undef @join;
}
