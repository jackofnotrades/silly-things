#!/usr/bin/perl

package Common;

use strict;
use warnings;
use utf8;
require Exporter;

our @VERSION    = '1.0';
our @ISA        = qw(Exporter);
our @EXPORT     = qw();
our @EXPORT_OK  = qw(get_previous get_next test_eq get_elapsed get_elapsed_
                     human binary unbinary baseN unbaseN convert_baseN 
                     is_palindrome foldl foldr is_anagram divisors gcf
                     gcd lcm totient degree_of_perfection fac primes 
                     is_prime is_anagram is_pandigital mean stddev stddevmean
                     stderr zip pentagonal pascal collatz collatz2 n_choose_k
                     histo median mode range);

sub convert_baseN
{
    # use baseN and unbaseN
    my ($num, $from_base, $to_base) = @_;
    if(($from_base && 
        ($from_base !~ m/^\d+$/ || $from_base < 2 || $from_base > 64)) ||
        ($to_base &&
        ($to_base !~ m/^\d+$/ || $to_base < 2 || $to_base > 64)))
    {
        return "Error: base must be at least 2 and not more than 64\n";
    }
    my @base_chars;
    if($from_base == 64 or $to_base == 64) {
        @base_chars  =  ('A' ... 'Z');
        push(@base_chars, ('a' ... 'z'));
        push(@base_chars, (0 ... 9));
        push(@base_chars, '+', '/');
    } else {
        @base_chars = (0 ... 9);
        push(@base_chars, ('a' ... 'z'));
        push(@base_chars, chr(241));
        push(@base_chars, ('A' ... 'Z'));
        push(@base_chars, chr(209));
    }
    my @base_vals  = (0 ... 63);
    my (%from_base_map, %to_base_rev_map);
    my ($i, $ret);
    $ret = $num if($from_base == 10);
    die("Base values must be between 2 and 64") if($from_base < 2  ||
                                                   $from_base > 64 ||
                                                   $to_base   < 2  ||
                                                   $to_base   > 64);
    unless($from_base == 10 || $from_base < 2 || $from_base > 64)
    {
        $i = 0;
        while($base_vals[$i] < $from_base)
        {
            $from_base_map{$base_chars[$i]} = $base_vals[$i];
            $i++;
        }
        $ret = &unbaseN(\%from_base_map, $num);
    }
    unless($to_base == 10 || $to_base < 2 || $to_base > 64)
    {
        $i = 0;
        while(defined($base_vals[$i]) && $base_vals[$i] < $to_base )
        {
            $to_base_rev_map{$base_vals[$i]} = $base_chars[$i];
            $i++;
        }
        $ret = &baseN(\%to_base_rev_map, $ret);
    }
    return $ret;
}

sub get_previous
{
    # get the value of the previous (by index)
    # element in an array (passed in by reference), 
    # finding it based on a value also passed in
    # NOTE: returns first match; may not be appropriate for non-unique lists
    my ($curr, $list) = @_;
    my $i = 0;
    my $ret;
    while(defined($list->[$i]))
    {
        if($curr eq $list->[$i])
        {
            $ret = $list->[$i - 1];
            last;
        }
        $i++;
    }
    return $ret;
}

sub get_next
{
    # get the value of the next (by index)
    # element in an array (passed in by reference),
    # finding it based on a value also passed in
    # NOTE: returns first match; may not be appropriate for non-unique lists
    my ($curr, $list) = @_;
    my $i = 0;
    my $ret;
    while(defined($list->[$i]))
    {
        if($curr eq $list->[$i])
        {   
            $ret = $list->[$i - 1];
            last;
        }
        $i++;
    }
    return $ret;
}

sub test_eq
{
    # test the return values from a subroutine
    # (passed by reference) for two values 
    # passed in; returns 1 if equal, 0 if not
    my ($subref, $val1, $val2) = @_;
    return (&$subref($val1) eq &$subref($val2)) ? 1
                                                : 0;
}

sub get_elapsed
{
    # derive elapsed time from two timestamps (from Perl time())
    # and return it in format ww:dd:hh:mm:ss
    # only returns sets necessary, so 01:32:06, not 00:00:01:32:06
    my ($start, $finish) = @_;
    die("INPUT ERROR: $start does not appear to be a valid seconds integer") if($start !~ m/^\d+$/);
    die("INPUT ERROR: $finish does not appear to be a valid seconds integer") if($finish !~ m/^\d+$/);
    my $elapsed = $finish - $start;
    my @units = (604800, 86400, 3600, 60, 1);
    my $i = 0;
    my $ret = '';
    my $multiplier;
    while($units[$i])
    {
        if($elapsed >= $units[$i])
        {
            while($elapsed >= $units[$i])
            {
                $elapsed -= $units[$i];
                $multiplier++;
            }
        }
        $ret = defined($multiplier) ?  $multiplier > 0 ? $ret .  ":" . sprintf('%02d', $multiplier)
                                                       : $ret . ''
                                    : defined($ret) ? $ret . ''
                                                    : '';
        $i++;
    }
    $ret =~ s/(^\:|\:$)//g;
    return $ret;
}

sub get_elapsed_human {
    # parse a get_elapsed return and put it into human units
    # e.g. 01:32:06 --> 1 hour 32 minutes 6 seconds
    my ($timestamp1, $timestamp2) = @_;
    my $timestamp;
    if($timestamp2) {
        $timestamp = &get_elapsed($timestamp1, $timestamp2);
    } else {
        $timestamp = $timestamp1;
        die("INPUT ERROR: $timestamp does not look like a valid &get_elapsed() timestamp") if($timestamp !~ m/^\d+[\:\d]*$/);
    }
    my @pieces = split(/\:/, $timestamp);
    my @units = ('week', 'day', 'hour', 'minute', 'second');
    my $i = scalar(@units) - 1;
    my $ret;
    while(defined($pieces[0])) {
        my $chunk = pop(@pieces);
        $ret .= defined($ret) ? &time_unitize($units[$i], int($chunk)) . $ret
                             : &time_unitize($units[$i], int($chunk));
        $i--;
    }
    return $ret;
}

sub time_unitize {
    # pluralize time units as appropriate
    my ($unit, $value) = @_;
    die("$value is not an integer") if($value !~ m/^\d+$/);
    return $value > 0 ? $value > 1 ? "$value $unit" . "s "
                                   : "$value $unit "
                      : '';
}

sub binary {
    my $n = $_[0];
    return $n if( $n == 0 || $n == 1 );
    my $k = int($n/2);
    my $b = $n % 2;
    my $E = &binary($k);
    return $E . $b; 
}

sub unbinary {
    return if(!defined($_[1]) or $_[1] eq '');
    my @bits  = split(//, $_[1]);
    my $bit   = shift @bits;
    my $scale = scalar(@bits);
    my $mult  = 2**$scale;
    my $num   = $_[0] + (int($bit) * $mult);
    my $left  = join('', @bits);
    $num     += &unbinary(0, $left) if($left);
    return $num;
}

sub baseN {
    my ($rmap, $num) = @_;
    my $base = scalar(keys %{$rmap});
    return $rmap->{$num} if($rmap->{$num});
    my $k = int($num/$base);
    my $b = $num % $base;
    my $E = &baseN($rmap, $k);
    return $E . $rmap->{$b};
}

sub unbaseN {
    my ($map, $undone) = @_;
    my $valid = "a-z" . chr(241) . "A-Z" . chr(209);
    return 0 if( $undone !~ m/^[$valid\d]+$/);
    my $base       = scalar(keys %{$map});
    my $scale      = length($undone);
    my $digital    = $undone =~ m/^\d+$/;
    my @bits        = split(//, sprintf("%0$scale" . "s", $undone));
    my $bit        = sprintf('%01s', shift @bits);
    $undone        = join("", @bits);
    $scale         = scalar(@bits);
    my $multiplier = $base**$scale;
    my $done         += int($map->{$bit}) * $multiplier;
    $done         += &unbaseN($map, $undone);
    return $done;
}

sub is_palindrome {
    my $p              =  shift;
    my $length         =  length($p);
    my $is_palindrome  =  0;
    my @elements       =  split(//, $p);
    my ($head, $tail);
    $head  =  join('', @elements[0 .. (int($length / 2) - 1)]);
    if($length % 2 == 0) {
        $tail  =  join('', reverse @elements[(int($length / 2)) .. (scalar(@elements) - 1)]);
    } else {
        $tail  =  join('', reverse @elements[((int($length / 2)) + 1) .. (scalar(@elements) - 1)]);
    }
    if($head eq $tail) {
        $is_palindrome++;
    }
    return $is_palindrome;
}

sub foldr {
    my ($op, $first, @list)  =  @_;
    return &fold($op, $first, (reverse @list));
}

sub foldl {
    return &fold(@_);
}

sub fold {
    my ($op, $first, @list)  =  @_;
    my $result               =  $first;
    while(defined($list[0])) {
        $result   =  eval ( "$result$op" . (shift @list) );
    }
    return $result;
}

sub zip {
    # canonical functional programming zip function
    # take a set of lists (refs to them, actually)
    # and return a list (array) of (references to) n-tuples (smaller arrays)
    my @lists       =  @_;
    my $ref_length  =  scalar(@{$lists[0]});
    for my $list (@lists) {
        die("Common::zip:\n\tError:\tuneven list sizes") if(scalar(@{$list}) != $ref_length);
    }
    my $idx  =  0;
    my @return;
    while(defined($lists[0]->[$idx])) {
        my @tuple;
        for my $list (@lists) {
            push(@tuple, $list->[$idx]);
        }
        push(@return, \@tuple);
        $idx++;
    }
    return @return;
}

sub is_prime {
    my $num  =  shift;
    my (%primes, %checked);
    map { $primes{$_}++; } (2 .. $num);
    for my $n (sort { $a <=> $b } keys %primes) {
        next if($checked{$n});
        $checked{$n}++;
        my $multiplier  =  2;
        while(my $multiple = $n * $multiplier <= $num) {
            if($multiple == $num) {
                return 0;
            }
            delete $primes{$multiple};
        }
    }
    if(exists($primes{$num})) {
        return 1;
    } else {
        return 0;
    }
}

sub is_anagram {
    my ($first, $second)  =  @_;
    my (%first, %second);
    if($first =~ m/[a-z]+/i) {
        $first  =  lc($first);
    }   
    if($second =~ m/[a-z].+/i) {
        $second  =  lc($second);
    }   
    @first{  (split('', $first))  }  =  1;
    @second{ (split('', $second)) }  =  1;
    my $ordered_first                =  join('', sort { $a cmp $b } keys %first);
    my $ordered_second               =  join('', sort { $a cmp $b } keys %first);
    if($ordered_first eq $ordered_second) {
        return 1;
    } else {
        return 0;
    }
}

sub divisors {
    my $num  =  shift;
    my %divisors;
    for my $d (1 .. int($num / 2)) {
        $divisors{$d}++ if($num % $d == 0);
    }
    return keys %divisors;
}

sub gcf {
    my ($num1, $num2)  =  @_;
    my @divs1          =  reverse sort { $a <=> $b } (&divisors($num1));
    my @divs2          =  reverse sort { $a <=> $b } (&divisors($num2));
    my $ret            =  $divs1[0] > $divs2[0] ? $divs1[0] : $divs2[0];
    return $ret;
}

sub lcm {
    my ($num1, $num2)  =  @_;
    my $limit          =  $num1 * $num2;
    my (%m1, %m2);
    for my $m ( [$num1, \%m1], [$num2, %m2] ) {
        my $multiplier  =  1;
        while(my $n = $m->[0] * $multiplier <= $limit) {
            $m->[1]->{$n}++;
            $multiplier++;
        }
    }
    for my $k (sort { $a <=> $b } keys %m1) {
        if(exists $m2{$k}) {
            return $k;
        }
    }
    return $limit;
}

sub gcd {
    my ($a, $b)  =  @_; 
    while($b) {
        ($a, $b)  =  ($b, $a % $b);
    }   
    return $a; 
}

sub totient {
    my $p  =  my $number  =  shift;
    my $t  =  0;  
    while($p > 0) {
        if(&gcd($p, $number) == 1) {
            $t++;
        }   
        $p--;
    }   
    return $t; 
}

sub degree_of_perfection {
    my $num      =  shift;
    my @divs     =  &divisors($num);
    my $div_sum  =  &foldl('+', $divs[0], @divs[1.. (scalar(@divs) - 1)]);
    my $ret      =  $div_sum > $num ? 1 : $div_sum < $num ? (-1) : 0;
    return $ret;
}

sub digit_fac_sum {
    my $num   =  shift;
    my @num   =  split(//, $num);
    my @facs;
    map { push(@facs, &fac($_)); } @num;
    return &foldl('+', shift @facs, @facs);
}

sub fac {
    my $num  =  shift;
    my $sum  =  1;
    while($num) {
        $sum *= $num;
        $num--;
    }
    return $sum;
}

sub primes {
    # return the list of prime numbers up to the supplied limit
    my $limit         =  shift;
    my %primes;
    map { $primes{$_}++; } (2 .. $limit);
    for my $n (2 .. $limit) {
        my $multiplier  =  2;
        while((my $multiple = $n * $multiplier) <= $limit) {
            #print "Removing $n * $multiplier = $multiple\n";
            delete $primes{$multiple};
            $multiplier++;
        }
    }
    return sort { $a <=> $b } keys %primes;
}

sub is_pandigital {
    my $num     =  shift;
    my $digits  =  length($num);
    return 0 if($digits > 9);
    my %digits;
    map { $digits{$_}++; } (split(//, $num));
    for my $n (1 .. $digits) {
        if(!exists $digits{$n}) {
            return 0;
        }
    }
    return 1;
}

#sub rgb_to_hsl {
    # convert an RGB triplet to an HSL triplet
#    my ($red, $green, $blue)  =  @{shift}{'red', 'green', 'blue'};
#    my (%max, %min, %hsl);
#    for my $color (\[$red, 'red'], \[$green, 'green'], \[$blue, 'blue']) {
#        if(!defined($max{'value'}) || $color->[0] > $max{'value'}) {
#            $max{'color'}  =  $color->[1];
#            $max{'value'}  =  $color->[0];
#        } elsif(!defined($min{'value'}) || $color->[0] < $min{'value'}) {
#            $min{'color'}  =  $color->[1];
#            $min{'value'}  =  $color->[0];
#        }
#    }
#    $hsl{'luminosity'}  =  ($max{'value'} + $min{'value'}) / 2;
#    if($max{'value'} == $min{'value'}) {
#        $hsl{'hue'}         =  0;
#        $hsl{'saturation'}  =  0;
#    } elsif($max{'color'} eq 'red') {
#        $hsl{'hue'}         =  (60 * (($green - $blue)/($max{'value'} - $min{'value'})) + 360) % 360;
#    } elsif($max{'color'} eq 'green') {
#        $hsl{'hue'}  =  (60 * (($blue - $red) / ($max{'value'} - $min{'value'}))) + 120;
#    } elsif($max{'color'} eq 'blue') {
#        $hsl{'hue'}  =  (60 * (($red - $green) / ($max{'value'} - $min{'value'}))) + 240;
#    }
#    if($hsl{'luminosity'} <= 0.5) {
#        $hsl{'saturation'}  =  ($max{'value'} - $min{'value'}) / (2 * $hsl{'luminosity'});
#    } else {
#        $hsl{'saturation'}  =  ($max{'value'} - $min{'value'}) / (2 - (2 * $hsl{'luminosity'}));
#    }
#    return \%hsl;
#}

#sub hsl_to_rgb {
#    # convert an HSL triplet to an RGB triplet
#    my ($hue, $saturation, $luminosity)  =  @{shift}{'hue', 'saturation', 'luminosity'};
#
#}

sub complement_color {
    # invert the hue of an RGB triplet and return the corresponding RGB triplet
}

sub approximate_complement {
    # find an appropriate color to overlay text onto an (possibly heterogenously colored) image; return the RGB triplet
}

sub mean {
    # find the mean of a group of samples
    return ((&foldl('+', 0, @_)) / scalar(@_));
}

sub median {
    # find the median of a group of samples
    my @sorted  =  @_;
    @sorted     =  sort { $a <=> $b } @sorted;
    if(scalar(@sorted) % 2 == 0) {
        my $half  =  int(scalar(@sorted) / 2);
        return (($sorted[$half] + $sorted[$half + 1]) / 2);
    } else {
        return $sorted[int((scalar(@sorted) + 1) / 2)];
    }
}

sub mode {
    # find the mode(s) of a group of samples
    my (%members, @modes);
    for my $sample (@_) {
        $members{$sample}++;
    }
    my $highest  =  0;
    for my $member (reverse (sort { $members{$a} <=> $members{$b} } keys %members)) {
        if($members{$member} >= $highest) {
            push(@modes, $member);
            $highest  =  $members{$member} if($members{$member} > $highest);
        }
    }
    return @modes;
}

sub range {
    my @sorted  =  sort { $a <=> $b } @_;
    return ($sorted[-1] - $sorted[0]);
}

sub stddev {
    # find the standard deviation of a group of samples
    my @sample  =  @_;
    my $mean    =  &mean(@sample);
    my $distances;
    for my $s (@sample) {
        $distances += (($mean - $s)**2);
    }
    return (sqrt($distances / (scalar(@sample) - 1)));
}

sub stddevmean {
    # find the standard deviation of the mean of a group of samples
    my @sample  =  @_;
    return (&stddev(@sample) / (sqrt(scalar(@sample))));
}

sub stderr {
    # find standard error, given standard deviation and reliability index
    my ($stddev, $reliability)  =  @_;
    return ($stddev * sqrt(1 - $reliability));
    #or just
    #return((shift) * (sqrt(1 - (shift))));
}

sub pentagonal {
    # make pentagonal numbers up to a limit
    my $limit = shift;
    my $n     = 1;
    my $done;
    my ($idx, $best_diff, $best_idx, $worse, %pents) = (0, $limit); 
    while(!$done) {
        print "\r", '#' x length($n);
        print "\r\t\t$best_idx" if(defined($best_idx));
        my $pent      = ($n * (3 * $n - 1) / 2);
        my $next_pent = (($n + 1) * (3 * ($n + 1) - 1) / 2);
        my $sum       = $pent + $next_pent;
        my $diff      = $next_pent - $pent;
        $pents{$pent}++;
        $pents{$next_pent}++;
        if(!exists($pents{$sum}) || !exists($pents{$diff})) {
            $idx++;
            $n++;
            next;
        } else {
            if($diff < $best_diff) {
                $best_idx  = $idx;
                $best_diff = $diff;
                print $n, " --> ", $pent, " --> $best_diff\n";
            } elsif($diff > $best_diff) {
                $worse++;
                $done++ if($n >= $limit && defined($best_idx) && $worse >= 10);
            }
            $n++;
            $idx++;
        }
    }
}

sub pascal {
    # create a Pascal's triangle to a grid size
    my $grid_size  =  shift;
    my @grid;
    push(@{$grid[0]}, 1);
    push(@{$grid[1]}, 1, 1);
    my $row  =  2;
    while($row <= ($grid_size * 2)) {
        push(@{$grid[$row]}, 1);
        my $idx  =  0;
        while(defined($grid[($row - 1)][($idx + 1)])) {
            push(@{$grid[$row]}, ($grid[($row - 1)][$idx] + $grid[($row - 1)][($idx + 1)]));
            $idx++;
        }
        push(@{$grid[$row]}, 1);
        $row++;
    }
    print $grid[($grid_size * 2)][$grid_size], "\n";
    #print Dumper(\@grid), "\n";
}

sub collatz {
    my $n = shift;
    die("Must be a positive natural number (not $n)") if($n != int($n) or $n < 0);
    print"$n\n";
    while($n > 1) {
        $n = ($n % 2 == 0) ? ($n / 2) : ($n * 3 + 1);
        print "$n\n";
    }
}

sub collatz2 {
    my $n = shift;
    die("Must be a non-zero natural number (not $n)") if($n != int($n) or $n == 0);
    print "$n\n";
    while(abs($n) != 1) {
        $n = ($n % 2 == 0) ? ($n / 2) : ($n > 1) ? (($n * 3 + 1) / 2) : (($n * 3 - 1) / 2);
        print"$n\n"
    }
}

sub n_choose_k {
    my ($n, $k)  =  @_;
    die("n and k must be integers") if($n != int($n) or $k != int($k));
    my ($N, $K)  =  ($n, 1);
    for my $num (($n-($k+1)) .. ($n - 1)) {
        $N *= $num;
    }
    for my $kay (1 .. $k) {
        $K *= $kay;
    }
    return ($N / $K);
}

sub histo {
    use Toolbox qw(use_the_schwartz);
    my @data  =  @_;
    my (%data, $sort_numerically, $sort_alphabetically);
    for my $datum (@data) {
        $data{$datum}++;
        if($datum =~ m/^\d+$/) {
            $sort_numerically++;
        } elsif($datum =~ m/^\w+$/) {
            $sort_alphabetically++;
        } else {
            $sort_numerically++;
            $sort_alphabetically++;
        }
    }
    my $sort  =  $sort_numerically && $sort_alphabetically ? sub { return &use_the_schwartz(\@_); } 
                                                           : $sort_alphabetically ? sub { return ($_[0] cmp $_[1]); } 
                                                                                  : $sort_numerically ? sub { return ($_[0] <=> $_[1]); } 
                                                                                                      : sub { return &use_the_schwartz(\@_); };
    my ($high_val, $longest_label, @labels)  =  (0, 0);
    for my $datum (sort { &$sort($a, $b) } keys %data) {
        push(@labels, $datum);
        $high_val = $data{$datum} if($data{$datum} > $high_val);
        $longest_label = length($datum) if(length($datum) > $longest_label);
    }
    my $report = my $blank_line  =  " " x (scalar(@labels) * ($longest_label + 2)) . "\n";
    my $half_longest  =  int($longest_label / 2);
    my $target  =  int((length($blank_line) - 1) / scalar(@labels));
    for my $line (reverse (1 .. $high_val)) {
        $report .= "|";
        for my $l (@labels) {
            my $diff    =  int(($target - 2) / 2);
            my $even    =  ($target % 2 == 0);
            my $prefix  =  " " x $diff;
            my $suffix  =  " " x ($even ? ($diff - 1) : $diff);
            $report    .=  $prefix . (($data{$l} >= $line) ? "#" : " ") . $suffix . "|";
        }
        $report .= "\n";
    }
    $report .=  "|";
    for my $label (@labels) {
        my $diff    =  ($target - length($label)) - 1;
        my $prefix  =  " " x int($diff / 2);
        my $suffix  =  " " x ($diff % 2 == 0 ? int(($diff - 1) / 2) : int($diff / 2));
        $report    .=  $prefix . $label . $suffix . "|";
    }
    print "$report\n";
}

1;
