#!/usr/bin/perl

package Roman;

use strict;
use warnings;
use Common qw(get_previous test_eq);

our @VERSION = '1.1';
our @ISA = qw(Exporter);
our @EXPORT = qw(facere);
our @EXPORT_OK = qw(barbarize romanize subtractive_notation);
our %romani = ('i' => 1,
              'v' => 5,
              'x' => 10,
              'l' => 50,
              'c' => 100,
              'd' => 500,
              'm' => 1000);
our @barbars = ('m', 'd', 'c', 'l', 'x', 'v', 'i');
our %thresholds = ('i' => 2,
                   'v' => 0,
                   'x' => 2,
                   'l' => 0,
                   'c' => 2,
                   'd' => 0,
                   'm' => 0);
our (@subtractors, $number);

sub facere
{
    # make it happen
    $number = shift;
    # populate @subtractors for later use
    for my $b (@barbars)
    {
        if($thresholds{$b} > 0)
        {
            push(@subtractors, $b);
        }
    }
    my $answer;
    # if an integer is supplied, translate it to a Roman numeral,
    # transform it to subtractive notation (e.g. MCCCCXVIIII -->
    # MCDXIX), render it in uppper-case, and return it
    if($number =~ m/^\d+$/)
    {
        $answer = &romanize($number);
        $answer = &subtractive_notation($answer);
        $answer = uc($answer);
        return $answer;
    }
    # if a Roman numeral is passed in (checked only for character
    # content, not well-formedness), translate it to ordinary
    # decimals and return it
    elsif($number =~ m/^[mdclxvi]+$/i)
    {
        $answer = &barbarize($number);
        return $answer;
    }
    # if anything else is passed in, send a smarmy warning message and exit with an error status
    else
    {
        warn("INPUT ERROR:  I'm not convinced that \'$number\' is a roman numeral or an integer\n") && exit 238;
    }
}

sub romanize
{
    # create a roman numeral from an integer
    my $barbar = shift;
    my $ret;
    my @nums   = split('', "$barbar");
    my $mult   = 10;
    # step through the digits of the number passed in
    # and convert each to the appropriate Roman numeral
    while(defined($nums[0]))
    {
        # shift the first value off the @nums array so we eventually
        # reach a termination point when we run out of digits
        my $digit = shift @nums;
        my $scale = scalar(@nums);
        # derive the appropriate number by multiplying this digit
        # by its scale (10 to the power of the number os digits 
        # right of this one), but don't inadvertently make it 0
        $digit    = $scale > 0 ? $digit * ($mult**$scale)
                               : $digit;
        my $i = 0;
        # run the number you just created through the sieve
        # by using the @barbars entries as a (Roman) numerically-ordered
        # lookup table to the integer values in %romani;
        # subtract the largest value smaller than $digit
        # from it, adding that Roman numeral to the
        # return variable as subtracted from $digit; recurse to
        # the next Roman numeral if necessary
        while($digit)
        {
            if($digit < int($romani{$barbars[$i]}))
            {
                $i++;
            }
            else
            {
                while($digit >= int($romani{$barbars[$i]}))
                {
                    $digit -= int($romani{$barbars[$i]});
                    $ret   .= $barbars[$i];
                }
            }
        }
    }
    # return the assembled Roman numeral; note that this is lower-case
    return $ret;
}

sub barbarize
{
    # derive an integer from a roman numeral
    # capture the remaining Roman numeral to process
    # and the integer calculated so far
    my ($roman, $barbar) = @_;
    # return the integer if there are no more Roman digits
    # without this exit point, this subroutine will endlessly loop!
    return $barbar if(!$roman);
    # split the full Roman number into Roman digits
    my @romans = split(//, $roman);
    # lower-case them all so they match our lookup tables
    map { $_ = lc($_) } @romans;
    my ($first, $second);
    # if $romans[1] doesn't exist, we're at the last digit
    if($romans[1])
    {
        ($first, $second) = (int($romani{$romans[0]}), int($romani{$romans[1]}));
        if($first >=  $second)
        {
            $barbar += $first;
            shift @romans;
        }
        else
        {
            $barbar += $second - $first;
            splice(@romans, 0, 2);
        }
    }
    # if we're at the last digit, we can just add the translated value
    else
    {
        $barbar += int($romani{$romans[0]});
        shift @romans;
    }
    # process the rest of the digits by recursion
    my $ret = &barbarize(join('', @romans), $barbar);
    return $ret;
}

sub subtractive_notation
{
    my $normal = shift;
    my ($ret, %translations);
    my $i = 0;
    # process the Roman numeral based on the valid set of subtractive elements ('c', 'x', 'i')
    # powers of 10 seem to be used for subtractive notation
    # i => 10**0
    # x => 10**1
    # c => 10**2
    # and were allowed to apply up to the next order of magnitude
    # e.g. i could be subtracted from v and x
    #      v could not be used in subtractive notation
    #      x could be subtracted from l and c
    #      l could not be used in subtractive notation
    #      c could be subtracted from d and m
    #      d could presumably not be used in subtractive notation
    #      m could presumably be subtracted from (5000) and (10000)
    while($subtractors[$i])
    {
        my $s = "$subtractors[$i]" . "{4}";
        my ($t_val, $s_val, $notation, $equal);
        # first look for digits inside the number
        # that are repeated 4 times
        # if found, check that:
        # nnnnn == n - (n++)
        # where 'n' is the repeating numeral found
        # and n++ is the next highest Roman numeral
        # e.g. cccc == cd
        # if they are equal, translate that section
        # of the numeral string
        while(my @suspects = $normal =~ m/(.)($s)/)
        {
            my ($low, $subtract_by) = ($1, $2);
            my $translate = $low . $subtract_by;
            my $repeated = $subtract_by;
            $subtract_by =~ s/(.){2,}/$1/;
            my $j = 0;
            while($barbars[$j])
            {
                if($low eq $barbars[$j])
                {
                    $notation = $subtract_by . $barbars[$j - 1];
                    $equal = &Common::test_eq(\&barbarize, $translate, $notation);
                    if($equal)
                    {
                        $normal =~ s/$translate/$notation/;
                        last;
                    }
                    else
                    {
                        my $next;
                        my $k = 0;
                        $next = &Common::get_previous($subtract_by, \@barbars);
                        ($t_val, $s_val) = ("$subtract_by$next", $repeated);
                        $equal = &Common::test_eq(\&barbarize, $t_val, $s_val);
                        if($equal)
                        {
                            $notation = $subtract_by . $next;
                            $normal   =~ s/$repeated/$notation/;
                            last;
                        }
                    }
                }
                $j++;
            }
        }
        # make sure to check for repeating digits at the beginning of the number too
        my $begin = $normal =~ m/^($s)/;
        if($begin)
        {
            $t_val = $1;
            $s_val = $subtractors[$i] . &Common::get_previous($subtractors[$i], \@barbars);
            $equal = &Common::test_eq(\&barbarize, $t_val, $s_val);
            if($equal)
            {
                $normal =~ s/^$t_val/$s_val/;
            }
        }
        $i++;
    }
    return $normal;
}

return 1;
