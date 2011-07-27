#!/bin/bash

# 

canonical=$(readlink -f $0)
dir=$(dirname $canonical)
script=$(basename $canonical)
# strip off the suffix
base=${script%.*}
# in case script starts with a period and doesn't have a suffix
[ -z $base ] && base=$script

# Load config
config=$dir/.${base#.}.cfg
[ -f $config ] && source $config

# Set default values if not loaded from config
# defaults associates variables with default values.
declare -A defaults=([foo]="my foo" [bar]="my bar" [baz]="sensible")
for x in ${!defaults[@]} ; do
    d=${defaults[$x]}
    declare $x="${!x:-$d}"
done
vars="${!defaults[@]}"  # keys of the defaults
for x in $vars ; do
    old_x="${!x}"
    # echo -n "What is $x? [${!x}] "
    read -p "What is $x? [${!x}] " new_x
    test -n "$new_x" && declare $x="$new_x" && echo "$x changed to '${!x}'"
done
for x in $vars ; do
    # if it's in the config, update it.
    if grep -q "^$x=" $config ; then
        sed -i "s/^$x=.*/$x=\"${!x}\"/;" $config
    else
        echo "$x=\"${!x}\"" >> $config
    fi
done

exit 0

#### NOTES and EXPERIMENTS

read -s -p "New password: " pass

echo "canonical=$canonical"
echo "dir=$dir"
echo "script=$script"
echo "base=$base"
echo "config=$config"

first=${1:-one}
second=${2:-two}
third=${3:-three}

echo "first=$first"
echo "second=$second"
echo "third=$third"

[ -f $config ] && source $config

declare -A meta
meta[canonical]=$(readlink -f $0)
meta[dir]=$(dirname ${meta[canonical]})
meta[script]=$(basename ${meta[canonical]})
# strip off the suffix
meta[base]=${meta[script]%.*}
[ -z ${meta[base]} ] && meta[base]=${meta[script]}

echo "meta[canonical]=${meta[canonical]}"
echo "meta[dir]=${meta[dir]}"
echo "meta[script]=${meta[script]}"
echo "meta[base]=${meta[base]}"



dir=${0%/*}
# '~' should be expanded, but just in case...
if   [ "${dir:0:2}" == '~/' ] ; then dir=$HOME${dir#~}
# relative path: chop off '.' and prefix current dir
elif [ "${dir:0:2}" == './' ] ; then dir=$(pwd)${dir#.}
# other relative path: prefix current dir
elif [ "${dir:0:1}" != '/'  ] ; then dir=$(pwd)/$dir ; fi

script=${0##*/}
base=${script%.*}
logfile=$dir/$base.log

declare -A meta
meta[script]=$script
meta[dir]=$dir
meta[base]=$base
meta[logfile]=$logfile
# echo "dir=$dir"
# echo "script=$script"
# echo "base=$base"
# echo "logfile=$logfile"
# echo "meta[dir]=${meta[dir]}"
# echo "meta[script]=${meta[script]}"
# echo "meta[base]=${meta[base]}"
# echo "meta[logfile]=${meta[logfile]}"

