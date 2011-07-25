#!/bin/bash

function help_msg {
    echo "Usage: $0 [-ac] [-b param] [arg ...]"
    echo "    -a          Set flag a"
    echo "    -b param    Specify a 'b'"
    echo "    -d          Debug (specify multiple to increase level)"
    echo "    -h          Show help text"
    exit 0
}

function usage {
    echo "invalid parameter -$1"
    echo "Usage: $0 [-ad] [-b param] [arg ...]"
    exit 1
}

while getopts ":ab:dh" option ; do
    case $option in
        a) a=true ;;
        b) b=$OPTARG ;;
        d) d=${d:-0} ; d=$(($d + 1)) ;;
        h) help_msg ;;
        \?) usage $OPTARG ;;
    esac
done
echo "a=$a"
echo "b=$b"
echo "d=$d"

shift $((OPTIND - 1))
echo "1=$1"
echo "2=$2"
echo "3=$3"
echo "4=$4"
echo "5=$5"
echo "6=$6"

