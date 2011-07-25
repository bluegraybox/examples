#!/bin/bash

declare -A flags values
flags=([d]=debug [h]=show_help)
values=([f]=foo [b]=bar)
flag_opt=""
value_opt=""
for x in "${!flags[@]}" ; do flag_opt="$flag_opt$x" ; done
for x in "${!values[@]}" ; do value_opt="$value_opt$x:" ; done

while getopts ":$flag_opt$value_opt" option ; do
    case $option in
        \?) echo "Unknown option -$OPTARG" ;;
        [$flag_opt])
            x=${flags[$option]}
            declare $x=${!x:-0}
            declare $x=$((${!x} + 1)) ;;
        [$value_opt]) declare ${values[$option]}="$OPTARG" ;;
    esac
done
shift $((OPTIND - 1))

echo Flags
for x in "${flags[@]}" ; do echo "$x=${!x}" ; done
echo Values
for x in "${values[@]}" ; do echo "$x=${!x}" ; done

