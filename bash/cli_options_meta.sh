#!/bin/bash

function get_my_opts () {
    # echo "param count=${#@}"
    # echo "params=" "$@"
    # for x in "$@" ; do echo "param=$x" ; done
    local -A _flags=$1 _values=$2
    shift 2
    local flag_opt=""
    local value_opt=""
    local x
    for x in "${!_flags[@]}" ; do flag_opt="$flag_opt$x" ; done
    for x in "${!_values[@]}" ; do value_opt="$value_opt$x:" ; done
    # echo "flag_opt=$flag_opt, value_opt=$value_opt"

    while getopts ":$flag_opt$value_opt" option "$@" ; do
        case $option in
            \?) echo "Unknown option -$OPTARG" ;;
            # *) echo "option=$OPTARG" ;;&
            [$flag_opt])
                x=${_flags[$option]}
                local $x=${!x:-0}
                local $x=$((${!x} + 1)) ;;
            [$value_opt]) local ${_values[$option]}="$OPTARG" ;;
        esac
    done
    shift $((OPTIND - 1))

    echo Flags
    for x in "${_flags[@]}" ; do echo "$x=${!x}" ; done
    echo Values
    for x in "${_values[@]}" ; do echo "$x=${!x}" ; done
}

function main () {
    local -A flags values
    flags="([d]=debug [h]=show_help)"
    values="([f]=foo [b]=bar)"
    get_my_opts "$flags" "$values" "$@"
}
main "$@"
