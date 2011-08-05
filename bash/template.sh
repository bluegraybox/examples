#!/bin/bash

# Example script using lib_config.sh to manage variables.

# The metadata you need to define for your application.
flags="[d]=debug [v]=verbose"
values="[n]=name [a]=age [p]=pet"
saved_vars="name age pet"
labels="[name]=\"your name\" [age]=\"your age\" [pet]=\"your pet's name\""  # Careful with the quotes here

# Load the library - SET THIS PATH
source ~/bin/lib_config.sh $0

# lib_config.sh boilerplate
# Note that the way that function parameters are quoted is important.
# Load saved config first, then command-line options to overwrite it.
load_config
process_options "$flags" "$values" "$@"
confirm_config "$labels"
save_config "$saved_vars"

# Your code starts here

