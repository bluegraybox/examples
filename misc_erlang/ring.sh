#!/bin/bash

erl -smp enable +P 1200300 -noshell -noinput -run ring main $*
