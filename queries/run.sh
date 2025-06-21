#!/bin/bash

usage() {
    echo "$0 <script> [--player-number|-n number]" >&2
    exit 1
}

while (( "$#" > 0 )); do
    case "$1" in
        --player-number|-n)
            (( "$#" < 2 )) && usage
            player_number="$2"
            shift 2
            ;;
        -*)
            usage
            ;;
        *)
            [[ -n "$script" ]] && usage
            script="$1"
            shift 1
    esac
done

[[ ! -f "$script" ]] && usage

{
    [[ -n "$player_number" ]] && echo ".param set :playerNumber $player_number"
    cat "$script"
} | sqlite3 -box "$(dirname $0)/../output/db"
