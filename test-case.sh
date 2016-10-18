#!/bin/bash

ARGS="-r .zsh --last-exit-code=0 --last-pipe-status=1 --renderer-arg=client_id=7822 --renderer-arg=shortened_path=~/powerline-hs --jobnum=0 --renderer-arg=mode=viins --renderer-arg=default_mode=emacs --width=171 $@"

echo Left:
env VIRTUAL_ENV=/tmp/venv ./powerline-hs shell aboveleft $ARGS
echo -e '\n'

echo Right:
env VIRTUAL_ENV=/tmp/venv ./powerline-hs shell right $ARGS

