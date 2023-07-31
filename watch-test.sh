#!/bin/bash

set -x

while true; do
  time "$@"
  echo ">>>>>>> EXIT CODE : $?"
  inotifywait -qre close_write,create,delete,move --exclude '^.git|^./.git/|^./_build' .
done
