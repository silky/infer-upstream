#!/bin/sh

for i in $(find . -maxdepth 1 -type d -not -name .)
do
    if [ -d "$i/.git" ]; then
        cd "$i"
        if git remote | grep 'upstream' > /dev/null; then
            echo "fetching upstream for ${PWD##*/} ..."
            git fetch upstream
            # Assume branches are called 'master'.
            git diff --shortstat master upstream/master
        fi
        cd ..
    fi
done
