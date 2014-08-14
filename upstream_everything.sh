#!/bin/sh

for i in $(find . -maxdepth 1 -type d -not -name .)
do
    if [ -d "$i/.git" ]; then
        cd "$i"
        if git remote | grep 'upstream' > /dev/null; then
            echo "'upstream' remote already exists in ${PWD##*/}"
        else
            UPSTREAM=`infer-upstream --using-cwd`
            if [ $? -eq 0 ]; then
                git remote add upstream ${UPSTREAM}
                echo "created 'upstream' remote in ${PWD##*/} pointing at ${UPSTREAM}"
            else
                echo "can't determine upstream for ${PWD##*/}"
            fi
        fi
        cd ..
    fi
done
