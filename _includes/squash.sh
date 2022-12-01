#!/usr/bin/env bash
set -euo pipefail

nPullRequests=40
nCommitsPerPullRequest=15
repo=sample


rm -fr $repo
mkdir $repo
cd $repo
git init
touch README
git add README
git commit -m "Initial commit"
git branch squashed

random_string () {
    array=()
    for i in {a..z} {A..Z} {0..9}; do
        array[$RANDOM]=$i
    done
    printf %s ${array[@]::200} $'\n'
}

create_branch () {
    branch=$1
    git checkout -b $branch
    nCommits=$(( $RANDOM % $nCommitsPerPullRequest + 1 ))
    for j in $(eval echo {1..$nCommits}) ; do
        echo $(random_string) >> ${branch}.txt
        git add ${branch}.txt
        git commit -m "Change $j to branch $branch"
    done

    git checkout master
    git merge --no-ff $branch -m "Merged pull request $branch"

    git checkout squashed
    git merge --squash $branch
    git commit -am "Merged pull request $branch (squashed)"

    git branch -D $branch
    git checkout master
}


for i in $(eval echo {1..$nPullRequests})  ; do
    create_branch $i
done
