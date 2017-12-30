#!/bin/bash

# Temporarily store uncommited changes
# git stash

# Verify correct branch
git checkout develop

# Build new files
stack exec website clean
stack exec website build

# Get previous files
git fetch --all
git checkout -b master --track origin/master

# Overwrite existing files with new files
cp -a docs/. .

# Commit
git add -A
git commit -m "Publish."

# Push
git push origin master:master

# Restoration
git checkout develop
git branch -D master
# git stash pop
