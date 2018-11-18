#!/usr/bin/env bash
# set -o xtrace

green="\033[1;32m"
red="\033[1;31m"
reset="\033[0m"

ok="$green[OK]$reset"
fail="$red[FAIL]$reset"

pwd=$(pwd)
dist="./dist"


# Check git status of master

status=$(git status --porcelain)
if [ -n "$status" ]
then
  echo -e "$fail . is not clean, will not deploy!"
  exit 1
fi


# Check git status of gh-pages

cd $dist
status=$(git status --porcelain)
cd $pwd

if [ -n "$status" ]
then
  echo -e "$fail ./dist is not clean, will not deploy!"
  exit 1
fi


# Run webpack

NODE_ENV=production npx webpack -p --silent


# Check for changes

cd $dist
status=$(git status --porcelain)
cd $pwd

if [ -z "$status" ]
then
  echo -e "$fail ./dist did not change, nothing to deploy!"
  exit 1
fi


# Commit changes

sha=$(git rev-parse HEAD)

cd $dist
git commit --all -m "Deploy $sha"
git push
cd $pwd

echo -e "$ok Deployed to https://hamster.world"
