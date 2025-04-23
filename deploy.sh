#!/usr/bin/env bash
# set -o xtrace

green="\033[1;32m"
red="\033[1;31m"
reset="\033[0m"

function fail {
  echo -e "$red[FAIL]$reset $1"
  exit 1
}

function ok {
  echo -e "$green[OK]$reset $1"
  exit 0
}

pwd=$(pwd)
dist="./dist"

# Check for dist folder

test -d $dist || fail "dist directory is missing, ensure to checkout README.md on how to create it!"

# Check git status of master

test -z "$(git status --porcelain)" || fail ". is not clean, will not deploy!"
test "$(git rev-parse @{u})" = "$(git rev-parse HEAD)" || fail ". is not pushed yet, will not deploy!"

# Check git status of gh-pages

cd $dist
status=$(git status --porcelain)
cd $pwd

test -z "$status" || fail "./dist is not clean, will not deploy!"

# Run webpack

NODE_ENV=production npx webpack -p --display errors-only

# Check for changes

cd $dist
status=$(git status --porcelain)
cd $pwd

test -n "$status" || fail "./dist did not change, nothing to deploy!"

# Commit changes

sha=$(git rev-parse HEAD)

cd $dist
git commit --all -m "Deploy $sha" --quiet
git push --quiet
cd $pwd

ok "Deployed to https://hamster.mariouher.com"
