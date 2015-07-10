#!/bin/sh

if [ $# != 1 ]; then
  echo "Please provide a branch number." >&2
  exit 1
fi

set -e

: ${FB_DEV:-https://subversion/svn/dev/backstop}

cd ${BACKSTOP_DIR}
if [ ! -f app/HOW_DO_I_GET ]; then
    echo "Cannot find app/HOW_DO_I_GET" >&2
    echo "Are you sure your $BACKSTOP_DIR is the correct directory?" >&2
    exit 1
fi

branch="$1"

switch $branch
svn merge $FB_DEV .
echo "Commit your changes in another terminal and hit enter here when completed."
read input
current_rev=$(svn info | grep Revision: | awk '{print $2; }')
svn_branch=$(svn info | grep --color=auto -E "^URL" | awk '{print $2; }')
svn update

svn diff $FB_DEV@$current_rev $svn_branch@$current_rev > FB-$branch.patch
echo "Patch saved to FB-$branch.patch"
