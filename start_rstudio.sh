#!/bin/bash

if [[ "$1" == "-h" ]]
then
cat << EOF
$0 (tag)

will start interactive environment for tag (TAG)
EOF
exit 0
fi

PWD=$(pwd)
. ${PWD}/.myconfig.sh
tag=${1:-$tag}
case $USER in
  codespace)
  WORKSPACE=/workspaces
  ;;
  *)
  WORKSPACE=$PWD
  ;;
esac
  
# pull the docker if necessary

docker pull $dockerrepo:$tag

# map the cache
if [[ -d .cache ]] ; then
  export DOCKEREXTRA="-v $PWD/.cache:/home/rstudio/.cache"
fi
# Dropbox stuff
if [[ ! -z $DROPBOX_SECRET_BASE ]]; then export DOCKEREXTRA="$DOCKEREXTRA -e DROPBOX_SECRET_BASE=$DROPBOX_SECRET_BASE" ; fi
if [[ ! -z $DROPBOX_SECRET_RLKEY ]]; then export DOCKEREXTRA="$DOCKEREXTRA -e DROPBOX_SECRET_RLKEY=$DROPBOX_SECRET_RLKEY" ; fi

[[ -z $(which xdg-open 2>/dev/null) ]] || xdg-open http://localhost:8787
# for OSX
[[ -z $(which open 2>/dev/null) ]] ||   open http://localhost:8787


docker run $DOCKEREXTRA -e DISABLE_AUTH=true -v "$WORKSPACE":/home/rstudio/${PWD##*/} --rm -p 8787:8787 $dockerrepo:$tag
