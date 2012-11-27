#!/bin/bash

# Script to generate site documentation.
# It is also possible to (optionally) deploy the documentation to
# the github gh-pages branch. The current branch must be clean (no
# changes in 'git status') when deploying to gh-pages.
#
# Usage:
#   ./doc.sh        - will only generate the documentation
#   ./doc.sh deploy - will do the above and deploy to github

# create site documentation
mvn clean && mvn -U -Pit && mvn site

VERSION=`grep version pom.xml | head -1 | awk '{print $1}' | sed 's/<version>//' | sed 's/<\/version>//'`

# deploy to gh-pages
if [ "$1" == "deploy" ]; then
    CURRENT=`git branch | grep "*" | awk '{print $2}'`
    git checkout gh-pages
    rm -rf $VERSION
    mkdir -p $VERSION
    cp -R target/site/* $VERSION
    git add $VERSION
    git commit -a -m "Regenerated site documentation."
    read -n 1 -p "Press <ENTER> to push site changes."
    git push origin gh-pages
    git checkout $CURRENT
fi
