#!/bin/bash

# Script to generate site documentation.
# It is also possible to (optionally) deploy the documentation to
# the github gh-pages branch. The current branch must be clean (no
# changes in 'git status') when deploying to gh-pages.
#
# Usage:
#   ./edoc.sh        - will only generate the documentation
#   ./edoc.sh deploy - will do the above and deploy to github

# create edoc and cleanup
mvn clean && mvn -Pit && mvn site

# deploy to gh-pages
if [ "$1" == "deploy" ]; then
    CURRENT=`git branch | grep "*" | awk '{print $2}'`
    git checkout gh-pages
    rm -rf *
    cp -R target/site/* .
    git add .
    git commit -a -m "Regenerated site documentation."
    git push origin gh-pages
    git checkout $CURRENT
fi
