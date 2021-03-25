#!/usr/bin/env bash

set -ex

clojure -Spom
clojure -M:jar
clojure -X ivarref.pom-patch/clojars-repo-only!
VERSION=$(clojure -X ivarref.pom-patch/set-patch-version! :patch :commit-count+1)

git add pom.xml
git commit -m "Release $VERSION"
git tag -a v$VERSION -m "Release v$VERSION"
git push --follow-tags

clojure -M:deploy

echo "Released $VERSION"

rm *.pom.asc