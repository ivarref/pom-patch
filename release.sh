#!/usr/bin/env bash

set -ex

clojure -Spom
clojure -M:jar

COMMIT_COUNT="$(git rev-list --count HEAD)"
let "NEXT_PATCH=COMMIT_COUNT+1"

clojure -X ivarref.pom-patch/clojars-repo-only!
VERSION=$(clojure -X ivarref.pom-patch/set-patch-version! :patch "$NEXT_PATCH")

echo "version is $VERSION"
clojure -X ivarref.pom-patch/update-tag!
