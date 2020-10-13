#!/bin/sh

set -xe

curl -sOL `curl -s https://api.github.com/repos/roswell/roswell/releases/latest | jq -r '.assets | .[] | select(.name|test("deb$")) | .browser_download_url'` --output roswell.deb
dpkg -i roswell.deb
export PATH=~/.roswell/bin:$PATH
ros install metastatedev/org-generation
