#!/bin/bash -x

set -euo pipefail
TOKEN="***REMOVED***"

curl https://www.googleapis.com/oauth2/v1/tokeninfo \
     -d access_token=${TOKEN}
