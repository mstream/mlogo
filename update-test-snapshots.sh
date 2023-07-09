#!/usr/bin/env bash

VERSION=1.31.1

npm run test:ui:production:update

docker run \
  --rm \
  --network host \
  -v $(pwd):/work/ \
  -w /work/ \
  "mcr.microsoft.com/playwright:v${VERSION}-jammy" \
  bash -c 'npm ci && npm run test:ui:production:update'

npm ci
