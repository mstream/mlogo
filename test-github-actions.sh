#!/usr/bin/env bash

set -E
set -e

act lint

# `gh auth login` has to be run manually to make this work

GITHUB_TOKEN=$(gh auth token)

if [ -z "${GITHUB_TOKEN}" ]; then
  echo "GitHub token missing"
  exit 1
fi

act \
  --container-architecture 'linux/amd64' \
  --secret GITHUB_TOKEN="${GITHUB_TOKEN}"

