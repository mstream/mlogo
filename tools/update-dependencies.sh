#!/usr/bin/env bash

nix flake update
npm update
spago upgrade-set
