#!/bin/sh

elm-package install && \
elm-live --port=2018 --open --pushstate --debug -- example/Main.elm --output index.js