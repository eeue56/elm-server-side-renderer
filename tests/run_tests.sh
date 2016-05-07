#!/bin/bash
elm-make BasicTests.elm --output tests.js
node tests.js
