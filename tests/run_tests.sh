#!/bin/bash
elm-make BasicTests.elm --output tests.js
if [ $? -eq 0 ]
  then node tests.js
fi
