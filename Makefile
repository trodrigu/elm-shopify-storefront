#!make
include .env
make: src/Main.elm
		./env.sh > _build/env.js && elm-live src/Main.elm --pushstate --open --start-page=index.html -- --output=_build/elm.js 