make: src/Main.elm
		./env.sh > _build/env.js && elm-live src/Main.elm --pushstate --open --dir=_build --start-page=_build/index.html -- --output=_build/elm.js 