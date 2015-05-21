all: runner.js

runner.js: runner.elm
	elm-make runner.elm --output=runner.js
