all: index.html

index.html: runner.elm
	elm-make runner.elm --output=index.html
