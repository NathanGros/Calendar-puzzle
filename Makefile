make :
	dune build --profile=release
	./_build/default/solveall.exe
