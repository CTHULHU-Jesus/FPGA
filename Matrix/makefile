all: test


test: Matrix.hs MatrixTest/test.hs makefile
	clash -o matTest MatrixTest/test.hs Matrix.hs
