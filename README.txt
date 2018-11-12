Project 2 Genetic Programming

Written by:
	TEAM NLP
	Cristopher Hernandez
	Suyash Singh

Contact:
	cristopherh@csu.fullerton.edu
	suyash.vardhansingh@csu.fullerton.edu

Class:
	CPSC-481-03

Introduction:
	The goal of this project is to build a lisp program which attempts to create arithmetic expressions
	that describe a 3D surface. The expressions (a population of 50) are formed randomly initially, then selectively 'bred' and mutated
	using in genetic programming fashion for 50 generations. Each expression is scored based on a series of given sample
	points which relate to the 3D surface.

External Requirements:
	None, aside from Lisp

Build:
	Executable from the command line without compilation. 
  	However, a compiled version can be created using:

  	(COMPILE-FILE "project2.lisp")

  	In a Common Lisp REPL

Installation:
	files list:
		project2.lisp
		
Other Files:
	average_graph.png - generated graph of average scores over 50 generations
	best_graph.png - generated graph of best scores over 50 generations
	wors_graph.png - generated graph of worst scores over 50 generations
	testoutput.txt - raw output from generated data
	test_data.xlsx - excel formatted data

Setup:
	This solution was written and tested with GNU Common Lisp for Windows.
  	CLISP for windows can be downloaded and installed here: https://sourceforge.net/projects/clisp/files/

Usage:
  	run on command line:

  	clisp project2.lisp

  	load compiled .fas in REPL:

  	(LOAD "project2.fas")

Extra Features:
  	None

Bugs:
  	No known bugs at this time.
