all: thesis.pdf assignment.pdf

../_build/default/thesis/tex/thesis.pdf: FORCE
	dune build ./tex/thesis.pdf

../_build/default/thesis/tex/assignment.pdf: FORCE
	dune build ./tex/assignment.pdf

thesis.pdf: ../_build/default/thesis/tex/thesis.pdf
	cp ../_build/default/thesis/tex/thesis.pdf . -f

assignment.pdf: ../_build/default/thesis/tex/assignment.pdf
	cp ../_build/default/thesis/tex/assignment.pdf . -f

FORCE:
