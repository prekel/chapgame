all: thesis.pdf assignment.pdf presentation.pdf

../_build/default/thesis/tex/thesis.pdf: FORCE
	dune build ./tex/thesis.pdf

../_build/default/thesis/tex/assignment.pdf: FORCE
	dune build ./tex/assignment.pdf

../_build/default/thesis/tex/presentation.pdf: FORCE
	dune build ./tex/presentation.pdf

thesis.pdf: ../_build/default/thesis/tex/thesis.pdf
	cp ../_build/default/thesis/tex/thesis.pdf . -f

assignment.pdf: ../_build/default/thesis/tex/assignment.pdf
	cp ../_build/default/thesis/tex/assignment.pdf . -f

presentation.pdf: ../_build/default/thesis/tex/presentation.pdf
	cp ../_build/default/thesis/tex/presentation.pdf . -f

FORCE:
