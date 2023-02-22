all: thesis.pdf assignment.pdf presentation.pdf

./tex/thesis.pdf: FORCE
	cd ./tex && xelatex thesis.tex
	cd ./tex && xelatex thesis.tex

./tex/assignment.pdf: FORCE
	cd ./tex && xelatex assignment.tex
	cd ./tex && xelatex assignment.tex

./tex/presentation.pdf: FORCE
	cd ./tex && xelatex presentation.tex
	cd ./tex && xelatex presentation.tex

thesis.pdf: ./tex/thesis.pdf
	cp ./tex/thesis.pdf . -f

assignment.pdf: ./tex/assignment.pdf
	cp ./tex/assignment.pdf . -f

presentation.pdf: ./tex/presentation.pdf
	cp ./tex/presentation.pdf . -f

clean:
	cd ./tex && find . -type f ! -name '*.tex' -delete

FORCE:
