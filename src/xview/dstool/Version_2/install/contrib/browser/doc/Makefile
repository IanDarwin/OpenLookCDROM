LATEX_DOC = browser
DATA_FILES = browser_fig.ps

${LATEX_DOC}: ${LATEX_DOC}.tex
	latex ${LATEX_DOC}

clean: FORCE
	${RM} ${LATEX_DOC}.aux ${LATEX_DOC}.log ${LATEX_DOC}.dvi *~

storage: FORCE
	make clean
	compress ${LATEX_DOC}.tex ${DATA_FILES}

FORCE:
