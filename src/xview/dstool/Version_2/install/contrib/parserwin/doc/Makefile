LATEX_DOC = parser
DATA_FILES = \
	parser_fig.ps \
	lorenz.p \
	kotorus.p \
	duffing.p \
	vanderpol.p \
	d4symm.p

${LATEX_DOC}: ${LATEX_DOC}.tex
	latex ${LATEX_DOC}

clean: FORCE
	${RM} ${LATEX_DOC}.aux ${LATEX_DOC}.log ${LATEX_DOC}.dvi *~

storage: FORCE
	make clean
	compress ${LATEX_DOC}.tex ${DATA_FILES}

unstorage: FORCE
	uncompress ${LATEX_DOC}.tex ${DATA_FILES}

FORCE:
