.SUFFIXES: .ps .dvi .tex .tex,v .bbl .bib .bib,v .rs .rs,v

CO = co
VPATH = RCS

%: %,v
	${CO} $@

.dvi.ps:
	dvips $*.dvi > $*.ps

.rs.ps: 
	ras2ps $*.rs > $*.ps

TARGET = final-report.ps

SRC =\
addendums.tex\
calculation-testing.tex\
calculation-errors.tex\
conclusions.tex\
copying.tex\
database-testing.tex\
database-errors.tex\
detailed-design.tex\
errors.tex\
file-testing.tex\
file-errors.tex\
final-report.tex\
function.tex\
gui-testing.tex\
gui-errors.tex\
installation.tex\
intro.tex\
latex-testing.tex\
latex-errors.tex\
pert-algorithm.tex\
preliminary-design.tex\
proj-desc.tex\
project-plan.tex\
requirements-spec.tex\
scenario.tex\
summary.tex\
techniques.tex\
testing-spec.tex\
testing.tex\
user-manual.tex\
3tasks.rs\
analyze-menu.rs\
chart-menu.rs\
default.rs\
dependency.rs\
det-des-task-info.rs\
export-menu.rs\
file-menu.rs\
main-window.rs\
proj-des-task-info.rs\
req-spec-task-info.rs\
resources.rs\
task-info.rs\
test_gantt.rs\
test_pert.rs\
xplan.rs

BIBOBJ = final-report.bbl

PSOBJ =\
3tasks.ps\
analyze-menu.ps\
chart-menu.ps\
default.ps\
dependency.ps\
det-des-task-info.ps\
export-menu.ps\
file-menu.ps\
main-window.ps\
proj-des-task-info.ps\
req-spec-task-info.ps\
resources.ps\
task-info.ps\
test_gantt.ps\
test_pert.ps\
xplan.ps

final-report.ps: final-report.dvi

final-report.dvi: ${SRC} ${PSOBJ} ${BIBOBJ}
	latex final-report.tex
	latex final-report.tex

final-report.bbl: final-report.bib ${SRC} ${PSOBJ}
	latex final-report.tex
	bibtex final-report
