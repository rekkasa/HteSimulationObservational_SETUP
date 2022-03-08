RAW = data/raw
IDS = $(shell seq 1 2)
DIR = $(addprefix $(RAW)/scenario_, $(IDS))
EVALFILES = $(addsuffix /evaluation.rds, $(DIR))

print-% :
	@echo '$*=$($*)'

$(EVALFILES) : code/SetupSimulationScript.R\
	       data/processed/analysisIds.csv
	$< $@

data/processed/analysisIds.csv : code/WriteAnalysisIds.R
	$<

data/processed/leacySettings.rds : code/LeacySimulationSettings.R
	$<


docs/literature_review.html : submission/literature_review.rmd
	R -e 'rmarkdown::render("submission/literature_review.rmd", output_format = "all", output_dir = "docs")'

.PHONY:
data : $(EVALFILES)

clean:
	rm -rf data/raw/scenario_*
