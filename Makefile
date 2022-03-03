data/processed/analysisIds.csv : code/WriteAnalysisIds.R
	$<

data/processed/leacySettings.rds : code/LeacySimulationSettings.R
	$<


submission/literature_review.html : submission/literature_review.rmd
	R -e 'rmarkdown::render("submission/literature_review.rmd", output_format = "all")'
 
