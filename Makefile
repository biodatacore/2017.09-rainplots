# Dummy targets
all: data/all_models_linear_output.tsv data/*.rds \
plots/*.pdf

clean:
	rm -f plots/*.pdf data/all_models_linear_output.tsv data/*.rds

clean_plots:
	rm -f plots/*.pdf

.PHONY: all clean clean_plots

# Linear models
data/all_models_linear_output.tsv: eicdata/FHS/ scripts/modeling.R
	Rscript scripts/modeling.R

# Clean model outputs for viz:
data/*.rds: data/all_models_linear_output.tsv scripts/viz_prep.R
	Rscript scripts/viz_prep.R

# Rainplots
plots/rainplot.pdf: data/plotdat.rds scripts/utils.R scripts/rainplot.R
	Rscript scripts/rainplot.R

plots/top50_rainplot.pdf: data/top50_plotdat.rds scripts/utils.R scripts/rainplot.R
	Rscript scripts/rainplot.R

plots/top25_rainplot.pdf: data/top25_plotdat.rds scripts/utils.R scripts/rainplot.R
	Rscript scripts/rainplot.R

# Heatmap
plots/heatmaps.pdf: data/plotdat.rds data/top50_plotdat.rds scripts/utils.R scripts/heatmap.R
	Rscript scripts/heatmap.R

# Volcano Plot
plots/volcano.pdf: data/plotdat.rds data/top50_plotdat.rds scripts/utils.R scripts/volcano.R
	Rscript scripts/volcano.R

# Manhattan plot
plots/manhattan.pdf: data/plotdat.rds data/top50_plotdat.rds scripts/utils.R scripts/manhattan.R
	Rscript scripts/manhattan.R

# Spark and PVal Plot
plots/spark_and_pvalue.pdf: data/plotdat.rds data/top50_plotdat.rds scripts/utils.R scripts/spark_and_p.R
	Rscript scripts/spark_and_p.R
