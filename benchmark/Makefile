all:
	Rscript -e "devtools::install(build = TRUE)"
	find -name "*.qmd" -exec quarto render {} \;
