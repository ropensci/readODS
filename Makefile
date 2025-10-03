clean:
	@Rscript -e 'devtools::clean_dll()'

test:
	@Rscript -e 'devtools::clean_dll()'
	@Rscript -e 'devtools::load_all()'

check:
	@echo "Local"
	@Rscript -e 'devtools::install()'
	@Rscript -e 'devtools::check()'

site:
	@Rscript -e 'devtools::document()'
	@Rscript -e 'pkgdown::build_site()'

install:
	@Rscript -e 'devtools::clean_dll()'
	@Rscript -e 'devtools::install()'

clang_format=`which clang-format-18`

format: $(shell find . -name '*.h') $(shell find . -name '*.hpp') $(shell find . -name '*.cpp')
	@${clang_format} -i $?
