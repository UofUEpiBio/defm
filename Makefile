docs: 
	Rscript -e 'devtools::document()'

build:
	R CMD build .
	
install: 
	R CMD INSTALL .

check: build
	R CMD check --as-cran defm_*.tar.gz

update:
	rsync -avz ../barry/barry.hpp inst/include/barry.hpp && \
		rsync -avz ../barry/defm.hpp inst/include/defm.hpp

debug:
	$(MAKE) clean && \
		DEFM_CONFIG="-DBARRY_DEBUG" R CMD INSTALL .

clean:
	Rscript --vanilla -e 'devtools::clean_dll()'; \
		rm -rf *.Rcmdcheck 

README.md: README.Rmd
	Rscript --vanilla -e 'rmarkdown::render("README.Rmd")'

.PHONY: build update clean
