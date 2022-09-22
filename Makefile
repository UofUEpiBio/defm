build: 
	Rscript -e 'Rcpp::compileAttributes()' && \
		Rscript -e 'roxygen2::roxygenize()' && \
		R CMD INSTALL .
update:
	rsync -avz ../barry/barry.hpp inst/include/barry.hpp && \
		rsync -avz ../barry/defm.hpp inst/include/defm.hpp

.PHONY: build update
