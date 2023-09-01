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
		DEFM_CONFIG="-DBARRY_DEBUG -fsanitize=address" R CMD INSTALL .

clean:
	Rscript --vanilla -e 'devtools::clean_dll()'; \
		rm -rf *.Rcmdcheck 

README.md: README.Rmd
	Rscript --vanilla -e 'rmarkdown::render("README.Rmd")'

.PHONY: build update clean

inst/NEWS: NEWS.md
	Rscript -e "rmarkdown::pandoc_convert('NEWS.md', 'plain', output='inst/NEWS')" && \
		head -n 80 inst/NEWS

# Thanls to Dirk Eddelbuettel for the Dockerfile
# https://dirk.eddelbuettel.com/blog/2015/01/18/
docker-check: clean build
	docker run --rm -ti -v $(PWD):/mnt -w/mnt \
		--cap-add=SYS_PTRACE \
		rocker/r-devel-san \
	 	make littler-install-deps

littler-install-deps:
	apt update && apt install --no-install-suggests -y libssl-dev && \
		RDscript --vanilla -e 'install.packages(c("Rcpp", "texreg"), repos = "https://cloud.r-project.org")' && \
		RD CMD check --as-cran defm_0.1-1.tar.gz
