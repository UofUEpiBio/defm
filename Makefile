docs: 
	Rscript -e 'devtools::document()'

build:
	R CMD build .
	
install: 
	R CMD INSTALL defm_*.tar.gz

check: 
	R CMD check --as-cran defm_*.tar.gz

update:
	rsync -avz ../barry/include/barry inst/include 

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


docker-run:
	docker run --rm -ti -v $(PWD):/mnt -w/mnt gvegayon/defm:latest bash

# Thanls to Dirk Eddelbuettel for the Dockerfile
# https://dirk.eddelbuettel.com/blog/2015/01/18/
docker-check:
	docker run --rm -ti -v $(PWD):/mnt -w/mnt gvegayon/defm:clang make docker-check-all


# -fno-sanitize=float-divide-by-zero,vptr \
		\ # -fno-sanitize-recover"
#-DBARRY_DEBUG -fsanitize=address -fno-sanitize=float-divide-by-zero,vptr -fno-sanitize-recover"
docker-check-all: 
	RD CMD build . && \
		DEFM_CONFIG="-DBARRY_DEBUG" _R_CHECK_FORCE_SUGGESTS_=false \
		RD CMD check defm_*.tar.gz
