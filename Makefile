help:
	@echo "Makefile for defm"
	@echo ""
	@echo "  make docs         Generate documentation"
	@echo "  make build        Build package tarball"
	@echo "  make install      Install package from tarball"
	@echo "  make check        Check package from tarball"
	@echo "  make update       Update barry include files"
	@echo "  make debug        Install package with debug flags"
	@echo "  make clean        Clean up generated files"
	@echo "  make README.md    Generate README.md from README.Rmd"
	@echo "  make docker-run   Run a docker container with R and barry installed"
	@echo "  make docker-check All checks inside a docker container"

docs: 
	Rscript -e 'devtools::document()'

build:
	R CMD build .
	
install: 
	Rscript --vanilla -e 'devtools::install()'

check: 
	Rscript --vanilla -e 'devtools::check()'

debug:
	$(MAKE) clean && \
		DEFM_CONFIG="-DBARRY_DEBUG -fsanitize=address" R CMD INSTALL .

clean:
	Rscript --vanilla -e 'devtools::clean_dll()'; \
		rm -rf *.Rcmdcheck 

.PHONY: build update clean


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

README.md: README.qmd
	quarto render README.qmd