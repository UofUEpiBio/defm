FROM rocker/r-devel-san

RUN apt update && apt install --no-install-suggests -y libssl-dev

RUN RDscript --vanilla -e \
  'install.packages(c("Rcpp", "texreg"), repos = "https://cloud.r-project.org")'

CMD ["bash"]



