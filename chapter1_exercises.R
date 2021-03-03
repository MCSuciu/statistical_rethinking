remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")

install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)

pkgbuild::has_build_tools(debug = TRUE)


install.packages(c("devtools","mvtnorm","loo","coda"),dependencies=TRUE)
library(devtools)
install.packages("rlang")
install_github("rmcelreath/rethinking",ref="Experimental")
