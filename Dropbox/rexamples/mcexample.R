install.packages("mc2d")
library(mc2d)

##1D monte carlo
ndvar(1001)

conc <- 10
cook <- mcstoc(rempiricalD, values=c(1,1/5,1/50), prob=c(0.027,0.373,0.600))
serving <- mcstoc(rgamma,shape=3.93,rate=0.0806)
expo <- conc * cook * serving
dose <- mcstoc(rpois,lambda=expo)
r <- 0.001
risk <- 1-(1-r)^dose
EC1 <- mc(cook,serving,expo,dose,risk)
print(EC1)

##http://cran.r-project.org/web/packages/mc2d/vignettes/docmcEnglish.pdf
