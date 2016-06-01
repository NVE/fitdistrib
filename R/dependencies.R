# Packages to load for this package

# Checking forinstalled packages
a <- installed.packages()
packages <- a[,1]



# RNetCDF package for manipulating NetCDF files
# evd     Functions for extreme value distributions
# ismev   An Introduction to Statistical Modeling of Extreme Values (gum.fit)
# pracam  Practical Numerical Math routines. We use newtonRaphson which actually bugs quite often
# nsRFA   Includes l-moment calculations and BayesianMCMC
# fBasics Some basic statistical functions
# stats4  for mle()
# MASS    for fitdistr
# glogis  Generalized logistics
# fitdistrplus For mmedist, mledist (in GAMMA.r)
# plyr    For the failwith(NA, f) function!
packages <- c('RNetCDF', 'evd', 'ismev', 'pracma', 'nsRFA', 'fBasics', 'stats4', 'MASS', 'glogis', 'fitdistrplus', 'plyr')



# installing and loading libraries if required
for(package in packages){

  # if package is installed locally, load
  if(package %in% rownames(installed.packages())) {
    do.call('library', list(package))
    # do.call('use_package', list(package))

  # if package is not installed locally, download, then load
  } else {
    install.packages(package)
    do.call("library", list(package))
    # do.call('use_package', list(package))
  }
}







