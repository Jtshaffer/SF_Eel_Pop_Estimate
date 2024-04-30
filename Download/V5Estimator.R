# Function for V5 estimator of variance
# obtained from https://faculty.washington.edu/joel/Software.html

SystematicVar5<-function(Data,FPC =T,SamplingFrac=0)
{
  #######################
  ## Variance estimator for mean count of a single systematic sample
  ## Estimator 'v5' in Wolter 1984, based on higher order
  ## differences.
  ##
  ## Assumes systematic in one dimension (e.g., time, transect)
  ####################
  # ARGUMENTS
  ##################
  # Data - vector of observations, in sample sequence
  # FPC - logical - use Finite Population Correction?  See Cochran (1977).
  # SamplingFrac - Fraction (0< <1) of the population sampled.
  #############
  # Last Edit: 1 May 2006
  # Author: Joel H. Reynolds, joel_reynolds@fws.gov
  ####################
  # REFERENCES
  ############
  # Cochran, W. G. 1977. Sampling, 3rd edition. Wiley & Sons.
  #
  # Wolter, K. 1984.  An Investigation of Some Estimators of Variance for
  #   Systematic Sampling.  Journal of the American Statistical Association
  #   79 (388), 781-790.
  #
  ###################
  
  # Check Data
  if (is.na(length(Data))) stop(warning("Only applicable to vector objects."))
  if (sum(is.na(Data))>0) stop(warning("No missing data allowed."))
  if (!is.numeric(Data)) stop(warning("Only applicable to numeric observations."))
  
  n<-length(Data)
  # Calculate differences
  cj<-(1/2)*Data[c(5:n)]-Data[c(4:(n-1))]+Data[c(3:(n-2))]-Data[c(2:(n-3))]+(1/2)*Data[c(1:(n-4))]
  
  # calculate sum of squares
  var5est<-(sum(cj^2)/(3.5*(n-4)))/n
  
  # apply FPC
  if (FPC) var5est<-(1-SamplingFrac)*var5est
  
  return(c("VarEst"=var5est,"DF"=n-4))
}