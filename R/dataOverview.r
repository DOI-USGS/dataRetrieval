#' Data Overview for WRTDS
#'
#' Gives a summary of data to be used for WRTDS analysis
#'
#' @param Daily dataframe
#' @param Sample dataframe
#' @keywords data import USGS WRTDS
#' @export
#' @seealso \code{\link{mergeReport}}
#' @examples
#' # These examples require an internet connection to run
#' exDaily <- getNWISDaily('01594440','00060', '1985-01-01', '1985-03-31', interactive=FALSE)
#' exSample <- getNWISSample('01594440','01075', '1985-01-01', '1985-03-31', interactive=FALSE)
#' dataOverview(Daily = exDaily, Sample = exSample)
dataOverview <- function(Daily, Sample ){

  numDays<-length(Daily$Date)
  numSamples<-length(Sample$Date)
  numYears<-round(numDays/365.25,digits=0)
  cat("\n Discharge Record is",numDays,"days long, which is",numYears,"years")
  cat("\n First day of the discharge record is", as.character(Daily$Date[1]),"and last day is",as.character(Daily$Date[numDays]))
  cat("\n The water quality record has",numSamples,"samples")
  cat("\n The first sample is from", as.character(Sample$Date[1]),"and the last sample is from",as.character(Sample$Date[numSamples]))
  if(Sample$Date[1]<Daily$Date[1]) cat("\n WE HAVE A PROBLEM first sample is from before the first daily discharge")	
  if(Sample$Date[numSamples]>Daily$Date[numDays]) cat("\n WE HAVE A PROBLEM last sample is from after the last daily discharge")
  Qmin<-signif(min(Daily$Q),digits=3)
  Qmean<-signif(mean(Daily$Q),digits=3)
  Qmax<-signif(max(Daily$Q),digits=3)
  Cmin<-signif(min(Sample$ConcHigh),digits=2)
  Cmean<-signif(mean(Sample$ConcHigh),digits=2)
  Cmax<-signif(max(Sample$ConcHigh),digits=2)
  cat("\n Discharge: Minimum, mean and maximum",Qmin,Qmean,Qmax)
  cat("\n Concentration: Minimum, mean and maximum",Cmin,Cmean,Cmax)
  pct<-sum(Sample$Uncen)
  pct<-((numSamples-pct)/numSamples)*100
  cat("\n Percentage of the sample values that are censored is",signif(pct,digits=2),"%")
}
