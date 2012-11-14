#' Data Overview for WRTDS
#'
#' Gives a summary of data to be used for WRTDS analysis
#'
#' @param localDaily dataframe
#' @param localSample dataframe
#' @keywords data import USGS WRTDS
#' @export
#' @seealso \code{\link{mergeReport}}
#' @examples
#' # These examples require an internet connection to run
#' exDaily <- getDVData('01594440','00060', '1985-01-01', '1985-03-31', interactive=FALSE)
#' exSample <- getSampleData('01594440','01075', '1985-01-01', '1985-03-31', interactive=FALSE)
#' dataOverview(localDaily = exDaily, localSample = exSample)
dataOverview <- function(localDaily = Daily, localSample = Sample){
  numDays<-length(localDaily$Date)
  numSamples<-length(localSample$Date)
  numYears<-round(numDays/365.25,digits=0)
  cat("\n Discharge Record is",numDays,"days long, which is",numYears,"years")
  cat("\n First day of the discharge record is", as.character(localDaily$Date[1]),"and last day is",as.character(localDaily$Date[numDays]))
  cat("\n The water quality record has",numSamples,"samples")
  cat("\n The first sample is from", as.character(localSample$Date[1]),"and the last sample is from",as.character(localSample$Date[numSamples]))
  if(localSample$Date[1]<localDaily$Date[1]) cat("\n WE HAVE A PROBLEM first sample is from before the first daily discharge")	
  if(localSample$Date[numSamples]>localDaily$Date[numDays]) cat("\n WE HAVE A PROBLEM last sample is from after the last daily discharge")
  Qmin<-signif(min(localDaily$Q),digits=3)
  Qmean<-signif(mean(localDaily$Q),digits=3)
  Qmax<-signif(max(localDaily$Q),digits=3)
  Cmin<-signif(min(localSample$ConcHigh),digits=2)
  Cmean<-signif(mean(localSample$ConcHigh),digits=2)
  Cmax<-signif(max(localSample$ConcHigh),digits=2)
  cat("\n Discharge: Minimum, mean and maximum",Qmin,Qmean,Qmax)
  cat("\n Concentration: Minimum, mean and maximum",Cmin,Cmean,Cmax)
  pct<-sum(localSample$Uncen)
  pct<-((numSamples-pct)/numSamples)*100
  cat("\n Percentage of the sample values that are censored is",signif(pct,digits=2),"%")
}