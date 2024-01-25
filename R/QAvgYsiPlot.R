#' Graph PACN ysi data for non-marine resources
#'
#' YSI data are graphs for YSI parameters
#'
#' @param x A dataset in the format of ysidata. Must include the fields parameter, year, quarter, year_quarter, depth, corrected_mean.
#' @param param A parameter name. Typically Chl, pH, Sal, ODOConc, ODOSat, SpCond, Temp, and Turbid+.
#' @param main.title The desired graph title.
#' @param y.label The desired y axis label
#' @param axis.digits The number of digits desired for the y axis.
#' @returns A plot of mean values over time in the PACN desired format.
#' @export

QAvgYsiPlot <-
function(x,param,main.title,y.label,axis.digits){

  yearquarters<-unique(x$year_quarter)
  minyearquarter<-min(yearquarters)
  maxyearquarter<-max(yearquarters)
  yearquarterscomplete<-seq(minyearquarter,maxyearquarter,by=0.25)
  alldates<-data.frame(list(year_quarter=yearquarterscomplete))

  y.limits<-c(min(x[x$parameter==param,]$corrected_mean),max(x[x$parameter==param,]$corrected_mean))

  ysidata2<- x %>%
    dplyr::filter(parameter==param)%>%
    dplyr::select(year,quarter,year_quarter,corrected_mean)%>%
    dplyr::group_by(year_quarter)%>%
    dplyr::summarize(N=n(),year=mean(year),quarter=mean(quarter),average_value=mean(corrected_mean),
                     SE=sd(corrected_mean)/sqrt(length(corrected_mean)),
                     lower.ci = average_value - qt(1 - (0.05 / 2), N - 1) * SE,
                     upper.ci = average_value + qt(1 - (0.05 / 2), N - 1) * SE,
                     lower.ci = ifelse(lower.ci<0,0,lower.ci))%>%
    dplyr::mutate(errorup=upper.ci,errordown=lower.ci)


  ysidata3 <- dplyr::left_join(alldates,ysidata2,dplyr::join_by("year_quarter"))

  limits<- aes(ymin=errordown,ymax=errorup)

  avgplot <- ggplot2::qplot(year_quarter, average_value,
                   data=ysidata3,
                   xlab="Date",
                   ylab=y.label,
                   main=main.title)

  maxi <-y.limits[2]
  axis.digits <- ifelse(maxi<1,2,ifelse(maxi<10,2,ifelse(maxi<100,1,0)))

  avgplotformatted <- avgplot +ggplot2::geom_errorbar(limits)+ggplot2::scale_y_continuous(limits=y.limits,labels = function(x) format(x, nsmall = axis.digits))

  return(avgplot +ggplot2::geom_errorbar(limits)+ggplot2::scale_y_continuous(limits=y.limits,labels = function(x) format(x, nsmall = axis.digits)))

}
