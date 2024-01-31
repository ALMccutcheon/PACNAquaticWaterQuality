#' Graph PACN nutrient data for non-marine resources
#'
#' Nutrient data are graphs for nutrient parameters
#'
#' @param x A dataset in the format of nutdata. Must include the fields parameter, year, quarter, year_quarter,mean_value.
#' @param param A parameter name. Typically TDN, TDP, NO3.
#' @param main.title The desired graph title.
#' @param y.label The desired y axis label
#' @param axis.digits The number of digits desired for the y axis.
#' @returns A plot of mean values over time in the PACN desired format.
#' @export

QAvgNutPlot <- function(x,param,main.title,y.label,axis.digits=1){

  yearquarters<-unique(x$year_quarter)
  minyearquarter<-min(yearquarters)
  maxyearquarter<-max(yearquarters)
  yearquarterscomplete<-seq(minyearquarter,maxyearquarter,by=0.25)
  alldates<-data.frame(list(year_quarter=yearquarterscomplete))

  nutdata2<- x %>%
    dplyr::filter(parameter==param)%>%
    dplyr::select(year,quarter,year_quarter,mean_value)%>%
    dplyr::group_by(year_quarter)%>%
    dplyr::summarize(N=n(),year=mean(year),quarter=mean(quarter),average_value=mean(mean_value,na.rm=T),
                     SE=sd(mean_value,na.rm=T)/sqrt(N),
                     lower.ci = ifelse(N==1,NA,average_value-stats::qt(1 - (0.05 / 2), N - 1) * SE),
                     upper.ci = ifelse(N==1,NA,average_value+stats::qt(1 - (0.05 / 2), N - 1) * SE),
                     lower.ci = ifelse(lower.ci<0,0,lower.ci))%>%
    dplyr::mutate(errorup=upper.ci,errordown=lower.ci)

  nutdata3 <- dplyr::left_join(alldates,nutdata2,dplyr::join_by("year_quarter"))

  y.limits <- c(min(nutdata2$lower.ci,na.rm=T),max(nutdata2$upper.ci,na.rm=T))

  limits<- aes(ymin=errordown,ymax=errorup)

  avgplot <- ggplot2::qplot(year_quarter, average_value,
                   data=nutdata3,
                   xlab="Date",
                   ylab=y.label,
                   main=main.title)

  maxi <-y.limits[2]
  axis.digits <- ifelse(maxi<1,2,ifelse(maxi<10,2,ifelse(maxi<100,1,0)))

  avgplotformatted <- avgplot +ggplot2::geom_errorbar(limits)+ggplot2::scale_y_continuous(limits=y.limits,labels = function(x) format(x, nsmall = axis.digits))

  return(avgplot +ggplot2::geom_errorbar(limits)+ggplot2::scale_y_continuous(limits=y.limits,labels = function(x) format(x, nsmall = axis.digits)))

}
