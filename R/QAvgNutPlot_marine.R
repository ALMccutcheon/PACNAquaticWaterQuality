#' Graph PACN nutrient data for marine resources
#'
#' Nutrient data are graphs for nutrient parameters
#'
#' @param x A dataset in the format of nutdata. Must include the fields parameter, year, quarter, year_quarter,mean_value and depth.
#' @param param A parameter name. Typically TDN, TDP, NO3.
#' @param main.title The desired graph title.
#' @param y.label The desired y axis label
#' @param axis.digits The number of digits desired for the y axis.
#' @param errorbartype The type of error bar desired. Default is "CI" for 95% confidence interval. Otherwise it will do standard error.
#' @returns A plot of mean values over time in the PACN desired format.
#' @export

QAvgNutPlot_marine <-
function(x,param,main.title,y.label,axis.digits,errorbartype="CI"){

  yearquarters<-unique(x$year_quarter)
  minyearquarter<-min(yearquarters)
  maxyearquarter<-max(yearquarters)
  yearquarterscomplete<-seq(minyearquarter,maxyearquarter,by=0.25)
  alldates<-data.frame(list(year_quarter=yearquarterscomplete))

  surnutdata<- x %>%
    dplyr::filter(parameter==param&depth=="Surface")%>%
    dplyr::select(year,quarter,year_quarter,mean_value)%>%
    dplyr::group_by(year_quarter)%>%
    dplyr::summarize(N=n(),year=mean(year),quarter=mean(quarter),average_value=mean(mean_value,na.rm=T),
                     SE=sd(mean_value,na.rm=T)/sqrt(N),
                     lower.ci = ifelse(N==1,NA,average_value-stats::qt(1 - (0.05 / 2), N - 1) * SE),
                     upper.ci = ifelse(N==1,NA,average_value+stats::qt(1 - (0.05 / 2), N - 1) * SE),
                     lower.ci = ifelse(lower.ci<0,0,lower.ci))%>%
    dplyr::mutate(errorup=upper.ci,errordown=lower.ci,errorupse=average_value+SE,errordownse=average_value-SE)

  surnutdata2 <- dplyr::left_join(alldates,surnutdata,dplyr::join_by("year_quarter"))

  botnutdata<- x %>%
    dplyr::filter(parameter==param&depth=="Bottom")%>%
    dplyr::select(year,quarter,year_quarter,mean_value)%>%
    dplyr::group_by(year_quarter)%>%
    dplyr::summarize(N=n(),year=mean(year),quarter=mean(quarter),average_value=mean(mean_value,na.rm=T),
                     SE=sd(mean_value,na.rm=T)/sqrt(N),
                     lower.ci = ifelse(N==1,NA,average_value-stats::qt(1 - (0.05 / 2), N - 1) * SE),
                     upper.ci = ifelse(N==1,NA,average_value+stats::qt(1 - (0.05 / 2), N - 1) * SE),
                     lower.ci = ifelse(lower.ci<0,0,lower.ci))%>%
    dplyr::mutate(errorup=upper.ci,errordown=lower.ci,errorupse=average_value+SE,errordownse=average_value-SE)

  botnutdata2 <- dplyr::left_join(alldates,botnutdata,dplyr::join_by("year_quarter"))

  nutdata2 <- rbind(data.frame(surnutdata2,"depth"=rep("Surface",nrow(surnutdata2))),data.frame(botnutdata2,"depth"=rep("Bottom",nrow(botnutdata2))))

  if(errorbartype=="CI"){
    limits<- aes(ymin=errordown,ymax=errorup)
    y.limits <- c(min(nutdata2$lower.ci,na.rm=T),max(nutdata2$upper.ci,na.rm=T))
  }else{
    limits<- aes(ymin=errordownse,ymax=errorupse)
    y.limits <- c(min(nutdata2$errordownse,na.rm=T),max(nutdata2$errorupse,na.rm=T))}

  avgplot <- ggplot2::qplot(year_quarter, average_value,
                   data=nutdata2,
                   xlab="Date",
                   ylab=y.label,
                   colour=depth,
                   main=main.title)

  maxi <-y.limits[2]
  axis.digits <- ifelse(maxi<1,2,ifelse(maxi<10,2,ifelse(maxi<100,1,0)))

  avgplotformatted <- avgplot  +ggplot2::geom_errorbar(limits)+ggplot2::scale_y_continuous(limits=y.limits,labels = function(x) format(x, nsmall = axis.digits))

  return(avgplot
         +ggplot2::geom_errorbar(limits)+ggplot2::scale_y_continuous(limits=y.limits,labels = function(x) format(x, nsmall = axis.digits)))

}
