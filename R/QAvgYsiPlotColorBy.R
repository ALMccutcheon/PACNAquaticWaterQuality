#' Graph PACN ysi data for non-marine resources colored by station or location
#'
#' YSI data are graphed for YSI parameters
#'
#' @param x A dataset in the format of ysidata. Must include the fields parameter, year, quarter, year_quarter, depth, corrected_mean.
#' @param param A parameter name. Typically Chl, pH, Sal, ODOConc, ODOSat, SpCond, Temp, and Turbid+.
#' @param main.title The desired graph title.
#' @param y.label The desired y axis label
#' @param axis.digits The number of digits desired for the y axis.
#' @param colorby The variable you'd like to colorby either loc_name or station_id
#' @param errorbartype The type of error bar desired. Default is "CI" for 95% confidence interval. Otherwise it will do standard error.
#' @returns A plot of mean values over time in the PACN desired format.
#' @export

QAvgYsiPlotColorBy <-
function(x,param,main.title,y.label,axis.digits,colorby="loc_name",errorbartype="CI"){

    yearquarters<-unique(x$year_quarter)
    minyearquarter<-min(yearquarters)
    maxyearquarter<-max(yearquarters)
    yearquarterscomplete<-seq(minyearquarter,maxyearquarter,by=0.25)
    alldates<-data.frame(list(year_quarter=yearquarterscomplete))
    allsites<-data.frame(list(station_id=unique(x$station_id)))
    alldates_sites<-merge(alldates,allsites)
    alllocations<-data.frame(list(loc_name=unique(x$loc_name)))
    alldates_locations<-merge(alldates,alllocations)

  if(colorby=="loc_name"){
    ysidata2<- x %>%
      dplyr::filter(parameter==param)%>%
      dplyr::select(year,quarter,year_quarter,corrected_mean,loc_name)%>%
      dplyr::group_by(year_quarter,loc_name)%>%
      dplyr::summarize(N=n(),year=mean(year),quarter=mean(quarter),average_value=mean(corrected_mean,na.rm=T),
                       SE=sd(corrected_mean,na.rm=T)/sqrt(N),
                       lower.ci = ifelse(N==1,NA,average_value-stats::qt(1 - (0.05 / 2), N - 1) * SE),
                       upper.ci = ifelse(N==1,NA,average_value+stats::qt(1 - (0.05 / 2), N - 1) * SE),
                       lower.ci = ifelse(lower.ci<0,0,lower.ci))%>%
      dplyr::mutate(errorup=upper.ci,errordown=lower.ci,errorupse=average_value+SE,errordownse=average_value-SE)

    ysidata3 <- dplyr::left_join(alldates_locations,ysidata2,dplyr::join_by("year_quarter","loc_name"))

    if(errorbartype=="CI"){
      limits<- aes(ymin=errordown,ymax=errorup)
      y.limits <- c(min(ysidata2$lower.ci,na.rm=T),max(ysidata2$upper.ci,na.rm=T))
    }else{
      limits<- aes(ymin=errordownse,ymax=errorupse)
      y.limits <- c(min(ysidata2$errordownse,na.rm=T),max(ysidata2$errorupse,na.rm=T))}

    avgplot <- ggplot2::qplot(year_quarter, average_value,
                              data=ysidata3,
                              xlab="Date",
                              ylab=y.label,
                              color=loc_name,
                              main=main.title)

    maxi <-y.limits[2]
    axis.digits <- ifelse(maxi<1,2,ifelse(maxi<10,2,ifelse(maxi<100,1,0)))

    avgplotformatted <- avgplot +ggplot2::geom_errorbar(limits)+ggplot2::scale_y_continuous(limits=y.limits,labels = function(x) format(x, nsmall = axis.digits))
  }else{if(colorby=="station_id"){
    ysidata2<- x %>%
      dplyr::filter(parameter==param)%>%
      dplyr::select(year,quarter,year_quarter,corrected_mean,station_id,sd)%>%
      dplyr::group_by(year_quarter,station_id)%>%
      dplyr::summarize(N=n(),year=mean(year),quarter=mean(quarter),average_value=mean(corrected_mean,na.rm=T),
                       SE=sd/sqrt(N),
                       lower.ci = ifelse(N==1,NA,average_value-stats::qt(1 - (0.05 / 2), N - 1) * SE),
                       upper.ci = ifelse(N==1,NA,average_value+stats::qt(1 - (0.05 / 2), N - 1) * SE),
                       lower.ci = ifelse(lower.ci<0,0,lower.ci))%>%
      dplyr::mutate(errorup=upper.ci,errordown=lower.ci,errorupse=average_value+SE,errordownse=average_value-SE)


    ysidata3 <- dplyr::left_join(alldates_sites,ysidata2,dplyr::join_by("year_quarter","station_id"))

    if(errorbartype=="CI"){
      limits<- aes(ymin=errordown,ymax=errorup)
      y.limits <- c(min(ysidata2$lower.ci,na.rm=T),max(ysidata2$upper.ci,na.rm=T))
    }else{
      limits<- aes(ymin=errordownse,ymax=errorupse)
      y.limits <- c(min(ysidata2$errordownse,na.rm=T),max(ysidata2$errorupse,na.rm=T))}

    avgplot <- ggplot2::qplot(year_quarter, average_value,
                              data=ysidata3,
                              xlab="Date",
                              ylab=y.label,
                              color=station_id,
                              main=main.title)

    maxi <-y.limits[2]
    axis.digits <- ifelse(maxi<1,2,ifelse(maxi<10,2,ifelse(maxi<100,1,0)))

    avgplotformatted <- avgplot +ggplot2::geom_errorbar(limits)+ggplot2::scale_y_continuous(limits=y.limits,labels = function(x) format(x, nsmall = axis.digits))
  }}
    return(avgplot +ggplot2::geom_errorbar(limits)+ggplot2::scale_y_continuous(limits=y.limits,labels = function(x) format(x, nsmall = axis.digits)))

  }
