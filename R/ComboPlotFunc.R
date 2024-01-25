#' Create the multi-plot figure for Water Quality Briefs
#'
#' Creates a 9 plot figure with nutrient and ysi plots, suitable for publication
#'
#' @param data1 A dataset in the format of kahocentralnutdata. Must include the fields parameter, year, quarter, year_quarter, mean_value and (for marine only) depth.
#' @param data2 A dataset in the format of kahocentralysidata. Must include the fields parameter, year, quarter, year_quarter, corrected_mean and (for marine only) depth.
#' @param type Resource type. Marine or Non-Marine.
#' @param years A list of years to be used as breaks on the graphs. Default is c(2009,2011,2013,2015,2017,2019,2021)
#' @param yearrange A list of length two indicating the min and max years displayed on the graph axes. Defaults is set to c(2008,2022).
#' @param graphtitle The desired graph title. Default is set to "Water Quality at Selected Park and Resource".
#' @returns A multi-plot of mean values for 9 parameters over time in the PACN desired format.
#' @export

ComboPlotFunc <-
function(data1,data2,type="Non-Marine",years=c(2009,2011,2013,2015,2017,2019,2021),yearrange=c(2008,2022),graphtitle="Water Quality at Selected Park and Resource"){

  my_theme_avg<- list(ggplot2::geom_line(),ggplot2::geom_point(),ggplot2::theme_bw(), ggplot2::theme(panel.grid.major = ggplot2::element_line(color = NA),panel.grid.minor =  ggplot2::element_line(color = NA),legend.position="none",plot.title = ggplot2::element_text(size=10)),ggplot2::scale_x_continuous(breaks=years, limits=yearrange))

  my_theme_avg_marine<- list(ggplot2::geom_line(),ggplot2::geom_point(),ggplot2::scale_colour_manual(values=c("red","blue")),ggplot2::theme_bw(),
                                       theme(panel.grid.major = ggplot2::element_line(color = NA),panel.grid.minor =  ggplot2::element_line(color = NA),legend.position="none",plot.title = ggplot2::element_text(size=10)),ggplot2::scale_x_continuous(breaks=years,limits=yearrange))


  if(type=="Marine"){
    TDNAvg<-QAvgNutPlot_marine(data1,"TDN","TDN","TDN (mg/L)",2)+my_theme_avg_marine
    TDPAvg<-QAvgNutPlot_marine(data1,"TDP","TDP","TDP (mg/l)",0)+my_theme_avg_marine
    NO3Avg<-QAvgNutPlot_marine(data1,"NO3",expression("NO"[3]^"-"*"+NO"[2]^"-"),expression("NO"[3]^"-"*"+NO"[2]^"-"*" (mg/l)"),0)+my_theme_avg_marine

    ChlAvg<-QAvgYsiPlot_marine(data2,"Chl","Chlorophyll","Chlorophyll (ug/L)",1)+my_theme_avg_marine
    pHAvg<-QAvgYsiPlot_marine(data2,"pH","pH","pH",2)+my_theme_avg_marine
    SalAvg<-QAvgYsiPlot_marine(data2,"Sal","Salinity","Salinity (ppt)",2)+my_theme_avg_marine
    ODOConcAvg<-QAvgYsiPlot_marine(data2,"ODOConc","Dissolved Oxygen Concentration","DO Concentration (mg/L)",1)+my_theme_avg_marine
    ODOSatAvg<-QAvgYsiPlot_marine(data2,"ODOSat","Dissolved Oxygen Saturation","DO (% Saturation)",1)+my_theme_avg_marine
    SpCondAvg<-QAvgYsiPlot_marine(data2,"SpCond","Specific Conductivity","Specific Conductivity (uS/cm)",2)+my_theme_avg_marine
    TempAvg<-QAvgYsiPlot_marine(data2,"Temp","Temperature",expression(paste("Temperature (",degree,"C)")),1)+my_theme_avg_marine
    TurbidAvg<-QAvgYsiPlot_marine(data2,"Turbid+","Turbidity","Turbidity (NTU)",1)+my_theme_avg_marine

  }else{

    #####
    #### Make Average Plots for all parameters ####
    # Adjust main title, axis title, and number of digits on the y axis as needed

    TDNAvg<-QAvgNutPlot(data1,"TDN","TDN","TDN (mg/L)",2)+my_theme_avg
    TDPAvg<-QAvgNutPlot(data1,"TDP","TDP","TDP (mg/l)",0)+my_theme_avg
    NO3Avg<-QAvgNutPlot(data1,"NO3",expression("NO"[3]^"-"*"+NO"[2]^"-"),expression("NO"[3]^"-"*"+NO"[2]^"-"*" (mg/l)"),0)+my_theme_avg

    ChlAvg<-QAvgYsiPlot(data2,"Chl","Chlorophyll","Chlorophyll (ug/L)",1)+my_theme_avg
    pHAvg<-QAvgYsiPlot(data2,"pH","pH","pH",2)+my_theme_avg
    SalAvg<-QAvgYsiPlot(data2,"Sal","Salinity","Salinity (ppt)",2)+my_theme_avg
    ODOConcAvg<-QAvgYsiPlot(data2,"ODOConc","Dissolved Oxygen Concentration","DO Concentration (mg/L)",1)+my_theme_avg
    ODOSatAvg<-QAvgYsiPlot(data2,"ODOSat","Dissolved Oxygen Saturation","DO (% Saturation)",1)+my_theme_avg
    SpCondAvg<-QAvgYsiPlot(data2,"SpCond","Specific Conductivity","Specific Conductivity (uS/cm)",2)+my_theme_avg
    TempAvg<-QAvgYsiPlot(data2,"Temp","Temperature",expression(paste("Temperature (",degree,"C)")),1)+my_theme_avg
    TurbidAvg<-QAvgYsiPlot(data2,"Turbid+","Turbidity","Turbidity (NTU)",1)+my_theme_avg
  }

  #### Combination Graphs ####
  # Plots for the nine main parameters on one page
  # Adjust the plot titles.

  return(ggpubr::annotate_figure(ggpubr::ggarrange(TDNAvg,TDPAvg,NO3Avg,ChlAvg,pHAvg,SalAvg,TempAvg,TurbidAvg,ODOSatAvg,nrow=3,ncol=3,common.legend=TRUE,legend="bottom"),top=graphtitle))

}
