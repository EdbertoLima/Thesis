library(lubridate)  ### Packpage to convert data
library(tidyverse) ###
library(plyr) ###
library(plotly) ###
library(ggplot2) ###

tblchuvaANA = read.delim("D:/Documentos/Chuvas.csv", sep = ";")
tblchuvaANA$Data <- as.Date(tblchuvaANA$Data,
                                     format = "%d/%m/%Y")
tblchuvaANA1998_2019 <- tblchuvaANA %>%
                        filter(Data >= as.Date('1998-01-01'))

listANA = lapply(1:nrow(tblchuvaANA1998_2019), function(x){
                    output_df = data.frame("EstacaoCodigo"  = tblchuvaANA1998_2019[x,"EstacaoCodigo"],
                                            "NivelConsistencia" = tblchuvaANA1998_2019[x,"NivelConsistencia"],
                                            "Data" = seq.Date(as.Date(tblchuvaANA1998_2019[x,"Data"], "%d/%m/%Y"), 
                                               by = "day", length.out = days_in_month(as.Date(tblchuvaANA1998_2019[x,"Data"], "%d/%m/%Y"))),
                                            "TipoMedicaoChuvas" = tblchuvaANA1998_2019[x,"TipoMedicaoChuvas"],
                                            "Chuva" = (t(tblchuvaANA1998_2019[x,] %>% 
                                                             select(starts_with("Chuva"),-contains("Status")) %>% 
                                                             select(1:days_in_month(as.Date(tblchuvaANA1998_2019[x,"Data"], "%d/%m/%Y"))))),
                                            "ChuvaStatus" = (t(tblchuvaANA1998_2019[x,] %>% 
                                                             select(starts_with("Chuva") & ends_with("Status")) %>% 
                                                             select(1:days_in_month(as.Date(tblchuvaANA1998_2019[x,"Data"], "%d/%m/%Y")))))
                                    ) 
                                    names(output_df)[5:6] = c("Chuva", "ChuvaStatus")
                                    return(output_df)
}
                )

Chuva_Ana = do.call(rbind, listANA)


### Plot https://towardsdatascience.com/time-series-calendar-heatmaps-9f576578fcfe
Chuva_Ana$weekday  =  as.POSIXlt(Chuva_Ana$Data)$wday
Chuva_Ana$weekdayf<-factor(Chuva_Ana$weekday,levels=rev(0:6),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE) 
Chuva_Ana$monthf<-factor(month(Chuva_Ana$Data),levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE) 
Chuva_Ana$yearmonth<- factor(as.yearmon(Chuva_Ana$Data))

Chuva_Ana$week <- as.numeric(format(Chuva_Ana$Data,"%W"))
Chuva_Ana<-ddply(Chuva_Ana,.(yearmonth),transform,monthweek=1+week-min(week))
Chuva_Ana.Est= split(Chuva_Ana, f = Chuva_Ana$EstacaoCodigo[drop = TRUE])

lapply(names(Chuva_Ana.Est), function(x) 
  ggplot(Chuva_Ana.Est[[x]], aes(monthweek, weekdayf, fill = Chuva)) + 
          geom_tile(colour = "white") + 
          facet_grid(year(Chuva_Ana.Est[[x]]$Data)~monthf) + 
          scale_fill_gradientn( colours = c("white", "lightblue", "blue"), values = c(0,0.1,1)) + 
          xlab("Week of Month") + 
          ylab("") + 
          theme_classic()+
          ggtitle(paste0("Time-Series Rain ",x)) + 
          labs(fill = "Chuva"))



