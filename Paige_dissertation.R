#R code for generating plots for your BFF/labmate/brother's GF/'beaker to your bunsen'
#Initial start 06-25-2017
#Last updated 07-08-2017

setwd("/Users/SarahHu 1/Desktop/Projects/")
library(reshape2)
library(ggplot2)
# library(ggthemes)
# library("wesanderson")
# names(wes_palettes)
# Import data that has been meticulously collected over the past four years. It is definately real data. Was not fabricated.

#Diel time scale
d<-read.delim("Pdiss_diel.txt",header=TRUE); names(d)
d.m<-melt(d); head(d.m)
unique(d.m$variable)
d.m<-subset(d.m, !(variable %in% "Sleepiness"))
#Factor and assign colors/line types
d.m$time<-factor(d.m$Time.of.day, levels=c("7:00 AM","8:00 AM","9:00 AM","10:00 AM","11:00 AM","12:00 PM","1:00 PM","2:00 PM","3:00 PM","4:00 PM","5:00 PM","6:00 PM","7:00 PM","8:00 PM","9:00 PM","10:00 PM","11:00 PM"))
d.m$var<-factor(d.m$variable, levels=c("Annoyed", "Productivity", "chattiness","Number.of.hours.after.coffee"))

diel<-ggplot(d.m, aes(x=time, y=value, linetype= Person, group=Person))+geom_smooth(size=2.2,stat = "smooth",se = FALSE, aes(fill=var, color=var))+ theme_bw()+scale_linetype_manual(values=c("solid","twodash"))+theme(axis.text.x = element_text(size=13,angle=45, color="black", hjust=1, vjust=1),axis.text.y = element_text(color="black", size=13))+labs(x="", y="Scale (1-10)")+geom_rect(data=NULL,aes(xmin=13.5,xmax=18,ymin=-Inf,ymax=Inf),fill="#737373",alpha=0.008)+geom_smooth(size=2.2,stat = "smooth",se = FALSE, aes(fill=var, color=var))+theme(legend.position = "top")+theme(strip.text.x=element_blank(),strip.background = element_blank(),panel.border = element_rect(colour = "black"))+scale_color_manual(values=wes_palette("Darjeeling"))+facet_grid(var~.)
diel
#saved as SVG: W:1290 ,H:500


##############################################################################################################
#Import year to year data (again, not fabricated)
yr<-read.delim("Pdiss_interannual.txt",header=TRUE); names(yr)
yr.m<-melt(yr); head(yr.m)
# unique(yr.m$Month.year)
#Factor and assign colors/line types
yr.m$yrmonth<-factor(yr.m$Month.year, levels=c("Jul-2013","Oct-2013","Jan-2014","Apr-2014","Jul-2014","Oct-2014","Jan-2015","Apr-2015","Jul-2015","Oct-2015","Jan-2016","Apr-2016","Jul-2016","Oct-2016","Jan-2017","Apr-2017","Jul-2017"))
yr.m<-na.omit(yr.m)
# annual<-ggplot(yr.m, aes(x=yrmonth, y=value, linetype= Person, group=variable))+geom_smooth(size=2.2,stat = "smooth",se = FALSE, aes(fill=variable, color=variable))+facet_grid(.~Person)+theme_bw()+scale_linetype_manual(values=c("solid","twodash"))+theme(axis.text.x = element_text(size=13,angle=45, color="black", hjust=1, vjust=1),axis.text.y = element_text(color="black", size=13))+labs(x="", y="Scale (1-10)")+theme(legend.position = "top")+theme(strip.text.x=element_blank(),strip.background = element_blank(),panel.border = element_rect(colour = "black"))+scale_color_manual(values=wes_palette("Cavalcanti"))
# annual
#saved as SVG: W:1290 ,H:500

##Bar plots to show interannual trends
head(yr.m)
unique(yr.m$variable)
annual_bar<-ggplot(yr.m, aes(x=yrmonth, y=value))+geom_bar(stat = "identity",position="dodge",color="black",aes(fill=variable, color=variable, alpha=Person))+facet_wrap(~variable,ncol=1)+theme_few()+theme(axis.text.x = element_text(size=13,angle=45, color="black", hjust=1, vjust=1),axis.text.y = element_text(color="black", size=13))+labs(x="", y="Scale (1-10)")+theme(legend.position = "top")+theme(strip.text.x=element_blank(),strip.background = element_blank(),panel.border = element_rect(colour = "black"))+scale_fill_manual(values=c("#cb181d","#238443", "#810f7c", "#e7298a"))+scale_alpha_manual(values=c(0.4,1))
annual_bar
#save SVG - W:750, H: 850

###
#Searching for seasonal trends
##Possible seasonal trend in sass and pun frequency
split<-colsplit(yr.m$Month.year, "-", c("Month", "Year"))
byyear<-data.frame(split,yr.m)
head(byyear)
byyear$mon<-factor(byyear$Month, levels=c("Jan", "Apr", "Jul", "Oct"), labels = c("Winter", "Spring", "Summer", "Fall"))
yr.box<-ggplot(byyear, aes(x=mon, y=value, fill=variable))+geom_boxplot(color="black")+theme_few() +facet_grid(variable~Person, scales = "free")+scale_fill_manual(values=c("#810f7c", "#e7298a"))
rm<-c("Tolerance", "PhD.student.greatness")
yr.box %+% subset(byyear, !(variable %in% rm))
#save SVG: W:790, H: 600
# box<-ggplot(df_w[order(df_w$time_order),], aes(x=time_order, y=Ratio, fill=tax))+facet_wrap(~tax, scales = "free")+geom_boxplot(color="black")+theme_bw() +theme(axis.line = element_line(colour = "black"),panel.grid.minor = element_blank(),panel.background = element_blank(),strip.background = element_blank(), axis.text.x = element_text(angle=45,hjust = 1,vjust = 1))+theme(legend.position="right")+labs(title="Boxplots representing OTU RNA:DNA ratios",x="",y="")+scale_fill_manual(values = tax_color)+geom_rect(data=NULL,aes(xmin=3.5,xmax=6.5,ymin=-Inf,ymax=Inf),fill="#737373",alpha=0.1)+geom_boxplot(color="black")