#upload latest compiled data and name it with the current date
library(ggplot2)
library(psych)
library(plyr)
library(dplyr)
library(plotly)
library(grid)
library(gridExtra)
library(readxl)
library(ggpubr)
library(RColorBrewer)
library(scales)
library(cowplot)
library("shiny")
library(reshape2)
library(pastecs)
library(openxlsx)
library(forcats)
library(ggforce)
library(gplots)
 

####### read data(loading data)
library(readxl)
IDR_Data_all <- read_excel("C:/Users/horac/Box/HSJakpa/USFexperiments/IDR_POU/IDR_pou_Data_master.xlsx",
                       sheet = "All_Data")
View(IDR_Data_all)

######transforming character variables to numeric variable 
IDR_Data_all $nitrite_all <- as.numeric(IDR_Data_all $nitrite_all)
IDR_Data_all $Free_NH3_N <- as.numeric(IDR_Data_all $Free_NH3_N)
IDR_Data_all $mono_mgL_all <- as.numeric(IDR_Data_all $mono_mgL_all)

######make some new columns
IDR_Data_all $Loc.f<-revalue(factor(IDR_Data_all $Loc),c("0"="Tap","1"="Filter 1", "2"="Filter 2", "3"="Filter 3")) #making filter location a factor variable in new column
IDR_Data_all $Loc.f<-revalue(factor(IDR_Data_all $Loc),c("0"="Tap","1"="Filter 1", "2"="Filter 2", "3"="Filter 3"))
IDR_Data_all $Loc.fname<-revalue(factor(IDR_Data_all $Loc), c("0"="Tap","1"="Filter 1", "2"="Filter 2", "3"="Filter 3"))

IDR_Data_all $ff.f<-factor(IDR_Data_all $stagff)#factoring staganation and first flow sample catergories
IDR_Data_all $ff.name<-revalue(factor(IDR_Data_all $stagff), c("0"="Stagnant","1"="First Flow" ))#making new coloumn for staganation and first flow samples 
levels(IDR_Data_all $ff.name)
levels(IDR_Data_all $ff.name) = c("Stagnation","First Flow", "")# making levels to creat an order btw stag and FF samples 
IDR_Data_all $ff.name[which(is.na(IDR_Data_all $ff.name))]<-"" # replacing NA with empty cells 

IDR_Data_all $prepost.name<-revalue(factor(IDR_Data_all $flushprepost), c("1"="Stagnation", "2"="Postflush")) #factoring anf recoding stag as prefush and PF as Postflush

IDR_Data_all $CoCe.name<-revalue(factor(IDR_Data_all $CoCe), c("20"="End of day", "21"="Mornings")) # factoring and recoing CO( friday PF as end of day) AND Ce (monday stag as monring)

#####
IDR_Data_all $ff.dur<-IDR_Data_all $stagdur_min
IDR_Data_all $ff.dur[IDR_Data_all $stag=="1" & IDR_Data_all $expcode=="1"]<-1080
IDR_Data_all $ff.dur[IDR_Data_all $stag=="1" & IDR_Data_all $expcode=="3"]<-3960
IDR_Data_all $ff.dur[IDR_Data_all $stag=="1" & IDR_Data_all $expcode=="2"]<-0

IDR_Data_all $stag.dur<-IDR_Data_all $stagdur_min
IDR_Data_all $stag.dur[IDR_Data_all $stag=="1" & IDR_Data_all $expcode=="1"]<-"Overnight"
IDR_Data_all $stag.dur[IDR_Data_all $stag=="1" & IDR_Data_all $expcode=="3"]<-"Weekend"


IDR_Data_all $flush.dur<-revalue(factor(IDR_Data_all $OvWkPF), c("2"="Overnight", "3"="Weekend"))
#IDR_Data_all $flush.dur[IDR_Data_all $expcode=="6" & IDR_Data_all $OvWkPF=="2"]<-"Overnight"
#IDR_Data_all $flush.dur[IDR_Data_all $expcode=="6" & IDR_Data_all $OvWkPF=="3"]<-"Weekend"
levels(IDR_Data_all $flush.dur)

IDR_Data_all $stagprof.f<-revalue(factor(IDR_Data_all $Stagprof), c("1"="Stagnation", "2"="Postflush"))
IDR_Data_all $prepost.name<-revalue(factor(IDR_Data_all $flushprepost), c("1"="Stagnation", "2"="Postflush"))
levels(IDR_Data_all $stagprof.f)

IDR_Data_all$InfEff.name<-IDR_Data_all$flushprepost
IDR_Data_all $InfEff.name[IDR_Data_all $flushprepost=="0"]<-"Unfiltered"
IDR_Data_all $InfEff.name[IDR_Data_all $flushprepost=="1" & IDR_Data_all $stagff=="0"]<-"Stagnation"
IDR_Data_all $InfEff.name[IDR_Data_all $flushprepost=="1" & IDR_Data_all $stagff=="1"]<-"Firstflow"
IDR_Data_all $InfEff.name[IDR_Data_all $flushprepost=="2"]<-"Filtered"
levels(IDR_Data_all $InfEff.name) = c("0"="Unfiltered","1"="Stagnation", "2"="Filtered")
levels(IDR_Data_all $InfEff.name)

IDR_Data_all $flush.name[IDR_Data_all $flushprepost=="0"]<-"Tap"
IDR_Data_all $flush.name[IDR_Data_all $flushprepost=="1" & IDR_Data_all $stagff=="0"]<-"Stagnation"
IDR_Data_all $flush.name[IDR_Data_all $flushprepost=="1" & IDR_Data_all $stagff=="1"]<-"Firstflow"
IDR_Data_all $flush.name[IDR_Data_all $flushprepost=="2"]<-"Post Flush"
levels(IDR_Data_all $flush.name) = c("0"="Tap","1"="Stagnation","1"="Firstflow", "2"="Postflush")
levels(IDR_Data_all $flush.name)

View(IDR_Data_all )

#levels(IDR_Data_all $Pre_Postflush.name)
#levels(IDR_Data_all $Pre_Postflush.name) = c("0"="Tap","1"="Stagnation", "2"="Postflush")
#IDR_Data_all $Pre_Postflush.name[which(is.na(IDR_Data_all $Pre_Postflush.name))]<-"111"
#IDR_Data_all $Pre_Postflush.name<-NA

IDR_Data_all $Pre_Postflush.name<-factor(IDR_Data_all $OvWkPF)
IDR_Data_all $Pre_Postflush.name<-revalue(factor(IDR_Data_all $OvWkPF), c("0"="Tap","1"="Stagnation", "2"="Postflush"))
IDR_Data_all $Pre_Postflush.name[which(is.na(IDR_Data_all $Pre_Postflush.name))]<-""
levels(IDR_Data_all $Pre_Postflush.name) = c("Tap","Staganation", "Postflush","") 
levels(IDR_Data_all $Pre_Postflush.name)

IDR_Data_all$stagffpf.name<-IDR_Data_all$stagffpf
IDR_Data_all$stagffpf.name[IDR_Data_all$stagffpf=="0" & IDR_Data_all$stagff=="0"]<-"Stagnation"
IDR_Data_all$stagffpf.name[IDR_Data_all$stagffpf=="1" & IDR_Data_all$stagff=="1"]<-"Firstflow"
IDR_Data_all$stagffpf.name[IDR_Data_all$stagffpf=="2"]<-"Post Flush"
levels(IDR_Data_all$stagffpf.name) = c("0"="Stagnation","1"="Firstflow", "2"="Postflush")
levels(IDR_Data_all$stagffpf.name)

IDR_Data_all $ff.f<-factor(IDR_Data_all $stagff)
IDR_Data_all $ff.name<-revalue(factor(IDR_Data_all $stagff), c( "0"="Stagnant","1"="First Flow"))
levels(IDR_Data_all $ff.name)
levels(IDR_Data_all $ff.name) = c("Stagnation","First Flow", "") 
IDR_Data_all $ff.name[which(is.na(IDR_Data_all $ff.name))]<-""


IDR_Data_all $phase.f<-factor(IDR_Data_all $phase)
IDR_Data_all $phase<-revalue(factor(IDR_Data_all $phase), c("0"="Transition","1"="Pre_FClP","2"="FClP", "3"="Post_FClP"))

levels(IDR_Data_all $phase)
levels(IDR_Data_all $phase.f) = c("0"="Transition","1"="Pre_FClP","2"="FClP", "3"="Post_FClP")
#IDR_Data_all $phase.f[which(is.na(IDR_Data_all $phase.f))]<-""


IDR_Data_all $Residual.f<-revalue(factor(IDR_Data_all $Residual), c("0"="Transition","1"="Chloramine","2"="Free-Chlorine"))
levels(IDR_Data_all $Residual) = c("0"="Transition","1"="Chloramine","2"="Free-Chlorine")
levels(IDR_Data_all $Residual)

IDR_Data_all $Burn.f<-factor(IDR_Data_all $Burn)
IDR_Data_all $Burn<-revalue(factor(IDR_Data_all $Burn), c("0"="Transition","1"="Chloramine","2"="FClP 1", "3"="FClP 2"))
  levels(IDR_Data_all $Burn)
levels(IDR_Data_all $Burn) = c("0"="Transition","1"="Chloramine","2"="FClP 1", "3"="FClP 2")

IDR_Data_all $LmtFClP.f<-factor(IDR_Data_all $LmtFClP)
#IDR_Data_all $LmtFClP<-revalue(factor(IDR_Data_all $LmtFClP), c("0"="Transition","1"="Pre-FClP1","2"="FClP 1", "3"="Post-FClP 1","4"="FClP 2","5"="Post-FClP 2"))
levels(IDR_Data_all $LmtFClP) = c("0"="Transition","1"="Pre-FClP1","2"="FClP 1", "3"="Post-FClP 1","4"="FClP 2","5"="Post-FClP 2")
levels(IDR_Data_all $LmtFClP)

IDR_Data_all  <- mutate(IDR_Data_all ,Bvol=round(cumvol/0.300))


IDR_Data_all $ageweeks.f<-factor(IDR_Data_all $ageweeks)

IDR_Data_all $weeksgp5<-((ceiling(((IDR_Data_all $ageweeks))/(5))))
IDR_Data_all $weeksgp5.f<-factor(IDR_Data_all $weeksgp5)
IDR_Data_all $weeksgp5.fname<-revalue(factor(IDR_Data_all $weeksgp5),c("1"="1-5", "2"="6-10", "3"="11-15", "4"="16-20","7"="31-35", "8"="36-40","9"="41-45", "10"="46-50", "11"="51-55"))
#, ))


#change format of date time to standard one. Create new column called "mydates"
IDR_Data_all $OrigDate<-as.POSIXct(IDR_Data_all $OrigDate)
IDR_Data_all $mydates<-as.POSIXct(IDR_Data_all $OrigDate)
min(IDR_Data_all $mydates, na.rm=TRUE)
max(IDR_Data_all $mydates, na.rm=TRUE)

IDR_Data_all $asDate <- as.Date(IDR_Data_all $OrigDate, format = "%m/%d/%Y")
IDR_Data_all $asMonth <- months(IDR_Data_all $mydates)
IDR_Data_all $asMonthYear.f <- factor(format(as.Date(IDR_Data_all $mydates, format = "%Y-%m")))

IDR_Data_all $monthyear <- format(as.Date(IDR_Data_all $OrigDate), "%Y-%m")

IDR_Data_all $asDate <- as.Date(IDR_Data_all $OrigDate, format = "%m/%d/%Y")
IDR_Data_all $asMonthYear.f <- factor(format(as.Date(IDR_Data_all $OrigDate, format = "%Y-%m")))


IDR_Data_all $N_NN.f<-factor(IDR_Data_all $N_NN)
IDR_Data_all $N_NN.f<-factor(IDR_Data_all $N_NN)
IDR_Data_all $N_NN.f<-revalue(factor(IDR_Data_all $N_NN), c("1"="Nitrifying", "2"="Not Nitrifying"))
levels(IDR_Data_all $N_NN.f)
#levels(IDR_Data_all $N_NN.f) = c("1"="Nitrifying", "2"="Not Nitrifying") 

IDR_Data_all $FtUF.f<-factor(IDR_Data_all $FtUF)
IDR_Data_all $FtUF.f<-factor(IDR_Data_all $FtUF)
IDR_Data_all $FtUF.f<-revalue(factor(IDR_Data_all $FtUF), c("0"="Unfiltered", "1"="Filtered"))
levels(IDR_Data_all $FtUF.f) = c("0"="Unfiltered", "1"="Filtered") 
levels(IDR_Data_all $FtUF.f)
#####Date formats#########

xminmonth<-as.POSIXlt(as.Date(max(IDR_Data_all $mydates, na.rm=TRUE))-30)
xmin<-as.POSIXlt(as.Date(min(IDR_Data_all $mydates, na.rm=TRUE)))
xmax<-as.POSIXlt(as.Date(max(IDR_Data_all $mydates, na.rm=TRUE)))

xmin
xmax
xminmonth

#colour codes####
stag4colors<-c("black", "dodgerblue", "navy", "orange")
stag3colors_black<-c("black", "grey50", "grey91")
stag3colors<-c( "dodgerblue", "navy","barkblue")
stag2colors_black<-c("grey90","grey4")
stag2colors<-c("#332288", "#882255")

#color codes#
overweekcol = c("dodgerblue", "navy")#c("dodgerblue", "#882255")
Stagdurcol =  c("dodgerblue", "navy")#c("dodgerblue", "#882255")
Pre_post = c("#fff7bc","#31a354")
Pre_post3col= c("#fc9272","#fff7bc","#31a354")
Pre_post4col= c("#fc9272","#fff7bc","#31a354","#636363")
tapstagpf=c("#ffffcc","#525252","#cccccc","#f7f7f7")
greyhue = c("#f0f0f0","#bdbdbd","#636363")

weekcolors2<-c( "lightgoldenrod1", "chocolate1", "orangered4",  "black", "navy",  "skyblue2","slategray1", "yellow4", "yellowgreen")

Watercolor=c("#ffffcc","grey90")
Burncolors2<-c( "#ce1256", "#d7b5d8")
Burncolors3<-c( "lightgoldenrod1", "chocolate1", "orangered4")
N_NN <- c("#fa9fb5","#c51b8a")

#####FILTER DATA TO REMOVE TRANSITION ########

IDR_Data<- as.data.frame( IDR_Data_all %>% filter( phase!="Transition"))
View(IDR_Data)
############################################################################################################################

######statisitical comparisms#####
my_comparisonsphase <- list(c("Burn", "Post-Burn"), c("Burn", "Pre-Burn"),c("Post-Burn", "Pre-Burn"))
my_comparisonsPFSTAG <- list(c("Post Flush", "Stagnation"))
my_comparisonsPFFFSTAG <- list(c("Firstflow", "Post Flush"), c("Post Flush", "Stagnation"), c("Stagnation", "Firstflow"))
my_comparisonsOVwk <- list(c("Weekend", "Overnight"))

#summary stats od data base on phase period######
#install.packages("stargazer")
####normality test######
#library(starga)

####shapiro.test(IDR_Data $nitrite_all) # => p = NORMALITY TEST 

#####summary statistics Inffluent and Effluent only #######

install.packages(openxlsx)
library(openxlsx)
Tap<-  IDR_Data %>% filter((mydates >= "2020-07-13") & phase!="0" & (Loc ==0))### without transition data facet by resisudal
datasumtap <- Tap %>%
  group_by(Burn) %>%
  summarise_at(c("pH_all","tempC_all","DO_mgL","TCl2_mgL_all","FCl2" ,"mono_mgL_all","Free_NH3_N","nitrate_all","nitrite_all","TAtp_RLU"), mean,na.rm = TRUE)          
datasumtaptable<- as.data.frame(datasumtap)
datasumtap
write.xlsx(datasumtaptable,file="C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/POU thesis data sumry/tapmean summary.xlsx")

Tap<-  IDR_Data %>% filter((mydates >= "2020-07-13")& phase!="0" & (Loc ==0))
datasumtapSD <- Tap %>%
  group_by(Burn) %>%
  summarise_at(c("pH_all","tempC_all","DO_mgL","TCl2_mgL_all","FCl2","mono_mgL_all","Free_NH3_N","nitrate_all","nitrite_all","TAtp_RLU"), sd,na.rm = TRUE) 
datasumtapSD 
write.xlsx(datasumtapSD ,file="C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/POU thesis data sumry/tap summary(SD).xlsx")
#addWorksheet(tapmeansummary, "Sheet 2")

Tap<-  IDR_Data %>% filter((mydates >= "2020-07-13") & phase!="0" & (Loc ==0))### without transition data facet by Lmtburn pahses
datasumtap <- Tap %>%
  group_by(LmtFClP) %>%
  summarise_at(c("pH_all","tempC_all","DO_mgL","TCl2_mgL_all","FCl2" ,"mono_mgL_all","Free_NH3_N","nitrate_all","nitrite_all","TAtp_RLU"), mean,na.rm = TRUE)          
datasumtaptable<- as.data.frame(datasumtap)
datasumtap
write.xlsx(datasumtaptable,file="C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/POU thesis data sumry/tapmean LmtFClP summary.xlsx")

Tap<-  IDR_Data %>% filter((mydates >= "2020-07-13")& phase!="0" & (Loc ==0))
datasumtapSD <- Tap %>%
  group_by(LmtFClP) %>%
  summarise_at(c("pH_all","tempC_all","DO_mgL","TCl2_mgL_all","FCl2","mono_mgL_all","Free_NH3_N","nitrate_all","nitrite_all","TAtp_RLU"), sd,na.rm = TRUE) 
datasumtapSD
write.xlsx(datasumtapSD ,file="C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/POU thesis data sumry/tapSD LmtFClP summary(SD).xlsx")



Tap<-  IDR_Data %>% filter((mydates >= "2020-07-13") & (Loc ==0))#### with transition data
datasumtap <- Tap %>%
  group_by(Burn) %>%
  summarise_at(c("pH_all","tempC_all","DO_mgL","TCl2_mgL_all","FCl2" ,"mono_mgL_all","Free_NH3_N","nitrate_all","nitrite_all","TAtp_RLU"), min,na.rm = TRUE)          
datasumtaptable<- as.data.frame(datasumtap)
write.xlsx(datasumtaptable,file="C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/POU thesis data sumry/tap min summary.xlsx")

Tap<-  IDR_Data %>% filter((mydates >= "2020-07-13") & (Loc ==0))
datasumtapSD <- Tap %>%
  group_by(Burn) %>%
  summarise_at(c("pH_all","tempC_all","DO_mgL","TCl2_mgL_all","FCl2","mono_mgL_all","Free_NH3_N","nitrate_all","nitrite_all","TAtp_RLU"), mean,na.rm = TRUE) 
datasumtapSD 
write.xlsx(datasumtapSD ,file="C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/POU thesis data sumry/tap max summary(SD).xlsx")

filter1<-  IDR_Data %>% filter((mydates >= "2020-07-13") &  flushprepost=="2")
datasumFilter1 <- filter1 %>%
  group_by() %>%
    summarise_at(c("pH_all","tempC_all","DO_mgL","TCl2_mgL_all","FCl2","mono_mgL_all","Free_NH3_N","nitrate_all","nitrite_all","TAtp_RLU"), sd, na.rm = TRUE)          
datasumFilter1
datasumfilter1table<- as.data.frame(datasumFilter1)
write.xlsx(datasumFilter1,file="C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/POU thesis data sumry/Filter1 summary.xlsx")


filter1<-  IDR_Data %>% filter((mydates >= "2020-07-13") & (Loc ==1)&flushprepost=="2" )
datasumFilter1SD<- filter1 %>%
  group_by(LmtFClP) %>%
  summarise_at(c("pH_all","tempC_all","DO_mgL","TCl2_mgL_all","FCl2","mono_mgL_all","Free_NH3_N","nitrate_all","nitrite_all","TAtp_RLU"), sd, na.rm = TRUE)          
datasumfilter1tableSD<- as.data.frame(datasumFilter1SD)
write.xlsx(datasumfilter1tableSD,file="C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/POU thesis data sumry/Filter1SD summary.xlsx")


filter2<-  IDR_Data %>% filter((mydates >= "2020-07-13") & (Loc ==2)&flushprepost=="2")
datasumFilter2 <- filter2 %>%
  group_by(Residual) %>%
  summarise_at(c("pH_all","tempC_all","DO_mgL","TCl2_mgL_all","FCl2","mono_mgL_all","Free_NH3_N","nitrate_all","nitrite_all","TAtp_RLU"), mean, na.rm = TRUE)          
datasumfilter2table<- as.data.frame(datasumFilter2)
write.xlsx(datasumfilter2table,file="C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/POU thesis data sumry/Filter2 summary.xlsx")

filter2<-  IDR_Data %>% filter((mydates >= "2020-07-13") & (Loc ==2)&flushprepost=="2")
datasumFilter2SD <- filter2 %>%
  group_by(Residual) %>%
  summarise_at(c("pH_all","tempC_all","DO_mgL","TCl2_mgL_all","FCl2","mono_mgL_all","Free_NH3_N","nitrate_all","nitrite_all","TAtp_RLU"), sd, na.rm = TRUE)          
datasumfilter2tableSD<- as.data.frame(datasumFilter2SD)
write.xlsx(datasumfilter2tableSD,file="C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/POU thesis data sumry/Filter2SD summary.xlsx")


filter3<-  IDR_Data %>% filter((mydates >= "2020-07-13") & (Loc ==3)&flushprepost=="2")
datasumFilter3 <- filter3 %>%
  group_by(Residual) %>%
  summarise_at(c("pH_all","tempC_all","DO_mgL","TCl2_mgL_all","FCl2","mono_mgL_all","Free_NH3_N","nitrate_all","nitrite_all","TAtp_RLU"), mean, na.rm = TRUE)          
datasumfilter3table<- as.data.frame(datasumFilter3)
write.xlsx(datasumfilter3table,file="C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/POU thesis data sumry/Filter3 summary.xlsx")

filter3<-  IDR_Data %>% filter((mydates >= "2020-07-13") & (Loc ==3)&flushprepost=="2")
datasumFilter3SD <- filter3 %>%
  group_by(Residual) %>%
  summarise_at(c("pH_all","tempC_all","DO_mgL","TCl2_mgL_all","FCl2","mono_mgL_all","Free_NH3_N","nitrate_all","nitrite_all","TAtp_RLU"), sd, na.rm = TRUE)          
datasumfilter3tableSD<- as.data.frame(datasumFilter3SD)
write.xlsx(datasumfilter3tableSD,file="C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/POU thesis data sumry/Filter3SD summary.xlsx")

######################################
####STganation  filter use sample summarry###############Date less than March before second Burn########

Tap<-  IDR_Data %>% filter((mydates <= "2021-03-01") & (Loc ==0))
datasumtap <- Tap %>%
  group_by(phase) %>%
  summarise_at(c("pH_all","tempC_all","DO_mgL","TCl2_mgL_all","FCl2" ,"mono_mgL_all","Free_NH3_N","nitrate_all","nitrite_all","TAtp_RLU"), mean,na.rm = TRUE) 

filter1<- as.data.frame( IDR_Data %>% filter((mydates >= "2020-08-05") & (Loc ==1)&pahse!="0" & flushprepost=="1" &stagff=="0"))
datasumF1 <- filter1 %>%
  group_by(LmtFClP) %>%
  summarise_at(c("pH_all","tempC_all","DO_mgL","TCl2_mgL_all","FCl2","mono_mgL_all","Free_NH3_N","nitrate_all","nitrite_all","TAtp_RLU"), mean, na.rm = TRUE)          
datasumF1 
datasumf1table<- as.data.frame(datasumF1)
write.xlsx(datasumF1,file="C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/POU thesis data sumry/F1March mean  summary.xlsx")


filter1<-  IDR_Data %>% filter((mydates >= "2020-08-05") & (Loc ==1)&flushprepost=="1" &stagff=="0")
datasumF1<- filter1 %>%
  group_by(LmtFClP) %>%
  summarise_at(c("pH_all","tempC_all","DO_mgL","TCl2_mgL_all","FCl2","mono_mgL_all","Free_NH3_N","nitrate_all","nitrite_all","TAtp_RLU"), sd, na.rm = TRUE)          
datasumfilter1tableSD<- as.data.frame(datasumF1)
datasumF1
write.xlsx(datasumfilter1tableSD,file="C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/POU thesis data sumry/Filter1SD summary.xlsx")


filter2<-  as.data.frame(IDR_Data %>% filter( (Loc ==2)&flushprepost=="1" &stagff=="0"))
datasumF2<- filter2 %>%
  group_by(LmtFClP) %>%
  summarise_at(c("pH_all","tempC_all","DO_mgL","TCl2_mgL_all","FCl2","mono_mgL_all","Free_NH3_N","nitrate_all","nitrite_all","TAtp_RLU"), mean, na.rm = TRUE)          
datasumf2table<- as.data.frame(datasumF2)
datasumF2
write.xlsx(datasumf2table,file="C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/POU thesis data sumry/F2March summary.xlsx")

filter2<-  IDR_Data %>% filter((mydates >= "2020-08-05") & (Loc ==2)&flushprepost=="1" &stagff=="0")
datasumF2SD <- filter2 %>%
  group_by(Residual) %>%
  summarise_at(c("pH_all","tempC_all","DO_mgL","TCl2_mgL_all","FCl2","mono_mgL_all","Free_NH3_N","nitrate_all","nitrite_all","TAtp_RLU"), sd, na.rm = TRUE)          
datasumf2tableSD<- as.data.frame(datasumF2SD)
write.xlsx(datasumf2tableSD,file="C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/POU thesis data sumry/F2SDMarch summary.xlsx")


filter3<-  IDR_Data %>% filter((mydates >= "2020-08-05") &  (Loc ==3)&flushprepost=="1" &stagff=="0")
datasumF3 <- filter3 %>%
  group_by(LmtFClP) %>%
  summarise_at(c("pH_all","tempC_all","DO_mgL","TCl2_mgL_all","FCl2","mono_mgL_all","Free_NH3_N","nitrate_all","nitrite_all","TAtp_RLU"), mean, na.rm = TRUE)          
datasumfilter3table<- as.data.frame(datasumF3)
datasumF3
write.xlsx(datasumf3table,file="C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/POU thesis data sumry/F3March summary.xlsx")

filter3<-  IDR_Data %>% filter((mydates >= "2020-08-05") & (Loc ==3)&flushprepost=="1" &stagff=="0")
datasumF3SD <- filter3 %>%
  group_by(Residual) %>%
  summarise_at(c("pH_all","tempC_all","DO_mgL","TCl2_mgL_all","FCl2","mono_mgL_all","Free_NH3_N","nitrate_all","nitrite_all","TAtp_RLU"), sd, na.rm = TRUE)          
datasumf3tableSD<- as.data.frame(datasumF3SD)
datasumF3SD
write.xlsx(datasumf3tableSD,file="C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/POU thesis data sumry/F3SDMarch summary.xlsx")

############################################


########Stagnation plots######
#POINT PLOT AND BOXPLOTS######
######NO2#########
pstag_NO2_leg<-ggplot(subset(IDR_Data, phase!="Transition" & stag=="1" & stagdur_min=="1080" |phase!="Transition" & stagdur_min=="3960" ), aes(x=as.Date(mydates), y=nitrite_all, fill=factor(stagdur_min)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 10, angle = 0)) + xlab("Dates of operation")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="Stagnation:", values=Stagdurcol, labels=c("Overnight", "weekend")) + 
  theme(legend.position="bottom")+theme(legend.text = element_text(size=16)) + scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,4)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+geom_hline(yintercept=(0.025), color="#fc9272")+geom_hline(yintercept=(0.05), color="#de2d26")+geom_hline(yintercept=(1), color="red")+
   theme(plot.title = element_text(size=12))
pstag_NO2_leg
ggsave(pstag_NO2_leg, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NO2_leg", Sys.Date(), ".jpeg", sep=""), width=9, height=5, units="in")

pstag_NO2_agedays_leg<-ggplot(subset(IDR_Data, phase!="Transition" & stag=="1" & stagdur_min=="1080" |phase!="Transition" & stagdur_min=="3960" ), aes(x=agedays, y=nitrite_all, fill=factor(stagdur_min))) + 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 15, angle = 0)) + xlab("Filter Age(days)")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="Stagnation:", values=Stagdurcol, labels=c("Overnight", "weekend")) + 
  theme(legend.position="bottom")+theme(legend.text = element_text(size=16)) +  coord_cartesian(ylim=c(0,4)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+scale_x_continuous(breaks = scales::breaks_extended(15))+
  theme(plot.title = element_text(size=12))
pstag_NO2_agedays_leg
ggsave(pstag_NO2_agedays_leg, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NO2_agedays_leg_", Sys.Date(), ".jpeg", sep=""), width=9, height=6, units="in")

pstag_NO2_phase<-ggplot(subset(IDR_Data, phase!="Transition" & stag=="1" & stagdur_min=="1080" |phase!="Transition" & stagdur_min=="3960" ), aes(x=agedays, y=nitrite_all, fill=factor(Residual))) + 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 15, angle = 0)) + xlab("Filter Age(days)")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=Burncolors2 ) + 
  theme(legend.position="bottom")+theme(legend.text = element_text(size=16)) +  coord_cartesian(ylim=c(0,2.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+scale_x_continuous(breaks = scales::breaks_extended(5))+
  theme(plot.title = element_text(size=12))
pstag_NO2_phase
ggsave(pstag_NO2_phase, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NO2_phase_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

my_comparisonsOVwk <- list(c("Weekend", "Overnight"))
pstagbox_NO2_leg_all<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"& phase!="Transition" & stag=="1" & stagdur_min=="1080" |mydates >= "2020-08-28"&phase!="Transition" & stagdur_min=="3960"), aes(x=factor(stag.dur), y=nitrite_all, fill=factor(stag.dur))) + 
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_bw() + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 13, angle = 0))+ xlab("")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="Stagnation:", values=Stagdurcol, labels=c("Overnight", "weekend")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size=16)) + coord_cartesian(ylim=c(0,2.5)) + theme(axis.text.x = element_text(angle = 15,size=15, hjust = 1))+theme(legend.text = element_text(size = 15))
  ggtitle(paste(subtitle= paste("\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))#+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagbox_NO2_leg_all
ggsave(pstagbox_NO2_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_NO2_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
pstagbox_NO2_stat<-pstagbox_NO2_leg_all+ stat_compare_means(comparisons = my_comparisonsOVwk,label = "p.format", size=4, hjust=0.5,label.y = c(2.25))+
  stat_compare_means(aes(group=stag.dur), label = "p.signif", symnum.args = list(cutpoints = c(0, 0.01, 0.05, 0.1, 1), symbols = c("****","***", "**", "*", "ns")),label.y=2.5, color="red", size=10, hjust=0.5)
pstagbox_NO2_stat
ggsave(pstagbox_NO2_stat, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_NO2_stat_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")


my_comparisonsOVwk <- list(c("Weekend", "Overnight"))
######FIRST BURN AND BEYOUNG#####
#pstagboxPsburn_NO2_leg_all<-ggplot(subset(IDR_Data,asDate>="2020-08-12"&phase.f=="Post_FClP"& stag=="1" & stagdur_min=="1080" |asDate>="2020-08-12"& phase.f=="Post_FClP"& stag=="1" & stagdur_min=="3960" ), aes(x=factor(stag.dur), y=nitrite_all, fill=factor(stag.dur))) + 
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 3,outlier.alpha = 0.5)+theme_bw() + facet_wrap(Loc.f~phase.f, ncol=3)+ theme(strip.text.x = element_text(size = 15, angle = 0))+ xlab("")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="Stagnation:", values=Stagdurcol, labels=c("Overnight", "weekend")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")) + coord_cartesian(ylim=c(0,2.5)) + theme(axis.text.x = element_text(angle = 15,size=12, hjust = 1))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagboxPsburn_NO2_leg_all
ggsave(pstagboxPsburn_NO2_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_NO2_leg_all_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")
pstagboxPsburn_NO2_stat<-pstagboxPsburn_NO2_leg_all+ stat_compare_means(comparisons = my_comparisonsOVwk,label = "p.format", size=4, hjust=0.5,label.y = c(2.25))
pstagboxPsburn_NO2_stat
ggsave(pstagboxPsburn_NO2_stat, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_NO2_stat_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")
###########
pstagbox_NO3_leg_all<-ggplot(subset(IDR_Data, mydates >= "2020-08-28"& phase!="Transition" & stagff=="0" & stagdur_min=="1080" |mydates >= "2020-08-28"& stagdur_min=="3960" ), aes(x=factor(stag.dur), y=nitrate_all, fill=factor(stag.dur))) + 
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_bw() + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 13, angle = 0))+ xlab("")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="Stagnation:", values=Stagdurcol, labels=c("Overnight", "weekend")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")) + coord_cartesian(ylim=c(0.5,7)) + theme(axis.text.x = element_text(angle = 15,size=10, hjust = 1))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=10))#+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0))
pstagbox_NO3_leg_all
ggsave(pstagbox_NO3_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_NO3_leg_all_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")
pstagbox_NO3_stat<-pstagbox_NO3_leg_all+ stat_compare_means(comparisons = my_comparisonsOVwk,label = "p.format", size=4, hjust=0.5,label.y = c(5.6))#+stat_compare_means(comparisons = my_comparisonsOVwk,label = "p.signif", size=4, hjust=0.5,label.y = c(5.5))
pstagbox_NO3_stat
ggsave(pstagbox_NO3_stat, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_NO3_stat_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")

pstagboxPsburn_NO3_leg_all<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"&phase!="0" &stagff=="0" & stagdur_min=="1080" |mydates >= "2020-08-28"&phase!="0" & stagff=="0" & stagdur_min=="3960"  ), aes(x=factor(stag.dur), y=nitrate_all, fill=factor(stag.dur))) + 
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_bw() + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 15, angle = 0))+ xlab("")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="Stagnation:", values=Stagdurcol, labels=c("Overnight", "weekend")) + 
    theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+ coord_cartesian(ylim=c(0,6)) + theme(axis.text.x = element_text(angle = 15,size=10, hjust = 1))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagboxPsburn_NO3_leg_all
ggsave(pstagboxPsburn_NO3_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagboxPsburn_NO3_leg_all_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")
pstagboxPsburn_NO3_stat<-pstagboxPsburn_NO3_leg_all+ stat_compare_means(comparisons = my_comparisonsOVwk,label = "p.format", size=4, hjust=0.5,label.y = c(5.5))
pstagboxPsburn_NO3_stat
ggsave(pstagboxPsburn_NO3_stat, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagboxPsburn_NO3_stat_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")



pCostag_days<-ggplot(subset(IDR_Data,phase!="0" & CoCe=="20" | CoCe=="21"  ), aes(x=agedays, y=nitrite_all, colour=factor(CoCe.name))) +geom_point(size=3) + theme_bw() + coord_cartesian(ylim=c(0,4))+
  xlab("Filter Age (days)")+ ylab("Nitrite (mg/L-N))") +scale_color_manual(name="", values=c("orange", "navy"))  +  
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+ annotate("text", label="after last flush", x=50,y= 0.1, size=4, color="chocolate", hjust=0) +scale_x_continuous(breaks = scales::breaks_extended(5))+
  annotate("text", label="morning stagnation", x=25,y= 1.25, size=4, color="navy", hjust=0) + facet_wrap(~Loc.f)+ theme(strip.text.x = element_text(size = 15, angle = 0)) + theme(strip.text.x = element_text(size = 14)) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12)) + theme(legend.title = element_text(size=12)) + theme(legend.text = element_text(size = 14)) +
   theme(plot.title = element_text(size=10))
pCostag_days 
ggsave(pCostag_days, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pCostag_days_", Sys.Date(), ".jpeg", sep=""), width=10, height=6, units="in")


####NO3##########
pstag_NO3_leg<-ggplot(subset(IDR_Data, stag=="1"  & stagdur_min=="1080" | stagdur_min=="3960"), aes(x=as.Date(mydates), y=nitrate_all, fill=factor(stagdur_min)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 15, angle = 0)) + xlab("Dates of operation")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=Stagdurcol, labels=c("Overnight", "weekend")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14)) +theme(legend.text = element_text(size = 13),legend.position="bottom")+theme(legend.text = element_text(size=16)) + scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,7)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
pstag_NO3_leg
ggsave(pstag_NO3_leg, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/report Plots/stag_FFplot/pstag_NO3_leg_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstag_NO3_agedays_leg<-ggplot(subset(IDR_Data, stag=="1"  & stagdur_min=="1080" | stagdur_min=="3960"), aes(x=agedays, y=nitrate_all, fill=factor(stagdur_min))) + 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 13, angle = 0)) + xlab("Filter Age(days)")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=Stagdurcol, labels=c("Overnight", "weekend")) + 
  theme(legend.position="bottom",legend.text = element_text(size = 13))+theme(legend.text = element_text(size=16)) +  coord_cartesian(ylim=c(0,7)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+scale_x_continuous(breaks = scales::breaks_extended(5))+
theme(plot.title = element_text(size=12))
pstag_NO3_agedays_leg
ggsave(pstag_NO3_agedays_leg, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NO3_agedays_leg_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

######ALL DATA#####
pstagbox_NO3burn_leg_all<-ggplot(subset(IDR_Data,stagdur_min=="1080"&  phase.f=="Post_FClP" & stag=="1"  | stagdur_min=="3960"&  phase.f=="Post_FClP" & stag=="1"  ), aes(x=factor(stag.dur), y=nitrate_all, fill=factor(stag.dur))) + 
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_bw() + facet_wrap(Loc.f~phase.f, ncol=3)+ theme(strip.text.x = element_text(size = 13, angle = 0))+ xlab("")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=Stagdurcol, labels=c("Overnight", "weekend")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14)) +theme(legend.text = element_text(size = 13),legend.position="bottom")+ coord_cartesian(ylim=c(0,6)) + theme(axis.text.x = element_text(angle = 15,size=10, hjust = 1))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagbox_NO3burn_leg_all
ggsave(pstagbox_NO3burn_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_NO3_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
pstagbox_NO3burn_stat<-pstagbox_NO3burn_leg_all+ stat_compare_means(comparisons = my_comparisonsOVwk,method = "wilcox",label = "p.format", size=4, hjust=0.5,label.y = c(5.5))
pstagbox_NO3burn_stat
ggsave(pstagbox_NO3burn_stat, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_NO3burn_stat_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
#######

pstagbox_NH3<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"&phase!="0"& stag=="1" & stagdur_min=="1080" | mydates >= "2020-08-28"&stagdur_min=="3960" ), aes(x=factor(stag.dur), y=Free_NH3_N, fill=factor(stag.dur))) + 
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_bw() + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 13, angle = 0))+ xlab("")+ ylab("Free-NH3 (mg/L-N)") +scale_fill_manual(name="", values=Stagdurcol, labels=c("Overnight", "weekend")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14)) +theme(legend.text = element_text(size = 13),legend.position="bottom") + coord_cartesian(ylim=c(0,1.25)) + theme(axis.text.x = element_text(angle = 15,size=10, hjust = 1))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))
pstagbox_NH3
ggsave(pstagbox_NH3, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_NH3_leg_all_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")
pstagbox_NH3_stat<-pstagbox_NH3+ stat_compare_means(comparisons = my_comparisonsOVwk,label = "p.format", size=4, hjust=0.5,label.y = c(1))+
  stat_compare_means( aes(label = ..p.signif..), color="red",label.x = 1.5, label.y = 0.8, size=5, hjust=0.5)
pstagbox_NH3_stat
ggsave(pstagbox_NH3_stat, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_NH3_stat_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")

pstagbox_NOX_NH3<-ggarrange(pstagbox_NH3_stat, pstagbox_NO2_stat,pstagbox_NO3_stat,ncol=1,   common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstagbox_NOX_NH3
ggsave(pstagbox_NOX_NH3, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_NOX_NH3_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

pCostagNO3_days<-ggplot(subset(IDR_Data, CoCe=="20" | CoCe=="21"  ), aes(x=agedays, y=nitrate_all, colour=factor(CoCe.name))) +geom_point(size=3) + theme_bw() + coord_cartesian(ylim=c(0,7))+
  xlab("Days in operation")+ ylab("Nitrate (mg/L-N))") +scale_color_manual(name="", values=c("orange", "navy"))  +  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="bottom")+theme(legend.text = element_text(size=16)) + annotate("text", label="after last flush", x=75,y= 0.5, size=5, color="chocolate", hjust=0) +scale_x_continuous(breaks = scales::breaks_extended(5))+
  annotate("text", label="morning stagnation", x=0,y= 4, size=4, color="navy", hjust=0) + facet_wrap(~Loc.f) + theme(strip.text.x = element_text(size = 14)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16)) + theme(legend.title = element_text(size=14)) + theme(legend.text = element_text(size = 14)) +
   theme(plot.title = element_text(size=10))
pCostagNO3_days 
ggsave(pCostagNO3_days, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pCostagNO3_days_", Sys.Date(), ".jpeg", sep=""), width=10, height=6, units="in")

####TCL2########

####TATP#######
pstag_TAtp_<-ggplot(subset(IDR_Data, stag=="1"  & stagdur_min=="1080" | stagdur_min=="3960" ), aes(x=agedays, y=TAtp_RLU, fill=factor(stagdur_min))) + + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=0, ymax=10^(5), alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=0, ymax=10^(5), alpha=0.45)+
  annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=0, ymax=10^(5), alpha=0.6)+ geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4 )+ theme(strip.text.x = element_text(size = 15, angle = 0))+ xlab("Dates of operation")+ ylab("Total ATP (RLU)") +scale_fill_manual(name="", values=overweekcol, labels=c("Overnight", "weekend"))  +scale_x_continuous(breaks = scales::breaks_extended(10))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x)))+ coord_cartesian(ylim=c(10^(0),10^(4.5))) +  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14)) + theme(legend.text = element_text(size = 13),legend.position="bottom") + 
 theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme(plot.title = element_text(size=12))
pstag_TAtp_
 ggsave(pstag_TAtp_, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_TAtp__", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

  pstag_TAtpdates_<-ggplot(subset(IDR_Data, stag=="1"  & stagdur_min=="1080" | stagdur_min=="3960" ), aes(x=as.Date(mydates), y=TAtp_RLU, fill=factor(stagdur_min))) + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=0, ymax=10^(5), alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=0, ymax=10^(5), alpha=0.45)+
    annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=0, ymax=10^(5), alpha=0.6)+ geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4 )+ theme(strip.text.x = element_text(size = 15, angle = 0))+ xlab("Dates of operation")+ ylab("Total ATP (RLU)") +scale_fill_manual(name="", values=overweekcol, labels=c("Overnight", "weekend"))  +scale_x_date(breaks = pretty_breaks(10))+
   scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x)))+ coord_cartesian(ylim=c(10^(0),10^(4.5))) +theme(legend.text = element_text(size=16))+  theme(axis.text = element_text(size = 12),axis.title.x=element_text(size = 15),axis.title.y=element_text(size = 14,legend.position="bottom") + 
   theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme(plot.title = element_text(size=12))
  pstag_TAtpdates_
 ggsave( pstag_TAtpdates_, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_TAtp__", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
 
 pstag_TAtpdates_zoom<-ggplot(subset(IDR_Data,mydates <= "2020-10-15"&  stag=="1"  & stagdur_min=="1080" | mydates <= "2020-10-15"& stagdur_min=="3960" ), aes(x=as.Date(mydates), y=TAtp_RLU, fill=factor(stagdur_min))) + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=0, ymax=10^(5), alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=0, ymax=10^(5), alpha=0.6)+ 
   geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4 )+ theme(strip.text.x = element_text(size = 15, angle = 0))+ xlab("Dates of operation")+ ylab("Total ATP (RLU)") +scale_fill_manual(name="", values=overweekcol, labels=c("Overnight", "weekend"))  +scale_x_date(breaks = pretty_breaks(10))+
   scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x)))+ coord_cartesian(ylim=c(10^(0),10^(4.5))) +  theme(axis.text = element_text(size = 12),axis.title.x=element_text(size = 15),axis.title.y=element_text(size = 14,legend.position="bottom") + 
   theme(axis.text.x = element_text(size=15,angle = 45, hjust = 1))+theme(axis.text.y = element_text(size=15, hjust = 1))+theme(legend.title = element_text(color = "black", size = 20),legend.text = element_text(color = "black", size = 15)) #change legend text size and colr+theme(plot.title = element_text(size=12))
 pstag_TAtpdates_zoom
 ggsave( pstag_TAtpdates_zoom, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_TAtpdates_zoom_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
 
 pstagPF_TAtpdateszoom_<-ggplot(subset(IDR_Data, mydates <= "2020-11-20"& flushprepost=="0"| mydates <= "2020-11-20" & flushprepost=="2" & Loc!="0"), aes(x=as.Date(mydates), y=TAtp_RLU, fill=factor(InfEff.name))) + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=0, ymax=10^(5), alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=0, ymax=10^(5), alpha=0.6)+ 
   geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4 )+ theme(strip.text.x = element_text(size = 15, angle = 0))+ xlab("Dates of operation")+ ylab("Total ATP (RLU)") +scale_fill_manual(name="", values=c("#ffffcc","#636363"))  +scale_x_date(breaks = pretty_breaks(5))+guides(fill = guide_legend(reverse=TRUE))+
    scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x)))+ coord_cartesian(ylim=c(10^(0),10^(3.5))) +  theme(axis.text = element_text(size = 12),axis.title.x=element_text(size = 15),axis.title.y=element_text(size = 14,legend.position="bottom") +
   theme(legend.title = element_text(color = "black", size = 20),legend.text = element_text(color = "black", size = 12))+theme(legend.text = element_text(size=16))+ #change legend text size and colr
   theme(axis.text.x = element_text(size=15,angle = 45, hjust = 1))+theme(axis.text.y = element_text(size=15, hjust = 1))+theme(plot.title = element_text(size=12))
 pstagPF_TAtpdateszoom_
 ggsave(pstagPF_TAtpdateszoom_, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/ pstagPF_TAtpdateszoom_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
 
 my_comparisonsphase <- list( c("FClP", "Pre-FClP"),c("FClP", "Post-FClP"),c("Post-FClP", "Pre-FClP"))
 pstagboxphase_TAtp_zoom_all<-ggplot(subset(IDR_Data,  flushprepost=="0"| flushprepost=="2" ), aes(x=(factor(LmtFClP)), y=TAtp_RLU, fill=factor(phase.f))) +  scale_fill_manual(name="", values=Burncolors3) +
   geom_boxplot(outlier.colour = "black",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_bw() +  facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 14))  + ylab("Total ATP (RLU)")+xlab("") + facet_grid(~Loc.f) +
   theme(strip.text.x = element_text(size = 14))+scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x)))+
   theme(legend.title = element_text(color = "black", size = 20),legend.text = element_text(color = "black", size = 15))+theme(axis.text=element_text(size=14), axis.title=element_text(size=16)) + theme(legend.title = element_text(size=14)) + theme(legend.position="none") +
   ggtitle(paste(subtitle= paste("","\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))+theme(axis.text.x = element_text(angle = 45, hjust = 1))
 pstagboxphase_TAtp_zoom_all
 ggsave(pstagboxphase_TAtp_zoom_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_TCL_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
 pstagboxphase_stat_TCL_zoom_all<-pstagboxphase_TAtp_zoom_all + stat_compare_means(comparisons = my_comparisonsphase ,label = "p.format", size=4, hjust=0.5,label.y = c(2.1, 2.3,2.7))
 pstagboxphase_stat_TCL_zoom_all
 ggsave(pstagboxphase_stat_TCL_zoom_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagboxphase_stat_TCL_zoom_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
 
 my_comparisonsOVwk <- list(c("Weekend", "Overnight"))
pstagbox_TAtp_leg_all<-ggplot(subset(IDR_Data, mydates >= "2020-08-28"& stag=="1"  & stagdur_min=="1080" | mydates >= "2020-08-28"&stagdur_min=="3960" ), aes(x=as.factor(stag.dur), y=TAtp_RLU, fill=factor(stag.dur))) + coord_cartesian(ylim=c(10^(1),10^(5)))+
  geom_boxplot(outlier.colour = "black",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_bw() +  facet_wrap(Loc.f~Residual,ncol = 3) +xlab("Stagnation Duration")+ ylab("Total TAtp_RLU)")+scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_fill_manual(name="",  values=overweekcol, labels=c("Overnight", "weekend"))  + theme(strip.text.x = element_text(size = 12)) + theme(axis.text=element_text(size=12), axis.title=element_text(size=12),axis.text.x = element_text(angle = 15, hjust = 1)) + theme(legend.title = element_text(size=14)) + theme(legend.position="bottom") +
  ggtitle(paste(subtitle= paste("\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagbox_TAtp_leg_all
ggsave(pstagbox_TAtp_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_TAtp_leg_all", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")
pstagbox_TAtp_stat_bymonth_all<-pstagbox_TAtp_leg_all+ stat_compare_means(comparisons = my_comparisonsOVwk ,label = "p.format", size=4, hjust=0.5,label.y = c(4 ))+
  stat_compare_means( aes(label = ..p.signif..),label.x = 1.5, label.y = 3.5,hide.ns=T, color="red", size=5, hjust=0.5)
pstagbox_TAtp_stat_bymonth_all
ggsave(pstagbox_TAtp_stat_bymonth_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_TAtp_stat_bymonth_all_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")

my_comparisonsPFFFSTAG <- list(c("Firstflow", "Post Flush"), c("Post Flush", "Stagnation"), c("Stagnation", "Firstflow"))
pstagFFPFbox_TAtp_leg_all<-ggplot(subset(IDR_Data, mydates >= "2020-08-28"&!is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"&flushprepost=="2" ), aes(x=reorder(factor(flush.name),stagffpf), y=TAtp_RLU, fill=factor(flush.name))) + coord_cartesian(ylim=c(10^(0),10^(5)))+
  geom_boxplot(outlier.colour = "black",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_bw() +  facet_wrap(Loc.f~.,ncol = 3) + theme(strip.text.x = element_text(size = 14)) + xlab("")+ ylab("Total TAtp_RLU)")+scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_fill_manual(name="",  values=Pre_post4col)  + theme(strip.text.x = element_text(size = 14)) + theme(axis.text=element_text(size=14), axis.title=element_text(size=16),axis.text.x = element_text(angle = 45, hjust = 1)) + theme(legend.title = element_text(size=14)) + theme(legend.position="bottom") +guides(fill = guide_legend(reverse=TRUE))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=165, Filter2=165, Filter3=165"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagFFPFbox_TAtp_leg_all
ggsave(pstagFFPFbox_TAtp_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_TAtp_leg_all", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")
pstagFFPFbox_TAtp_stat_bymonth_all<-pstagFFPFbox_TAtp_leg_all+ #stat_compare_means(comparisons =my_comparisonsPFFFSTAG ,label = "p.format", size=4, hjust=0.5,label.y = c(2.1, 3.2,3.5))+
  stat_compare_means( comparisons =my_comparisonsPFFFSTAG, aes(label = ..p.signif..),label.y = c(2.5, 3.5,4.5), size=5, hjust=0.5)
pstagFFPFbox_TAtp_stat_bymonth_all
ggsave(pstagFFPFbox_TAtp_stat_bymonth_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPFbox_TAtp_stat_bymonth_alll_", Sys.Date(), ".jpeg", sep=""), width=8, height=8, units="in")

my_comparisonsPFFFSTAG <- list(c("Firstflow", "Post Flush"), c("Post Flush", "Stagnation"), c("Stagnation", "Firstflow"))
pstagFFPFbox_TAtp_leg_phase<-ggplot(subset(IDR_Data, mydates >= "2020-08-28"&!is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"&flushprepost=="2" ), aes(x=reorder(factor(flush.name),stagffpf), y=TAtp_RLU, fill=factor(flush.name))) + coord_cartesian(ylim=c(10^(0),10^(5)))+
  geom_boxplot(outlier.colour = "black",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_bw() +  facet_wrap(Loc.f~Residual,ncol=2) + theme(strip.text.x = element_text(size = 12)) + xlab("Stagnation Duration")+ ylab("Total TAtp_RLU)")+scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_fill_manual(name="",  values=Pre_post4col) + theme(axis.text=element_text(size=12), axis.title=element_text(size=12),axis.text.x = element_text(angle = 12, hjust = 1)) + theme(legend.title = element_text(size=12)) + theme(legend.position="bottom") +guides(fill = guide_legend(reverse=TRUE))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagFFPFbox_TAtp_leg_phase
ggsave(pstagFFPFbox_TAtp_leg_phase, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_TAtp_leg_all", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")
pstagFFPFbox_TAtp_stat_phase<-pstagFFPFbox_TAtp_leg_phase+ stat_compare_means(comparisons =my_comparisonsPFFFSTAG ,label ="p.signif",  size=4, hjust=0.5,label.y = c(2.5, 3.5,4.5))
pstagFFPFbox_TAtp_stat_phase
ggsave(pstagFFPFbox_TAtp_stat_phase, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPFbox_TAtp_stat_phase_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")



pCostagATP_days<-ggplot(subset(IDR_Data, CoCe=="20" | CoCe=="21"  ), aes(x=agedays, y=TAtp_RLU, colour=factor(CoCe.name))) +geom_point(size=3) + theme_bw() + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  xlab("Days in operation")+ ylab("Total ATP_RLU") +scale_color_manual(name="", values=c("orange", "navy"))  +  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="bottom") + annotate("text", label="after last flush", x=75,y= 10^(1), size=5, color="chocolate", hjust=0) +scale_x_continuous(breaks = scales::breaks_extended(10))+
  annotate("text", label="morning stagnation", x=0,y= 10^(3), size=4, color="navy", hjust=0) + facet_wrap(~Loc.f)  +  theme(strip.text.x = element_text(size = 12)) +theme(legend.title = element_text(color = "black", size = 20),legend.text = element_text(color = "black", size = 15))+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=13)) + theme(legend.title = element_text(size=14)) + theme(legend.text = element_text(size = 16)) +
  theme(plot.title = element_text(size=10))
pCostagATP_days 
ggsave(pCostagATP_days, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pCostagATP_days_", Sys.Date(), ".jpeg", sep=""), width=10, height=6, units="in")

####MONO#####
pstag_Mono_leg<-ggplot(subset(IDR_Data, stag=="1"  & stagdur_min=="1080" | stagdur_min=="3960" ), aes(x=as.Date(mydates), y=mono_mgL_all, fill=factor(stagdur_min)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() +  facet_wrap(Loc.f~.,ncol=4) + theme(strip.text.x = element_text(size = 14)) + xlab("Dates of operation")+ ylab("mono (mg/L-N)") +scale_fill_manual(name="", values=overweekcol, labels=c("Overnight", "weekend")) + 
  theme(legend.position="bottom") + scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,0.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
 theme(plot.title = element_text(size=12))+ theme(legend.text = element_text(size = 16)) 
pstag_Mono_leg
ggsave(pstag_Mono_leg, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_Mono_leg_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

####FNH3#######

pstag_Free_NH3_leg<-ggplot(subset(IDR_Data, stag=="1"  & stagdur_min=="1080" | stagdur_min=="3960"), aes(x=as.Date(mydates), y=Free_NH3_N, fill=factor(stagdur_min)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() +  facet_wrap(Loc.f~.,ncol=4) + theme(strip.text.x = element_text(size = 14)) + xlab("Dates of operation")+ ylab("Free_NH3_N") +scale_fill_manual(name="", values=overweekcol, labels=c("Overnight", "weekend")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,1.7)) + theme(axis.text.x = element_text(size=12,angle = 45, hjust = 1))+theme(axis.text.y = element_text(size=12))+
  theme(plot.title = element_text(size=12))
pstag_Free_NH3_leg
ggsave(pstag_Free_NH3_leg, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_Free_NH3_leg_all", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagbox_NH3<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"& stag=="1" & stagdur_min=="1080" | mydates >= "2020-08-28"& stagdur_min=="3960" ), aes(x=factor(stag.dur), y=Free_NH3_N, fill=factor(stag.dur))) + 
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_bw() + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 15, angle = 0))+ xlab("Flush Event")+ ylab("Free-NH3 (mg/L-N)") +scale_fill_manual(name="", values=Stagdurcol, labels=c("Overnight", "weekend")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size=16)) + coord_cartesian(ylim=c(0,1.25)) + theme(axis.text.x = element_text(angle = 45,size=12, hjust = 1))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagbox_NH3
ggsave(pstagbox_NH3, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_NH3_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagbox_NH3_stat<-pstagbox_NH3+ stat_compare_means(comparisons = my_comparisonsOVwk,label = "p.format", size=4, hjust=0.5,label.y = c(1.2))+
  stat_compare_means( aes(label = ..p.signif..),label.x = 1.5, label.y = 1.15, color="red", size=5, hjust=0.5,hide.ns=T)
  pstagbox_NH3_stat
ggsave(pstagbox_NH3_stat, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_NH3_stat_", Sys.Date(), ".jpeg", sep=""), width=8, height=8, units="in")

pstagboxBurn_NH3<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"&stagdur_min=="1080" & stag=="1"|mydates >= "2020-08-28"& stagdur_min=="3960"& stag=="1"  ), aes(x=factor(stag.dur), y=Free_NH3_N, fill=factor(stag.dur))) + 
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_bw() + facet_wrap(Loc.f~Residual, ncol=3)+ theme(strip.text.x = element_text(size = 15, angle = 0))+ xlab("Flush Event")+ ylab("Free_NH3_N (mg/L-N)") +scale_fill_manual(name="Stagnation:", values=Stagdurcol, labels=c("Overnight", "weekend")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")) + coord_cartesian(ylim=c(0,1.5)) + theme(axis.text.x = element_text(angle = 45,size=15, hjust = 1))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagboxBurn_NH3
ggsave(pstagboxBurn_NH3, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagboxBurn_NO3_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
pstagboxBurn_NH3_stat<-pstagboxBurn_NH3+ stat_compare_means(comparisons = my_comparisonsOVwk,label = "p.format", size=4, hjust=0.5,label.y = c(1.2))+
  stat_compare_means( aes(label = ..p.signif..),label.x = 1.5, label.y = 1, color="red", size=5, hjust=0.5,hide.ns=T)
pstagboxBurn_NH3_stat
ggsave(pstagboxBurn_NH3_stat, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagboxBurn_NH3_stat_", Sys.Date(), ".jpeg", sep=""), width=8, height=8, units="in")


########stagnation multiplots #######

pstag_Free_NH3_leg<-ggplot(subset(IDR_Data, stag=="1"  & stagdur_min=="1080" | stagdur_min=="3960"), aes(x=as.Date(mydates), y=Free_NH3_N, fill=factor(stagdur_min)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
geom_point(size=3, shape=21)+theme_bw() +  facet_wrap(Loc.f~.,ncol=4) + theme(strip.text.x = element_text(size = 14)) + xlab("Dates of operation")+ ylab("Free_NH3_N") +scale_fill_manual(name="", values=overweekcol, labels=c("Overnight", "weekend")) + 
theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,1.7)) + theme(axis.text.x = element_text(size=12,angle = 45, hjust = 1))+theme(axis.text.y = element_text(size=12))+
theme(plot.title = element_text(size=12))
pstag_Free_NH3_leg
ggsave(pstag_Free_NH3_leg, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_Free_NH3_leg_all", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
        
pstag_NO2_leg<-ggplot(subset(IDR_Data, stag=="1" & stagdur_min=="1080" | stagdur_min=="3960" ), aes(x=as.Date(mydates), y=nitrite_all, fill=factor(stagdur_min)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 14)) + xlab("Dates of operation")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=Stagdurcol, labels=c("Overnight", "weekend")) + 
  theme(axis.text = element_text(size = 10.5),axis.title.x=element_text(size = 10.5),axis.title.y=element_text(size = 10.5),legend.text=element_text(size=10.5),legend.position="bottom")+theme(legend.text = element_text(size=16))+theme(legend.text = element_text(size = 16))+ scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,3)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
pstag_NO2_leg

pstag_NO3_leg<-ggplot(subset(IDR_Data, stag=="1"  & stagdur_min=="1080" | stagdur_min=="3960"), aes(x=as.Date(mydates), y=nitrate_all, fill=factor(stagdur_min)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4) + xlab("Dates of operation")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=Stagdurcol, labels=c("Overnight", "weekend")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))+ scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,7)) + theme(axis.text.x = element_text(size=12,angle = 45, hjust = 1))+theme(axis.text.y = element_text(size=15))+
   theme(plot.title = element_text(size=12))
pstag_NO3_leg



pstag_NOX_<-ggarrange(pstag_NO2_leg, pstag_NO3_leg,pstag_TAtp_,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstag_NOX_
ggsave(pstag_NOX_, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NOX_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

  pstag_NOX_NH3<-ggarrange(pstag_Free_NH3_leg,pstag_NO2_leg, pstag_NO3_leg,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
  pstag_NOX_NH3
ggsave(pstag_NOX_NH3, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NOX_NH3_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

pstag_ph_leg<-ggplot(subset(IDR_Data, stag=="1" & stagdur_min=="1080" | stagdur_min=="3960" ), aes(x=as.Date(mydates), y=pH_all, fill=factor(stagdur_min)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 14)) + xlab("Dates of operation")+ ylab("pH") +scale_fill_manual(name="", values=Stagdurcol, labels=c("Overnight", "weekend")) + 
  theme(axis.text = element_text(size = 10.5),axis.title.x=element_text(size = 10.5),axis.title.y=element_text(size = 10.5),legend.text=element_text(size=10.5),legend.position="bottom")+theme(legend.text = element_text(size = 16))+ scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(7.25,8)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
pstag_ph_leg

pstag_Temp_leg<-ggplot(subset(IDR_Data, stag=="1" & stagdur_min=="1080" | stagdur_min=="3960" ), aes(x=as.Date(mydates), y=tempC_all, fill=factor(stagdur_min)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=30, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=300, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=30, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=30, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 14)) + xlab("Dates of operation")+ ylab("Tempreture C") +scale_fill_manual(name="", values=Stagdurcol, labels=c("Overnight", "weekend")) + 
  theme(axis.text = element_text(size = 10.5),axis.title.x=element_text(size = 10.5),axis.title.y=element_text(size = 10.5),legend.text=element_text(size=10.5),legend.position="bottom")+theme(legend.text = element_text(size = 16))+ scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(21.5,25)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
pstag_Temp_leg



pstag_PH_TMP<-ggarrange(pstag_ph_leg,pstag_Temp_leg,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstag_PH_TMP
ggsave(pstag_PH_TMP, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_PH_TMP_DO_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")



pstag_DO_leg<-ggplot(subset(IDR_Data, stag=="1" & stagdur_min=="1080" | stagdur_min=="3960" ), aes(x=as.Date(mydates), y=DO_mgL, fill=factor(stagdur_min)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 14)) + xlab("Dates of operation")+ ylab("DO mg/L") +scale_fill_manual(name="", values=Stagdurcol, labels=c("Overnight", "weekend")) + 
  theme(axis.text = element_text(size = 10.5),axis.title.x=element_text(size = 10.5),axis.title.y=element_text(size = 10.5),legend.text=element_text(size=10.5),legend.position="bottom")+theme(legend.text = element_text(size = 16))+ scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(1,10)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
pstag_DO_leg

pstag_TAtp_<-ggplot(subset(IDR_Data, stag=="1"  & stagdur_min=="1080" | stagdur_min=="3960" ), aes(x=as.Date(mydates), y=TAtp_RLU, fill=factor(stagdur_min))) + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=0, ymax=10^(5), alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=0, ymax=10^(5), alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4 )+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Total ATP (RLU)") +scale_fill_manual(name="", values=overweekcol, labels=c("Overnight", "weekend")) + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) +  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))+ scale_x_date(breaks = pretty_breaks(10)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
pstag_TAtp_ 

pstag_ATP_DO<-ggarrange(pstag_DO_leg,pstag_TAtp_ ,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstag_ATP_DO
ggsave(pstag_ATP_DO, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_ATP_DO_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

pstag_NOX_ATP_ph<-ggarrange(pstag_NO2_leg,pstag_ph_leg,pstag_TAtp_ ,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstag_NOX_ATP_ph
ggsave(pstag_NOX_ATP_ph, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NOX_ATP_ph_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

######Pre/FF/POSTFLUSH########

######NO2#########
pFFpf_NO2_leg_all<-ggplot(subset(IDR_Data, flushprepost =="1"&stagff=="0" |flushprepost=="2"), aes(x=as.Date(mydates), y=nitrite_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4) + theme(strip.text.x = element_text(size = 14)) + xlab("Dates of operation: Month")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=Pre_post) +guides(fill = guide_legend(reverse=TRUE))+ 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14)) +theme(legend.text = element_text(size = 13),legend.position="bottom")+ scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,3)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +#geom_hline(yintercept=(1), color="red")+
   theme(plot.title = element_text(size=12))
pFFpf_NO2_leg_all
ggsave(pFFpf_NO2_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pFFpf_NO2_leg_all_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

filter1<- ggplot(subset(IDR_Data, Loc=="1"&flushprepost=="0"| Loc=="1"&!is.na(stagff) & flushprepost =="1" |Loc=="1"&flushprepost=="2"),aes(x=as.Date(mydates), y=nitrite_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4) + theme(strip.text.x = element_text(size = 14)) + scale_x_date(breaks = pretty_breaks(10))+ xlab("Dates of operation: Month")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col) +guides(fill = guide_legend(reverse=TRUE))+  
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14)) +theme(legend.text = element_text(size = 13),legend.position="bottom") + coord_cartesian(ylim=c(0,2.5)) + theme(axis.text.x = element_text(angle = 45,size=12, hjust = 1)) +geom_hline(yintercept=(1), color="red")+
  theme(plot.title = element_text(size=12))
filter1
#ggsave(pFFpf_NO2_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pFFpf_NO2_leg_all_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")


filter2<-ggplot(subset(IDR_Data, Loc=="2"&flushprepost=="0"| Loc=="2"&!is.na(stagff) & flushprepost =="1" |Loc=="2"&flushprepost=="2"),aes(x=as.Date(mydates), y=nitrite_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation: Month")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col)+guides(fill = guide_legend(reverse=TRUE))+  
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size=16))  + coord_cartesian(ylim=c(0,2.5)) + theme(axis.text.x = element_text(angle = 45,size=12, hjust = 1)) +geom_hline(yintercept=(1), color="red")+scale_x_date(breaks = pretty_breaks(10))+
  theme(plot.title = element_text(size=12))
filter2
#ggsave(pFFpf_NO2_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pFFpf_NO2_leg_all_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")


filter3<-ggplot(subset(IDR_Data, Loc=="3"&flushprepost=="0"| Loc=="3"&!is.na(stagff) & flushprepost =="1" |Loc=="3"&flushprepost=="2"), aes(x=as.Date(mydates), y=nitrite_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation: Month")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col)+guides(fill = guide_legend(reverse=TRUE))+ 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14)) +theme(legend.text = element_text(size = 13),legend.position="bottom") + coord_cartesian(ylim=c(0,2.5)) + theme(axis.text.x = element_text(angle = 45,size=12, hjust = 1)) +geom_hline(yintercept=(1), color="red")+scale_x_date(breaks = pretty_breaks(10))+
  theme(plot.title = element_text(size=12))
filter3
#ggsave(pFFpf_NO2_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pFFpf_NO2_leg_all_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

filter1_2_3 <- ggarrange(filter1, filter2,filter3,nrow=1,common.legend = TRUE,labels = c("A","B", "C"))
filter1_2_3
ggsave(filter1_2_3, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/filter1_2_3_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")


pFFpfbox_NO2_leg_all<-ggplot(subset(IDR_Data, mydates >= "2020-08-28"& phase!="Transt" & stagff=="0"& flushprepost=="1"|mydates >= "2020-08-28"& phase!="Transt"&flushprepost=="2" & Loc!="0"), aes(x=reorder(factor(flush.name),stagffpf), y=nitrite_all, fill=factor(flush.name))) + 
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5) + facet_wrap(Loc.f~phase.f, ncol=3)+ theme(strip.text.x = element_text(size = 13)) + xlab("Flush Event")+ ylab("Nitrite (mg/L-N)")+scale_fill_manual(name="", values=Pre_post,labels=c("Postflush", "Stagnatgion")) + theme_bw()+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")) + coord_cartesian(ylim=c(-0.2,2.5)) + theme(axis.text.x = element_text(angle = 45,size=12, hjust = 1))+guides(fill = guide_legend(reverse = TRUE))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pFFpfbox_NO2_leg_all
ggsave(pFFpfbox_NO2_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pFFpfbox_NO2_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
pFFpfbox_NO2_stat_bymonth_all<-pFFpfbox_NO2_leg_all+ stat_compare_means( aes(label = ..p.signif..), color="red",label.x =1.5, label.y =2.1, size=5, hjust=0.5)+
  stat_compare_means(label = "p.format",label.x =1.5,method = "wilcox.test") 
pFFpfbox_NO2_stat_bymonth_all
ggsave(pFFpfbox_NO2_stat_bymonth_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pFFpfbox_NO2_stat_bymonth_all_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")

tapdata <- subset(IDR_Data, flushprepost=="0")
pstagpf_NO2_leg_days<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"&  !is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"&flushprepost=="2"), aes(x=agedays, y=nitrite_all, fill=factor(flush.name))) + scale_x_continuous(breaks = scales::breaks_extended(5))+
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Age(days)")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col) + guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14)) +theme(legend.text = element_text(size = 13),legend.position="bottom") + coord_cartesian(ylim=c(0,2.5)) + theme(axis.text.x = element_text(size=15,angle = 45, hjust = 1)) +
   theme(plot.title = element_text(size=12))
pstagpf_NO2_leg_days
ggsave(pstagpf_NO2_leg_days, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpf_NO2_leg_days", Sys.Date(), ".jpeg", sep=""), width=11, height=8, units="in")

pstagpfffbox_NO2_leg_all<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"&  !is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"&flushprepost=="2"), aes(x=factor(Residual), y=nitrite_all, fill=factor(flush.name))) + 
 geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5) + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 14)) + xlab("Flush Event")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col) + theme_bw()+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+ coord_cartesian(ylim=c(-0.2,2.5)) + theme(axis.text.x = element_text(angle = 45,size=10, hjust = 1))+ guides(fill = guide_legend(reverse=TRUE))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=165, Filter2=165, Filter3=165"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))# + geom_hline(yintercept = mean(tapdata$nitrite_all), linetype = 2)
pstagpfffbox_NO2_leg_all
ggsave(pstagpfffbox_NO2_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpfffbox_NO2_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
pstagpfffbox_NO2_stat_bymonth_all<-pstagpfffbox_NO2_leg_all+ stat_compare_means(comparisons = my_comparisonsPFFFSTAG,aes(group=flush.name),label = "p.signif", color="red",label.x = 1.5, label.y = 1.15, size=5, hjust=0.5)
 pstagpfffbox_NO2_stat_bymonth_all
pstagpfffbox_NO2_stat_ <- pstagpfffbox_NO2_leg_all+ stat_compare_means(method= "wilcox.test",label = "p.signif", ref.group =".all.") 
pstagpfffbox_NO2_stat_ 
ggsave(pstagpfffbox_NO2_stat_, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpfffbox_NO2_stat_", Sys.Date(), ".jpeg", sep=""), width=8, height=8, units="in")

####NO3##########
pstag_NO3_leg_all1<-ggplot(subset(IDR_Data,flushprepost==0 |flushprepost==1 & stagff=="0"|  flushprepost==2), aes(x=as.Date(mydates), y=nitrate_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=c("#ffffcc","#78c679","#636363")) +guides(fill = guide_legend(reverse=TRUE)) +
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+ scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,7)) + theme(axis.text.x = element_text(size=12,angle = 45, hjust = 1))+
   theme(plot.title = element_text(size=12))
pstag_NO3_leg_all1 
ggsave(pstag_NO3_leg_all1, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NO3_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

#pstag_NO3<-ggplot(subset(IDR_Data,flushprepost==0 |flushprepost==1|  flushprepost==2), aes(x=as.Date(mydates), y=nitrate_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col) +guides(fill = guide_legend(reverse=TRUE)) +
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+ scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,7)) + theme(axis.text.x = element_text(size=12,angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
pstag_NO3 
ggsave(pstag_NO3, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NO3_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstag_NO3_leg_all<-ggplot(subset(IDR_Data,flushprepost==0 | !is.na(stagff) & flushprepost =="1" |flushprepost=="2" ), aes(x=as.Date(mydates), y=nitrate_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col) +guides(fill = guide_legend(reverse=TRUE)) +
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size=16))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+ scale_x_date(breaks = pretty_breaks(10)) +coord_cartesian(ylim=c(0,7)) +
 theme(plot.title = element_text(size=12))
pstag_NO3_leg_all 
ggsave(pstag_NO3_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NO3_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagFFPF_Free_NH3_month<-ggplot(subset(IDR_Data, !is.na(stagff) & flushprepost =="1" &stagff=="0"| flushprepost=="2" ), aes(x=as.Date(mydates), y=Free_NH3_N, fill=factor(flush.name)))+ annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+  
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Dates of operation")+ ylab("Free_NH3 (mg/L-N)") +scale_fill_manual(name="", values=c("#ffffcc","#78c679","#636363"))  +guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16)) + theme(axis.text.x = element_text(size=12,angle = 45, hjust = 1))  + scale_x_date(breaks = pretty_breaks(15))  +coord_cartesian(ylim=c(0,2)) +
  theme(plot.title = element_text(size=12))
pstagFFPF_Free_NH3_month
ggsave(pstagFFPF_Free_NH3_month, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_Free_NH3_month", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

pstagFFPF_Free_NH3_month_all<-ggplot(subset(IDR_Data,flushprepost==0 | Loc=="3" & !is.na(stagff) & flushprepost =="1" | Loc=="3" & flushprepost=="2" ), aes(x=agedays, y=Free_NH3_N, fill=factor(flush.name)))+scale_x_continuous(breaks = scales::breaks_extended(10))+ #annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+  
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Days of operation")+ ylab("Free_NH3 (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col)  +guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14),legend.position="bottom")+theme(legend.text = element_text(size = 16)) + theme(axis.text.x = element_text(size=14,angle = 45, hjust = 1))   +coord_cartesian(ylim=c(0,1.25)) +#+ scale_x_date(breaks = pretty_breaks(15)) 
  theme(plot.title = element_text(size=12))
pstagFFPF_Free_NH3_month_all
gsave(pstagFFPF_Free_NH3_month_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_Free_NH3_month_all", Sys.Date(), ".jpeg", sep=""), width=9, height=5, units="in")

pstag_NO3_month<-ggplot(subset(IDR_Data,flushprepost==0 | Loc=="3" & !is.na(stagff) & flushprepost =="1" | Loc=="3" & flushprepost=="2" ), aes(x=agedays, y=nitrate_all, fill=factor(flush.name)))  + scale_x_continuous(breaks = scales::breaks_extended(10))+ #annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Days of operation")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14),legend.position="bottom",)+theme(legend.text = element_text(size=16))+coord_cartesian(ylim=c(0,6))+ theme(axis.text.x = element_text(size = 14,angle = 45, hjust = 1)) +guides(fill = guide_legend(reverse=TRUE))+ 
  theme(plot.title = element_text(size=12))
pstag_NO3_month
ggsave(pstag_NO3_month, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NO3_month_", Sys.Date(), ".jpeg", sep=""), width=9, height=5, units="in")

pstag_NO3_zoom <-ggplot(subset(IDR_Data, !is.na(stagff) & flushprepost =="1" |flushprepost=="2" ), aes(x=agedays, y=nitrate_all, fill=factor(flush.name))) + scale_x_continuous(breaks = scales::breaks_extended(10))+# annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.5) + 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme(legend.text = element_text(size=16))+coord_cartesian(ylim=c(0,4)) +scale_x_continuous(limits = c(0, 75))+guides(fill = guide_legend(reverse=TRUE))+
  theme(plot.title = element_text(size=12))
pstag_NO3_zoom
ggsave(pstag_NO3_zoom, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NO3_zoom_", Sys.Date(), ".jpeg", sep=""), width=9, height=5, units="in")

pstagpf_NO2_month<- ggplot(subset(IDR_Data,  !is.na(stagff) & flushprepost =="1" |  flushprepost=="2"), aes(x=as.Date(mydates), y=nitrite_all, fill=factor(flush.name))) + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14),legend.position="bottom")+theme(legend.text = element_text(size=16))+guides(fill = guide_legend(reverse=TRUE))+ theme(axis.text.x = element_text(angle = 45, size=14,hjust = 1)) + scale_x_date(breaks = pretty_breaks(15))  +coord_cartesian(ylim=c(0,4)) +
  theme(plot.title = element_text(size=12))
pstagpf_NO2_month
ggsave(pstagpf_NO2_month, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpf_NO2_month_", Sys.Date(), ".jpeg", sep=""), width=9, height=5, units="in")

pstagffpf_NO2_month_ALL<- ggplot(subset(IDR_Data,flushprepost==0 | Loc=="3" & !is.na(stagff) & flushprepost =="1" | Loc=="3" & flushprepost=="2" ), aes(x=agedays, y=nitrite_all, fill=factor(flush.name))) + scale_x_continuous(breaks = scales::breaks_extended(10))+ #annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14),legend.position="bottom")+theme(legend.text = element_text(size=16))+guides(fill = guide_legend(reverse=TRUE))+ theme(axis.text.x = element_text(angle = 45, size=14,hjust = 1)) +coord_cartesian(ylim=c(0,2.5))+ #  scale_x_date(breaks = pretty_breaks(15)) 
  theme(plot.title = element_text(size=12))
pstagffpf_NO2_month_ALL
ggsave(pstagffpf_NO2_month_ALL, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagffpf_NO2_month_ALL_", Sys.Date(), ".jpeg", sep=""), width=9, height=5, units="in")


filter123NOX_ATP_month <-ggarrange(pstagFFPF_Free_NH3_month,pstagpf_NO2_month, pstag_NO3_month ,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
filter123NOX_ATP_month 
ggsave(filter123NOX_ATP_month , filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/filter123NOX_ATP_month _", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")
##############

NH3_NOX_month_all <-ggarrange(pstagFFPF_Free_NH3_month_all,pstagffpf_NO2_month_ALL, pstag_NO3_month ,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
NH3_NOX_month_all 
ggsave(NH3_NOX_month_all , filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/NH3_NOX_month_all_", Sys.Date(), ".jpeg", sep=""), width=9, height=8, units="in")
##############
pstagFFPF_Free_NH3_days<-ggplot(subset(IDR_Data, !is.na(stagff) & flushprepost =="1" &stagff=="0"| flushprepost=="2" ), aes(x=agedays, y=Free_NH3_N, fill=factor(flush.name))) + scale_x_continuous(breaks = scales::breaks_extended(12))+# annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+  
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Dates of operation")+ ylab("Free_NH3 (mg/L-N)") +scale_fill_manual(name="", values=c("#ffffcc","#78c679","#636363"))  +guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14),legend.position="bottom")+theme(legend.text = element_text(size = 16)) + theme(axis.text.x = element_text(size=14,angle = 45, hjust = 1)) +coord_cartesian(ylim=c(0,1.7)) +
  theme(plot.title = element_text(size=12))
pstagFFPF_Free_NH3_days
ggsave(pstagFFPF_Free_NH3_days, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_Free_NH3_days", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

pstag_NO3_days<-ggplot(subset(IDR_Data, !is.na(stagff) & flushprepost =="1" &stagff=="0"| flushprepost=="2" ), aes(x=agedays, y=nitrate_all, fill=factor(flush.name)))  + scale_x_continuous(breaks = scales::breaks_extended(12)) + 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=c("#ffffcc","#78c679","#636363")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14),legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + theme(axis.text.x = element_text(size = 14,angle = 45, hjust = 1)) + coord_cartesian(ylim=c(0,7)) +guides(fill = guide_legend(reverse=TRUE))+
  theme(plot.title = element_text(size=12))
pstag_NO3_days
ggsave(pstag_NO3_days, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NO3_days_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

#pstag_NO3_zoom <-ggplot(subset(IDR_Data,mydates >= "2020-08-28"&  !is.na(stagff) & flushprepost =="1" &stagff=="0"|mydates >= "2020-08-28"& flushprepost=="2" ), aes(x=agedays, y=nitrate_all, fill=factor(flush.name))) + scale_x_continuous(breaks = scales::breaks_extended(10))+# annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.5) + 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+coord_cartesian(ylim=c(0,4)) +scale_x_continuous(limits = c(0, 75))+guides(fill = guide_legend(reverse=TRUE))+
  theme(plot.title = element_text(size=12))
pstag_NO3_zoom
ggsave(pstag_NO3_zoom, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NO3_zoom_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagpf_NO2_days<- ggplot(subset(IDR_Data,  !is.na(stagff) & flushprepost =="1" &stagff=="0"| flushprepost=="2" ), aes(x=agedays, y=nitrite_all, fill=factor(flush.name))) + scale_x_continuous(breaks = scales::breaks_extended(12))  + 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=c("#ffffcc","#78c679","#636363")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14),legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + theme(axis.text.x = element_text(size = 14,angle = 45, hjust = 1)) + coord_cartesian(ylim=c(0,3.7)) +guides(fill = guide_legend(reverse=TRUE))+
  theme(plot.title = element_text(size=12))
pstagpf_NO2_days

filter123NOX_ATP <-ggarrange(pstagFFPF_Free_NH3_days,pstagpf_NO2_days, pstag_NO3_days ,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
filter123NOX_ATP
ggsave(filter123NOX_ATP, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/filter123NOX_ATP_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")
########ZOOM pre and Burn ##########
pstagFFPF_Free_NH3Prebrun<-ggplot(subset(IDR_Data,mydates<="2020-08-31" & flushprepost =="0" | mydates<="2020-08-31" &!is.na(stagff) &  mydates<="2020-08-31" &flushprepost =="1" | mydates<="2020-08-31" &flushprepost=="2" ), aes(x=as.Date(mydates), y=Free_NH3_N, fill=factor(flush.name)))  +annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Dates of operation")+ ylab("Free_NH3 (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col)  +guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))  + scale_x_date(breaks = pretty_breaks(10))  +coord_cartesian(ylim=c(0,1)) +
  theme(plot.title = element_text(size=12))
pstagFFPF_Free_NH3Prebrun
ggsave(pstagFFPF_Free_NH3Prebrun, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_Free_NH3Prebrun", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstag_NO3_Prebrun<-ggplot(subset(IDR_Data,mydates<="2020-08-31" & flushprepost =="0" | mydates<="2020-08-31" &!is.na(stagff) &  mydates<="2020-08-31" &flushprepost =="1" | mydates<="2020-08-31" &flushprepost=="2"), aes(x=as.Date(mydates), y=nitrate_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+ theme(axis.text.x = element_text(size = 15,angle = 45, hjust = 1))+ scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,7)) +guides(fill = guide_legend(reverse=TRUE))+
  theme(plot.title = element_text(size=12))
pstag_NO3_Prebrun 
ggsave(pstag_NO3_Prebrun, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NO3_Prebrun_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstag_NO3_zoom <-ggplot(subset(IDR_Data, !is.na(stagff) & flushprepost =="1" |flushprepost=="2" ), aes(x=agedays, y=nitrate_all, fill=factor(flush.name))) + scale_x_continuous(breaks = scales::breaks_extended(10))+# annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.5) + 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col) + 
   theme(axis.text.x = element_text(angle = 45, hjust = 1))+coord_cartesian(ylim=c(0,4)) +scale_x_continuous(limits = c(0, 75))+guides(fill = guide_legend(reverse=TRUE))+
  theme(plot.title = element_text(size=12))
pstag_NO3_zoom
ggsave(pstag_NO3_zoom, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NO3_zoom_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagpf_NO2_Prebrun<- ggplot(subset(IDR_Data,mydates<="2020-08-31" & flushprepost =="0" | mydates<="2020-08-31" &!is.na(stagff) &  mydates<="2020-08-31" &flushprepost =="1" | mydates<="2020-08-31" &flushprepost=="2"), aes(x=as.Date(mydates), y=nitrite_all, fill=factor(flush.name))) + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+guides(fill = guide_legend(reverse=TRUE))+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_date(breaks = pretty_breaks(10))  +coord_cartesian(ylim=c(0,2.5)) +
  theme(plot.title = element_text(size=12))
pstagpf_NO2_Prebrun

filter123NOX_preburn <-ggarrange(pstagFFPF_Free_NH3Prebrun,pstagpf_NO2_Prebrun, pstag_NO3_Prebrun  ,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
filter123NOX_preburn 
ggsave(filter123NOX_preburn , filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/filter123NOX_preburn _", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")


filter1NO3<-ggplot(subset(IDR_Data, Loc=="1"&flushprepost==0|Loc=="1"&!is.na(stagff) & flushprepost =="1" |Loc=="1"&flushprepost=="2" ), aes(x=as.Date(mydates), y=nitrate_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.)+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col) +  guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ scale_x_date(breaks = pretty_breaks(10)) +coord_cartesian(ylim=c(0,7)) +
  theme(plot.title = element_text(size=12))
filter1NO3

filter2NO3<-ggplot(subset(IDR_Data, Loc=="2"&flushprepost==0|Loc=="2"&!is.na(stagff) & flushprepost =="1" |Loc=="2"&flushprepost=="2" ), aes(x=as.Date(mydates), y=nitrate_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.) + theme(strip.text.x = element_text(size = 14)) + xlab("Dates of operation")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col) + guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ scale_x_date(breaks = pretty_breaks(10)) +coord_cartesian(ylim=c(0,7)) +
  theme(plot.title = element_text(size=12))
filter2NO3

filter3NO3<-ggplot(subset(IDR_Data, Loc=="3"&flushprepost==0|Loc=="3"&!is.na(stagff) & flushprepost =="1" |Loc=="3"&flushprepost=="2" ), aes(x=as.Date(mydates), y=nitrate_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.)+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col) + guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+ scale_x_date(breaks = pretty_breaks(10)) +coord_cartesian(ylim=c(0,7)) +
  theme(plot.title = element_text(size=12))
filter3NO3

filter1_2_3NO3 <- ggarrange(filter1NO3, filter2NO3,filter3NO3,ncol=1, widths = c(2, 2),  common.legend = T, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
filter1_2_3NO3
  #ggarrange(filter1NO3, filter2NO3,filter3NO3,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
ggsave(filter1_2_3NO3, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/filter1_2_3NO3_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

asDate>="2020-08-12"&phase.f=="Post-Burn"&

#pstagpfbox_NO3_leg_all<-ggplot(subset(IDR_Data,  !is.na(stagff) & flushprepost =="1" |flushprepost=="2"), aes(x=factor(flush.name), y=nitrate_all, fill=factor(flush.name))) + 
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5) + facet_wrap(Loc.f~phase.f, ncol=3)+ theme(strip.text.x = element_text(size = 14)) + xlab("Flush Event")+ ylab("Nitrate(mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col) + theme_grey()+
  theme(legend.position="bottom") + coord_cartesian(ylim=c(-0,5.5)) + theme(axis.text.x = element_text(angle = 45,size=10, hjust = 1))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagpfbox_NO3_leg_all
ggsave(pstagpfffbox_NO2_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpfffbox_NO3_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
pstagpfbox_NO3_stat_bymonth_all<-pstagpfffbox_NO3_leg_all+ stat_compare_means(aes(group=flush.name), label = "p.signif",symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns")),label.y=2, color="red", size=6, hjust=0.5)  
pstagpfbox_NO3_stat_bymonth_all
ggsave(pstagpfbox_NO3_stat_bymonth_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpfbox_NO3_stat_bymonth_all_", Sys.Date(), ".jpeg", sep=""), width=8, height=8, units="in")

pstagpfffbox2_NO3_leg_all<-ggplot(subset(IDR_Data,phase.f!="Transt"& !is.na(stagff) & flushprepost =="1" &stagff=="0"|phase.f!="Transt"& flushprepost=="2"), aes(x=reorder(factor(flush.name),stagffpf), y=nitrate_all, fill=factor(flush.name))) + 
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5) + facet_wrap(Loc.f~Residual, nrow=3)+ theme(strip.text.x = element_text(size = 14)) + xlab("Flush Event")+ ylab("Nitrate(mg/L-N)") +scale_fill_manual(name="", values=Pre_post)+guides(fill = guide_legend(reverse=TRUE)) + theme_bw()+
  theme(legend.position="bottom") + coord_cartesian(ylim=c(0,6)) + theme(axis.text.x = element_text(angle = 45,size=10, hjust = 1))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagpfffbox2_NO3_leg_all
ggsave(pstagpfffbox2_NO3_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpfffbox_NO3_leg_all_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")
pstagpfffbox2_NO3_stat_bymonth_all<-pstagpfffbox2_NO3_leg_all+ stat_compare_means(label = "p.format", size=4, hjust=0.5,label.x =1.5,label.y = 5.5)+
  stat_compare_means( aes(label = ..p.signif..),label.x = 1.5, label.y = 5, color="red", size=5, hjust=0.5)
pstagpfffbox2_NO3_stat_bymonth_all
ggsave(pstagpfffbox2_NO3_stat_bymonth_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpfffbox2_NO3_stat_bymonth_all_", Sys.Date(), ".jpeg", sep=""), width=8, height=8, units="in")

pstagpfffbox2_NO3_leg_all2<-ggplot(subset(IDR_Data, asDate>="2020-08-12"& flushprepost =="1"&!is.na(stagff) & stagff=="0" |asDate>="2020-08-12"&  flushprepost=="2"),  aes(x=reorder(factor(flush.name),stagffpf), y=nitrate_all, fill=factor(flush.name))) + 
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5) + facet_wrap(Loc.f~Residual, ncol=3)+ theme(strip.text.x = element_text(size = 12)) + xlab("Flush Event")+ ylab("Nitrate(mg/L-N)") +scale_fill_manual(name="", values=Pre_post)+guides(fill = guide_legend(reverse=TRUE)) + theme_bw()+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")) + coord_cartesian(ylim=c(-0,7)) + theme(axis.text.x = element_text(angle = 15, hjust = 1))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))#+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagpfffbox2_NO3_leg_all2
ggsave(pstagpfffbox2_NO3_leg_all2, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpfffbox_NO3_leg_all_", Sys.Date(), ".jpeg", sep=""), width=8, height=8, units="in")
pstagpfffbox2_NO3_stat_bymonth_all2<-pstagpfffbox2_NO3_leg_all2+ stat_compare_means(label = "p.format", size=4, hjust=0.5,label.x =1.5,label.y =  6)+stat_compare_means(label = "p.signif",hide.ns=T, color="red",size=5, hjust=0.5,label.x =1.5,label.y =5)#+stat_compare_means(comparisons = my_comparisons,label = "p.format", size=4, hjust=0.5,label.y = c(4,5, 5.5))
pstagpfffbox2_NO3_stat_bymonth_all2
ggsave(pstagpfffbox2_NO3_stat_bymonth_all2, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpfffbox2_NO3_stat_bymonth_all2_", Sys.Date(), ".jpeg", sep=""), width=5, height=9, units="in")

pstagpfffbox2_NO2_leg_all2<-ggplot(subset(IDR_Data, asDate>="2020-08-12"& flushprepost =="1"&!is.na(stagff) & stagff=="0" |asDate>="2020-08-12"&  flushprepost=="2"),  aes(x=reorder(factor(flush.name),stagffpf), y=nitrite_all, fill=factor(flush.name))) + 
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5) + facet_wrap(Loc.f~Residual, ncol=3)+ theme(strip.text.x = element_text(size = 15)) + xlab("Flush Event")+ ylab("Nitrite(mg/L-N)") +scale_fill_manual(name="", values=Pre_post)+guides(fill = guide_legend(reverse=TRUE)) + theme_bw()+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")) + coord_cartesian(ylim=c(0,3)) +  theme(axis.text.x = element_text(angle = 15, hjust = 1))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=165, Filter2=165, Filter3=165"))) + theme(plot.title = element_text(size=12))#+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagpfffbox2_NO2_leg_all2
ggsave(pstagpfffbox2_NO3_leg_all2, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpfffbox_NO2_leg_all_", Sys.Date(), ".jpeg", sep=""), width=8, height=8, units="in")
pstagpfffbox2_NO2_stat_bymonth_all2<-pstagpfffbox2_NO2_leg_all2+ stat_compare_means(label = "p.format", size=4, hjust=0.5,label.x =1.5,label.y =2.6)+ stat_compare_means(label = "p.signif",hide.ns=T, color="red",size=5, hjust=0.5,label.x =1.5,label.y =  2.2)#+ stat_compare_means(comparisons = my_comparisons,label = "p.format", size=4, hjust=0.5,label.y = c(1.2,2, 2.4))
pstagpfffbox2_NO2_stat_bymonth_all2
ggsave(pstagpfffbox2_NO2_stat_bymonth_all2, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpfffbox2_NO2_stat_bymonth_all2_", Sys.Date(), ".jpeg", sep=""), width=5, height=9, units="in")

pstagFFPFbox__NH3NOx<- ggarrange(pstagpfffbox2_NO2_stat_bymonth_all2,pstagpfffbox2_NO3_stat_bymonth_all2 ,ncol=1, common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 12, face = "bold") )
pstagFFPFbox__NH3NOx 
ggsave(pstagFFPFbox__NH3NOx , filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPFbox__NH3NOx _", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagpfffbox2_NH3_leg_all2<-ggplot(subset(IDR_Data, asDate>="2020-08-12"& flushprepost =="1"&!is.na(stagff) & stagff=="0" |asDate>="2020-08-12"&  flushprepost=="2"), aes(x=reorder(factor(flush.name),stagffpf), y=Free_NH3_N, fill=factor(flush.name))) + 
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5) + facet_wrap(Loc.f~Residual, ncol=3)+ theme(strip.text.x = element_text(size = 12)) + xlab("")+ ylab("Free Ammonia (mg/L-N)") +scale_fill_manual(name="", values=Pre_post)+guides(fill = guide_legend(reverse=TRUE)) + theme_bw()+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")) + coord_cartesian(ylim=c(0,1.2)) +  theme(axis.text.x = element_text(angle = 15, hjust = 1))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=165, Filter2=165, Filter3=165"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagpfffbox2_NH3_leg_all2
ggsave(pstagpfffbox2_NH3_leg_all2, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpfffbox_NH3_leg_all_", Sys.Date(), ".jpeg", sep=""), width=8, height=8, units="in")
pstagpfffbox2_NH3_stat_bymonth_all2<-pstagpfffbox2_NH3_leg_all2+ stat_compare_means(label = "p.format", size=4, hjust=0.5,label.x =1.5,label.y =  1)+stat_compare_means(aes(label = ..p.signif..), color="red",size=5, hjust=0.5,label.x =1.5,label.y =  0.9)#+stat_compare_means(comparisons = my_comparisons,label = "p.format", size=4, hjust=0.5,label.y = c(4,5, 5.5))
pstagpfffbox2_NH3_stat_bymonth_all2
ggsave(pstagpfffbox2_NH3_stat_bymonth_all2, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpfffbox2_NH3_stat_bymonth_all2_", Sys.Date(), ".jpeg", sep=""), width=8, height=8, units="in")




pstagpfffbox2NH3<-ggplot(subset(IDR_Data, mydates >= "2020-08-28"& !is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"& flushprepost=="2" ), aes(x=reorder(factor(flush.name),stagffpf), y=Free_NH3_N, fill=factor(flush.name))) + 
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5) + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 15)) + xlab("Flush Event")+ ylab("Free Ammonia (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col)+guides(fill = guide_legend(reverse=TRUE)) + theme_bw()+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")) + coord_cartesian(ylim=c(0,1.3)) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=165, Filter2=165, Filter3=165"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagpfffbox2NH3
ggsave(pstagpfffbox2NH3, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpfffbox_NH3_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
pstagpfffbox2NH3_stat_bymonth<-pstagpfffbox2NH3+ stat_compare_means(comparisons = my_comparisonsPFFFSTAG,label = "p.format", size=4, hjust=0.5,label.y = c(0.9,1.1, 1.2))+stat_compare_means(label.y = (1.3))
pstagpfffbox2NH3_stat_bymonth
ggsave(pstagpfffbox2NH3_stat_bymonth, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpfffbox2NH3_stat_bymonth_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

#pstagFFPFbox__NH3NOx<- ggarrange(pstagpfffbox2_NO2_stat_bymonth_all2,pstagpfffbox2_NO3_stat_bymonth_all2 ,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstagFFPFbox__NH3NOx 
#ggsave(pstagFFPFbox__NH3NOx , filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPFbox__NH3NOx _", Sys.Date(), ".jpeg", sep=""), width=8, height=9, units="in")


pstag_NO3_agedays_leg_all<-ggplot(subset(IDR_Data, flushprepost==2| flushprepost==1 & stagff=="0"|flushprepost==0 ), aes(x=agedays, y=nitrate_all, fill=factor(flush.name))) + 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Filter Age(days)")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=c("#ffffcc","#78c679","#636363")) + guides(fill = guide_legend(reverse=TRUE)) + 
  theme(legend.position="bottom")   + coord_cartesian(ylim=c(0,7)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+scale_x_continuous(breaks = scales::breaks_extended(5))+
  ggtitle(paste(subtitle= paste("Stagnation"))) + theme(plot.title = element_text(size=12))
pstag_NO3_agedays_leg_all
ggsave(pstag_NO3_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NO3_agedays_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

####TCL2########


pstag_TCL_leg_all<-ggplot(subset(IDR_Data, flushprepost ==1 & stagff=="0"|flushprepost==2 ), aes(x=as.Date(mydates), y=TCl2_mgL_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Total Chlorine (mg/L)") +scale_fill_manual(name="", values=c("#ffffcc","#78c679","#636363")) +guides(fill = guide_legend(reverse=TRUE)) + 
  theme(legend.position="bottom") + scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,3)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  ggtitle(paste(subtitle= paste("","\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))
pstag_TCL_leg_all
ggsave(pstag_TCL_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_TCL_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagFFPF_TCL_leg_all<-ggplot(subset(IDR_Data,!is.na(stagff) & flushprepost ==1 |flushprepost==2 ), aes(x=as.Date(mydates), y=TCl2_mgL_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Total Chlorine (mg/L)") +scale_fill_manual(name="", values=Pre_post4col) + guides(fill = guide_legend(reverse=TRUE)) +
  theme(legend.position="bottom") + scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,3)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  ggtitle(paste(subtitle= paste("",""))) + theme(plot.title = element_text(size=12))
pstagFFPF_TCL_leg_all
ggsave(pstagFFPF_TCL_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_TCL_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagFFPF_TCL_legall<-ggplot(subset(IDR_Data,!is.na(stagff) & flushprepost ==1 |flushprepost==2 ), aes(x=as.Date(mydates), y=TCl2_mgL_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Total Chlorine (mg/L)") +scale_fill_manual(name="", values=Pre_post4col) + guides(fill = guide_legend(reverse=TRUE)) +
  theme(legend.position="bottom") + scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,2.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  ggtitle(paste(subtitle= paste("",""))) + theme(plot.title = element_text(size=12))
pstagFFPF_TCL_legall
ggsave(pstagFFPF_TCL_legall, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_TCL_legall_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagTpPF_Monoall<-ggplot(subset(IDR_Data, !is.na(stagff) & flushprepost =="1" |flushprepost=="2" ), aes(x=as.Date(mydates), y=mono_mgL_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() +  facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Dates of operation")+ ylab("mono (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col) + guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,2.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle(paste(subtitle= paste("","\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))
pstagTpPF_Monoall
ggsave(pstagTpPF_Monoall, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagTpPF_Monoall", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagFFPFCL2 <- ggarrange(pstagTpPF_Monoall,pstagFFPF_TCL_legall ,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstagFFPFCL2  
ggsave(pstagFFPFCL2  , filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPFCL2 _", Sys.Date(), ".jpeg", sep=""), width=9, height=10, units="in")

pstag_TCL_leg_days<-ggplot(subset(IDR_Data,flushprepost==0 | Loc=="3" & !is.na(stagff) & flushprepost =="1" | Loc=="3" & flushprepost=="2" ), aes(x=agedays, y=TCl2_mgL_all, fill=factor(flush.name))) +  
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Filter Age(days)")+ ylab("Total Chlorine (mg/L-Cl)") +scale_fill_manual(name="", values=Pre_post4col) + guides(fill = guide_legend(reverse=TRUE)) + 
  theme(legend.position="bottom")  + coord_cartesian(ylim=c(0,5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+scale_x_continuous(breaks = scales::breaks_extended(12))+
  ggtitle(paste(subtitle= paste("",""))) + theme(plot.title = element_text(size=12))
pstag_TCL_leg_days
ggsave(pstag_TCL_leg_days, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_TCL_leg_days_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstag_Monodays<-ggplot(subset(IDR_Data,flushprepost==0 | Loc=="3" & !is.na(stagff) & flushprepost =="1" | Loc=="3" & flushprepost=="2" ), aes(x=agedays, y=mono_mgL_all, fill=factor(flush.name))) +  
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Filter Age(days)")+ ylab("mono (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col) + guides(fill = guide_legend(reverse=TRUE)) + 
  theme(legend.position="bottom")  + coord_cartesian(ylim=c(0,5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+scale_x_continuous(breaks = scales::breaks_extended(12))+
  ggtitle(paste(subtitle= paste("",""))) + theme(plot.title = element_text(size=12))
pstag_Monodays
ggsave(pstag_Monodays, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_Monodays_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagFFPFCL2days <- ggarrange(pstag_Monodays,pstag_TCL_leg_days ,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstagFFPFCL2days  
ggsave(pstagFFPFCL2days  , filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPFCL2days _", Sys.Date(), ".jpeg", sep=""), width=9, height=10, units="in")


my_comparisonsTCL <- list( c("Post Flush", "Stagnation"))
pstagbox_TCL_leg_all<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"&  !is.na(stagff) &stagff=="0"& flushprepost =="1" |mydates >= "2020-08-28"& flushprepost=="2"), aes(x=reorder(factor(flush.name),stagffpf), y=TCl2_mgL_all, fill=factor(flush.name))) +  scale_fill_manual(name="", values=c("#ffffcc","#78c679","#636363")) +guides(fill = guide_legend(reverse=TRUE)) +
  geom_boxplot(outlier.colour = "black",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_bw() +  facet_wrap(Loc.f~.,ncol=4)+ ylab("Total Chlorine mg/L-Cl") +xlab("") + facet_grid(~Loc.f) +coord_cartesian(ylim=c(0,1.7))+
  theme(strip.text.x = element_text(size = 14)) +scale_y_continuous(breaks = scales::breaks_extended(10))+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=13)) + theme(legend.title = element_text(size=12)) + theme(legend.position="bottom") +
  ggtitle(paste(subtitle= paste("","\nN: Filter1=165, Filter2=165, Filter3=165"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))+theme(axis.text.x = element_text(angle = 45, hjust = 1))
pstagbox_TCL_leg_all
ggsave(pstagbox_TCL_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_TCL_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
pstagbox_stat_TCL_leg_all<-pstagbox_TCL_leg_all + stat_compare_means(comparisons =my_comparisonsTCL,label = "p.format", size=5, hjust=0.5,label.y = 1.5)+
  stat_compare_means( aes(label = ..p.signif..),label.x = 1.5, label.y = 1.4, color="red", size=5, hjust=1.5)
  pstagbox_stat_TCL_leg_all
ggsave(pstagbox_stat_TCL_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_stat_TCL_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

my_comparisonsphase <- list(c("Burn", "Post-Burn"), c("Burn", "Pre-Burn"),c("Post-Burn", "Pre-Burn"))
my_comparisonsPFFFSTAG <- list(c("Firstflow", "Post Flush"), c("Post Flush", "Stagnation"), c("Stagnation", "Firstflow"))
my_comparisonsOVwk <- list(c("Weekend", "Overnight"))
pstagFFPF_TCL_leg_box<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"&  !is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"& flushprepost=="2"), aes(x=reorder(factor(flush.name),stagffpf), y=TCl2_mgL_all, fill=factor(flush.name))) + 
  geom_boxplot(outlier.colour = "black",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_classic2() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("")+ ylab("Total Chlorine mg/L-Cl") +guides(fill = guide_legend(reverse=TRUE))+scale_fill_manual(name="", values=Pre_post4col) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16))  + theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 13))+coord_cartesian(ylim=c(0,2))+
  ggtitle(paste(subtitle= paste("","\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagFFPF_TCL_leg_box
pstagffPFbox_stat_TCL_leg_all<-pstagFFPF_TCL_leg_box + stat_compare_means(comparisons =my_comparisonsPFFFSTAG ,aes(label = ..p.format..), size=4, hjust=0.5,label.y = c(1.3, 1.5, 1.7))+ stat_compare_means(size=4, hjust=0.5,label.y = (1.9) )
pstagffPFbox_stat_TCL_leg_all
ggsave(pstagffPFbox_stat_TCL_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagffPFbox_stat_TCL_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")


####TATP#######
pstagPF3_TAtp_<-ggplot(subset(IDR_Data, stagff=="0" & !is.na(stagff) & flushprepost =="1" |flushprepost=="2"), aes(x=as.Date(mydates), y=TAtp_RLU, fill=factor(flush.name))) + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=0, ymax=10^(5), alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=0, ymax=10^(5), alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol = 4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Dates of operation: Month ")+ ylab("Total ATP (RLU)")+scale_x_date(breaks = pretty_breaks(10)) +scale_fill_manual(name="",  values=Pre_post)  + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) + theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+guides(fill = guide_legend(reverse=TRUE))+
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))
pstagPF3_TAtp_
ggsave(pstagPF3_TAtp_, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagPF_TAtp__", Sys.Date(), ".jpeg", sep=""), width=8, height=6, units="in")

pstagPF_TAtp_<-ggplot(subset(IDR_Data, !is.na(stagff) & flushprepost =="1" |flushprepost=="2"), aes(x=as.Date(mydates), y=TAtp_RLU, fill=factor(flush.name))) + annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=0, ymax=10^(5), alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=0, ymax=10^(5), alpha=0.6)+ 
  geom_point(size=2, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol = 4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Dates of operation: Month ")+ ylab("Total ATP (RLU)")+scale_x_date(breaks = pretty_breaks(10)) +scale_fill_manual(name="",  values=Pre_post4col)  + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) + theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+guides(fill = guide_legend(reverse=TRUE))+
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))
pstagPF_TAtp_
ggsave(pstagPF_TAtp_, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagPF_TAtp__", Sys.Date(), ".jpeg", sep=""), width=9, height=6, units="in")

pstagFFPFbox_TAtp_leg_all<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"&  !is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"& flushprepost=="2" ), aes(x=reorder(factor(flush.name),stagffpf), y=TAtp_RLU, fill=factor(flush.name))) + coord_cartesian(ylim=c(10^(0),10^(5))) +
  geom_boxplot(outlier.colour = "black",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_bw() +  facet_wrap(Loc.f~.,ncol = 3)  + xlab("")+ ylab("Total TAtp (RLU)")+scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) +coord_cartesian(ylim=c(10^(0),10^(8))) +
  scale_fill_manual(name="",  values=Pre_post4col)  + theme(strip.text.x = element_text(size = 12)) + theme(axis.text=element_text(size=12), axis.title=element_text(size=13),axis.text.x = element_text(angle = 15, hjust = 1)) + theme(legend.title = element_text(size=13)) + theme(legend.position="bottom") +guides(fill = guide_legend(reverse=TRUE))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=165, Filter2=165, Filter3=165"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagFFPFbox_TAtp_leg_all
ggsave(pstagFFPFbox_TAtp_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_TAtp_leg_all", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")
pstagFFPFbox_TAtp_stat_bymonth_all<-pstagFFPFbox_TAtp_leg_all+ stat_compare_means(comparisons =my_comparisonsPFFFSTAG ,aes(label = ..p.format..), size=4, hjust=0.5,label.y = c(c(5, 6,7)))
pstagFFPFbox_TAtp_stat_bymonth_all
pstagFFPFbox_TAtp_stat<- pstagFFPFbox_TAtp_stat_bymonth_all + stat_compare_means(label.y = 7.8,size=4)
pstagFFPFbox_TAtp_stat
ggsave(pstagFFPFbox_TAtp_stat_bymonth_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPFbox_TAtp_stat_bymonth_alll_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")

pstagFFPFbox__ATP <- ggarrange(pstagPF_TAtp_, pstagFFPFbox_TAtp_stat ,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstagFFPFbox__ATP 
ggsave(pstagFFPFbox__ATP , filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPFbox__ATP_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")


pstag_TAtp_vol_<-ggplot(subset(IDR_Data, stag=="1"  & stagdur_min=="1080" | stagdur_min=="3960" ), aes(x=cumvol, y=TAtp_RLU, fill=factor(stagdur_min))) +  
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4 )+ theme(strip.text.x = element_text(size = 14)) + scale_x_continuous(breaks = scales::breaks_extended(10))+
  xlab("Cummulative Filter Volume")+ ylab("TAtp_RLU)") +scale_fill_manual(name="Stagnation:", values=overweekcol, labels=c("Overnight", "weekend")) + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) + theme(legend.position="bottom")  + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle(paste(subtitle= paste("Stagnation"))) + theme(plot.title = element_text(size=12))
pstag_TAtp_vol_
ggsave(pstag_TAtp_vol_, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_TAtp_vol_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagPF_TAtp_leg_box<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"& flushprepost==2| mydates >= "2020-08-28"&flushprepost==1& stagff=="0"), aes(x=reorder(factor(flush.name),stagffpf), y=TAtp_RLU, fill=factor(flush.name))) + 
  geom_boxplot(outlier.colour = "black",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("TAtp_RLU") +scale_fill_manual(name="Stagnation:", values=Pre_post) + 
  theme(legend.position="bottom") +  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x)))+ coord_cartesian(ylim=c(10^(0),10^(4.5))) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle(paste(subtitle= paste("Stagnation","\nN: Filter1=165, Filter2=165, Filter3=165"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagPF_TAtp_leg_box
pstagffbox_stat_TAtp_leg_all<-pstagPF_TAtp_leg_box + stat_compare_means(aes(label = ..p.format..),  size=5, hjust=0.5,label.y = c(4))+
  stat_compare_means(aes(label = ..p.signif..),  size=5, hjust=0.5,label.y = c(3.8))
  pstagffbox_stat_TAtp_leg_all
ggsave(pstagffbox_stat_TAtp_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagffbox_stat_TAtp_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstag_TAtp1_<-ggplot(subset(IDR_Data, stag=="1"  & stagdur_min=="1080" | stagdur_min=="3960" ), aes(x=agedays, y=TAtp_RLU, fill=factor(stagdur_min))) +# annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=0, ymax=10^(5), alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=0, ymax=10^(5), alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4 )+ theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Total ATP (RLU)") +scale_fill_manual(name="", values=overweekcol, labels=c("Overnight", "weekend")) + #+ scale_x_date(breaks = pretty_breaks(10))
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) +  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
pstag_TAtp1_

pstagFFPF_TAtp_<-ggplot(subset(IDR_Data,  !is.na(stagff) & flushprepost =="1" |flushprepost=="2"), aes(x=agedays, y=TAtp_RLU, fill=factor(flush.name))) + guides(fill = guide_legend(reverse=TRUE))+# annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-28"), ymin=0, ymax=100000, alpha=0.5) + 
  geom_point(size=3, shape=21)+theme_bw() + facet_grid(.~Loc.f) + theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Total ATP (RLU)")+scale_fill_manual(name="", values=Pre_post4col)  + scale_x_continuous(breaks = scales::breaks_extended(10))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x)))+ coord_cartesian(ylim=c(10^(0),10^(4.5))) + theme(axis.text = element_text(size = 12),axis.title.x=element_text(size = 15),axis.title.y=element_text(size = 14,legend.position="bottom")  + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle(paste(subtitle= paste("Flush Sample"))) + theme(plot.title = element_text(size=12))
pstagFFPF_TAtp_
ggsave(pstagFFPF_TAtp_, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_TAtp__", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

stagPfATP <-ggarrange(pstag_TAtp1_,pstagFFPF_TAtp_,ncol=1, widths = c(2, 2),  common.legend = F, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
stagPfATP
ggsave(stagPfATP, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/stagPfATP_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")



pstagFFPF_HPC_<-ggplot(subset(IDR_Data,  flushprepost==0 | !is.na(stagff) & flushprepost =="1" |flushprepost=="2"), aes(x=as.Date(mydates), y=HPC2, fill=factor(flush.name))) +  annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=0, ymax=10^(7), alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=0, ymax=10^(7), alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=0, ymax=10^(7), alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=0, ymax=10^(7), alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_grid(.~Loc.f) + theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("HPC CFU/mL")+scale_x_date(breaks = pretty_breaks(10)) +scale_fill_manual(name="Sample:", values=Pre_post4col)  + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) + theme(legend.position="bottom")  + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle(paste(subtitle= paste("Flush Sample"))) + theme(plot.title = element_text(size=12))
pstagFFPF_HPC_
ggsave(pstagFFPF_HPC_, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_HPC__", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagFFPF_TAtp_leg_box<-ggplot(subset(IDR_Data, mydates >= "2020-08-28"&!is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"&flushprepost=="2"), aes(x=reorder(factor(flush.name),stagffpf), y=TAtp_RLU, fill=factor(flush.name))) + 
  geom_boxplot(outlier.colour = "black",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 13))  + xlab("")+ ylab("TATP (RLU)") +guides(fill = guide_legend(reverse=TRUE))+scale_fill_manual(name="", values=Pre_post4col) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16)) +  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),limits=c(1,1e6),labels = scales::trans_format("log10", scales::math_format(10^.x))) + theme(axis.text.x = element_text(angle = 45,size=12, hjust = 1))+
  ggtitle(paste(subtitle= paste("","\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagFFPF_TAtp_leg_box
pstagffPFbox_stat_TAtp_leg_all<-pstagFFPF_TAtp_leg_box + stat_compare_means(comparisons =my_comparisonsPFFFSTAG ,aes(label = ..p.format..), size=4, hjust=0.5,label.y = c(3.5, 4, 5))+stat_compare_means( size=4, hjust=0.5,label.y = c(5.5))
pstagffPFbox_stat_TAtp_leg_all
ggsave(pstagffPFbox_stat_TAtp_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagffPFbox_stat_TAtp_leg_all_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")


pstag_TAtpdays_<-ggplot(subset(IDR_Data,  flushprepost==0 |flushprepost ==1 & stagff=="0"|flushprepost==2), aes(x=agedays, y=TAtp_RLU, fill=factor(flush.name))) + 
  geom_point(size=3, shape=21)+theme_bw() +theme(axis.text = element_text(size = 15),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))+ facet_wrap(Loc.f~.,ncol = 4) + theme(strip.text.x = element_text(size = 14))  + xlab("Filter Age(days)")+ ylab("Total ATP (RLU)") +scale_fill_manual(name="",  values=c("#ffffcc","#78c679","#636363"))+guides(fill = guide_legend(reverse=TRUE)) + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) + theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")  + theme(axis.text.x = element_text(size = 15,angle = 45, hjust = 1))+
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))+scale_x_continuous(breaks = scales::breaks_extended(5))
pstag_TAtpdays_
ggsave(pstag_TAtpdays_, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_TAtpdays_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")


###MONO#######

pstag_Mono_leg_all<-ggplot(subset(IDR_Data,  stagff=="0"& flushprepost=="1"|flushprepost=="2" & Loc!="0" ), aes(x=as.Date(mydates), y=mono_mgL_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() +  facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Dates of operation")+ ylab("mono (mg/L-N)") +scale_fill_manual(name="", values=Pre_post) + guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,2)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle(paste(subtitle= paste("","\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))
pstag_Mono_leg_all
ggsave(pstag_Mono_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_Mono_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagTpPF_Mono<-ggplot(subset(IDR_Data,flushprepost=="0"| !is.na(stagff) & flushprepost =="1" &stagff=="0"|flushprepost=="2" ), aes(x=as.Date(mydates), y=mono_mgL_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() +  facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Dates of operation")+ ylab("mono (mg/L-N)") +scale_fill_manual(name="", values=c("#ffffcc","#78c679","#636363")) + guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,4.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle(paste(subtitle= paste("","\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))
pstagTpPF_Mono
ggsave(pstagTpPF_Mono, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagTpPF_Mono", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagTpPF_Monoall<-ggplot(subset(IDR_Data,flushprepost=="0"| !is.na(stagff) & flushprepost =="1" |flushprepost=="2" ), aes(x=as.Date(mydates), y=mono_mgL_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() +  facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Dates of operation")+ ylab("mono (mg/L-N)") +scale_fill_manual(name="", values=Pre_post4col) + guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,4.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle(paste(subtitle= paste("","\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))
pstagTpPF_Monoall
ggsave(pstagTpPF_Monoall, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagTpPF_Monoall", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

####FNH3#######
pstag_Free_NH3_leg_all<-ggplot(subset(IDR_Data, flushprepost=="0"| flushprepost=="1" & stagff=="0" |flushprepost=="2"), aes(x=as.Date(mydates), y=Free_NH3_N, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() +  facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Dates of operation")+ ylab("Free_NH3_N)") +scale_fill_manual(name="", values=c("#ffffcc","#78c679","#636363")) +guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.x=element_text(size = 15),axis.title.y=element_text(size = 14,legend.position="bottom")+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,1.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))
pstag_Free_NH3_leg_all
ggsave(pstag_Free_NH3_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_Free_NH3_leg_all", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagFFPF_Free_NH3_leg_all<-ggplot(subset(IDR_Data,flushprepost=="0"| !is.na(stagff) & flushprepost =="1" &stagff=="0"|flushprepost=="2" ), aes(x=as.Date(mydates), y=Free_NH3_N, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Dates of operation")+ ylab("Free_NH3 (mg/L-N)") +scale_fill_manual(name="", values=c("#ffffcc","#78c679","#636363"))  +guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + theme(axis.text.x = element_text(angle = 45,size = 15, hjust = 1))+ scale_x_date(breaks = pretty_breaks(10)) +coord_cartesian(ylim=c(0,1.5)) +
  theme(plot.title = element_text(size=12))
pstagFFPF_Free_NH3_leg_all
ggsave(pstagFFPF_Free_NH3_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_Free_NH3_leg_all", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")


pstagTpPF_Free_NH3<-ggplot(subset(IDR_Data,flushprepost=="0"| !is.na(stagff) & flushprepost =="1" &stagff=="0"|flushprepost=="2" ), aes(x=as.Date(mydates), y=Free_NH3_N, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Dates of operation")+ ylab("Free_NH3 (mg/L-N)") +scale_fill_manual(name="", values=c("#ffffcc","#78c679","#636363"))  +guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + theme(axis.text.x = element_text(angle = 45,size = 15, hjust = 1))+ scale_x_date(breaks = pretty_breaks(10)) +coord_cartesian(ylim=c(0,1.25)) +
  theme(plot.title = element_text(size=12))
pstagTpPF_Free_NH3
ggsave(pstagTpPF_Free_NH3, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagTpPF_Free_NH3", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")


my_comparisons <- list(c("Firstflow", "Post Flush"), c("Post Flush", "Stagnation"), c("Stagnation", "Firstflow"))
pstagFFPF_NH3_leg_box<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"& !is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"&flushprepost=="2"), aes(x=reorder(factor(flush.name),stagffpf), y=Free_NH3_N, fill=factor(flush.name))) + 
  geom_boxplot(outlier.colour = "black",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+coord_cartesian(ylim=c(0,1.5))+theme_classic2() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("")+ ylab("Free_NH3 (mg/L-N)") +guides(fill = guide_legend(reverse=TRUE))+scale_fill_manual(name="", values=Pre_post4col) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16))  + theme(axis.text.x = element_text(angle = 15, hjust = 1,size = 12))+
  ggtitle(paste(subtitle= paste("","\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagFFPF_NH3_leg_box
pstagffPFbox_stat_NH3_leg_all<-pstagFFPF_NH3_leg_box + stat_compare_means(comparisons = my_comparisons,aes(label = ..p.format..), size=4, hjust=0.5,label.y = c(1, 1.2, 1.35))#+stat_compare_means( size=4, hjust=0.5,label.y = c(1.45))
pstagffPFbox_stat_NH3_leg_all
ggsave(pstagffPFbox_stat_NH3_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagffPFbox_stat_NH3_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagFFPF_NO2_box<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"& !is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"&flushprepost=="2"), aes(x=reorder(factor(flush.name),stagffpf), y=nitrite_all, fill=factor(flush.name))) + 
  geom_boxplot(outlier.colour = "black",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_classic2() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 12)) +coord_cartesian(ylim=c(0,3)) + xlab("")+ ylab("Nitrite (mg/L-N)") +guides(fill = guide_legend(reverse=TRUE))+scale_fill_manual(name="", values=Pre_post4col) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16))  + theme(axis.text.x = element_text(angle = 15, hjust = 1,size = 12))+#geom_hline(yintercept=(0.05), color="red")+
  ggtitle(paste(subtitle= paste("","\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))
pstagFFPF_NO2_box
pstagffPFbox_stat_NO2_all<-pstagFFPF_NO2_box + stat_compare_means(comparisons = my_comparisons,aes(label = ..p.format..), size=4, hjust=0.5,label.y = c(1.5,2,2.5))#+#stat_compare_means( size=4, hjust=0.5,label.y = c(2.8))
pstagffPFbox_stat_NO2_all
ggsave(pstagffPFbox_stat_NO2_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagffPFbox_stat_NO2_all_", Sys.Date(), ".jpeg", sep=""),width=7, height=9, units="in")

pstagFFPF_NO3_box<-ggplot(subset(IDR_Data, mydates >= "2020-08-28"&!is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"&flushprepost=="2"), aes(x=reorder(factor(flush.name),stagffpf), y=nitrate_all, fill=factor(flush.name))) + 
  geom_boxplot(outlier.colour = "black",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_classic2()+ facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("")+ ylab("Nitrate (mg/L-N)") +guides(fill = guide_legend(reverse=TRUE))+scale_fill_manual(name="", values=Pre_post4col) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16))  + theme(axis.text.x = element_text(angle = 15, hjust = 1,size = 12))+coord_cartesian(ylim=c(0,8))+
  ggtitle(paste(subtitle= paste("","\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagFFPF_NO3_box
pstagffPFbox_stat_NO3_all<-pstagFFPF_NO3_box + stat_compare_means(comparisons = my_comparisons, aes(label = ..p.format..), label.y = c(5.5,6.5,7.5), size=4, hjust=0.5)#+stat_compare_means( size=4, hjust=0.5,label.y = c(7))
  pstagffPFbox_stat_NO3_all
ggsave(pstagffPFbox_stat_NO3_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagffPFbox_stat_NO3_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

filter1_2_3StgFFPFBOX <-ggarrange(pstagffPFbox_stat_NO2_all, pstagffPFbox_stat_NO3_all,ncol=1,  common.legend = T, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
filter1_2_3StgFFPFBOX
ggsave(filter1_2_3StgFFPFBOX, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/filter1_2_3StgFFPFBOX_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")

pstagFFPF_NO2_leg_box<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"& !is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"&flushprepost=="2"), aes(x=reorder(factor(flush.name),stagffpf), y=nitrate_all, fill=factor(flush.name))) + 
  geom_boxplot(outlier.colour = "black",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_bw() + facet_wrap(Loc.f~phase.f)+ theme(strip.text.x = element_text(size = 14))  + xlab("")+ ylab("Nitrate (mg/L-N)") +guides(fill = guide_legend(reverse=TRUE))+scale_fill_manual(name="", values=Pre_post4col) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16)) + coord_cartesian(ylim=c(0,6.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 20))+
  ggtitle(paste(subtitle= paste("","\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=15))
pstagFFPF_NO2_leg_box
pstagffPFbox_stat_NO2_leg_all<-pstagFFPF_NO2_leg_box + stat_compare_means(comparisons = my_comparisons,aes(label = ..p.signif..),hide.ns=T, size=5, hjust=0.5,label.y = c(4,5.2, 6))
 pstagffPFbox_stat_NO2_leg_all
ggsave(pstagffPFbox_stat_NO2_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagffPFbox_stat_NO2_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

#####CoCE#####
coceNNH3<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"& CoCe=="20" |mydates >= "2020-08-28"& CoCe=="21"  ), aes(x=agedays, y=Free_NH3_N, colour=factor(CoCe.name))) +geom_point(size=2) + theme_bw() + coord_cartesian(ylim=c(0,2))+
  xlab("Filter Age (Days)")+ ylab("Free NH3 (mg/L-N)") +scale_color_manual(name="", values=c("orange", "navy"))  +  
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + annotate("text", label="", x=150,y= 00.6, size=5, color="chocolate", hjust=0) +scale_x_continuous(breaks = scales::breaks_extended(20))+
  annotate("text", label="", x=25,y= 0.75, size=5, color="navy", hjust=0) + facet_wrap(~Loc.f) + theme(strip.text.x = element_text(size = 14)) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12)) + theme(legend.title = element_text(size=12)) + theme(legend.text = element_text(size = 16)) + theme(axis.text.x = element_text(angle = 45, hjust = 1,size =12))+
  theme(plot.title = element_text(size=10))
coceNNH3

coceNO2<-ggplot(subset(IDR_Data, mydates >= "2020-08-28"&CoCe=="20" | mydates >= "2020-08-28"&CoCe=="21"  ), aes(x=agedays, y=nitrite_all, colour=factor(CoCe.name))) +geom_point(size=2) + theme_bw() + coord_cartesian(ylim=c(0,2.5))+
  xlab("Filter Age (Days)")+ ylab("Nitrite (mg/L-N)") +scale_color_manual(name="", values=c("orange", "navy"))  +  
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16)) + annotate("text", label="", x=75,y= 0.2, size=5, color="chocolate", hjust=0) +scale_x_continuous(breaks = scales::breaks_extended(20))+
  annotate("text", label="", x=25,y= 1.25, size=5, color="navy", hjust=0) + facet_wrap(~Loc.f) + theme(strip.text.x = element_text(size = 14)) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12)) + theme(legend.title = element_text(size=14)) + theme(legend.text = element_text(size = 16))+ theme(axis.text.x = element_text(angle = 45, hjust = 1,size =12)) +
  theme(plot.title = element_text(size=10))
coceNO2
#ggsave(pCostag_days, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pCostag_days_", Sys.Date(), ".jpeg", sep=""), width=10, height=6, units="in")


CoceNo3<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"& CoCe=="20" | mydates >= "2020-08-28"&CoCe=="21"  ), aes(x=agedays, y=nitrate_all, colour=factor(CoCe.name))) +geom_point(size=2) + theme_bw() + coord_cartesian(ylim=c(0,7))+
  xlab("Filter Age (Days)")+ ylab("Nitrate (mg/L-N)") +scale_color_manual(name="", values=c("orange", "navy"))  +  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + annotate("text", label="", x=100,y= 0.8, size=5, color="chocolate", hjust=0) +scale_x_continuous(breaks = scales::breaks_extended(20))+
  annotate("text", label="", x=20,y= 4, size=5, color="navy", hjust=0) + facet_wrap(~Loc.f) + theme(strip.text.x = element_text(size = 12)) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12)) + theme(legend.title = element_text(size=12)) + theme(legend.text = element_text(size = 16)) + theme(axis.text.x = element_text(angle = 45, hjust = 1,size =12))+
  theme(plot.title = element_text(size=12))
CoceNo3
#ggsave(pCostagNO3_days, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pCostagNO3_days_", Sys.Date(), ".jpeg", sep=""), width=10, height=6, units="in")

my_comparisonsMEndD <- list(c("End of day", "Mornings"))
coceNO2_leg_all<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"& CoCe=="20" | mydates >= "2020-08-28"&CoCe=="21" ), aes(x=as.factor(CoCe.name), y=nitrite_all, fill=factor(CoCe.name))) + 
  geom_boxplot(outlier.colour = "black",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_classic2() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("")+ ylab("Nitrite (mg/L-N)") +guides(fill = guide_legend(reverse=TRUE))+scale_fill_manual(name="", values=c("orange", "navy")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16))  + theme(axis.text.x = element_text(angle = 45, hjust = 1,size =12))+coord_cartesian(ylim=c(0,3))+
  ggtitle(paste(subtitle= paste("","\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))
coceNO2_leg_all
coceNO2_stat_leg_all<-coceNO2_leg_all + stat_compare_means(comparisons =my_comparisonsMEndD ,aes(label = ..p.signif..),  size=5, hjust=0.5,label.y = c(2.5))
coceNO2_stat_leg_all
ggsave(coceNO2_stat_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/coceNO2_stat_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")



CoceATP<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"& CoCe=="20" | mydates >= "2020-08-28"&CoCe=="21"  ), aes(x=agedays, y=TAtp_RLU, colour=factor(CoCe.name))) +geom_point(size=3) + theme_bw() + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  xlab("Filter Age (Days)")+ ylab("Total ATP_RLU") +scale_color_manual(name="", values=c("orange", "navy"))  +  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))+ annotate("text", label="", x=75,y= 10^(1), size=5, color="chocolate", hjust=0) +scale_x_continuous(breaks = scales::breaks_extended(10))+
  annotate("text", label="", x=0,y= 10^(3), size=4, color="navy", hjust=0) + facet_wrap(~Loc.f) + theme(strip.text.x = element_text(size = 14)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16)) + theme(legend.title = element_text(size=14)) + theme(legend.text = element_text(size = 14)) +
  theme(plot.title = element_text(size=10))

#ggsave(pCostagATP_days, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pCostagATP_days_", Sys.Date(), ".jpeg", sep=""), width=10, height=6, units="in")

filter1_2_3COCE <-ggarrange(coceNNH3,coceNO2, CoceNo3,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
filter1_2_3COCE
ggsave(filter1_2_3COCE, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/filter1_2_3COCE_", Sys.Date(), ".jpeg", sep=""), width=9, height=7, units="in")

########TIME SEREIS OF BURN AND BEYOUND#########
####NO2########
filter1ALL<-ggplot(subset(IDR_Data, asDate>="2020-08-12"&  stag=="0"  & Loc=="1"&stagdur_min=="1080" | Loc=="1"&stagdur_min=="3960"), aes(x=agedays, y=nitrite_all, fill=Residual)) +scale_x_continuous(limits = c(0, 100))+
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(~Loc.f)+ theme(strip.text.x = element_text(size = 14))   + xlab("Age(days)")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=c( "lightgoldenrod1", "chocolate1", "orangered4")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + coord_cartesian(ylim=c(0,2.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))
filter1ALL


filter2burn_pstburn<-ggplot(subset(IDR_Data, asDate>="2020-08-12"&  stag=="0"  & Loc=="2"&stagdur_min=="1080" | Loc=="2"&stagdur_min=="3960"), aes(x=agedays, y=nitrite_all, fill=Residual)) +scale_x_continuous(limits = c(0, 100))+
  geom_point(size=3, shape=21)+theme_bw() + facet_grid(~Loc.f)+ theme(strip.text.x = element_text(size = 14))   + xlab("Age(days)")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=c(  "chocolate1", "orangered4"))+ 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))  + coord_cartesian(ylim=c(0,2.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))
filter2burn_pstburn 

filter3pstburn<-ggplot(subset(IDR_Data, asDate>="2020-08-12"&  stag=="0"  & Loc=="3"&stagdur_min=="1080" | Loc=="3"&stagdur_min=="3960"), aes(x=agedays, y=nitrite_all, fill=Residual))  +scale_x_continuous(limits = c(0, 100))+
  geom_point(size=3, shape=21)+theme_bw() + facet_grid(~Loc.f)+ theme(strip.text.x = element_text(size = 14))   + xlab("Age(days)")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=c(  "chocolate1","orangered4"))+ 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))  + coord_cartesian(ylim=c(0,2.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))
filter3pstburn  

multiplot_approach_2<-ggarrange(filter1ALL, filter2burn_pstburn,filter3pstburn,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
multiplot_approach_2
ggsave(multiplot_approach_2, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/multiplot_approach_2_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

multiplot_approach_1<-ggplot(subset(IDR_Data, asDate>="2020-08-12"&  !is.na(stagff) & flushprepost =="1"& stagff=="0"&stag.dur=="Weekend" ), aes(x=agedays, y=nitrite_all, fill=Residual)) + scale_x_continuous(breaks = scales::breaks_extended(10))+
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,nrow=3) + theme(strip.text.x = element_text(size = 14))  + xlab("Age(days)")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=c(  "chocolate1", "orangered4")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + coord_cartesian(ylim=c(0,2.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))
multiplot_approach_1
ggsave(multiplot_approach_1, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/multiplot_approach_1_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

multiplot_approach_3<-ggplot(subset(IDR_Data, asDate>="2020-08-12"&  flushprepost =="1" ), aes(x=agedays, y=nitrite_all, fill=Residual)) + scale_x_continuous(breaks = scales::breaks_extended(10))+
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,nrow=3) + theme(strip.text.x = element_text(size = 14))  + xlab("Age(days)")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=c(  "chocolate1", "orangered4")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + coord_cartesian(ylim=c(0,2.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))
multiplot_approach_3
ggsave(multiplot_approach_3, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/multiplot_approach_3_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

########NO2#######
filter1NO2burn_pstburn<-ggplot(subset(IDR_Data, asDate>="2020-08-12"&  !is.na(stagff) &stagff=="0"& Loc=="1"&flushprepost =="1"&stag.dur=="Weekend"), aes(x=agedays, y=nitrite_all, fill=Residual)) + scale_x_continuous(breaks = scales::breaks_extended(10))+
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(~Loc.f) + theme(strip.text.x = element_text(size = 14))  + xlab("Age(days)")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=c( "chocolate1", "orangered4")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))  + coord_cartesian(ylim=c(0,3)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))
filter1NO2burn_pstburn


  filter2NO2burn_pstburn<-ggplot(subset(IDR_Data,asDate>="2020-08-12"&  !is.na(stagff) &stagff=="0"& Loc=="2"&flushprepost =="1"&stag.dur=="Weekend"  ), aes(x=agedays, y=nitrite_all, fill=Residual)) + scale_x_continuous(breaks = scales::breaks_extended(10))+
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(~Loc.f)+ theme(strip.text.x = element_text(size = 14))  + xlab("Age(days)")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=c(  "chocolate1", "orangered4"))+ 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))  + coord_cartesian(ylim=c(0,3)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))
filter2NO2burn_pstburn 

filter3NO2pstburn<-ggplot(subset(IDR_Data, asDate>="2020-08-12"&  !is.na(stagff) &stagff=="0"& Loc=="3"&flushprepost =="1" &stag.dur=="Weekend"), aes(x=agedays, y=nitrite_all, fill=Residual)) + scale_x_continuous(breaks = scales::breaks_extended(10))+
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(~Loc.f) + theme(strip.text.x = element_text(size = 14))  + xlab("Age(days)")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=c( "chocolate1","orangered4")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))  + coord_cartesian(ylim=c(0,3.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))
filter3NO2pstburn  

multiplot_approach_2NO2<-ggarrange(filter1NO2burn_pstburn, filter2NO2burn_pstburn,filter3NO2pstburn ,ncol=3, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
multiplot_approach_2NO2
ggsave(multiplot_approach_2NO2, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/multiplot_approach_2NO2_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

########NO3#######
filter1NO3ALL<-ggplot(subset(IDR_Data, asDate>="2020-08-12"&  !is.na(stagff) &stagff=="0"& Loc=="1"&flushprepost =="1"&stag.dur=="Weekend" ), aes(x=agedays, y=nitrate_all, fill=Residual)) + scale_x_continuous(breaks = scales::breaks_extended(10))+
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(~Loc.f) + theme(strip.text.x = element_text(size = 14))  + xlab("Age(days)")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=c( "chocolate1", "orangered4")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))  + coord_cartesian(ylim=c(0,5.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))
filter1NO3ALL


filter2NO3burn_pstburn<-ggplot(subset(IDR_Data,asDate>="2020-08-12"&  !is.na(stagff) &stagff=="0"& Loc=="2"&flushprepost =="1"&stag.dur=="Weekend"  ), aes(x=agedays, y=nitrate_all, fill=Residual)) + scale_x_continuous(breaks = scales::breaks_extended(10))+
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(~Loc.f)+ theme(strip.text.x = element_text(size = 14))  + xlab("Age(days)")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=c(  "chocolate1", "orangered4"))+ 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))  + coord_cartesian(ylim=c(0,5.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))
filter2NO3burn_pstburn 

filter3NO3pstburn<-ggplot(subset(IDR_Data, asDate>="2020-08-12"&  !is.na(stagff) &stagff=="0"& Loc=="3"&flushprepost =="1" &stag.dur=="Weekend"), aes(x=agedays, y=nitrate_all, fill=Residual)) + scale_x_continuous(breaks = scales::breaks_extended(10))+
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(~Loc.f) + theme(strip.text.x = element_text(size = 14))  + xlab("Age(days)")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=c( "chocolate1","orangered4")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))  + coord_cartesian(ylim=c(0,5.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))
filter3NO3pstburn  

multiplot_approach_2NO3<-ggarrange(filter1NO3ALL, filter2NO3burn_pstburn,filter3NO3pstburn ,ncol=3, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
multiplot_approach_2NO3
ggsave(multiplot_approach_2NO3, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/multiplot_approach_2NO3_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

#multiplot_approach_1NO3<-ggplot(subset(IDR_Data, asDate>="2020-08-12"&  !is.na(stagff) & flushprepost =="1" |flushprepost=="2"), aes(x=agedays, y=TAtp_RLU, fill=phase.f)) + scale_x_continuous(breaks = scales::breaks_extended(10))+
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,nrow=3)+ theme(strip.text.x = element_text(size = 14))   + xlab("Age(days)")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=c( "lightgoldenrod1", "chocolate1", "orangered4")) + 
    theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))  + coord_cartesian(ylim=c(0,5.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(subtitle= paste("Filter operation:", xmin, "-", xmax,"Pre_Post flush"))) + theme(plot.title = element_text(size=12))
#multiplot_approach_1NO3
#ggsave(multiplot_approach_1NO3, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/multiplot_approach_1NO3_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

#####Free AMMONIA ######
filter1NH3ALL<-ggplot(subset(IDR_Data, asDate>="2020-08-12"& stagff=="0"& !is.na(stagff) & Loc=="1"&flushprepost =="1"&stag.dur=="Weekend" ), aes(x=agedays, y=Free_NH3_N, fill=Residual)) + scale_x_continuous(breaks = scales::breaks_extended(10))+
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(~Loc.f)+ theme(strip.text.x = element_text(size = 14))   + xlab("Age(days)")+ ylab("Free_NH3 (mg/L-N)") +scale_fill_manual(name="", values=c(  "chocolate1", "orangered4")) + 
    theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))  + coord_cartesian(ylim=c(0,1.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))
filter1NH3ALL

filter2NH3ALL<-ggplot(subset(IDR_Data, asDate>="2020-08-12"& stagff=="0"& !is.na(stagff) & Loc=="2"&flushprepost =="1"&stag.dur=="Weekend"), aes(x=agedays, y=Free_NH3_N, fill=Residual)) + scale_x_continuous(breaks = scales::breaks_extended(10))+
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(~Loc.f)+ theme(strip.text.x = element_text(size = 14))   + xlab("Age(days)")+ ylab("Free_NH3 (mg/L-N)") +scale_fill_manual(name="", values=c( "chocolate1", "orangered4")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + coord_cartesian(ylim=c(0,1.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))
filter2NH3ALL

filter3NH3ALL<-ggplot(subset(IDR_Data, asDate>="2020-08-12"& stagff=="0"& !is.na(stagff) & Loc=="3"&flushprepost =="1"&stag.dur=="Weekend"), aes(x=agedays, y=Free_NH3_N, fill=Residual)) + scale_x_continuous(breaks = scales::breaks_extended(10))+
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(~Loc.f) + theme(strip.text.x = element_text(size = 14))  + xlab("Age(days)")+ ylab("Free_NH3 (mg/L-N)") +scale_fill_manual(name="", values=c("chocolate1"  ,"orangered4")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))  + coord_cartesian(ylim=c(0,1.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))
filter3NH3ALL
multiplot_approach_2NH3<-ggarrange(filter1NH3ALL, filter2NH3ALL,filter3NH3ALL,ncol=3, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
multiplot_approach_2NH3
ggsave(multiplot_approach_2NH3, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/multiplot_approach_2NH3_", Sys.Date(), ".jpeg", sep=""), width=9, height=7, units="in")

multiplot_approach_Nox_NH3<-ggarrange(multiplot_approach_2NH3, multiplot_approach_2NO2,multiplot_approach_2NO3,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
multiplot_approach_Nox_NH3
ggsave(multiplot_approach_Nox_NH3, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/multiplot_approach_Nox_NH3_", Sys.Date(), ".jpeg", sep=""), width=9, height=7, units="in")

#########TATP###########
filter1TATPALL<-ggplot(subset(IDR_Data, asDate>="2020-08-12"& stagff=="0"& !is.na(stagff)& Loc=="1"&!is.na(stagff) & flushprepost =="1" &stag.dur=="Weekend"), aes(x=agedays, y=TAtp_RLU, fill=Residual)) + scale_x_continuous(breaks = scales::breaks_extended(10))+
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol = 1) + theme(strip.text.x = element_text(size = 14))  + xlab("Age(days)")+ ylab("Total ATP (RLU)") +scale_fill_manual(name="", values=c(  "chocolate1", "orangered4","black")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))  + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))+ coord_cartesian(ylim=c(10^(0),10^(4))) 
filter1TATPALL


filter2TATPburn_pstburn<-ggplot(subset(IDR_Data, asDate>="2020-08-12"&  stagff=="0"& Loc=="2"&!is.na(stagff) & flushprepost =="1"&stag.dur=="Weekend"), aes(x=agedays, y=TAtp_RLU, fill=Residual)) + scale_x_continuous(breaks = scales::breaks_extended(10))+
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=3) + theme(strip.text.x = element_text(size = 14))  + xlab("Age(days)")+ ylab("Total ATP (RLU)") +scale_fill_manual(name="", values=c(  "chocolate1", "orangered4"))+ 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))  + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))+ coord_cartesian(ylim=c(10^(0),10^(4)))
filter2TATPburn_pstburn 

filter3TATPpstburn<-ggplot(subset(IDR_Data, asDate>="2020-08-12"& stagff=="0"& Loc=="3"&!is.na(stagff) & flushprepost =="1"&stag.dur=="Weekend"), aes(x=agedays, y=TAtp_RLU, fill=Residual)) + scale_x_continuous(breaks = scales::breaks_extended(10))+
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(~Loc.f)+ theme(strip.text.x = element_text(size = 14))   + xlab("Age(days)")+ ylab("Total ATP (RLU)") +scale_fill_manual(name="", values=c("chocolate1", "orangered4"))+ 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))  + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))+ coord_cartesian(ylim=c(10^(0),10^(4)))
filter3TATPpstburn  

multiplot_approach_2TATP<-ggarrange(filter1TATPALL, filter2TATPburn_pstburn,filter3TATPpstburn ,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
multiplot_approach_2TATP
ggsave(multiplot_approach_2TATP, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/multiplot_approach_2TATP_", Sys.Date(), ".jpeg", sep=""), width=5, height=9, units="in")

multiplot_approach_1TATP<-ggplot(subset(IDR_Data, asDate>="2020-08-12"&  flushprepost=="0"), aes(x=agedays, y=TAtp_RLU, fill=Residual)) + scale_x_continuous(breaks = scales::breaks_extended(10))+
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,nrow=1)+ theme(strip.text.x = element_text(size = 14))   + xlab("Age(days)")+ ylab("Total ATP (RLU)") +scale_fill_manual(name="", values=c(  "chocolate1", "orangered4")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))  + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(subtitle= paste("Post Flush Samples"))) + theme(plot.title = element_text(size=12))
multiplot_approach_1TATP
ggsave(multiplot_approach_1TATP, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/multiplot_approach_1TATP_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")

#PHYSIOLOGICAL CHRACTERISTICS OF WATER#####

Watercolor=c("#636363","#ffffcc")
#pstag_DO_leg_all<-ggplot(subset(IDR_Data, !is.na(stagff) & flushprepost =="1"&stagff=="0" |flushprepost=="2" ), aes(x=as.Date(mydates), y=DO_mgL, fill=factor(flush.name))) + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=15, alpha=0.5) + 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4) +coord_cartesian(ylim=c(0,12))+ xlab("Dates of operation")+ ylab("DO mg/L)") +scale_fill_manual(name="", values=Watercolor) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(10))   + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
#pstag_DO_leg_all

pstag_PH_leg_all<-ggplot(subset(IDR_Data,flushprepost =="0"| flushprepost=="2" ), aes(x=as.Date(mydates), y=pH_all, fill=factor(InfEff.name))) +annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))   +coord_cartesian(ylim=c(7.5, 8.5))+ xlab("Dates of operation")+ ylab("PH") +scale_fill_manual(name="", values=Watercolor) +  guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(5))   + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
pstag_PH_leg_all

my_comparisonsphase <- list(c("Burn", "Post-Burn"), c("Burn", "Pre-Burn"),c("Post-Burn", "Pre-Burn"))
my_comparisonsPFFFSTAG <- list(c("Firstflow", "Post Flush"), c("Post Flush", "Stagnation"), c("Stagnation", "Firstflow"))
my_comparisonsOVwk <- list(c("Weekend", "Overnight"))
pstagFFPF_pH_leg_box<-ggplot(subset(IDR_Data,flushprepost =="0"|  flushprepost=="2"), aes(x=as.factor(InfEff.name), y=pH_all, fill=factor(InfEff.name))) + 
  geom_boxplot(outlier.colour = "black",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_classic2() + facet_wrap(.~Loc.f, ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("")+ ylab("pH") +guides(fill = guide_legend(reverse=TRUE))+scale_fill_manual(name="", values=Watercolor) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16))  + theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 20))+coord_cartesian(ylim=c(7.5,8.5))+
  ggtitle(paste(subtitle= paste("","\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagFFPF_pH_leg_box
pstagffPFbox_stat_pH_leg_all<-pstagFFPF_pH_leg_box + stat_compare_means(aes(group=InfEff.name,label = ..p.signif..,ref.group = "Filtered"), color="red", size=4, hjust=0.5,label.y = c(1.4, 1.7, 1.8))
pstagffPFbox_stat_pH_leg_all
ggsave(pstagffPFbox_stat_pH_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagffPFbox_stat_pH_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagFFPF_pH_leg_box<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"&  !is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"&flushprepost=="2"), aes(x=factor(flush.name), y=pH_all, fill=factor(flush.name))) +  guides(fill = guide_legend(reverse=TRUE))+
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5) + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 15)) + xlab("")+ ylab("PH") +scale_fill_manual(name="", values=Pre_post4col) + theme_classic()+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")) + coord_cartesian(ylim=c(7,8.5)) + theme(axis.text.x = element_text(size = 15,angle = 45,size=10, hjust = 1))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=56, Filter2=56, Filter3=56"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagFFPF_pH_leg_box
ggsave(pstagpfffbox_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpfffbox_NO2_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
pstagFFPF_pH_statall<-pstagFFPF_pH_leg_box+ stat_compare_means(comparisons =my_comparisonsPFFFSTAG ,label = "p.format", size=4, hjust=0.5,label.y = c(8.1, 8.2, 8.4))
pstagFFPF_pH_statall
pstagFFPF_pH_stat_ <- pstagFFPF_pH_statall + stat_compare_means(label.y = c(8.5),size=4)
pstagFFPF_pH_stat_ 
ggsave(pstagFFPF_pH_stat_, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_pH_stat_", Sys.Date(), ".jpeg", sep=""), width=8, height=8, units="in")

pstagFFPF_Temp_leg_box<-ggplot(subset(IDR_Data, mydates >= "2020-08-28"& !is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"&flushprepost=="2"), aes(x=reorder(factor(flush.name),stagffpf), y=tempC_all, fill=factor(flush.name))) +  guides(fill = guide_legend(reverse=TRUE))+
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5) + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 15)) + xlab("")+ ylab("Tempreture C") +scale_fill_manual(name="", values=Pre_post4col) + theme_classic()+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")) + coord_cartesian(ylim=c(20,30)) + theme(axis.text.x = element_text(angle = 45,size=12, hjust = 1))+
  ggtitle(paste(subtitle= paste("\n N: Filter1=165, Filter2=165, Filter3=165"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagFFPF_Temp_leg_box
ggsave(pstagFFPF_Temp_leg_box, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpfffbox_NO2_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
pstagFFPF_Temp_statall<-pstagFFPF_Temp_leg_box+ stat_compare_means(comparisons =my_comparisonsPFFFSTAG ,label = "p.format", size=5, hjust=0.5,label.y = c(26, 27, 28))
pstagFFPF_Temp_statall
pstagFFPF_Temp_stat_ <- pstagFFPF_Temp_statall + stat_compare_means(label.y = c(29),size=4)
pstagFFPF_Temp_stat_ 
ggsave(pstagFFPF_Temp_stat_, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_Temp_stat_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")

pstagFFPF_DO_leg_box<-ggplot(subset(IDR_Data, mydates >= "2020-08-28"& !is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"&flushprepost=="2"),aes(x=reorder(factor(flush.name),stagffpf), y=DO_mgL, fill=factor(flush.name))) +  guides(fill = guide_legend(reverse=TRUE))+
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5) + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 15)) + xlab("")+ ylab("DO mg/L") +scale_fill_manual(name="", values=Pre_post4col) + theme_classic()+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")) + coord_cartesian(ylim=c(2,11.5)) + theme(axis.text.x = element_text(size = 14,,angle = 45, hjust = 1))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=165, Filter2=165, Filter3=165"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagFFPF_DO_leg_box
ggsave(pstagFFPF_DO_leg_box, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpfffbox_NO2_leg_all_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")
pstagFFPF_DO_statall<-pstagFFPF_DO_leg_box+ stat_compare_means(comparisons =my_comparisonsPFFFSTAG ,label = "p.format", size=4, hjust=0.5,label.y = c(7.5, 8.3, 9.5))
pstagFFPF_DO_statall
pstagFFPF_DO_stat_ <- pstagFFPF_DO_statall + stat_compare_means(label.y = c(11),size=4)
pstagFFPF_DO_stat_ 
ggsave(pstagFFPF_DO_stat_, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_DO_stat_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")

ptapPF_TCL_all<-ggplot(subset(IDR_Data,  !is.na(stagff) & flushprepost =="1" |flushprepost=="2" ), aes(x=as.Date(mydates), y=TCl2_mgL_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 14))  + theme(strip.text.x = element_text(size = 15, angle = 0)) + xlab("Dates of operation")+ ylab("Total Chlorine (mg/L)") +scale_fill_manual(name="",values=Pre_post4col) + guides(fill = guide_legend(reverse=TRUE))+ 
  theme(axis.text = element_text(size = 12),axis.title.x=element_text(size = 13),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))+ scale_x_date(breaks = pretty_breaks(5))  + coord_cartesian(ylim=c(0,3)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  theme(plot.title = element_text(size=12))
ptapPF_TCL_all
pstagFFPF_TCL_leg_box<-ggplot(subset(IDR_Data, mydates >= "2020-08-28"& !is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"&flushprepost=="2"), aes(x=reorder(factor(flush.name),stagffpf), y=TCl2_mgL_all, fill=factor(flush.name))) +  guides(fill = guide_legend(reverse=TRUE))+
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5) + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 15)) + xlab("")+ ylab("Total Chlorine (mg/L)") +scale_fill_manual(name="", values=Pre_post4col) + theme_classic()+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")) + coord_cartesian(ylim=c(0,2.5)) + theme(axis.text.x = element_text(size = 14,,angle = 15, hjust = 1))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=165, Filter2=165, Filter3=165"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagFFPF_TCL_leg_box
ggsave(pstagFFPF_TCL_leg_box, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpfffbox_NO2_leg_all_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")
pstagFFPF_TC_statall<-pstagFFPF_TCL_leg_box+ stat_compare_means(comparisons =my_comparisonsPFFFSTAG ,label = "p.adj", size=4, hjust=0.5,label.y = c(1.4, 1.7, 2))
pstagFFPF_TC_statall
pstagFFPF_TC_ <- pstagFFPF_TC_statall + stat_compare_means(label.y = c(2.3),size=4)
pstagFFPF_TC_
ggsave(pstagFFPF_TC_, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_TC_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")

pstag_TClmultiplot <-ggarrange( ptapPF_TCL_all,pstagFFPF_TC_,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B"),font.label = list(size = 14, face = "bold") )
pstag_TClmultiplot
ggsave(pstag_TClmultiplot, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_TClmultiplot_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")

pN_NN_PH_leg_all<-ggplot(subset(IDR_Data,flushprepost =="0"| !is.na(stagff) & flushprepost =="1" &stagff=="0" ), aes(x=as.Date(mydates), y=pH_all, fill=factor(N_NN.f))) + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=15, alpha=0.5) + 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))   +coord_cartesian(ylim=c(7, 8.5))+ xlab("Dates of operation")+ ylab("PH") +scale_fill_manual(name="", values=c("#ffffcc","#78c679","#636363")) +  guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(5))   + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
pN_NN_PH_leg_all
ggsave(pN_NN_PH_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pN_NN_PH_leg_all_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

pN_NN_PHbox_leg_all <-ggplot(subset(IDR_Data,!is.na(stagff) & flushprepost =="1" &stagff=="0"  ), aes(x=(factor(N_NN.f)), y=pH_all, fill=factor(N_NN.f))) + 
  geom_boxplot(outlier.colour = "black",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_bw() +  coord_cartesian(ylim=c(7.4, 8.2))+ facet_wrap(Loc.f~.,ncol=4) + ylab("pH_all") +xlab("Pre/Post Flush") +
  scale_fill_manual(name="Pre/Post", values=Pre_post) + theme(strip.text.x = element_text(size = 14)) + 
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16)) + theme(legend.title = element_text(size=14)) + theme(legend.position="bottom") +
  ggtitle(paste(subtitle= paste("","\nN: Filter1=165, Filter2=165, Filter3=165"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))+theme(axis.text.x = element_text(angle = 45, hjust = 1))
pN_NN_PHbox_leg_all
ggsave(pN_NN_PHbox_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/report Plots/stag_FFplot/pN_NN_PHbox_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
pN_NNbox_stat_pH_leg_all<-pN_NN_PHbox_leg_all + stat_compare_means(aes(group=flush.name),ref.group = "Stagnation", label = "p.signif",symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "")),label.y=8.1, color="red", size=6, hjust=0.5)+
  stat_compare_means(aes(group=flush.name), label = "p.format",ref.group = "Nitrifying",label.y=8.2,  size=5, hjust=0.5)
  pN_NNbox_stat_pH_leg_all
ggsave(pN_NNbox_stat_pH_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/report Plots/stag_FFplot/pN_NNbox_stat_pH_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

#########filtered and unfiltred wate charateristics#######
pstag_PH_leg_all<-ggplot(subset(IDR_Data,flushprepost =="0"| flushprepost=="2" ), aes(x=as.Date(mydates), y=pH_all, fill=factor(InfEff.name))) +annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))   +coord_cartesian(ylim=c(7.5, 8.5))+ xlab("Dates of operation")+ ylab("PH") +scale_fill_manual(name="", values=c("#ffffcc","#636363")) +  guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(5))   + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
pstag_PH_leg_all

pstag_Temp_leg_all<-ggplot(subset(IDR_Data,flushprepost =="0"| flushprepost=="2" ), aes(x=as.Date(mydates), y=tempC_all, fill=factor(FtUF.f))) + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=10, ymax=30, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=10, ymax=30, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=10, ymax=30, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=10, ymax=30, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))   +coord_cartesian(ylim=c(18,29))+ xlab("Dates of operation")+ ylab("Tempreture") +scale_fill_manual(name="", values=Watercolor) + #guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(5))   + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
pstag_Temp_leg_all

pstag_DO_l_all<-ggplot(subset(IDR_Data,flushprepost =="0"| flushprepost=="2" ), aes(x=as.Date(mydates), y=DO_mgL, fill=factor(FtUF.f))) + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=30, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=30, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=30, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=30, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))   +coord_cartesian(ylim=c(0,12))+ xlab("Dates of operation")+ ylab("DO (mg/L)") +scale_fill_manual(name="", values=Watercolor) + #guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(5))   + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
pstag_DO_l_all

pstag_PH_Temp <-ggarrange( pstag_PH_leg_all,pstag_Temp_leg_all,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstag_PH_Temp
ggsave(pstag_PH_Temp, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_PH_Temp_", Sys.Date(), ".jpeg", sep=""), width=8, height=9, units="in")

#pstag_PH_Temp_DO <-ggarrange( pstag_PH_leg_all,pstag_Temp_leg_all,pstag_DO_l_all,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
#pstag_PH_Temp_DO 
#ggsave(pstag_PH_Temp_DO , filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_PH_Temp_DO _", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

#######DISINFFECTION CHARACTERISTICS######
pstag_freeCL_leg_all<-ggplot(subset(IDR_Data,flushprepost=="0"|  flushprepost==2 ), aes(x=as.Date(mydates), y=FCl2, fill=factor(FtUF.f)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Dates of operation")+ ylab("Free Chlorine\n (mg/L)") +scale_fill_manual(name="", values=Watercolor) + #guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  theme(plot.title = element_text(size=12))
pstag_freeCL_leg_all

pstag_TCL_leg_all<-ggplot(subset(IDR_Data,flushprepost=="0"|  flushprepost==2 ), aes(x=as.Date(mydates), y=TCl2_mgL_all, fill=factor(FtUF.f)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Dates of operation")+ ylab("Total Chlorine\n (mg/L)") +scale_fill_manual(name="", values=Watercolor) +# guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(5))  + coord_cartesian(ylim=c(0,5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  theme(plot.title = element_text(size=12))
pstag_TCL_leg_all

pstag_Mono_leg_all<-ggplot(subset(IDR_Data, flushprepost=="0"| flushprepost=="2" & Loc!="0" ), aes(x=as.Date(mydates), y=mono_mgL_all, fill=factor(FtUF.f)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() +  facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Dates of operation")+ ylab("Monochloramine\n (mg/L-CL)") +scale_fill_manual(name="", values=Watercolor) + # guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(5))  + coord_cartesian(ylim=c(0,5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
pstag_Mono_leg_all
pstag_TCL_Mono <-ggarrange(pstag_Mono_leg_all,pstag_TCL_leg_all,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstag_TCL_Mono
ggsave(pstag_TCL_Mono, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_TCL_Mono_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

ptapPFFF_Mono_all<-ggplot(subset(IDR_Data,  !is.na(stagff) & flushprepost =="1" |flushprepost=="2" ), aes(x=as.Date(mydates), y=mono_mgL_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=2.5, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 12))  + theme(strip.text.x = element_text(size = 15, angle = 0)) + xlab("Dates of operation")+ ylab("Monochloramine\n (mg/L-CL)") +scale_fill_manual(name="",values=Pre_post4col) + guides(fill = guide_legend(reverse=TRUE))+ 
  theme(axis.text = element_text(size = 12),axis.title.x=element_text(size = 13),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))+ scale_x_date(breaks = pretty_breaks(5))  + coord_cartesian(ylim=c(0,2)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  theme(plot.title = element_text(size=12))
ptapPFFF_Mono_all

pstagFFPF_Mono_leg_box<-ggplot(subset(IDR_Data, mydates >= "2020-08-28"& !is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"&flushprepost=="2"), aes(x=reorder(factor(flush.name),stagffpf), y=mono_mgL_all, fill=factor(flush.name))) +  guides(fill = guide_legend(reverse=TRUE))+
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5) + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 15)) + xlab("")+ ylab("Monochloramine\n (mg/L-CL)") +scale_fill_manual(name="", values=Pre_post4col) + theme_classic()+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")) + coord_cartesian(ylim=c(0,1.8)) + theme(axis.text.x = element_text(size = 14,,angle = 15, hjust = 1))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=165, Filter2=165, Filter3=165"))) + theme(plot.title = element_text(size=12))#+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagFFPF_Mono_leg_box
ggsave(pstagFFPF_Mono_leg_box, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_Mono_leg_box_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")
pstagFFPF_Mono_statall<-pstagFFPF_Mono_leg_box+ stat_compare_means(comparisons =my_comparisonsPFFFSTAG ,label = "p.signif", size=4, hjust=0.5,label.y = c(1, 1.2, 1.5))
pstagFFPF_Mono_statall
pstagFFPF_Mono_ <-pstagFFPF_Mono_statall + stat_compare_means(label.y = c(1.7),size=3)
pstagFFPF_Mono_
ggsave(pstagFFPF_Mono_, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_Mono_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")

pstagFFPF_Monomultiplot <-ggarrange(ptapPFFF_Mono_all,pstagFFPF_Mono_,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B"),font.label = list(size = 14, face = "bold") )
pstagFFPF_Monomultiplot
ggsave(pstagFFPF_Monomultiplot, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_Monomultiplot_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")

#######Nitrification######
pstag_Free_NH3_leg2<-ggplot(subset(IDR_Data,  flushprepost=="1" & stagff=="0" |flushprepost=="2"), aes(x=as.Date(mydates), y=Free_NH3_N, fill=factor(prepost.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() +  facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Dates of operation")+ ylab("Free-NH3 (mg/L)") +scale_fill_manual(name="", values=c("#78c679","#ffffcc")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,1.25)) + theme(axis.text.x = element_text(angle = 15, hjust = 1))+
  theme(plot.title = element_text(size=12))
pstag_Free_NH3_leg2

pstagboxphase_Free_NH3_leg_all<-ggplot(subset(IDR_Data, mydates >= "2020-08-28"& flushprepost=="1" & stagff=="0" |mydates >= "2020-08-28"&flushprepost=="2"), aes(x=reorder(factor(flush.name),stagffpf), y=Free_NH3_N, fill=factor(prepost.name))) +  scale_fill_manual(name="", values=c("#ffffcc","#78c679")) +
  geom_boxplot(outlier.colour = "black",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_bw() +  facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 14))  + ylab("Free-NH3 (mg/L)") +xlab("") + facet_grid(~Loc.f) +coord_cartesian(ylim=c(0,1.2))+
  theme(strip.text.x = element_text(size = 14)) +
  theme(axis.title=element_text(size=12)) + theme(legend.title = element_text(size=14)) + theme(legend.position="none") +
  ggtitle(paste(subtitle= paste("","\nN: Filter1=165, Filter2=165, Filter3=165"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))+theme(axis.text.x = element_text(angle = 45, hjust = 1))
pstagboxphase_Free_NH3_leg_all
#ggsave(pstagbox_TCL_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagbox_TCL_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
pstagboxphase_stat_Free_NH3_leg_all<-pstagboxphase_Free_NH3_leg_all+ stat_compare_means(comparisons =my_comparisonsPFSTAG,label = "p.format", size=4, hjust=0.5,label.y = c(1))
pstagboxphase_stat_Free_NH3_leg_all
ggsave(pstagboxphase_stat_Free_NH3_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagboxphase_stat_Free_NH3_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

my_comparisonsphase <- list(c("Burn", "Post-Burn"), c("Burn", "Pre-Burn"),c("Post-Burn", "Pre-Burn"))
my_comparisonsPFSTAG <- list(c("Post Flush", "Stagnation"))
my_comparisonsPFFFSTAG <- list(c("Stagnation", "Firstflow"),c("Firstflow", "Post Flush"), c("Post Flush", "Stagnation") )
my_comparisonsOVwk <- list(c("Weekend", "Overnight"))

pstagFFPF_TCL_leg_box<-ggplot(subset(IDR_Data, mydates >= "2020-08-28"&!is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"&flushprepost=="2"),aes(x=reorder(factor(flush.name),stagffpf), y=TCl2_mgL_all, fill=factor(flush.name))) + 
  geom_boxplot(outlier.colour = "black",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_classic2() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("")+ ylab("Total Chlorine (mg/L)") +guides(fill = guide_legend(reverse=TRUE))+scale_fill_manual(name="", values=Pre_post4col) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16))  + theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 12))+coord_cartesian(ylim=c(0,2))+
  ggtitle(paste(subtitle= paste("","\nN: Filter1=165, Filter2=165, Filter3=165"))) + theme(plot.title = element_text(size=13))
pstagFFPF_TCL_leg_box
pstagffPFbox_stat_TCL_leg_all<-pstagFFPF_TCL_leg_box + stat_compare_means(comparisons =my_comparisonsPFFFSTAG ,aes(label = ..p.format..),  size=4, hjust=0.5,label.y = c(1.4, 1.7, 1.8))
pstagffPFbox_stat_TCL_leg_all
ggsave(pstagffPFbox_stat_TCL_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagffPFbox_stat_TCL_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
 
pstagFFPF_DO_leg_box<-ggplot(subset(IDR_Data, mydates >= "2020-08-28"&!is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"&flushprepost=="2"), aes(x=reorder(factor(flush.name),stagffpf), y=DO_mgL, fill=factor(flush.name))) + 
  geom_boxplot(outlier.colour = "black",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_classic2() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("")+ ylab("DO mg/L-Cl") +guides(fill = guide_legend(reverse=TRUE))+scale_fill_manual(name="", values=Pre_post4col) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16))  + theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 12))+coord_cartesian(ylim=c(2,14))+
  ggtitle(paste(subtitle= paste("N:Filter1=56, Filter2=56 Filter3=56"))) + theme(plot.title = element_text(size=13))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagFFPF_DO_leg_box
pstagffPFbox_stat_DO_leg_all<-pstagFFPF_DO_leg_box+ stat_compare_means(comparisons =my_comparisonsPFFFSTAG , aes(label = ..p.format..), size=4, hjust=0.5,label.y = c(11, 12, 13))
pstagffPFbox_stat_DO_leg_all
ggsave(pstagffPFbox_stat_DO_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagffPFbox_stat_DO_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagFFPF_Temp_leg_box<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"& !is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"&flushprepost=="2"), aes(x=reorder(factor(flush.name),stagffpf), y=tempC_all, fill=factor(flush.name))) + 
  geom_boxplot(outlier.colour = "black",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5)+theme_classic2() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("")+ ylab("Tempreture C") +guides(fill = guide_legend(reverse=TRUE))+scale_fill_manual(name="", values=Pre_post4col) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16))  + theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 12))+coord_cartesian(ylim=c(20,30))+
  ggtitle(paste(subtitle= paste("","\nN: Filter1=165, Filter2=165, Filter3=165"))) + theme(plot.title = element_text(size=15))
pstagFFPF_Temp_leg_box
pstagffPFbox_stat_Temp_leg_all<-pstagFFPF_Temp_leg_box+ stat_compare_means(comparisons =my_comparisonsPFFFSTAG ,label = "p.format", size=4, hjust=0.5,label.y = c(27, 28, 29))
pstagffPFbox_stat_Temp_leg_all
ggsave(pstagffPFbox_stat_Temp_leg_all, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagffPFbox_stat_Temp_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pFFpf_NO2_leg_all<-ggplot(subset(IDR_Data, flushprepost =="1"&stagff=="0" |flushprepost=="2"  ), aes(x=as.Date(mydates), y=nitrite_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Dates of operation: Month")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=c("#ffffcc","#78c679")) +guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,3)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size=12))
pFFpf_NO2_leg_all

pstag_NO3_leg_all<-ggplot(subset(IDR_Data,flushprepost==1 & stagff=="0"|  flushprepost==2), aes(x=as.Date(mydates), y=nitrate_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Dates of operation (Month)")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=c("#ffffcc","#78c679")) +guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,7)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
pstag_NO3_leg_all 

pstag_NH3_NO2_NO3 <-ggarrange(pstag_Free_NH3_leg2, pFFpf_NO2_leg_all,pstag_NO3_leg_all,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstag_NH3_NO2_NO3 
ggsave(pstag_NH3_NO2_NO3, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NH3_NO2_NO3_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

########Biological##########
pstag_DO_leg_all<-ggplot(subset(IDR_Data, flushprepost==0 |flushprepost=="2" ), aes(x=as.Date(mydates), y=DO_mgL, fill=factor(InfEff.name))) +annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=15, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=15, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=15, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=15, alpha=0.6)+  
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))   +coord_cartesian(ylim=c(1.5,12))+ xlab("Dates of operation (Month)")+ ylab("DO (mg/L)") +scale_fill_manual(name="", values=c("#ffffcc","#636363")) +  guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(5))   + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
pstag_DO_leg_all

pstagPF_TAtp_<-ggplot(subset(IDR_Data,   flushprepost==0 |flushprepost==2 ), aes(x=as.Date(mydates), y=TAtp_RLU, fill=factor(InfEff.name))) + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=0, ymax=10^(5), alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=0, ymax=10^(5), alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol = 4) + theme(strip.text.x = element_text(size = 14))   + xlab("Dates of operation (Month)")+ ylab("Total ATP (RLU)")+scale_x_date(breaks = pretty_breaks(5)) +scale_fill_manual(name="",  values=c("#ffffcc","#636363"))  +  guides(fill = guide_legend(reverse=TRUE))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x)))+coord_cartesian(ylim=c(10^(0),10^(3))) + theme(legend.position="bottom")  + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + theme(plot.title = element_text(size=12))
pstagPF_TAtp_
ggsave(pstagPF_TAtp_, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagPF_TAtp__", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")


pstag_DO_TATP <-ggarrange(pstag_DO_leg_all, pstagPF_TAtp_,ncol = 1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstag_DO_TATP
ggsave(pstag_DO_TATP, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_DO_TATP_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

ptapstag_DO_leg_days<-ggplot(subset(IDR_Data, flushprepost==0| !is.na(stagff) & flushprepost =="1"&stagff=="0"  ), aes(x=ageweeks, y=DO_mgL)) + scale_x_continuous(breaks = scales::breaks_extended(12))+
  geom_point(aes(shape=Residual,fill=N_NN.f),size=2.5)+scale_shape_manual(values = c(21,23))+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))   +coord_cartesian(ylim=c(0,12))+ xlab("Filter Age(Weeks)")+ ylab("DO (mg/L)") +scale_fill_manual(name="", values=c("red", "#99d8c9")) + guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16))  + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
ptapstag_DO_leg_days

ptapstag_NH3_leg_days<-ggplot(subset(IDR_Data, flushprepost==0| !is.na(stagff) & flushprepost =="1"&stagff=="0"), aes(x=ageweeks, y=Free_NH3_N, fill=factor(N_NN.f))) + scale_x_continuous(breaks = scales::breaks_extended(12))+
  geom_point(aes(shape=Residual),size=2.5)+scale_shape_manual(values = c(21,23))+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Filter Age(Weeks)")+ ylab("FreeAmmonia (mg/L-N)") +scale_fill_manual(name="", values=c("red", "#99d8c9")) + guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))) +theme(legend.text = element_text(size = 16)) + coord_cartesian(ylim=c(0,2)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size=12))
ptapstag_NH3_leg_days 

ptapstag_NO2_leg_days<-ggplot(subset(IDR_Data, flushprepost==0| !is.na(stagff) & flushprepost =="1"&stagff=="0"), aes(x=ageweeks, y=nitrite_all, fill=factor(N_NN.f))) + scale_x_continuous(breaks = scales::breaks_extended(12))+ geom_hline(yintercept=(0.025), color="red")+#geom_hline(yintercept=(0.025), color="blue")+
  geom_point(aes(shape=Residual),size=2.5)+scale_shape_manual(values = c(21,23))+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))  + xlab("Filter Age(Weeks)")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=c("red", "#99d8c9")) + guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")) +theme(legend.text = element_text(size = 16)) + coord_cartesian(ylim=c(0,0.15)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size=12))
 ptapstag_NO2_leg_days
 ptapstag_DO_NO2 <-ggarrange(ptapstag_DO_leg_days,ptapstag_NH3_leg_days, ptapstag_NO2_leg_days,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
 ptapstag_DO_NO2
ggsave( ptapstag_DO_NO2 , filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/ ptapstag_DO_NO2 ", Sys.Date(), ".jpeg", sep=""), width=11, height=8, units="in")

#multiplot_approach_1TATP<-ggplot(subset(IDR_Data, asDate>="2020-08-12"&  flushprepost=="2"), aes(x=agedays, y=TAtp_RLU, fill=factor(N_NN.f))) + scale_x_continuous(breaks = scales::breaks_extended(10))+
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,nrow=1)+ theme(strip.text.x = element_text(size = 14))   + xlab("Age(days)")+ ylab("Total ATP (RLU)") +scale_fill_manual(name="", values=c(  "chocolate1", "orangered4")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))  + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(subtitle= paste("Post Flush Samples"))) + theme(plot.title = element_text(size=12))
#multiplot_approach_1TATP
#ggsave(multiplot_approach_1TATP, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/multiplot_approach_1TATP_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")


######multiplot NOX and TATP#####
pstagFFPF_Free_NH3_NOX<-ggplot(subset(IDR_Data,  flushprepost =="1"&stagff=="0" |flushprepost=="2"  ), aes(x=as.Date(mydates), y=Free_NH3_N, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Dates of operation")+ ylab("Free_NH3 (mg/L-N)") +scale_fill_manual(name="", values=c("#ffffcc","#78c679"))  +guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ scale_x_date(breaks = pretty_breaks(10)) +coord_cartesian(ylim=c(0,1.7)) +
  theme(plot.title = element_text(size=12))
pstagFFPF_Free_NH3_NOX
ggsave(pstagFFPF_Free_NH3_NOX, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_Free_NH3_NOX_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstag_NOx_NO2<-ggplot(subset(IDR_Data,  flushprepost =="1"& stagff=="0" |flushprepost=="2"  ), aes(x=as.Date(mydates), y=nitrite_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4) + xlab("Dates of operation")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="", values=c("#ffffcc","#78c679")) + guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom"))+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,3)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size=12))
pstag_NOx_NO2
ggsave(pstag_NOx_NO2, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NOx_NO2_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstag_NOx_NO3<-ggplot(subset(IDR_Data, flushprepost==1 & stagff=="0"|  flushprepost==2), aes(x=as.Date(mydates), y=nitrate_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4) + theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=c("#ffffcc","#78c679")) + guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,7)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
pstag_NOx_NO3 
ggsave(pstag_NOx_NO3 , filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NOx_NO3 _", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")


pstagPF_TAtp2<-ggplot(subset(IDR_Data,flushprepost ==1 & stagff=="0"|flushprepost==2 ), aes(x=as.Date(mydates), y=TAtp_RLU, fill=factor(flush.name))) + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=0, ymax=10^(5), alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=0, ymax=10^(5), alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol = 4)+ theme(strip.text.x = element_text(size = 14))    + xlab("Dates of operation")+ ylab("Total ATP (RLU)")+scale_x_date(breaks = pretty_breaks(10)) +scale_fill_manual(name="",  values=c("#ffffcc","#78c679","#636363"))  + guides(fill = guide_legend(reverse=TRUE))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) + theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))  + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
pstagPF_TAtp2
ggsave(pstagPF_TAtp2, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagPF_TAtp2_", Sys.Date(), ".jpeg", sep=""), width=9, height=6, units="in")

pstag_NOx_TATP <-ggarrange(pstagFFPF_Free_NH3_NOX,pstag_NOx_NO2, pstag_NOx_NO3,ncol = 1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstag_NOx_TATP
ggsave(pstag_NOx_TATP, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NOx_TATP_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

#########ATP AND NH3########


pstagPFFF_tempC_all_leg_2<-ggplot(subset(IDR_Data, !is.na(stagff) & flushprepost =="1" |flushprepost=="2"), aes(x=as.Date(mydates), y=tempC_all, fill=factor(flush.name)))+annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=30, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=30, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=30, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=30, alpha=0.6)+  
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4) + theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Tempreture C") +scale_fill_manual(name="", values=Pre_post4col) +  guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.x=element_text(size = 15),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))+ scale_x_date(breaks = pretty_breaks(5))  + coord_cartesian(ylim=c(20,28)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size=12))+annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=40, alpha=0.5)  
pstagPFFF_tempC_all_leg_2

pstagFFPF_Temp_leg_box<-ggplot(subset(IDR_Data, mydates >= "2020-08-28"& !is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"&flushprepost=="2"), aes(x=reorder(factor(flush.name),stagffpf), y=tempC_all, fill=factor(flush.name))) + guides(fill = guide_legend(reverse=TRUE))+
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5) + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 15)) + xlab("")+ ylab("Tempreture") +scale_fill_manual(name="", values=Pre_post4col) + theme_classic()+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")) + coord_cartesian(ylim=c(20,30)) + theme(axis.text.x = element_text(angle = 15,size=12, hjust = 1))+
  ggtitle(paste(subtitle= paste("\n N: Filter1=243, Filter2=182, Filter3=165"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagFFPF_Temp_leg_box
ggsave(pstagFFPF_Temp_leg_box, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpfffbox_NO2_leg_all_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
pstagFFPF_Temp_statall<-pstagFFPF_Temp_leg_box+ stat_compare_means(comparisons =my_comparisonsPFFFSTAG ,label ="p.format" ,hide.ns = T, size=4, hjust=0.5,label.y = c(26, 27, 28))
pstagFFPF_Temp_statall
pstagFFPF_Temp_stat_ <- pstagFFPF_Temp_statall + stat_compare_means(label.y = c(29),size=4)
pstagFFPF_Temp_stat_ 
ggsave(pstagFFPF_Temp_stat_, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_Temp_stat_", Sys.Date(), ".jpeg", sep=""), width=8, height=8, units="in")
pstagPFFFMULTIPLOT_Temp<-ggarrange(pstagPFFF_tempC_all_leg_2,pstagFFPF_Temp_stat_,ncol = 1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstagPFFFMULTIPLOT_Temp
ggsave(pstagPFFFMULTIPLOT_Temp, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagPFFFMULTIPLOT_Temp_", Sys.Date(), ".jpeg", sep=""), width=8, height=8, units="in")

pstag_pH_all_leg_2<-ggplot(subset(IDR_Data, !is.na(stagff) & flushprepost =="1" |flushprepost=="2"), aes(x=as.Date(mydates), y=pH_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4) + theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("PH") +scale_fill_manual(name="", values=Pre_post4col) +  guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.x=element_text(size = 14,axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))+ scale_x_date(breaks = pretty_breaks(5))  + coord_cartesian(ylim=c(7,8.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +#geom_hline(yintercept=(3.624), color="red")
  theme(plot.title = element_text(size=12))
pstag_pH_all_leg_2

pstagFFPF_pH_leg_box<-ggplot(subset(IDR_Data, mydates >= "2020-08-28"& !is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"&flushprepost=="2"), aes(x=reorder(factor(flush.name),stagffpf), y=pH_all, fill=factor(flush.name))) +  guides(fill = guide_legend(reverse=TRUE))+
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5) + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 12)) + xlab("")+ ylab("PH") +scale_fill_manual(name="", values=Pre_post4col) + theme_classic()+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")) + coord_cartesian(ylim=c(7.3,9.2)) + theme(axis.text.x = element_text(size = 14,,angle =15, hjust = 1))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=165, Filter2=165, Filter3=165"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagFFPF_pH_leg_box
ggsave(pstagFFPF_pH_leg_box, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_pH_leg_box_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")
pstagFFPF_pH_statall<-pstagFFPF_pH_leg_box+ stat_compare_means(comparisons =my_comparisonsPFFFSTAG ,label = "p.format", size=4, hjust=0.5,label.y = c(8.5, 8.7, 9))
pstagFFPF_pH_statall
pstagFFPF_pH_stat_ <- pstagFFPF_pH_statall 
pstagFFPF_pH_stat_ 
ggsave(pstagFFPF_pH_stat_, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_pH_stat_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagPFFFMULTIPLOT_pH<-ggarrange(pstag_pH_all_leg_2,pstagFFPF_pH_stat_ ,ncol = 1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstagPFFFMULTIPLOT_pH
ggsave(pstagPFFFMULTIPLOT_pH, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagPFFFMULTIPLOT_pH_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")


pstag_DO_all_leg_2<-ggplot(subset(IDR_Data, !is.na(stagff) & flushprepost =="1" |flushprepost=="2"), aes(x=as.Date(mydates), y=DO_mgL, fill=factor(flush.name))) + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=15, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=15, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=15, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=15, alpha=0.6)+  
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4) + theme(strip.text.x = element_text(size = 12))  + xlab("Dates of operation")+ ylab("DO_mgL") +scale_fill_manual(name="", values=Pre_post4col,labels=c("Stagnation", "First Flow", "Post Flush")) + # guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.x=element_text(size = 14,axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))+ scale_x_date(breaks = pretty_breaks(5))  + coord_cartesian(ylim=c(0,12)) + theme(axis.text.x = element_text(angle = 15, hjust = 1)) +#geom_hline(yintercept=(3.624), color="red")
  theme(plot.title = element_text(size=12))
pstag_DO_all_leg_2

pstagFFPF_DO_leg_box<-ggplot(subset(IDR_Data,mydates >= "2020-08-28"&  !is.na(stagff) & flushprepost =="1" |mydates >= "2020-08-28"&flushprepost=="2"), aes(x=reorder(factor(flush.name),stagffpf), y=DO_mgL, fill=factor(flush.name))) + guides(fill = guide_legend(reverse=TRUE))+
  geom_boxplot(outlier.colour = "orange",outlier.shape = 19,outlier.size = 2,outlier.alpha = 0.5) + facet_wrap(Loc.f~., ncol=3)+ theme(strip.text.x = element_text(size = 12)) + xlab("")+ ylab("DO mg/L") +scale_fill_manual(name="", values=c("#c2e699","#ffffcc","#78c679"),labels=c("First Flow", "Post Flush","Stagnation" )) + theme_classic()+
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom")) + coord_cartesian(ylim=c(2,12)) + theme(axis.text.x = element_text(size = 14,,angle = 15, hjust = 1))+
  ggtitle(paste(subtitle= paste("\nN: Filter1=165, Filter2=165, Filter3=165"))) + theme(plot.title = element_text(size=12))+labs(caption = "\nStatistical significance calculated by MWW test \n****: p <= 0.0001: highly significant, *: p <= 0.05: significant,p > 0.05: not significant")+theme(plot.caption=element_text(size=11,hjust = 0.5))
pstagFFPF_DO_leg_box
ggsave(pstagFFPF_DO_leg_box, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagpfffbox_NO2_leg_all_", Sys.Date(), ".jpeg", sep=""), width=6, height=9, units="in")

pstagFFPF_DO_statall<-pstagFFPF_DO_leg_box+ stat_compare_means(comparisons =my_comparisonsPFFFSTAG ,label = "p.format", size=4, hjust=0.5,label.y = c(8, 9, 10))
pstagFFPF_DO_statall
pstagFFPF_DO_stat_ <- pstagFFPF_DO_statall + stat_compare_means(label.y = c(11),size=4)
pstagFFPF_DO_stat_ 
ggsave(pstagFFPF_DO_stat_, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagFFPF_DO_stat_", Sys.Date(), ".jpeg", sep=""), width=8, height=9, units="in")

pstagPFFFMULTIPLOT_DO<-ggarrange(pstag_DO_all_leg_2,pstagFFPF_DO_stat_ ,ncol = 1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstagPFFFMULTIPLOT_DO
ggsave(pstagPFFFMULTIPLOT_DO, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagPFFFMULTIPLOT_DO_", Sys.Date(), ".jpeg", sep=""), width=7, height=9, units="in")

pstagPFFFMULTIPLOT_PHDOTEMP<-ggarrange(pstag_pH_all_leg_2,pstagPFFF_tempC_all_leg_2,pstag_DO_all_leg_2,ncol = 1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstagPFFFMULTIPLOT_PHDOTEMP
ggsave(pstagPFFFMULTIPLOT_PHDOTEMP, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagPFFFMULTIPLOT_PHDOTEMP_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

pstag_TCL_leg_2<-ggplot(subset(IDR_Data, !is.na(stagff) & flushprepost =="1" |flushprepost=="2"), aes(x=as.Date(mydates), y=TCl2_mgL_all, fill=factor(flush.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4) + theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("TOTAL Chlorine (mg/L)") +scale_fill_manual(name="", values=Pre_post4col) +  guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.x=element_text(size = 15),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))+ scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,3)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +#geom_hline(yintercept=(3.624), color="red")
  theme(plot.title = element_text(size=12))
pstag_TCL_leg_2


pstag_TAtp_all_leg_2<-ggplot(subset(IDR_Data, !is.na(stagff) & flushprepost =="1" |flushprepost=="2"), aes(x=as.Date(mydates), y=HPC2, fill=factor(flush.name))) + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=0, ymax=10^(5), alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=0, ymax=10^(5), alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4) + theme(strip.text.x = element_text(size = 14))  + xlab("Dates of operation")+ ylab("Total ATP (RLU)") +scale_fill_manual(name="", values=Pre_post4col) +  guides(fill = guide_legend(reverse=TRUE))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x)))+coord_cartesian(ylim=c(10^(0),10^(4)))+
  theme(axis.text = element_text(size = 12),axis.title.x=element_text(size = 15),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))+ scale_x_date(breaks = pretty_breaks(10)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +#geom_hline(yintercept=(3.624), color="red")
  theme(plot.title = element_text(size=12))
pstag_TAtp_all_leg_2

pstagPFFFMULTIPLOT_TCL_ATP<-ggarrange(pstag_TCL_leg_2,pstag_TAtp_all_leg_2,ncol = 1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstagPFFFMULTIPLOT_TCL_ATP
ggsave(pstagPFFFMULTIPLOT_TCL_ATP, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstagPFFFMULTIPLOT_TCL_ATP_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

pstagPF_TAtp2_<-ggplot(subset(IDR_Data,  flushprepost ==1 & stagff=="0"|flushprepost==2 ), aes(x=as.Date(mydates), y=TAtp_RLU, fill=factor(flush.name))) + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=0, ymax=10^(5), alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=0, ymax=10^(5), alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol = 4)+ theme(strip.text.x = element_text(size = 14))    + xlab("Dates of operation")+ ylab("Total ATP (RLU)")+scale_x_date(breaks = pretty_breaks(10)) +scale_fill_manual(name="",  values=c("#ffffcc","#78c679","#636363"))  +  guides(fill = guide_legend(reverse=TRUE))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x))) +theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + theme(axis.text.x = element_text(size = 14,,angle = 45, hjust = 1))+
  ggtitle(paste(subtitle= paste("Flush Sample"))) + theme(plot.title = element_text(size=12))
pstagPF_TAtp2_
pstag_TCL_TATP<-ggarrange(pstag_TCL_leg_2,pstagPF_TAtp2_,ncol = 1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstag_TCL_TATP
ggsave(pstag_TCL_TATP, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_TCL_TATP_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")


#########Burn and Beyound####
pstag_Free_NH3_leg22<-ggplot(subset(IDR_Data, asDate>="2020-08-12"&  !is.na(stagff) &stagff=="0"& flushprepost =="1"&stag.dur=="Weekend"), aes(x=agedays, y=Free_NH3_N, fill=Residual))+ scale_x_continuous(breaks = scales::breaks_extended(10)) + 
  geom_point(size=3, shape=21)+theme_bw() +  facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Filter Age(days)")+ ylab("Free-NH3 (mg/L-N))") +scale_fill_manual(name="", values=c(  "chocolate1", "orangered4")) + 
  theme(axis.text = element_text(size = 12),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + coord_cartesian(ylim=c(0,2)) + theme(axis.text.x = element_text(size=10, angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
pstag_Free_NH3_leg22

pFFpf_NO2_leg_22<-ggplot(subset(IDR_Data, asDate>="2020-08-12"&  !is.na(stagff) &stagff=="0"& flushprepost =="1"&stag.dur=="Weekend"), aes(x=agedays, y=nitrite_all, fill=Residual))+ scale_x_continuous(breaks = scales::breaks_extended(10))+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Filter Age(days)")+ ylab("Nitrite (mg/L-N)") +scale_fill_manual(name="",values=c(  "chocolate1", "orangered4")) + 
  theme(axis.text = element_text(size = 12),axis.title.x=element_text(size = 10),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) +  coord_cartesian(ylim=c(0,3.5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size=12))+geom_hline(yintercept=(0.05), color="blue")+geom_hline(yintercept=(1), color="red")
pFFpf_NO2_leg_22

pstag_NO3_leg_22<-ggplot(subset(IDR_Data, asDate>="2020-08-12"&  !is.na(stagff) &stagff=="0"& flushprepost =="1"&stag.dur=="Weekend"), aes(x=agedays, y=nitrate_all, fill=Residual))+ scale_x_continuous(breaks = scales::breaks_extended(10))+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~., ncol=4)+ theme(strip.text.x = element_text(size = 14))   + xlab("Filter Age(days)")+ ylab("Nitrate (mg/L-N)") +scale_fill_manual(name="", values=c("chocolate1", "orangered4")) + 
  theme(axis.text = element_text(size = 12),axis.title.x=element_text(size = 10),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + coord_cartesian(ylim=c(0,7)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
pstag_NO3_leg_22

pstag_NH3_NO2_NO3_22 <-ggarrange(pstag_Free_NH3_leg22, pFFpf_NO2_leg_22,pstag_NO3_leg_22,ncol=1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
pstag_NH3_NO2_NO3_22 
ggsave(pstag_NH3_NO2_NO3_22, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/pstag_NH3_NO2_NO3_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

#########tcl,mono and ATP########
ptapPF_Mono_<-ggplot(subset(IDR_Data, flushprepost=="0"|  flushprepost==2  ), aes(x=as.Date(mydates), y=mono_mgL_all, fill=factor(InfEff.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 14))  + theme(strip.text.x = element_text(size = 15, angle = 0))+ xlab("Dates of operation")+ ylab("Mono-amine (mg/L)") +scale_fill_manual(name="", values=c("#ffffcc","#636363")) + guides(fill = guide_legend(reverse=TRUE))+
  theme(axis.text = element_text(size = 12),axis.title.x=element_text(size = 13),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(size=12))
ptapPF_Mono_

ptapPF_TCL_<-ggplot(subset(IDR_Data,flushprepost=="0"|  flushprepost==2 ), aes(x=as.Date(mydates), y=TCl2_mgL_all, fill=factor(InfEff.name)))  + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=-10, ymax=10, alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=-10, ymax=10, alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=-10, ymax=10, alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol=4)+ theme(strip.text.x = element_text(size = 14))  + theme(strip.text.x = element_text(size = 15, angle = 0)) + xlab("Dates of operation")+ ylab("Total Chlorine (mg/L)") +scale_fill_manual(name="", values=c("#ffffcc","#636363")) + guides(fill = guide_legend(reverse=TRUE))+ 
  theme(axis.text = element_text(size = 12),axis.title.x=element_text(size = 13),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16))+ scale_x_date(breaks = pretty_breaks(10))  + coord_cartesian(ylim=c(0,5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  theme(plot.title = element_text(size=12))
ptapPF_TCL_

ptapPF_TAtp_<-ggplot(subset(IDR_Data, flushprepost=="0"|flushprepost==2 ), aes(x=as.Date(mydates), y=TAtp_RLU, fill=factor(InfEff.name))) + annotate("rect", xmin=as.Date("2020-08-07"), xmax=as.Date("2020-08-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2020-08-12"), xmax=as.Date("2020-08-28"), ymin=0, ymax=10^(5), alpha=0.6) + annotate("rect", xmin=as.Date("2021-03-06"), xmax=as.Date("2021-03-31"), ymin=0, ymax=10^(5), alpha=0.45)+ annotate("rect", xmin=as.Date("2021-03-09"), xmax=as.Date("2021-03-29"), ymin=0, ymax=10^(5), alpha=0.6)+ 
  geom_point(size=3, shape=21)+theme_bw() + facet_wrap(Loc.f~.,ncol = 4) + theme(strip.text.x = element_text(size = 14))  + theme(strip.text.x = element_text(size = 15, angle = 0)) + xlab("Dates of operation")+ ylab("Total ATP RLU")+scale_x_date(breaks = pretty_breaks(10)) +scale_fill_manual(name="Sample:",  values=c("#ffffcc","#636363"))  +  guides(fill = guide_legend(reverse=TRUE))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x)))+coord_cartesian(ylim=c(10^(0),10^(3.5))) +theme(axis.text = element_text(size = 15),axis.title.y=element_text(size = 14,legend.position="bottom",)+theme(legend.text = element_text(size = 16)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle(paste(subtitle= paste(""))) + theme(plot.title = element_text(size=12))
ptapPF_TAtp_

ptapPF_cL2_TATP<-ggarrange( ptapPF_Mono_,ptapPF_TCL_,ptapPF_TAtp_,ncol = 1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
ptapPF_cL2_TATP
ggsave( ptapPF_cL2_TATP, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/ ptapPF_cL2_TATP_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")
ptapPF_cL2<-ggarrange( ptapPF_Mono_,ptapPF_TCL_,ncol = 1, widths = c(2, 2),  common.legend = TRUE, legend = "bottom", align = "v",labels = c("A","B", "C"),font.label = list(size = 14, face = "bold") )
ptapPF_cL2
ggsave( ptapPF_cL2, filename=paste("C:/Users/horac/Box/HSJakpa/USFexperiments/Data Analyisis/thesis plots/ ptapPF_cL2_", Sys.Date(), ".jpeg", sep=""), width=9, height=9, units="in")

#########BOx plot with Tap#####
