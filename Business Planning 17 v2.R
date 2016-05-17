
#Get Date

## DOWNLOAD CURRENT REPORTS FROM https://mhs.health.mil/toc/apptmetrics.shtml, save BMR as BMR only.
##  REMOTE CONNECTION TAKES ABOUT 30 MIN
## PCM Report  https://mhs.health.mil/toc/tools/Capacity/TOC/PCMCAP.xls

# remove all R objects in the working directory
rm(list=ls(all=TRUE))

#Increase memory size (java out of memory and must be before pkg load)
options(java.parameters = "-Xmx8000m")

# id packages
packages <- c("plyr","dplyr","readxl", "zoo",  "stringi", "ggplot2","tidyr","stringr")

#install and library
lapply(packages, require, character.only = TRUE)

#set folders
proj_name = "Business Planning 17"
base_path="U:/NRMC/Data Science"
SP_path="Y:/"

#set project path
proj_path = paste(base_path,"/",proj_name,sep = "")
out_path = file.path(proj_path, "Output")
cleaned_path = file.path(proj_path, "CleanData")
data_path = file.path(proj_path, "RawData")
share_path  = file.path(SP_path, proj_name)

dir.create(file.path(proj_path), showWarnings = FALSE, recursive = FALSE, mode = "0777")
dir.create(file.path(out_path), showWarnings = FALSE, recursive = FALSE, mode = "0777")
dir.create(file.path(cleaned_path), showWarnings = FALSE, recursive = FALSE, mode = "0777")
dir.create(file.path(data_path), showWarnings = FALSE, recursive = FALSE, mode = "0777")
dir.create(file.path(share_path), showWarnings = FALSE, recursive = FALSE, mode = "0777")


#########
#Set Business Plan FY to measure to
#See Business Plans
yrs=unique(value$FY); yrs
YR.Measure='FY17'
Current.date = 'FY15M03-FY16M02'
Month.as="Feb.2016"
#########


#returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

date <- Sys.Date()

dat.file <- list.files(file.path(data_path), pattern="*.xlsx", recursive = F, full.names=T, all.files = T, include.dirs = T)

value<- read_excel(dat.file, sheet = 20, col_names = TRUE, col_types = NULL, na = "",skip = 0)

#Get just RHC-A
value=filter(value, RMC == "RHC-A")
#Identify just HRPs in RHC-A
hrp<-unique(value$PDMISID)
#keep only those HRPs in Enroll and Core

##########Clean DFS
###Value
str(value)
value$Workload=round(value$Workload, 2);value$PPS_Plan_Value=round(value$PPS_Plan_Value, 2)
#Variables equalling total value
tot=c("OP_RVUs" , "APCs")
value=value[value$Workload_Type %in% tot, -2]

current.value=select(value, everything() ) %>%
  filter(FY==Current.date) %>%
  group_by(PDMISID, FacilityName, Data_Feed,FY) %>%
  transmute(Actuals = sum(PPS_Plan_Value)) 

#Remove duplicate rowes
current.value = unique(current.value)
current.value=current.value[ ,c(-3,-4)]
nam=paste(Current.date, ".Actual.Value", sep=" "); colnames(current.value)[3]=nam

plan.value=select(value, everything() ) %>%
  filter(FY==YR.Measure) %>%
  group_by(PDMISID, FacilityName, Data_Feed,FY) %>%
  transmute(Plan = sum(PPS_Plan_Value)) 

#Remove duplicate rowes
plan.value = unique(plan.value)
plan.value=plan.value[ ,c(-3,-4)]
nam=paste(YR.Measure, "Value", sep=" "); colnames(plan.value)[3]=nam

value = merge(plan.value,current.value,  all = T)
remove(plan.value);remove(current.value)

###core\
core<- read_excel(dat.file, sheet = 3, col_names = TRUE, col_types = rep('text',10), na = "",skip = 0)#| in a regex means "or"
#keep only those HRPs in Enroll and Core
core<- core[core$PDMISID %in% hrp ,]#percent_vec = paste(1:100, "%", sep = "")
#core$Value22=as.numeric(sub("%","",core$Value))/100

#Test if % in string and replace and if so convert to decimal
#core=core[ ,-10:-11]
core=core[ ,-1]
#For now, we only need CMS or Proponent in data feed.
core2=filter(core, core$`Data Feed`== "CMS or Proponent")
core2$Value.Format= c("Number", "Percent")[grepl("%", core2$Value) + 1]
core2$Goal.Percent=c("No","Yes") [grepl("%", core2$'Green Standard') + 1]
core2$'Green Standard'=round(as.numeric(gsub('%', "",core2$'Green Standard' ))/100,2)
core2$'Red Standard'=round(as.numeric(gsub('%', "",core2$'Red Standard' ))/100,2)
core2$Score=ifelse( core2$Value >= core2$'Green Standard', 1,
                    ifelse(core2$Value <= core2$'Red Standard', -1, 0))
core3=core2
colnames(core3)=gsub(" ",".",colnames(core3))
colnames(core3)=gsub("/",".",colnames(core3))
core4=select(core3, everything()) %>%
  group_by(PDMISID,Performance.Measure.Name, Year) %>%
  summarise(Data.Feed = first(Data.Feed), MHS.Service.Goal=first(MHS.Service.Goal),
            Value=first(Value), Green.Standard=first(Green.Standard), Red.Standard=first(Red.Standard),
            prev_value=first(prev_value),Value.Format=first(Value.Format),Goal.Percent=first(Goal.Percent),
            Score=first(Score))


Perf.Plan=filter(core, core$`Data Feed`== "Performance Plan")
Perf.Plan=Perf.Plan[ ,c(1,3,6)]; colnames(Perf.Plan)[3]="FY17.Plan"
colnames(Perf.Plan)=gsub(" ",".",colnames(Perf.Plan))
colnames(Perf.Plan)=gsub("/",".",colnames(Perf.Plan))

Complete = merge(Perf.Plan, core4, all=T)
Complete$Color=ifelse( Complete$Score == 1, "Green",
                    ifelse(Complete$Score == -1,"Red", "Amber"));Keep=c(1,2,4,7,14,13,3,8,9)
Complete=Complete[ ,Keep]; colnames(Complete)[3]="Actual.as.of"
Complete.No.Na=na.omit(Complete) #Get only completes
Incompletes=Complete[!complete.cases(Complete),] #Save incompletes for review
Complete.No.Na$Type="Core"



###Enroll Data
enroll<- read_excel(dat.file, sheet = 21, col_names = TRUE, col_types = NULL, na = "",skip = 0)
enroll<- enroll[enroll$PDMISID %in% hrp ,]
enroll$Desc=paste(enroll$Data_Feed,enroll$FY,sep="-")
enroll=filter(enroll,RMC=="RHC-A")
enroll=enroll[ ,c(-2,-4,-5)]
enroll=spread(enroll, key=Desc, value=Enrollee_Count, fill = NA, convert = FALSE, drop = TRUE)
enroll2=enroll[ ,c(1,2,9,11)];
enroll2$'Green Standard'=1.0;enroll2$'Red Standard'=1.0;enroll2$Value=enroll2$`M2_Actuals-FY16 (M4)`/enroll2$`Perf_Plan-FY17`
enroll2$Score=ifelse( enroll2$Value >= enroll2$'Green Standard', 1,
                    ifelse(enroll2$Value < enroll2$'Red Standard', -1, 0))
enroll2$'Performance Measure Name'="Enrollment";enroll2$Actual.as.of=as.character(Sys.Date())
colnames(enroll2)[4]='FY17.Plan'
enroll2$Color=ifelse( enroll2$Score == 1, "Green",
                      ifelse(enroll2$Score == -1,"Red", "Amber"))
colnames(enroll2)=gsub(" ",".",colnames(enroll2))
colnames(enroll2)=gsub("/",".",colnames(enroll2))
KEEP=c("PDMISID","Performance.Measure.Name", "Actual.as.of", "Value","Color","Score", "FY17.Plan" ,
       "Green.Standard","Red.Standard"  )


hrp.names=enroll2[ ,c(1,2)];enroll2=enroll2[ ,KEEP]
enroll2$Type="Enroll"

Complete.No.Na=rbind(Complete.No.Na,enroll2)
#####
value$'Green Standard'=1.0;value$'Red Standard'=1.0;
colnames(value)
value$Value=value$'FY15M03-FY16M02 .Actual.Value'/value$`FY17 Value`
value$Score=ifelse( value$Value >= value$'Green Standard', 1,
                      ifelse(value$Value < value$'Red Standard', -1, 0))
value$'Performance Measure Name'="Value";value$Actual.as.of=as.character(Sys.Date())
value$Color=ifelse( value$Score == 1, "Green",
                      ifelse(value$Score == -1,"Red", "Amber"))
colnames(value)=gsub(" ",".",colnames(value))
colnames(value)=gsub("/",".",colnames(value))
colnames(value)[3]="FY17.Plan";value=value[ ,KEEP]
value$Type="Value"

Complete.No.Na=rbind(Complete.No.Na,value)



###ADD in weightings. 
wght=read.csv(file.path(data_path, "Weighting.csv"))
Complete.No.Na=merge(Complete.No.Na,wght)
Complete.No.Na$Score2=Complete.No.Na$Score*Complete.No.Na$Weight

#Complete.No.Na2=Complete.No.Na
#Complete.No.Na2$Value=NULL

Summary2=select(Complete.No.Na, everything()) %>%
  group_by(PDMISID,Type) %>%
  summarise(Score3=as.character(sum(Score2))) %>%
  spread(key=Type,value=Score3)

Summary2$Enroll=gsub("-0.33","Missed",Summary2$Enroll)
Summary2$Enroll=gsub("0.33","Obtained",Summary2$Enroll)

Summary2$Value=gsub("-0.33","Missed",Summary2$Value)
Summary2$Value=gsub("0.33","Obtained",Summary2$Value)
#summarise(Core=sum(Score == -1)/sum(Score == 1),
#            Enroll=sum(Enroll == -1)/sum(Enroll == 1),
#            Value=sum(Value == -1)/sum(Value == 1))

#creates a logical vector which is TRUE at every location that x occurs, and when suming, the logical vector is coerced to numeric which converts TRUE to 1 and FALSE to 0
#Ratio=sum(Summary2$Score == -1)/sum(Summary2$Score == 1)

Summary=select(Complete.No.Na, everything()) %>%
  group_by(PDMISID) %>%
  summarise(Composite = sum(Score2))
Summary$MTF.Rank=row_number(desc(Summary$Composite)) 

Final.Summary=Summary[ ,-2]
Final.Summary=merge(Final.Summary,Summary2)

Metrics=unique(Complete.No.Na$`Performance Measure Name`)

Final.Summary=merge(Final.Summary, hrp.names)
Final.Summary=Final.Summary[ ,c(1,6,3,4,5,2)]
Final.Summary=arrange(Final.Summary, Final.Summary$'MTF.Rank')
Final.Summary$As.of.Date=Month.as

#write.csv(Incompletes,"Missing Values by metric and MTF.csv")
##write.csv(Final.Summary,"Summary MTF.csv", row.names = F)
#write.csv(Complete.No.Na,"Data for Weighting.csv")
#write.csv(Metrics,"Need Weighting.csv")
library(xlsx)
write.xlsx(Final.Summary, file.path(share_path, 'Summary MTF2.xlsx'), sheetName="Summary MTF", 
                    col.names=TRUE, row.names=F, append=FALSE, showNA=TRUE)


##########################################

tst=  filter(core, core$PDMISID=="0089" & core$Year != "FY17",core$'Performance Measure Name' == "Body Mass Index (% Healthy)")
  
d=mean(as.numeric(tst$Value))
d
