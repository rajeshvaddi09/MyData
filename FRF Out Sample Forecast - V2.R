
##############################Summary and Version ################################### 
#######                                                                       #######
#######        Objective       : Freigth Rate Out Sample Forecasting          #######
#######        Author          : Rajesh Vaddi                                 #######
#######        R Version       : 3.1.2                                        #######
#######        Version(V0)     : Base code                                    #######
#######        Version - V1    : CHANGES                                      #######
#######                                                                       #######
#####################################################################################

#####   Installing and loading Dependencies #####
wants <- c("TTR", "Hmisc", "data.table", "DataCombine", "reshape2", "zoo", "plyr", "forecast",
           "reshape", "gtools", "car", "sqldf", "foreach", "DMwR", "moments", "RODBC", "gdata","doSNOW")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, require, character.only = TRUE)
rm("wants","has")

###### Local Directory ######
SOURCE_DIR <- "E:\\Freight Rate V1"

#Brent_Future             <- read.csv(paste0(SOURCE_DIR,"/data/Brent_Future.csv"),header= TRUE)
#TradeRegion              <- read.csv(paste0(SOURCE_DIR,"/data/Trade_regions.csv"),header= TRUE)
#portToCountry            <- read.csv(paste0(SOURCE_DIR,"/data/PortCode_Country.csv"),header= TRUE)
#Dollar_Conversion        <- read.csv(paste0(SOURCE_DIR,"/data/Dollar_Conversion_Value.csv"), sep=",", header=T)
#Lookup_1                 <- read.csv(paste0(SOURCE_DIR,"/data/MasterCalculation.csv"), sep=",", header=T)
#Region_Size_Combos_1     <- read.csv(paste0(SOURCE_DIR,"/data/Region_Size_Combos_Route.csv"), sep=",", header=T)
#Best_Models_Base_WSL     <- read.csv(paste0(SOURCE_DIR,"/results/Best_Models_Base_WSL V2.csv"), header= TRUE)
#Best_Models_Base_LumpSum <- read.csv(paste0(SOURCE_DIR,"/results/Best_Models_Base_LumpSum V2.csv"), header= TRUE)

connection <- odbcDriverConnect("Driver=SQL Server; Server=10.45.88.171; Database=RA; Uid=maritime; Pwd=trial,123")

Brent_Future             <- sqlQuery(connection, "SELECT * From  FR_Brent_Future")
TradeRegion              <- sqlQuery(connection, "SELECT * From  FR_TradeRegion")
portToCountry            <- sqlQuery(connection, "SELECT * From  FR_PortToCountry")
Dollar_Conversion        <- sqlQuery(connection, "SELECT * From  FR_Dollar_Conversion")
Lookup_1                 <- sqlQuery(connection, "SELECT * From  FR_MasterCalculation")
Region_Size_Combos_1     <- sqlQuery(connection, "SELECT * From  FR_Region_Size_Combos_Route")
Best_Models_Base_WSL     <- sqlQuery(connection, "SELECT * From  FR_Best_Models_Base_WSL")
Best_Models_Base_LumpSum <- sqlQuery(connection, "SELECT * From  FR_Best_Models_Base_LumpSum")
master                   <- sqlQuery(connection, "SELECT * From  FR_MasterCalculation_Lumpsum")

Dollar_Conversion        <- read.csv("E:\\Freight Rate V1\\results\\Dollar Conversion.csv", sep=",", header=T)

Best_Models_Base_WSL_India <- read.csv("E:\\Freight Rate V1\\results\\Best_Models_Base_WSL_New V2.csv")
Best_Models_Base_WSL <- rbind(Best_Models_Base_WSL,Best_Models_Base_WSL_India[c(245:276),])

#Dollar_Conversion$YR2016 <- Dollar_Conversion$YR2015

Lookup_1 <- Lookup_1[c(1:93),]
India_EC <- Lookup_1[Lookup_1$Region %in%  "ARABIAN GULF - INDIA",]
India_EC$Region <- "ARABIAN GULF - INDIA_EC"
India_WC <- Lookup_1[Lookup_1$Region %in%  "ARABIAN GULF - INDIA",]
India_WC$Region <- "ARABIAN GULF - INDIA_WC"
Lookup_1 <- rbind(Lookup_1,India_EC,India_WC)

India_EC <- Dollar_Conversion[Dollar_Conversion$Trade_Region_destination %in% "INDIA",]
India_EC$Trade_Region_destination <- "INDIA_EC"
India_WC <- Dollar_Conversion[Dollar_Conversion$Trade_Region_destination %in% "INDIA",]
India_WC$Trade_Region_destination <- "INDIA_WC"
Dollar_Conversion <- rbind(Dollar_Conversion,India_EC,India_WC)

####################   Dates Initialisation  ########################
#Best_Models_Base_WSL$Training_Dates <-as.character(Best_Models_Base_WSL$Training_Dates)
#Training_Dates <- gsub("^\\s+|\\s+$", "",unlist(strsplit(unique(Best_Models_Base_WSL$Training_Dates)," - ")))
#Training_Dates <- as.Date(Training_Dates)

Forecasting_Period <- 72 

add.months= function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]
sys_date <- as.Date(as.character(Sys.time()))
sys_date <- as.Date(paste(year(sys_date), month(sys_date),"01",sep="-"))
Data_Pull <- sys_date-1
Training_Dates <- as.Date(c(as.Date("2010-01-01"),add.months(sys_date,-13)))
date1 <- as.POSIXlt(Training_Dates[1])
date2 <- as.POSIXlt(Training_Dates[2])
Training_Period <- abs((date1$year*12 + date1$mon) - (date2$year*12 + date2$mon))+1
Training_Period_1 <- as.numeric(c(1:(Training_Period)))
Validation_Period <- c((max(Training_Period_1)+1):(max(Training_Period_1)+12))
Forecasting_Period_1 <- as.numeric(c(((Training_Period)+1):((Training_Period)+Forecasting_Period)))
End_Date <- add.months(Training_Dates[2],Forecasting_Period)
End_Date_1 <- add.months(Training_Dates[2],length(which(grepl("A_",colnames(Best_Models_Base_WSL)))))
chk <- as.POSIXlt(End_Date_1)
Forecasting_Period_2 <- abs((date1$year*12 + date1$mon) - (chk$year*12 + chk$mon))+1

########################################################################
#######                                                          #######
#######              Fixture Data Extraction                     #######
#######                                                          #######
########################################################################
connection <- odbcDriverConnect("Driver=SQL Server; Server=10.45.88.171; Database=RA; Uid=maritime; Pwd=trial,123")

Fixture_2010_11_Data <- sqlQuery(connection, "SELECT * From  FR_Fixture_2010_11_Clean_Data" )

fRate <- sqlQuery(connection, sprintf("SELECT
                                      LRNO AS LRIMOShipNo,
                                      FIXTURE_ID AS FixtureID,
                                      PORT_OF_ORIGIN AS OriginPort,
                                      PORT_OF_DESTINATION AS DestinationPort,
                                      SHIPNAME AS ShipnameReported,
                                      SIZE AS Size,
                                      CHARTERER AS Charterer,
                                      CHARTER_ID AS ChartererCode,
                                      CHARTER_PARTY_TONNAGE AS CharterPartyTonnage,
                                      CHARTER_PARTY_COMMODITY AS CharterPartyCommodity,
                                      CHARTER_PARTY_TERMS AS CharterPartyTerms,
                                      CHARTER_PARTY_RATE AS CharterPartyRate,
                                      CHARTER_PARTY_DATES AS CharterPartyDates,
                                      COMMODITY AS Commodity,
                                      COMMODITY_SUBTYPE AS CommodirtySubtype,
                                      ORIGIN_AREA AS OriginArea,
                                      DESTINATION_AREA AS DestinationArea,
                                      ON_FIXTURE AS DateOnFixture,
                                      OFF_FIXTURE AS DateOffFixture,
                                      OLD_PORT_ID AS OriginPortCode,
                                      OLD_PORT_ID_DESTINATION AS DestinationPortCode,
                                      Reported AS DateReported
                                      From  ABSD_FIXTURES 
                                      WHERE CAST(Reported as date) BETWEEN '2010-01-01' AND %s",paste0("'",Data_Pull,"'")))

fRate$DateReported <- as.Date(as.character(fRate$DateReported))
fRate_raw <- subset(fRate, DateReported >=  as.Date('2012-01-01') & DateReported <= Data_Pull)
fRate_raw_10_11 <- subset(fRate, DateReported >= as.Date('2010-01-01') & DateReported <= as.Date('2011-12-31'))
#rm(fRate)
#close(connection)

#############       India Routes BreakUp        #############
fRate_India <- fRate
fRate_India$OriginPortCode      <- as.character(fRate$OriginPortCode)
fRate_India$DestinationPortCode <- as.character(fRate$DestinationPortCode)
fRate_India$OriginPort          <- as.character(fRate_India$OriginPort)
fRate_India$DestinationPort     <- as.character(fRate_India$DestinationPort)
fRate_India$OriginArea          <- as.character(fRate_India$OriginArea)
fRate_India$DestinationArea     <- as.character(fRate_India$DestinationArea)

East_Coast <-c('PO8098','PO7042','PO9292','PO3770','PO1810','PO5329','PO1785','PO1780','PO1781','PO8350',
               'PO1788','PO9161','PO8094','PO9532','PO1796','PO8097','PO8561','PO6825','PO6769','PO1792',
               'PO7895','PO1799','PO3224','PO1808','PO6534','PO5334','PO3771','PO8175','PO8352','PO8069', 
               'PO9365','PO8351')

West_Coast <- c('PO3780','PO1772','PO1783','PO5330','PO1793','PO3776','PO1773','PO1807','PO6142',
                'PO7451','PO1779','PO9080','PO3774','PO1798','PO1804','PO1805','PO7041','PO7092','PO3779','PO6584',
                'PO1774','PO3775','PO5328','PO5327','PO1777','PO9345','PO9113','PO1806','PO7417','PO1784','PO1789',
                'PO8178','PO1775', 'PO7093', 'PO8349', 'PO8353', 'PO6102', 'PO1790', 'PO5333', 'PO9279', 'PO6141',
                'PO8558', 'PO1797', 'PO8560', 'PO7641', 'PO1800', 'PO8054', 'PO1771', 'PO1778', 'PO1786', 'PO5331',
                'PO3778','PO8559','PO9251','PO8052','PO6768','PO5326','PO9255','PO8262','PO7095','PO7328','PO5332',
                'PO1787','PO8263','PO1782','PO1809','PO1803','PO7513')

fRate_India <- fRate_India[fRate_India$OriginPortCode %in% c(East_Coast,West_Coast) | fRate_India$DestinationPortCode %in% c(East_Coast,West_Coast),]

fRate_India$OriginPort[fRate_India$OriginPortCode  %in% East_Coast] <- "India_EC"
fRate_India$OriginPort[fRate_India$OriginPortCode  %in% West_Coast] <- "India_WC"

fRate_India$DestinationPort[fRate_India$DestinationPortCode %in% East_Coast] <- "India_EC"
fRate_India$DestinationPort[fRate_India$DestinationPortCode %in% West_Coast] <- "India_WC"

fRate_India$OriginArea[fRate_India$OriginPortCode  %in% East_Coast] <- "India_EC"
fRate_India$OriginArea[fRate_India$OriginPortCode  %in% West_Coast] <- "India_WC"

fRate_India$DestinationArea[fRate_India$DestinationPortCode  %in% East_Coast] <- "India_EC"
fRate_India$DestinationArea[fRate_India$DestinationPortCode  %in% West_Coast] <- "India_WC"

fRate_India$OriginPortCode[fRate_India$OriginPortCode  %in% East_Coast] <- "India_EC"
fRate_India$OriginPortCode[fRate_India$OriginPortCode  %in% West_Coast] <- "India_WC"

fRate_India$DestinationPortCode[fRate_India$DestinationPortCode %in% East_Coast] <- "India_EC"
fRate_India$DestinationPortCode[fRate_India$DestinationPortCode %in% West_Coast] <- "India_WC"

#################################################################################
#########                                                               #########
#########                 Fixture Data Cleaning Code                    #########
#########                                                               #########
#################################################################################

fRate_raw_LumpSum <- rbind(fRate_raw_10_11,fRate_raw)
fRate_raw_LumpSum$CharterPartyTerms <- as.character(fRate_raw_LumpSum$CharterPartyTerms)
fRate_raw_LumpSum_1 <- fRate_raw_LumpSum[grepl("Lump",fRate_raw_LumpSum$CharterPartyTerms),]
fRate_raw_LumpSum_2 <- fRate_raw_LumpSum[grepl("LS",fRate_raw_LumpSum$CharterPartyTerms),]
fRate_raw_LumpSum <- as.data.frame(rbind(fRate_raw_LumpSum_1,fRate_raw_LumpSum_2))
fRate_raw_LumpSum$CharterPartyRate <- as.numeric(as.character(fRate_raw_LumpSum$CharterPartyRate))
fRate_raw_LumpSum <- fRate_raw_LumpSum[!is.na(fRate_raw_LumpSum$CharterPartyRate),]
#rm(fRate_raw_LumpSum_1,fRate_raw_LumpSum_2,fRate_raw_10_11)

#fRate_raw_WSL <- fRate_raw
fRate_raw_WSL <- rbind(fRate_raw,fRate_India)

i <- sapply(Region_Size_Combos_1, is.factor)
Region_Size_Combos_1[i] <- lapply(Region_Size_Combos_1[i], as.character)

i <- sapply(TradeRegion, is.factor)
TradeRegion[i] <- lapply(TradeRegion[i], as.character)

i <- sapply(portToCountry, is.factor)
portToCountry[i] <- lapply(portToCountry[i], as.character)
PortCode <- portToCountry

trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
  gsub("([\\|()])|\\s|\\old|\\Old|\\New|new", "", x)}

trim_w <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$|[A-z])", "", x)}

for (q in c(1,2)){
  if (q ==1){fRate_raw <- fRate_raw_WSL}
  if (q ==2){fRate_raw <- fRate_raw_LumpSum}
  portToCountry$Old_ID <- as.character(portToCountry$Old_ID)
  portToCountry$Country_Name<- as.character(portToCountry$Country_Name)
  portToCountry$Country_Port <- paste(portToCountry$Old_ID,portToCountry$Country_Name,sep='_')
  portToCountry <- portToCountry[(!(is.na(portToCountry$Old_ID)==TRUE) | !(is.na(portToCountry$Country_Name)==TRUE)),]
  portToCountry <- portToCountry[!duplicated(portToCountry$Country_Port),]
  
  #fRate_raw <- fRate_raw[(!is.na(fRate_raw$LRIMOShipNo)==TRUE ),]
  fRate_tanker <- fRate_raw[which(fRate_raw$Commodity =='Tanker' ),]
  fRate_tanker$CharterPartyCommodity_c <- as.character(fRate_tanker$CharterPartyCommodity)
  fRate_tanker$CharterPartyCommodity_c[(fRate_tanker$CharterPartyCommodity_c %in% c('il Dirty', 'Oi lDirty', 'Oil  Dirty','Oil Dikrty','Oil dirty','OIl Dirty','Oil Diryt','Oil Drity', 'OilDirty','yil Dirt','Oil Dirty'))] <- 'Dirty Oil'
  fRate_tanker$CharterPartyCommodity_c[(fRate_tanker$CharterPartyCommodity_c %in% c('Oil  Clean','Oil clean','Oil Clean','OIl Clean','Oil Cleana','Oil Clena','OilClean'))] <- 'Clean Oil'
  fRate_tanker$CharterPartyCommodity_c[(fRate_tanker$CharterPartyCommodity_c %in% c('Condensate','condensates','Condensates'))] <- 'Condensate'
  fRate_tanker$CharterPartyCommodity_c[(fRate_tanker$CharterPartyCommodity_c %in% c('Fuel Oil','Fuel Oily'))] <- 'Fuel Oil'
  fRate_tanker$CharterPartyCommodity_c[(fRate_tanker$CharterPartyCommodity_c %in% c('Jet Fuel','Jet Fule','Jet Oil'))] <- 'Jet Fuel'
  fRate_tanker$CharterPartyCommodity_c[(fRate_tanker$CharterPartyCommodity_c %in% c("Naphha","naphtha","Naphtha","Naptha"))] <- "Naphtha"
  
  fRate_tanker$CharterPartyDates_c <- as.character(fRate_tanker$CharterPartyDates)
  fRate_tanker$CharterPartyDates_c <- paste(gsub("^\\s+|\\s+$", "",substr(fRate_tanker$CharterPartyDates_c,4,1000000L)),
                                            substr(fRate_tanker$CharterPartyDates_c, 1,3),sep="-")
  fRate_tanker$CharterPartyDates_c[(fRate_tanker$CharterPartyDates_c %in% c('[Mar 13'))] <- '13-Mar'
  fRate_tanker$CharterPartyDates_c[(fRate_tanker$CharterPartyDates_c %in% c(']May 29'))] <- '29-May'
  fRate_tanker$CharterPartyDates_c[(fRate_tanker$CharterPartyDates_c %in% c('Aapr 11','Ajpr 16','Ap r4','Ap r6','Apar 17','Apar 23','Apar 3','Apar 6','Apar 8','Apr',"Apr'03",'Apr-31','Apr-70','Apr 1/10','Apr 15/30','Apr 20/21','Apr 28/30','Apra 24','Arp 20'))] <- '30-Apr'
  fRate_tanker$CharterPartyDates_c[(fRate_tanker$CharterPartyDates_c %in% c('Auag 10','Aug','Aug 1/10','Aug 1/5','Aug 5/10','Aug/Sep','Augt 27'))] <- '30-Aug'
  fRate_tanker$CharterPartyDates_c[(fRate_tanker$CharterPartyDates_c %in% c('Dec','Dec-38','Dec 27/30','Dec 5/10', 'RetroDec16'))] <- '30-Dec'
  fRate_tanker$CharterPartyDates_c[(fRate_tanker$CharterPartyDates_c %in% c('Feab 13','Feb','Feb-29','Feb-31','Feb 10/20','Feb 12/15','Feb 15/20','Feb 2/4','Feb 20/25','Feb 21/23','Feb 5/10','Feb 5/15','Feb 8/10','Feeb 3'))] <- '27-Feb'
  fRate_tanker$CharterPartyDates_c[(fRate_tanker$CharterPartyDates_c %in% c('JaFeb 4', 'Jan',  'Jan-90',  'Jan-99',  "Jan '02",  'Jan 20/30',	'Jan 25/30',	'Jan 28/30',	'Jan 5/15',	'Jan/Mar',	'Jn 25',	'Jna 22',	'Jna 26'))] <- '30-Jan'
  fRate_tanker$CharterPartyDates_c[(fRate_tanker$CharterPartyDates_c %in% c('Jul',  'Jul 1/10',	'Jul 10/15',	'Jul 10/25',	'Jul 12/20',	'Jul 15/25',	'Jul 28/30',	'Jul 28op23',	'Jul l3',	'Jul l5',	'Jul25/5Aug', 'Lul 7'))] <- '30-Jul'
  fRate_tanker$CharterPartyDates_c[(fRate_tanker$CharterPartyDates_c %in% c('Jun', 'Jun-31',	'Jun 1/10',	'Jun 15/20',	'Jun/Jul',	'Jun239'))] <- '30-Jun'
  fRate_tanker$CharterPartyDates_c[(fRate_tanker$CharterPartyDates_c %in% c('Ma 18',  'Ma 20',	'Ma 22',	'Mar',	'Mar 1/10',	'Mar 1/15',	'Mar 1/5',	'Mar 10/12',	'Mar 10/20',	'Mar 15/25',	'Mar 4/10',	'Mar 5/10',	'Mar 5/15',	'Mar i',	'Mar/Apr',	'Mary 19',	'Mawr 3', 'Mr 9'))] <- '30-Mar'
  fRate_tanker$CharterPartyDates_c[(fRate_tanker$CharterPartyDates_c %in% c('May',  'May-39',	'May 1/5',	'May 10/15',	'May 10/20',	'May 15/20',	'May 15/25',	'May 20/30',	'May 245',	'Maya 16',	'Maya 26',	'Maya 9',	'MKay 15'))] <- '30-May'
  fRate_tanker$CharterPartyDates_c[(fRate_tanker$CharterPartyDates_c %in% c('Nov',  'Nov-31',	'Nov 18/20',	'Nov 25/30',	'Nov1opNov3',	'Nvo 26',	'RetroNov26'))] <- '30-Nov'
  fRate_tanker$CharterPartyDates_c[(fRate_tanker$CharterPartyDates_c %in% c('Oct',  'Oct 1/10',	'Oct 10/20',	'Oct 25/30',	'Oct 5/15',	'Oct 5]',	'Oct/Nov',	'Oct25/3Oct'))] <- '30-Oct'
  fRate_tanker$CharterPartyDates_c[(fRate_tanker$CharterPartyDates_c %in% c('Se 68',  'Sep',	'Sep-31',	'Sep-58',	'Sep 1/10',	'Sep 1/15',	'Sep 10/20',	'Sep 11/12',	'Sep 20/25',	'Sep 25/30',	'Sep25/5Oct',	'Sep276',	'Sepj 21',	'Sept',	'Spe 2'))] <- '30-Sep'
  fRate_tanker$CharterPartyDates_c[(fRate_tanker$CharterPartyDates_c %in% c('Prompt','spot','Spot', 'Naphtha', '2003', '2008/2009', '2009'))] <- format(as.Date(fRate_tanker$DateReported[(fRate_tanker$CharterPartyDates_c %in% c('Prompt','spot','Spot', 'Naphtha', '2003', '2008/2009', '2009'))],format="%m/%d/%Y"),'%d-%b')
  
  fRate_tanker$DateReported_d <- as.Date(fRate_tanker$DateReported,format="%m/%d/%Y")
  fRate_tanker$Report_Date <- format(as.Date(fRate_tanker$DateReported,format="%m/%d/%Y"),"%d")
  fRate_tanker$Report_Month <- format(as.Date(fRate_tanker$DateReported,format="%m/%d/%Y"),"%m")
  fRate_tanker$Report_Year <- format(as.Date(fRate_tanker$DateReported,format="%m/%d/%Y"),"%Y")
  fRate_tanker$Report_MonthYr <- format(as.Date(fRate_tanker$DateReported,format="%m/%d/%Y"),"%m/%Y")
  fRate_tanker$Report_MonthYr_e <- as.Date(paste("01/",fRate_tanker$Report_MonthYr,sep=""),format="%d/%m/%Y")
  
  fRate_tanker$Charter_day <- format(as.Date(fRate_tanker$CharterPartyDates_c,format="%d-%b"),'%d')
  fRate_tanker$Charter_Month <- format(as.Date(fRate_tanker$CharterPartyDates_c,format="%d-%b"),'%m')
  fRate_tanker$Charter_Year <- ifelse(as.integer(fRate_tanker$Charter_Month)-as.integer(fRate_tanker$Report_Month) < -8, as.integer(fRate_tanker$Report_Year)+1, ifelse(as.integer(fRate_tanker$Charter_Month)-as.integer(fRate_tanker$Report_Month) > 8,as.integer(fRate_tanker$Report_Year)-1, as.integer(fRate_tanker$Report_Year)))
  fRate_tanker$CharterPartyDates_clean <- paste(fRate_tanker$Charter_Month,'/',fRate_tanker$Charter_day,'/',fRate_tanker$Charter_Year,sep='')
  fRate_tanker$day_lag <- as.Date(fRate_tanker$CharterPartyDates_clean,format='%m/%d/%Y')-as.Date(fRate_tanker$DateReported,format='%m/%d/%Y')
  
  fRate_tanker$Report_Date <- NULL
  fRate_tanker$Report_Month <- NULL
  fRate_tanker$Charter_day <- NULL
  fRate_tanker$Charter_Month <- NULL
  fRate_tanker$Charter_Year <- NULL
  fRate_tanker <- fRate_tanker[!(is.na(fRate_tanker$day_lag) ==TRUE),]
  
  if(q==1){
    fRate_tanker$CharterPartyRate <- as.character(fRate_tanker$CharterPartyRate)
    fRate_tanker$CharterPartyRate <- trim(fRate_tanker$CharterPartyRate)
    
    for(i in 1:length(fRate_tanker$CharterPartyRate)){
      if(grepl('op', fRate_tanker$CharterPartyRate[i])=='TRUE')
      {
        fRate_tanker$CharterPartyRate_WSL[i] <- strsplit(as.character(fRate_tanker$CharterPartyRate[i]), "op")[[1]][[1]]
        fRate_tanker$CharterPartyRate_WSH[i] <- strsplit(as.character(fRate_tanker$CharterPartyRate[i]), "op")[[1]][[2]]
      }
      else{
        if(grepl('w', fRate_tanker$CharterPartyRate[i])=='TRUE' | grepl('W', fRate_tanker$CharterPartyRate[i])=='TRUE')
        {      
          fRate_tanker$CharterPartyRate_WSL[i] <- fRate_tanker$CharterPartyRate[i]
          fRate_tanker$CharterPartyRate_WSH[i] <- 'NA'      
        }
        else{
          fRate_tanker$CharterPartyRate_WSL[i] <- 'NA'
          fRate_tanker$CharterPartyRate_WSH[i] <- 'NA'
        }
      }  
    }
    
    fRate_raw$CharterPartyRate <- trim(fRate_raw$CharterPartyRate)
    fRate_tanker$CharterPartyRate_WSL <- trim(fRate_tanker$CharterPartyRate_WSL)
    fRate_tanker$CharterPartyRate_WSL <- as.integer(trim_w(fRate_tanker$CharterPartyRate_WSL))
    fRate_tanker$CharterPartyRate_WSH <- trim(fRate_tanker$CharterPartyRate_WSH)
    fRate_tanker$CharterPartyRate_WSH <- as.integer(trim_w(fRate_tanker$CharterPartyRate_WSH))
    fRate_tanker_WS <- fRate_tanker[!(is.na(fRate_tanker$CharterPartyRate_WSL) ==TRUE) & !(fRate_tanker$DestinationArea) %in% c("Unreported") & !(fRate_tanker$OriginArea) %in% c("Unreported"),]
    fRate_tanker_WS_2 <- fRate_tanker_WS[(fRate_tanker_WS$CharterPartyRate_WSL<=800 & fRate_tanker_WS$CharterPartyCommodity_c %in% c('Dirty Oil', 'Clean Oil')),]}
  if(q==2){fRate_tanker_WS_2 <- fRate_tanker}
  
  ####### Maping the unknown portcode with portnames and Trade Regions in the Freight rates ###########
  TradeRegion$Country_Area <- toupper(TradeRegion$Country_Area)
  TradeRegion$Trade_Region <- toupper(TradeRegion$Trade_Region)
  fRate_tanker_WS_2 <- merge(x = fRate_tanker_WS_2, y = PortCode, by.x = "OriginPortCode", by.y="Old_ID", all.x=TRUE)
  setnames(fRate_tanker_WS_2, old = c('Country_code','Dec_Lat', 'Dec_Long', 'Country_Name'), new = c('Country_code_origin','Dec_Lat_origin', 'Dec_Long_origin', 'Country_Name_origin'))
  fRate_tanker_WS_2$tempReg_Origin <- toupper(paste(fRate_tanker_WS_2$Country_Name_origin, fRate_tanker_WS_2$OriginArea, sep="_"))
  fRate_tanker_WS_2 <- merge(x=fRate_tanker_WS_2, y=TradeRegion, by.x="tempReg_Origin", by.y="Country_Area", all.x=TRUE)
  fRate_tanker_WS_2$tempReg_Origin <- NULL
  setnames(fRate_tanker_WS_2, old = c('Trade_Region'), new = c('Trade_Region_origin'))
  
  fRate_tanker_WS_2 <- merge(x = fRate_tanker_WS_2, y = PortCode, by.x = "DestinationPortCode", by.y="Old_ID", all.x=TRUE)
  setnames(fRate_tanker_WS_2, old = c('Country_code','Dec_Lat', 'Dec_Long', 'Country_Name'), new = c('Country_code_Destination','Dec_Lat_Destination', 'Dec_Long_Destination', 'Country_Name_Destination'))
  fRate_tanker_WS_2$tempReg_dest <- toupper(paste(fRate_tanker_WS_2$Country_Name_Destination, fRate_tanker_WS_2$DestinationArea, sep="_"))
  fRate_tanker_WS_2 <- merge(x=fRate_tanker_WS_2, y=TradeRegion, by.x="tempReg_dest", by.y="Country_Area", all.x=TRUE)
  fRate_tanker_WS_2$tempReg_dest <- NULL
  setnames(fRate_tanker_WS_2, old = c('Trade_Region'), new = c('Trade_Region_destination'))
  
  ######################################## ----- End of Mapping ----- ##########################
  fRate_tanker_WS_2$Trade_Region_origin <- as.character(fRate_tanker_WS_2$Trade_Region_origin)
  fRate_tanker_WS_2$Trade_Region_destination<- as.character(fRate_tanker_WS_2$Trade_Region_destination)
  fRate_tanker_WS_2 <- fRate_tanker_WS_2[(!(is.na(fRate_tanker_WS_2$Trade_Region_origin) ==TRUE) | !(is.na(fRate_tanker_WS_2$Trade_Region_destination) ==TRUE)),]
  fRate_tanker_WS_2 <- fRate_tanker_WS_2[((fRate_tanker_WS_2$Trade_Region_origin !="NA") | (fRate_tanker_WS_2$Trade_Region_destination)!="NA"),]
  
  if(q==1){Fixture_Data_New <- fRate_tanker_WS_2[,c('LRIMOShipNo','FixtureID', 'Trade_Region_origin','Trade_Region_destination','Report_MonthYr_e','CharterPartyRate_WSL','Country_Name_origin','Country_Name_Destination','CharterPartyCommodity','Size')]
           setnames(Fixture_Data_New, old = c('Report_MonthYr_e', 'CharterPartyRate_WSL'), new = c('Report_MonthYr','CharterPartyRate'))
           Fixture_Data_Master <- Fixture_Data_New}
  
  if(q==2){Fixture_Data_New <- fRate_tanker_WS_2[,c('LRIMOShipNo','FixtureID', 'Trade_Region_origin','Trade_Region_destination','Report_MonthYr_e','CharterPartyRate','Country_Name_origin','Country_Name_Destination','CharterPartyCommodity','Size')]
           setnames(Fixture_Data_New, old = c('Report_MonthYr_e', 'CharterPartyRate'), new = c('Report_MonthYr','CharterPartyRate'))
           Fixture_Data_LumpSum <- Fixture_Data_New}
}


############################################################################################
#########                                                                          #########
#########                     Fixture Data Preparation                             #########
#########                                                                          #########
############################################################################################

Start_Time <- Sys.time()
for (S_Loop in c("WSL","LumpSum")){
  if (S_Loop == "WSL") {
    Fixture_Data_Master$Country_Name_origin      <- as.character(Fixture_Data_Master$Country_Name_origin)
    Fixture_Data_Master$Country_Name_Destination <- as.character(Fixture_Data_Master$Country_Name_Destination)
    Fixture_Data_Master$CharterPartyCommodity    <- as.character(Fixture_Data_Master$CharterPartyCommodity)
    Fixture_Data_Master$Size                     <- as.numeric(as.character(Fixture_Data_Master$Size))
    Fixture_Data_Master$CharterPartyRate         <- as.numeric(as.character(Fixture_Data_Master$CharterPartyRate))
    Fixture_Data_Master$Trade_Region_origin      <- as.character(Fixture_Data_Master$Trade_Region_origin)
    Fixture_Data_Master$Trade_Region_destination <- as.character(Fixture_Data_Master$Trade_Region_destination)
    Fixture_Data_Master$LRIMOShipNo              <- as.character(Fixture_Data_Master$LRIMOShipNo)
    Fixture_Data_Master$FixtureID                <- as.character(Fixture_Data_Master$FixtureID)
    
    Fixture_2010_11_Data$Report_MonthYr           <- as.Date(as.character(Fixture_2010_11_Data$Report_MonthYr))
    Fixture_2010_11_Data$Country_Name_origin      <- as.character(Fixture_2010_11_Data$Country_Name_origin)
    Fixture_2010_11_Data$Country_Name_Destination <- as.character(Fixture_2010_11_Data$Country_Name_Destination)
    Fixture_2010_11_Data$CharterPartyCommodity    <- as.character(Fixture_2010_11_Data$CharterPartyCommodity)
    Fixture_2010_11_Data$Size                     <- as.numeric(as.character(Fixture_2010_11_Data$Size))
    Fixture_2010_11_Data$CharterPartyRate         <- as.numeric(as.character(Fixture_2010_11_Data$CharterPartyRate))
    Fixture_2010_11_Data$Trade_Region_origin      <- as.character(Fixture_2010_11_Data$Trade_Region_origin)
    Fixture_2010_11_Data$Trade_Region_destination <- as.character(Fixture_2010_11_Data$Trade_Region_destination)
    Fixture_2010_11_Data$LRIMOShipNo              <- as.character(Fixture_2010_11_Data$LRIMOShipNo)
    Fixture_2010_11_Data$FixtureID                <- as.character(Fixture_2010_11_Data$FixtureID)
    
    Fixture_Data <- Fixture_Data_Master
    Fixture_Data <- rbind(Fixture_Data,Fixture_2010_11_Data)
  }
  
  if (S_Loop == "LumpSum"){
    Fixture_Data <- Fixture_Data_LumpSum
    Fixture_Data$Country_Name_origin      <- as.character(Fixture_Data$Country_Name_origin)
    Fixture_Data$Country_Name_Destination <- as.character(Fixture_Data$Country_Name_Destination)
    Fixture_Data$CharterPartyCommodity    <- as.character(Fixture_Data$CharterPartyCommodity)
    Fixture_Data$Size                     <- as.numeric(as.character(Fixture_Data$Size))
    Fixture_Data$CharterPartyRate         <- as.numeric(as.character(Fixture_Data$CharterPartyRate))
    Fixture_Data$Trade_Region_origin      <- as.character(Fixture_Data$Trade_Region_origin)
    Fixture_Data$Trade_Region_destination <- as.character(Fixture_Data$Trade_Region_destination)
    Fixture_Data$LRIMOShipNo              <- as.character(Fixture_Data$LRIMOShipNo)
    Fixture_Data$FixtureID                <- as.character(Fixture_Data$FixtureID)
  }
  
  ######## New Regions Classification Code ######## 
  Fixture_Data$Country_Name_Destination  <- toupper(Fixture_Data$Country_Name_Destination)
  Fixture_Data$Country_Name_origin       <- toupper(Fixture_Data$Country_Name_origin)
  s <- as.character(c("THAILAND","INDONESIA","VIETNAM","PHILIPPINES","JAPAN","CHINA"))
  Test <- Fixture_Data[1,]
  for (i in c(1:length(s))){
    Test1 <- Fixture_Data[which(grepl(s[i],Fixture_Data$Country_Name_Destination)),]
    if (nrow(Test1) > 0 ) {Test <- rbind(Test,Test1)}
  }
  rm(Test1)
  Test <- Test[-1,]
  Test$Trade_Region_destination <- as.character(Test$Trade_Region_destination)
  Test$Country_Name_Destination <- as.character(Test$Country_Name_Destination)
  Test$Trade_Region_destination[which(Test$Country_Name_Destination %in% c("THAILAND","INDONESIA","VIETNAM","PHILIPPINES"))] <- "SOUTH EAST ASIA"
  Test$Trade_Region_destination[which(Test$Country_Name_Destination %in% c("JAPAN"))] <- "JAPAN"
  Test$Trade_Region_destination[which(Test$Country_Name_Destination %in% c("CHINA"))] <- "CHINA"
  Fixture_Data <- rbind(Fixture_Data,Test)
  
  Fixture_Data$Size_Class <- cut(Fixture_Data$Size, breaks=c(1,40000,55000,80000,125000,200000,1000000)-1, labels=paste('=',c(0,40,55,80,125,200),"to <",c(40,55,80,125,200,999)))
  Fixture_Data$Size_Class <- as.character(Fixture_Data$Size_Class)
  Fixture_Data$Size_Class[which(Fixture_Data$Size_Class == "= 0 to < 40")]    <- "Handy tanker + Handysize"
  Fixture_Data$Size_Class[which(Fixture_Data$Size_Class == "= 40 to < 55")]   <- "Handymax"
  Fixture_Data$Size_Class[which(Fixture_Data$Size_Class == "= 55 to < 80")]   <- "Panamax"
  Fixture_Data$Size_Class[which(Fixture_Data$Size_Class == "= 80 to < 125")]  <- "Aframax"
  Fixture_Data$Size_Class[which(Fixture_Data$Size_Class == "= 125 to < 200")] <- "Suezmax"
  Fixture_Data$Size_Class[which(Fixture_Data$Size_Class == "= 200 to < 999")] <- "VLCC + ULCC"
  
  Best_Models_Base_WSL$SizeClass <- as.character(Best_Models_Base_WSL$SizeClass)
  Best_Models_Base_WSL$Commodity <- as.character(Best_Models_Base_WSL$Commodity)
  a <- as.character(unique(Best_Models_Base_WSL$SizeClass)[!as.character(unique(Best_Models_Base_WSL$SizeClass)) %in% "All"])
  b <- gsub("^\\s+|\\s+$", "",unlist(strsplit(unique(Best_Models_Base_WSL$Commodity),"[+]")))
  
  Fixture_Data <- Fixture_Data[which(Fixture_Data$Size_Class %in% a & Fixture_Data$CharterPartyCommodity %in% b),]
  
  Fixture_Aggr_All_1 <- ddply(Fixture_Data,.(Trade_Region_origin,Trade_Region_destination,Report_MonthYr,Size_Class),summarize,WSL=mean(CharterPartyRate))
  chk <- ddply(Fixture_Data,.(Trade_Region_origin,Trade_Region_destination,Report_MonthYr),summarize,WSL=mean(CharterPartyRate))
  chk$Size_Class <-  "All"
  chk1 <- ddply(Fixture_Data,.(Size_Class,Report_MonthYr),summarize,WSL=mean(CharterPartyRate))
  chk1 <- as.data.frame(cbind(Trade_Region_origin = chk1$Size_Class, Trade_Region_destination = chk1$Size_Class, chk1))
  chk1 <- chk1[!is.na(chk1$Size_Class),]
  Fixture_Aggr_All_1 <- rbind(Fixture_Aggr_All_1,chk,chk1)
  rm(chk,chk1)
  
  Fixture_Aggr_All_1$Size_Class <- as.character(Fixture_Aggr_All_1$Size_Class)
  Fixture_Aggr_All_1$Size_Class[which(Fixture_Aggr_All_1$Size_Class == "= 0 to < 40")]    <- "Handy tanker + Handysize"
  Fixture_Aggr_All_1$Size_Class[which(Fixture_Aggr_All_1$Size_Class == "= 40 to < 55")]   <- "Handymax"
  Fixture_Aggr_All_1$Size_Class[which(Fixture_Aggr_All_1$Size_Class == "= 55 to < 80")]   <- "Panamax"
  Fixture_Aggr_All_1$Size_Class[which(Fixture_Aggr_All_1$Size_Class == "= 80 to < 125")]  <- "Aframax"
  Fixture_Aggr_All_1$Size_Class[which(Fixture_Aggr_All_1$Size_Class == "= 125 to < 200")] <- "Suezmax"
  Fixture_Aggr_All_1$Size_Class[which(Fixture_Aggr_All_1$Size_Class == "= 200 to < 999")] <- "VLCC + ULCC"
  
  Fixture_Aggr_All_1$Trade_Region_origin <- as.character(Fixture_Aggr_All_1$Trade_Region_origin)
  Fixture_Aggr_All_1$Trade_Region_origin[which(Fixture_Aggr_All_1$Trade_Region_origin == "= 0 to < 40")]    <- "Handy tanker + Handysize"
  Fixture_Aggr_All_1$Trade_Region_origin[which(Fixture_Aggr_All_1$Trade_Region_origin == "= 40 to < 55")]   <- "Handymax"
  Fixture_Aggr_All_1$Trade_Region_origin[which(Fixture_Aggr_All_1$Trade_Region_origin == "= 55 to < 80")]   <- "Panamax"
  Fixture_Aggr_All_1$Trade_Region_origin[which(Fixture_Aggr_All_1$Trade_Region_origin == "= 80 to < 125")]  <- "Aframax"
  Fixture_Aggr_All_1$Trade_Region_origin[which(Fixture_Aggr_All_1$Trade_Region_origin == "= 125 to < 200")] <- "Suezmax"
  Fixture_Aggr_All_1$Trade_Region_origin[which(Fixture_Aggr_All_1$Trade_Region_origin == "= 200 to < 999")] <- "VLCC + ULCC"
  
  Fixture_Aggr_All_1$Trade_Region_destination <- as.character(Fixture_Aggr_All_1$Trade_Region_destination)
  Fixture_Aggr_All_1$Trade_Region_destination[which(Fixture_Aggr_All_1$Trade_Region_destination == "= 0 to < 40")]    <- "Handy tanker + Handysize"
  Fixture_Aggr_All_1$Trade_Region_destination[which(Fixture_Aggr_All_1$Trade_Region_destination == "= 40 to < 55")]   <- "Handymax"
  Fixture_Aggr_All_1$Trade_Region_destination[which(Fixture_Aggr_All_1$Trade_Region_destination == "= 55 to < 80")]   <- "Panamax"
  Fixture_Aggr_All_1$Trade_Region_destination[which(Fixture_Aggr_All_1$Trade_Region_destination == "= 80 to < 125")]  <- "Aframax"
  Fixture_Aggr_All_1$Trade_Region_destination[which(Fixture_Aggr_All_1$Trade_Region_destination == "= 125 to < 200")] <- "Suezmax"
  Fixture_Aggr_All_1$Trade_Region_destination[which(Fixture_Aggr_All_1$Trade_Region_destination == "= 200 to < 999")] <- "VLCC + ULCC"
  
  Fixture_Aggr_All_1$Date <- as.Date(Fixture_Aggr_All_1$Report_MonthYr, format="%Y-%m-%d")
  Fixture_Aggr_All_1 <- subset(Fixture_Aggr_All_1,Fixture_Aggr_All_1$Date >= Training_Dates[1] & Fixture_Aggr_All_1$Date <= End_Date_1)
  
  Fixture_Aggr_All_1 <- data.frame(Fixture_Aggr_All_1[order(Fixture_Aggr_All_1$Date), ],row.names=NULL)
  
  #### Imputation Code
  Temp_Freq <- as.data.frame(table(Fixture_Aggr_All_1$Trade_Region_origin, Fixture_Aggr_All_1$Trade_Region_destination,Fixture_Aggr_All_1$Size_Class))
  Temp_Freq <- rename(Temp_Freq, c(Var1="Trade_Region_origin",Var2="Trade_Region_destination",Var3="Size_Class"))
  Temp_Freq <- Temp_Freq[which((Temp_Freq$Freq >= (Forecasting_Period_2 - 6)) & (Temp_Freq$Freq < Forecasting_Period_2)) ,]
  
  Fixture_Aggr_All_1$id <- paste0(Fixture_Aggr_All_1$Trade_Region_origin,Fixture_Aggr_All_1$Trade_Region_destination,Fixture_Aggr_All_1$Size_Class)
  Temp_Freq$id          <- paste0(Temp_Freq$Trade_Region_origin,Temp_Freq$Trade_Region_destination,Temp_Freq$Size_Class)
  Fixture_Aggr_Subset_1 <- Fixture_Aggr_All_1[which(!Fixture_Aggr_All_1$id %in% Temp_Freq$id),]
  Fixture_Aggr_Subset <- Fixture_Aggr_All_1[which(Fixture_Aggr_All_1$id %in% Temp_Freq$id),]
  Fixture_Aggr_Subset <- Fixture_Aggr_Subset[order(Fixture_Aggr_Subset$Trade_Region_origin,Fixture_Aggr_Subset$Trade_Region_destination,Fixture_Aggr_Subset$Size_Class,Fixture_Aggr_Subset$Date),]
  Fixture_Aggr_Subset <- Fixture_Aggr_Subset[,!names(Fixture_Aggr_Subset) %in% c("Date","id") ]
  Fixture_Aggr_T <- dcast(melt(Fixture_Aggr_Subset, id.vars=c("Trade_Region_origin","Trade_Region_destination","Report_MonthYr","Size_Class")),Trade_Region_origin+Trade_Region_destination+Size_Class~Report_MonthYr)
  
  for (m in c(6,5,4)){
    a <- as.numeric(which(is.na(Fixture_Aggr_T[,m])))
    if (length(a) != 0){
      for (i in c(a)){
        b <- mean(c(as.numeric(Fixture_Aggr_T[i,c((m+1):(m+3))])))
        Fixture_Aggr_T[i,m] <- b    
      }
    }
  }
  for (m in c(1:nrow(Fixture_Aggr_T))){
    a <- as.numeric(which(is.na(Fixture_Aggr_T[m,])))
    for (i in c(a)){
      b <- mean(c(as.numeric(Fixture_Aggr_T[m,c((i-3):(i-1))])))
      Fixture_Aggr_T[m,i] <- b
    }
  }
  
  Fixture_Aggr_Subset <- melt(Fixture_Aggr_T, id.vars=c("Trade_Region_origin","Trade_Region_destination","Size_Class"))
  colnames(Fixture_Aggr_Subset)[colnames(Fixture_Aggr_Subset) %in% c("variable","value")] <- paste(c("Report_MonthYr","WSL")) 
  Fixture_Aggr_Subset$Report_MonthYr <- as.Date(Fixture_Aggr_Subset$Report_MonthYr, format="%Y-%m-%d")
  Fixture_Aggr_Subset_1$Report_MonthYr <- as.Date(Fixture_Aggr_Subset_1$Report_MonthYr, format="%Y-%m-%d")
  Fixture_Aggr_Subset_1 <- Fixture_Aggr_Subset_1[,!names(Fixture_Aggr_Subset_1) %in% c("Date","id")]
  Fixture_Aggr_All_1 <- rbind(Fixture_Aggr_Subset_1,Fixture_Aggr_Subset)
  Fixture_Aggr_All_1$Date <- Fixture_Aggr_All_1$Report_MonthYr 
  
  #####Region and Size Class Summary
  Region_Size_Models <- as.data.frame(table(Fixture_Aggr_All_1$Trade_Region_origin, Fixture_Aggr_All_1$Trade_Region_destination,Fixture_Aggr_All_1$Size_Class))
  Region_Size_Models <- rename(Region_Size_Models, c(Var1="Trade_Region_origin",Var2="Trade_Region_destination",Var3="Size_Class"))
  Region_Size_Models <- Region_Size_Models[(which(Region_Size_Models$Freq==Forecasting_Period_2)),]
  Region_Size_Models <- data.frame(Region_Size_Models[order(Region_Size_Models$Trade_Region_origin,Region_Size_Models$Trade_Region_destination),],row.names=NULL)
  Region_Size_Models$Trade_Region_origin      <- as.character(Region_Size_Models$Trade_Region_origin)
  Region_Size_Models$Trade_Region_destination <- as.character(Region_Size_Models$Trade_Region_destination)
  Region_Size_Models <- subset(Region_Size_Models, Trade_Region_origin != "NONE" & Trade_Region_destination != "NONE") 
  Region_Size_Models$Combo <- paste(Region_Size_Models[,1], Region_Size_Models[,2], sep=" - ")
  Region_Size_Combos_1$Combo <- as.character(Region_Size_Combos_1$Combo)
  Combo <- paste(Region_Size_Combos_1[,1], Region_Size_Combos_1[,2], sep=" - ")
  Region_Size_Models$Combo <- as.character(Region_Size_Models$Combo)
  Region_Size_Models$Region_Route <- ifelse(Region_Size_Models$Combo %in% Combo, "Route", "Region")
  
  #Fixture Data Outlier Treatment
  library(moments)
  cv    =function(x){return(100*sd(x,na.rm=T)/mean(x,na.rm=T))}
  Range =function(x){return(max(x,na.rm=T)- min(x,na.rm=T))}
  N     =function(x){return(nrow(Dataset)-NROW(na.omit(x)))}
  summary_stats <- function(x)
  { return(c(
    nrow(Dataset),N(x),NROW(na.omit(x)),min(x,na.rm=T),quantile(x,c(.01,.05,.10,.25,.50,.75,.9,.95,.99),na.rm=T),
    max(x,na.rm=T),mean(x,na.rm=T),median(x,na.rm=T),sd(x,na.rm=T),cv(x),Range(x),IQR(x,na.rm=T),skewness(x,na.rm=T),kurtosis(x,na.rm=T)
  ))}
  Region_Size_Models$Size_Class <- as.character(Region_Size_Models$Size_Class)
  Summary_Data <-as.data.frame(c("OBS","N","NMIS","0% Min","1%","5%","10%","25% Q1","50% Median","75% Q3","90%","95%","99%","100% Max","Mean","Median","SD","CV (Mean/SD)","Range (Max-Min)","IQR (Q3-Q1)","Skewness(-1,+1)","Kurtosis(0,1)"))
  names(Summary_Data) <- "Stats"
  
  for (i in c(1: nrow(Region_Size_Models))){
    Dataset  <- subset(Fixture_Aggr_All_1,Trade_Region_origin == Region_Size_Models$Trade_Region_origin[i] & 
                         Trade_Region_destination == Region_Size_Models$Trade_Region_destination[i] &
                         Size_Class == Region_Size_Models$Size_Class[i])
    Summary_Data <- as.data.frame(cbind(Summary_Data,summary_stats(Dataset$WSL)))
    Var <- paste(as.character(Region_Size_Models[i,c(1,2,3)]), collapse=" - ")
    colnames(Summary_Data)[i+1] <- Var
  }
  Summary_Data[,-1] <-round(Summary_Data[,-1],2)
  
  for (i in c(2:ncol(Summary_Data))){
    Dif_95_99  <- round(((Summary_Data[13,i]-Summary_Data[12,i])/Summary_Data[12,i]),3)
    Dif_99_100 <- round(((Summary_Data[14,i]-Summary_Data[13,i])/Summary_Data[13,i]),3)
    a <- unlist(strsplit((colnames(Summary_Data)[i])," - "))
    b <-  which(Fixture_Aggr_All_1$Trade_Region_origin==a[1] & 
                  Fixture_Aggr_All_1$Trade_Region_destination==a[2] & 
                  Fixture_Aggr_All_1$Size_Class ==a[3])
    if (Dif_95_99  >= .25){Fixture_Aggr_All_1$WSL[b[(which(Fixture_Aggr_All_1$WSL[b] > Summary_Data[12,i]))]] <- Summary_Data[12,i]}
    if (Dif_99_100 >= .25){Fixture_Aggr_All_1$WSL[b[(which(Fixture_Aggr_All_1$WSL[b] > Summary_Data[13,i]))]] <- Summary_Data[13,i]}
  }
  rm(Dataset,a,b,Dif_95_99,Dif_99_100,i,Summary_Data)
  
  ######Missing Value Imputation Using KNN
  Fixture_Aggr_All_1 <- Fixture_Aggr_All_1[,c(1:5)]
  Fixture_Aggr_T <- dcast(melt(Fixture_Aggr_All_1, id.vars=c("Trade_Region_origin","Trade_Region_destination","Report_MonthYr","Size_Class")),Trade_Region_origin+Trade_Region_destination+Size_Class~Report_MonthYr)
  Fixture_Aggr_T$MissPer <- apply(Fixture_Aggr_T[, 4:ncol(Fixture_Aggr_T)], 1, function(x) sum(is.na(x))* 100 / length(x))
  Fixture_Aggr_T$Trade_Region_origin      <- as.factor(Fixture_Aggr_T$Trade_Region_origin)
  Fixture_Aggr_T$Trade_Region_destination <- as.factor(Fixture_Aggr_T$Trade_Region_destination)
  Fixture_Aggr_T$Size_Class               <- as.factor(Fixture_Aggr_T$Size_Class)
  #Fixture_Aggr_T_1 <- subset(Fixture_Aggr_T,MissPer <= '30' )
  #Fixture_Aggr_T_2 <- subset(Fixture_Aggr_T,MissPer > '30' )
  
  Fixture_Aggr_T_1 <- Fixture_Aggr_T[Fixture_Aggr_T$MissPer <= 30,]
  Fixture_Aggr_T_2 <- Fixture_Aggr_T[Fixture_Aggr_T$MissPer > 30,]
  
  if (S_Loop == "WSL"){
    Fixture_Aggr_T_1 <- knnImputation(Fixture_Aggr_T_1)
  }
  
  if (S_Loop == "LumpSum"){
    for (m in c(6,5,4)){
      a <- as.numeric(which(is.na(Fixture_Aggr_T_1[,m])))
      if (length(a) != 0){
        for (i in c(a)){
          b <- mean(c(as.numeric(Fixture_Aggr_T_1[i,c((m+1):(m+3))])))
          Fixture_Aggr_T[i,m] <- b    
        }
      }
    }
    for (m in c(1:nrow(Fixture_Aggr_T_1))){
      a <- as.numeric(which(is.na(Fixture_Aggr_T_1[m,])))
      for (i in c(a)){
        b <- mean(c(as.numeric(Fixture_Aggr_T_1[m,c((i-3):(i-1))])))
        Fixture_Aggr_T_1[m,i] <- b
      }
    }
  }
  Fixture_Aggr_All_1 <- Fixture_Aggr_T_1
  Fixture_Aggr_All_1 <- Fixture_Aggr_All_1[,!names(Fixture_Aggr_All_1) %in% "MissPer"]
  Fixture_Aggr_All_1 <- melt(Fixture_Aggr_All_1, id.vars=c("Trade_Region_origin","Trade_Region_destination","Size_Class"))
  Fixture_Aggr_All_1 <- Fixture_Aggr_All_1[which(Fixture_Aggr_All_1$value != "NA"),]
  names(Fixture_Aggr_All_1)[names(Fixture_Aggr_All_1) %in% c("variable","value")] <- c("Report_MonthYr","WSL")
  
  Fixture_Aggr_All_1$Trade_Region_origin <- as.character(Fixture_Aggr_All_1$Trade_Region_origin)
  Fixture_Aggr_All_1$Trade_Region_destination <- as.character(Fixture_Aggr_All_1$Trade_Region_destination)
  Fixture_Aggr_All_1$Size_Class <- as.character(Fixture_Aggr_All_1$Size_Class)
  Fixture_Aggr_All_1$Report_MonthYr <- as.Date(Fixture_Aggr_All_1$Report_MonthYr)
  #rm(Fixture_Aggr_T_1,Fixture_Aggr_T_2,Fixture_Aggr_T)
  
  #####Region and Size Class Summary
  Region_Size_Models <- as.data.frame(table(Fixture_Aggr_All_1$Trade_Region_origin, Fixture_Aggr_All_1$Trade_Region_destination,Fixture_Aggr_All_1$Size_Class))
  Region_Size_Models <- rename(Region_Size_Models, c(Var1="Trade_Region_origin",Var2="Trade_Region_destination",Var3="Size_Class"))
  Region_Size_Models <- Region_Size_Models[(which(Region_Size_Models$Freq==Forecasting_Period_2)),]
  Region_Size_Models <- data.frame(Region_Size_Models[order(Region_Size_Models$Trade_Region_origin,Region_Size_Models$Trade_Region_destination),],row.names=NULL)
  Region_Size_Models$Trade_Region_origin      <- as.character(Region_Size_Models$Trade_Region_origin)
  Region_Size_Models$Trade_Region_destination <- as.character(Region_Size_Models$Trade_Region_destination)
  Region_Size_Models <- subset(Region_Size_Models, Trade_Region_origin != "NONE" & Trade_Region_destination != "NONE") 
  Region_Size_Models$Combo <- paste(Region_Size_Models[,1], Region_Size_Models[,2], sep=" - ")
  Region_Size_Combos_1$Combo <- as.character(Region_Size_Combos_1$Combo)
  Combo <- paste(Region_Size_Combos_1[,1], Region_Size_Combos_1[,2], sep=" - ")
  Region_Size_Models$Combo <- as.character(Region_Size_Models$Combo)
  Region_Size_Models$Region_Route <- ifelse(Region_Size_Models$Combo %in% Combo, "Route", "Region")
  
  #######  Removing Globale Models #######
  Region_Size_Models <- Region_Size_Models[!Region_Size_Models$Trade_Region_origin %in% c("Aframax","Suezmax","VLCC + ULCC"),]
  #Region_Size_Models <- Region_Size_Models[-1,]
  
  if (S_Loop == "WSL"){
    test <- Best_Models_Base_WSL[,c("Region","SizeClass")]
    test <- unique(test)
    Region_Size_Models <- merge(x =Region_Size_Models, y =test ,
                                by.x = c("Combo","Size_Class"), 
                                by.y = c("Region","SizeClass"), all.y=TRUE)
    rm(test)
  }
  
  if (S_Loop == "LumpSum"){
    test <- Best_Models_Base_LumpSum[,c("Region","SizeClass")]
    test <- unique(test)
    Region_Size_Models <- merge(x =Region_Size_Models, y =test ,
                                by.x = c("Combo","Size_Class"), 
                                by.y = c("Region","SizeClass"), all.y=TRUE)
    rm(test)
  }
  
  Actuals <- Fixture_Aggr_All_1
  Actuals$Month <- as.numeric(format(Actuals$Report_MonthYr,format="%m"))
  Actuals$Year <- as.numeric(format(Actuals$Report_MonthYr,format="%Y"))
  Actuals$View <- "Actual"
  if (S_Loop == "WSL"){Actuals$ValueType <- "Worldscale"
                       Actuals <- Actuals[,c("Trade_Region_origin","Trade_Region_destination","Size_Class","ValueType","View","Year","Month","WSL")]
                       Actuals_WSL <- Actuals
  }  
  if (S_Loop == "LumpSum"){Actuals$ValueType <- "LumpSum"
                           Actuals <- Actuals[,c("Trade_Region_origin","Trade_Region_destination","Size_Class","ValueType","View","Year","Month","WSL")]
                           Actuals_LumpSum <- Actuals
  }
  
  # ------ Avg_5Years Code ------
  Base_Data <- Fixture_Aggr_All_1
  Base_Data$Year <- year(Base_Data$Report_MonthYr)
  Base_Data$Month <- month(Base_Data$Report_MonthYr)
  Base_Data$Report_MonthYr <- NULL
  Base_Data$CharterPartyCommodity <- "Crude"
  Base_Data_T <- dcast(melt(Base_Data, id.vars=c("CharterPartyCommodity","Trade_Region_origin","Trade_Region_destination","Size_Class","Month","Year")),CharterPartyCommodity+Trade_Region_origin+Trade_Region_destination+Size_Class+Month~Year)
  col <- c("CharterPartyCommodity","Trade_Region_origin","Trade_Region_destination","Size_Class","Month")
  year <- year(Sys.Date())
  year_ref <- as.character(seq(year-1,by=-1,length.out=5))
  Base_Data_T <- Base_Data_T[,colnames(Base_Data_T) %in% c(col,year_ref)]
  
  Base_Data_T$Avg_5Years <-apply(Base_Data_T[,colnames(Base_Data_T) %in% year_ref],1,mean)
  Avg_fixtures <- Base_Data_T[,colnames(Base_Data_T) %in% c(col,"Avg_5Years")]
  if (S_Loop == "WSL")    {Avg_fixtures$ValueType <- "Worldscale"}
  if (S_Loop == "LumpSum"){Avg_fixtures$ValueType <- "Lumpsum"}
  Avg_fixtures$Family <- "Tanker"
  Avg_fixtures$Region <- paste(Avg_fixtures$Trade_Region_origin,Avg_fixtures$Trade_Region_destination,sep=" - ")
  setnames(Avg_fixtures,old = c("CharterPartyCommodity","Size_Class","Trade_Region_origin","Trade_Region_destination"),new = c("Commodity","SizeClass","Origin","Destination"))
  if (S_Loop == "WSL"){Avg_fixtures_WSL         <- Avg_fixtures}
  if (S_Loop == "LumpSum"){Avg_fixtures_LumpSum <- Avg_fixtures}
  
  if (S_Loop == "WSL"){ 
    Dollar_Conversion$Size_Class <- as.character(Dollar_Conversion$Size_Class)
    Dollar_Conversion$Region <- paste(Dollar_Conversion$Trade_Region_origin,Dollar_Conversion$Trade_Region_destination,sep = " - ")
    Dollar_Conversion_1 <- Dollar_Conversion[,!colnames(Dollar_Conversion) %in% c("Load_Port","Discharge_Port","Trade_Region_origin","Trade_Region_destination","YR2009","YR2010","YR2011","YR2012","YR2013","YR2014","YR2016")]
    #Dollar_Conversion_1 <- Dollar_Conversion[,!colnames(Dollar_Conversion) %in% c("Load_Port","Discharge_Port","Trade_Region_origin","Trade_Region_destination","YR2009","YR2010","YR2011","YR2012","YR2013","YR2014","YR2015")]
    
    test <- unique(Best_Models_Base_WSL[,c("Region","SizeClass")])
    
    Avg_fixtures_WSL <- merge(x = Avg_fixtures_WSL, y = test,
                              by.x = c("Region","SizeClass"), 
                              by.y = c("Region","SizeClass"), all.y=TRUE)
    
    Avg_fixtures_WSL <- merge(x = Avg_fixtures_WSL, y = Dollar_Conversion_1,
                               by.x = c("Region","SizeClass"), 
                               by.y = c("Region","Size_Class"), all.x=TRUE)
   
    Avg_fixtures_WSL$DLR_Per_Ton <- (Avg_fixtures_WSL$Avg_5Years*Avg_fixtures_WSL$YR2015)/100
    #Avg_fixtures_WSL$DLR_Per_Ton <- (Avg_fixtures_WSL$Avg_5Years*Avg_fixtures_WSL$YR2016)/100
    
    Avg_fixtures_WSL <- Avg_fixtures_WSL[,!colnames(Avg_fixtures_WSL) %in% "YR2015"]
    #Avg_fixtures_WSL <- Avg_fixtures_WSL[,!colnames(Avg_fixtures_WSL) %in% "YR2016"]
    
    Lookup <- Lookup_1
    Lookup$Region      <- as.character(Lookup$Region)
    Lookup$SizeClass   <- as.character(Lookup$SizeClass)
    
    Lookup[,c(6:ncol(Lookup))] <- Lookup$Distance /(Lookup[,c(6:ncol(Lookup))]*24)
    Lookup <- Lookup[,!colnames(Lookup) %in% "Distance"]
    colnames(Lookup)[5:ncol(Lookup)] <- c(1:12)
    Lookup <- melt(Lookup, id.vars=c("Region","SizeClass","averagecommodityweight","turnaroundtime"))
    colnames(Lookup)[colnames(Lookup) %in% "variable"] <- "Month"
    colnames(Lookup)[colnames(Lookup) %in% "value"]    <- "No_of_Days"
    
    Lookup$Month           <- as.numeric(Lookup$Month)
    Avg_fixtures_WSL$Month <- as.numeric(Avg_fixtures_WSL$Month)
    
    Lookup <- Lookup[order(Lookup$Region,Lookup$SizeClass,Lookup$Month),]
    Avg_fixtures_WSL <- Avg_fixtures_WSL[order(Avg_fixtures_WSL$Region,Avg_fixtures_WSL$SizeClass,Avg_fixtures_WSL$Month),]
    
    Avg_fixtures_WSL <- merge(x = Avg_fixtures_WSL, y = Lookup,
                              by.x = c("Region","SizeClass","Month"), 
                              by.y = c("Region","SizeClass","Month"), all.x=TRUE)
    
    Avg_fixtures_WSL$DLR_Per_Day <- (Avg_fixtures_WSL$DLR_Per_Ton*Avg_fixtures_WSL$averagecommodityweight)/
                                     ((2*Avg_fixtures_WSL$No_of_Days)+Avg_fixtures_WSL$turnaroundtime)
    Avg_fixtures_WSL <- Avg_fixtures_WSL[,!colnames(Avg_fixtures_WSL) %in% c("averagecommodityweight","turnaroundtime","No_of_Days")]
    Avg_fixtures_WSL <- Avg_fixtures_WSL[order(Avg_fixtures_WSL$Region,Avg_fixtures_WSL$SizeClass,Avg_fixtures_WSL$Month),]
    
    Avg_fixtures_DLR_Per_Ton <- Avg_fixtures_WSL[,c("Region","SizeClass","Month","Commodity","Origin","Destination","ValueType","Family","DLR_Per_Ton")]
    Avg_fixtures_DLR_Per_Day <- Avg_fixtures_WSL[,c("Region","SizeClass","Month","Commodity","Origin","Destination","ValueType","Family","DLR_Per_Day")]
    Avg_fixtures_WSL         <- Avg_fixtures_WSL[,c("Region","SizeClass","Month","Commodity","Origin","Destination","ValueType","Family","Avg_5Years")]
    
    colnames(Avg_fixtures_DLR_Per_Ton)[colnames(Avg_fixtures_DLR_Per_Ton) %in% "DLR_Per_Ton"] <- "Avg_5Years"
    Avg_fixtures_DLR_Per_Ton$ValueType <- "$ Tonne"
    colnames(Avg_fixtures_DLR_Per_Day)[colnames(Avg_fixtures_DLR_Per_Day) %in% "DLR_Per_Day"] <- "Avg_5Years"
    Avg_fixtures_DLR_Per_Day$ValueType <- "$ Day"
    
    Avg_fixtures_WSL <- rbind(Avg_fixtures_WSL,Avg_fixtures_DLR_Per_Ton,Avg_fixtures_DLR_Per_Day)
    Avg_fixtures_WSL <- Avg_fixtures_WSL[order(Avg_fixtures_WSL$ValueType ,Avg_fixtures_WSL$Region,Avg_fixtures_WSL$SizeClass,Avg_fixtures_WSL$Month),]
    }

  if (S_Loop == "LumpSum"){
    test <- unique(Best_Models_Base_LumpSum[,c("Region","SizeClass")])
    Avg_fixtures_LumpSum <- merge(x = Avg_fixtures_LumpSum, y = test,
                              by.x = c("Region","SizeClass"), 
                              by.y = c("Region","SizeClass"), all.y=TRUE)
    
    Lookup             <- master
    Lookup$Region      <- as.character(Lookup$Region)
    Lookup$SizeClass   <- as.character(Lookup$SizeClass)
    Lookup             <- Lookup[,!colnames(Lookup) %in% c("Load_Port","Discharge_Port")]
    
    Avg_fixtures_LumpSum <- merge(x = Avg_fixtures_LumpSum, y = Lookup,
                                 by.x = c("Region","SizeClass"), 
                                 by.y = c("Region","SizeClass"), all.x=TRUE)
    Avg_fixtures_LumpSum$DLR_Per_Ton <- Avg_fixtures_LumpSum$Avg_5Years/Avg_fixtures_LumpSum$averagecommodityweight
    Avg_fixtures_LumpSum$DLR_Per_Day <- Avg_fixtures_LumpSum$Avg_5Years/((2*Avg_fixtures_LumpSum$Days_At_Sea)+Avg_fixtures_LumpSum$turnaroundtime)
  
    Avg_fixtures_LumpSum <- Avg_fixtures_LumpSum[,!colnames(Avg_fixtures_LumpSum) %in% c("averagecommodityweight","turnaroundtime","Days_At_Sea","Distance")]
    Avg_fixtures_LumpSum <- Avg_fixtures_LumpSum[order(Avg_fixtures_LumpSum$Region,Avg_fixtures_LumpSum$SizeClass,Avg_fixtures_LumpSum$Month),]
    
    Avg_fixtures_DLR_Per_Ton <- Avg_fixtures_LumpSum[,c("Region","SizeClass","Month","Commodity","Origin","Destination","ValueType","Family","DLR_Per_Ton")]
    Avg_fixtures_DLR_Per_Day <- Avg_fixtures_LumpSum[,c("Region","SizeClass","Month","Commodity","Origin","Destination","ValueType","Family","DLR_Per_Day")]
    Avg_fixtures_LumpSum         <- Avg_fixtures_LumpSum[,c("Region","SizeClass","Month","Commodity","Origin","Destination","ValueType","Family","Avg_5Years")]
    
    colnames(Avg_fixtures_DLR_Per_Ton)[colnames(Avg_fixtures_DLR_Per_Ton) %in% "DLR_Per_Ton"] <- "Avg_5Years"
    Avg_fixtures_DLR_Per_Ton$ValueType <- "$ Tonne"
    colnames(Avg_fixtures_DLR_Per_Day)[colnames(Avg_fixtures_DLR_Per_Day) %in% "DLR_Per_Day"] <- "Avg_5Years"
    Avg_fixtures_DLR_Per_Day$ValueType <- "$ Day"
    
    Avg_fixtures_LumpSum <- rbind(Avg_fixtures_LumpSum,Avg_fixtures_DLR_Per_Ton,Avg_fixtures_DLR_Per_Day)
    Avg_fixtures_LumpSum <- Avg_fixtures_LumpSum[order(Avg_fixtures_LumpSum$ValueType ,Avg_fixtures_LumpSum$Region,Avg_fixtures_LumpSum$SizeClass,Avg_fixtures_LumpSum$Month),]
    
    Avg_fixtures_Final <- rbind(Avg_fixtures_WSL,Avg_fixtures_LumpSum)
    }
  
  #####################################################################################################
  ###########                                                                               ###########
  ###########                              Preparing X Vars Data                            ###########
  ###########                                                                               ###########
  #####################################################################################################
  
  if (S_Loop == "WSL") {
  ## Setting up the connection with sql server ##
  connection <- odbcDriverConnect("Driver=SQL Server; Server=10.45.88.171; Database=IDDS03; Uid=maritime; Pwd=trial,123")
  
  ## Running extracting the data from table for specific variables.
  ## Purpose is to get the series id details for each mnemonic
  
  names <-  c('POILBNT$@WOR.M', 'POILWTI$@WOR.M',  'RX@WOR.M',  
              'IP@GBR.M',  'WPI$@BRA.M',  'WPI$@CHN.M',	'WPI$@IND.M',	
              'WPI$@USA.M',	'WPI$@AUS.M',	'WPI$@IDN.M',	'WPI$@ZAF.M',	
              'IP@WOR.M',	'IP@WOR.M', 'RMSPREAD@AUS.M',	'RMSPREAD@BRA.M',	
              'RMSPREAD@IND.M',	'RMSPREAD@ITA.M',	'RMSPREAD@JPN.M',	'RMSPREAD@NGA.M',	
              'RMSPREAD@USA.M',	'RMSPREAD@ZAF.M',	'RMLONG@DEU.M',	'RMLONG@ESP.M',	
              'RMLONG@FRA.M',	'RMLONG@ITA.M',	'RMLONG@GBR.M',	'RMLONG@USA.M',	'RMSHORT@USA.M',	
              'SRT%@BRA.M',	'SRT%@CHN.M',	'SRTR%@AUS.M',	'SRTR%@CHN.M',	'SRTR%@DEU.M',	
              'SRTR%@ESP.M',	'SRTR%@FRA.M',	'SRTR%@ITA.M',	'SRTR%@GBR.M',	'SRTR%@JPN.M',	
              'SRTR%@USA.M',	'SRTR%@ZAF.M','IP@EUN.M','IP@EMU.M',
              'IP@IND.M','IP@CHN.M','IP@USA.M','IP@ZAF.M','IP@IDN.M','IP@NGA.M')
  names1 <- paste0("'", names, "'", collapse = ",")
  
  PURCHDE1 <- sqlQuery(connection, sprintf("SELECT series_id, mnemonic, Dri_mnemonic, Wefa_mnemonic, startdate, enddate 
                                           FROM series_attr where mnemonic in (%s)",names1))
  
  series_id <- PURCHDE1[,1]
  series_id <- paste0(series_id, collapse = ",")
  
  ## Extracting data from SQL table based on series id.
  PURCHDE2 <- sqlQuery(connection, sprintf("select * from series_data where series_id in (%s) and (Date >= Convert(date, '2009-01-01'))",series_id))
  
  ## Preparing final data. getting variable names & mnemonic based on series id.
  rawDataMonthly <- data.frame(Report_MonthYr = as.Date(PURCHDE2$date, format = "%m/%d/%Y") ,series_id=PURCHDE2$series_id, 
                               VariableName=PURCHDE1[match(PURCHDE2$series_id, PURCHDE1$series_id), "Wefa_mnemonic"],
                               mnemonic = PURCHDE1[match(PURCHDE2$series_id, PURCHDE1$series_id), "Dri_mnemonic"],
                               datavalue = PURCHDE2$datavalue)
  
  ### Extracting Yearly data.
  connection <- odbcDriverConnect("Driver=SQL Server; Server=10.45.88.171; Database=IDDS03; Uid=maritime; Pwd=trial,123")
  names2 <- gsub("\\.\\M","\\.\\A",names1)
  PURCHDEYear1 <- sqlQuery(connection, sprintf("SELECT series_id, mnemonic, Dri_mnemonic, Wefa_mnemonic, startdate, enddate 
                                               FROM series_attr  where mnemonic in (%s)",names2))
  PURCHDEYear1 <- PURCHDEYear1[order(PURCHDEYear1$mnemonic),]
  PURCHDEYear1 <-do.call(rbind, lapply(split(PURCHDEYear1, PURCHDEYear1$mnemonic), head, 1))
  
  series_id_A <- PURCHDEYear1[,1]
  series_id_A <- paste0(series_id_A, collapse = ",")
  ## Extracting data from SQL table based on series id.
  PURCHDEYear2 <- sqlQuery(connection, sprintf("select * from series_data where series_id in (%s) and (Date > Convert(date, '2015-01-01') and Date <= convert(date, '2025-12-01'))
                                               ",series_id_A))
  
  rawDataYearly <- data.frame(Report_Yr = as.Date(PURCHDEYear2$date, format = "%m/%d/%Y"), series_id=PURCHDEYear2$series_id, 
                              VariableName=PURCHDEYear1[match(PURCHDEYear2$series_id, PURCHDEYear1$series_id), 
                                                        "Wefa_mnemonic"],
                              mnemonic = PURCHDEYear1[match(PURCHDEYear2$series_id, PURCHDEYear1$series_id), 
                                                      "Dri_mnemonic"],
                              datavalue = PURCHDEYear2$datavalue)
  
  finalOut <- as.data.frame(seq(as.Date("2009-01-01"), as.Date("2025-12-01"), by = "month"))
  colnames(finalOut) <- "Report_MonthYr"
  
  for(i in unique(as.character(rawDataMonthly$VariableName))) {
    output <- as.data.frame(subset(rawDataMonthly, rawDataMonthly$VariableName == i & rawDataMonthly$Report_MonthYr <= '2025-12-01'))
    output <- output[,-c(2:4)]
    colnames(output)[2] <- i
    output <- output[order(output$Report_MonthYr), ]
    maxYear <- format(max(output$Report_MonthYr)+1, '%Y')
    if(max(output$Report_MonthYr) != '2025-12-01' ){
      rawDataYearlyTemp <- subset(rawDataYearly, rawDataYearly$VariableName == i, select = c(Report_Yr,VariableName,series_id ,datavalue))
      rawDataYearlyTemp$year <- as.numeric(format(rawDataYearlyTemp$Report_Yr, '%Y'))
      years <- max(rawDataYearlyTemp$year) - min(rawDataYearlyTemp$year)
      years <- ((years+1)*12)-1 
      ym <- zooreg(rawDataYearlyTemp$datavalue, start=min(rawDataYearlyTemp$year), frequency=1) 
      ym <- round(na.spline(ym, xout=as.yearmon(start(ym)+(0:years)/12)),2)
      ym <- as.data.frame(ym)
      ym$Report_MonthYr <- as.Date(gsub("\\s", "",paste0("01 ",rownames(ym))),format="%d%B%Y")
      colnames(ym)[colnames(ym) %in% "ym"] <- i
      ym_temp <- subset(ym, ym$Report_MonthYr >= max(output$Report_MonthYr) + 1)
      output <- rbind(output,ym_temp)
    }
    finalOut <- merge(finalOut, output, by = "Report_MonthYr", all.x=T)
    #if(nrow(finalOut) < 204)break
  }
  
  setnames(finalOut, 
           old = c("IP_CHN","IP_EUN","IP_EMU","IP_GBR","IP_IDN","IP_IND",
                   "IP_NGA","IP_USA","IP_WOR","IP_ZAF","RMLONG_DEU","RMLONG_ESP",
                   "RMLONG_FRA","RMLONG_GBR","RMLONG_ITA","RMLONG_USA",
                   "RMSHORT_USA","WPI$_AUS","WPI$_BRA","WPI$_CHN","WPI$_IDN","WPI$_IND","WPI$_USA",
                   "RMSPREAD_AUS","RMSPREAD_ITA","RMSPREAD_JPN","RMSPREAD_NGA","RMSPREAD_USA","RMSPREAD_ZAF",
                   "SRT%_CHN","SRTR%_AUS","SRTR%_CHN","SRTR%_DEU","SRTR%_ESP","SRTR%_FRA","SRTR%_GBR","SRTR%_ITA","SRTR%_JPN", 
                   "SRTR%_USA","POILWTI$_WOR","RX_WOR","RMSPREAD_IND","SRT%_BRA","RMSPREAD_BRA",
                   "POILBNT$_WOR"), 
           
           new = c("IP_CHN","Eurozone_IPI","EuropeanUnion_IPI","IP_GBR","IPN_IDN","IP_IND",
                   "IPN_NGA","IPN_USA","IPN_WOR","IPN_ZAF","LongTerm_BondYields_DEU","LongTerm_BondYields_ESP",
                   "LongTerm_BondYields_FRA","LongTerm_BondYields_GBR","LongTerm_BondYields_ITA","LongTerm_BondYields_USA",
                   "ShortTerm_IR_USA","WPI_AUS","WPI_BRA","WPI_CHN","WPI_IDN","WPI_IND","WPI_USA",
                   "RMSPREAD_AUS","RMSPREAD_ITA","RMSPREAD_JPN","RMSPREAD_NGA","RMSPREAD_USA","RMSPREAD_ZAF",
                   "SRT_CHN","SRTR_AUS","SRTR_CHN","SRTR_DEU","SRTR_ESP","SRTR_FRA","SRTR_GBR","SRTR_ITA","SRTR_JPN",
                   "SRTR_USA","WTI_price","World_Exchange_Rate","RMSPREAD_IND","SRT_BRA","RMSPREAD_BRA",
                   "Crude_Price"))
 
  ZFA_Vars <- read.csv("E:\\Freight Rate V1\\results\\ZFA Vars.csv")
  finalOut <- cbind(finalOut,WPI_ZAF = ZFA_Vars$WPI_ZAF,SRTR_ZAF = ZFA_Vars$SRTR_ZAF)
  
  finalOut <- finalOut[,  c("Report_MonthYr","IP_CHN","Eurozone_IPI","EuropeanUnion_IPI","IP_GBR","IPN_IDN","IP_IND",
                            "IPN_NGA","IPN_USA","IPN_WOR","IPN_ZAF","LongTerm_BondYields_DEU","LongTerm_BondYields_ESP",
                            "LongTerm_BondYields_FRA","LongTerm_BondYields_GBR","LongTerm_BondYields_ITA","LongTerm_BondYields_USA",
                            "ShortTerm_IR_USA","WPI_AUS","WPI_BRA","WPI_CHN","WPI_IDN","WPI_IND","WPI_USA","WPI_ZAF",
                            "RMSPREAD_AUS","RMSPREAD_ITA","RMSPREAD_JPN","RMSPREAD_NGA","RMSPREAD_USA","RMSPREAD_ZAF",
                            "SRT_CHN","SRTR_AUS","SRTR_CHN","SRTR_DEU","SRTR_ESP","SRTR_FRA","SRTR_GBR","SRTR_ITA","SRTR_JPN",
                            "SRTR_USA","SRTR_ZAF","WTI_price","World_Exchange_Rate","RMSPREAD_IND","SRT_BRA","RMSPREAD_BRA",
                            "Crude_Price")]
  
  finalOut$Crude_Price[is.na(finalOut$Crude_Price)] <-  Brent_Future$Crude_Price[is.na(finalOut$Crude_Price)]
  finalOut$WTI_price[is.na(finalOut$WTI_price)] <-  Brent_Future$WTI_price[is.na(finalOut$WTI_price)]
  
  finalOut$IPN_CHN <- finalOut$IP_CHN
  finalOut$IPN_IND <- finalOut$IP_IND 
  
  finalOut$SRTR_EU <-  rowMeans(finalOut[,c("SRTR_DEU","SRTR_ESP","SRTR_FRA","SRTR_ITA")])
  finalOut$LongTerm_BondYields_EU <- rowMeans(finalOut[,c("LongTerm_BondYields_DEU",
                                                          "LongTerm_BondYields_ESP",
                                                          "LongTerm_BondYields_FRA",
                                                          "LongTerm_BondYields_ITA")])
  
  Brent_Future$Report_MonthYr <- as.Date(Brent_Future$Report_MonthYr,format = "%m/%d/%Y") 
  Brent_Future$Brent_Future_Lead4 <- c(Brent_Future$Brent_future[c(-1,-2,-3,-4)],NA,NA,NA,NA)
  Brent_Future <- Brent_Future[,!colnames(Brent_Future) %in% c("Crude_Price","WTI_price")]
  finalOut <- merge(finalOut, Brent_Future, by = "Report_MonthYr")
  finalOut$Brent_Premium <-finalOut$Brent_Future_Lead4 - finalOut$Crude_Price
  finalOut <- finalOut[,!colnames(finalOut) %in% "Brent_Future_Lead4"]
  
  connection <- odbcDriverConnect("Driver=SQL Server; Server=10.45.88.171; Database=RA; Uid=maritime; Pwd=trial,123")
  #Ship_Utility <- sqlQuery(connection, "SELECT * From  FR_X_Vars_ShipUtility" )
  #Ship_Utility$Report_MonthYr <- as.Date(as.character(Ship_Utility$Report_MonthYr),"%Y-%m-%d")
  Ship_Utility <- read.csv("E:\\Freight Rate V1\\results\\Ship_Utility.csv")
  Ship_Utility$Report_MonthYr <- as.Date(as.character(Ship_Utility$Report_MonthYr),"%m/%d/%Y")
  finalOut <- merge(finalOut, Ship_Utility, by = "Report_MonthYr", all.x= T)
  X_Vars_All1 <- finalOut 
  X_Vars_All1$Date <- as.Date(X_Vars_All1$Report_MonthYr, format="%m/%d/%Y")
  #close(connection)
  rm("Brent_Future" ,"finalOut","i","maxYear","names","names1","names2","output","PURCHDE1","PURCHDE2","PURCHDEYear1","PURCHDEYear2",
     "rawDataMonthly","rawDataYearly","rawDataYearlyTemp","series_id","series_id_A","Ship_Utility","years","ym","ym_temp")          
}
    
  if (S_Loop == "WSL"){
    ###########   ShipUtility Forecast    ############
    x <- which(grepl("Ship_Util",colnames(X_Vars_All1)))
    y <- colnames(X_Vars_All1[x])
    Util_Forecast <- subset(X_Vars_All1, X_Vars_All1$Date >= Training_Dates[1] & X_Vars_All1$Date <= Training_Dates[2])
    Util_Forecast <- Util_Forecast[,c("Report_MonthYr",y)]
    F_1 <- add.months(Training_Dates[2],1)
    F_2 <- add.months(Training_Dates[2],Forecasting_Period)
    
    q  <- length(x)
    pb = txtProgressBar(min = 0, max = q, initial = 0, char = ">", width = NA, title="ShipUtility Forecasting", label=="ShipUtility Forecasting", style = 3)
    
    options(show.error.messages=F) 
    for (i in c(1:length(x))){
      TS <- ts(Util_Forecast[,1+i],frequency=12)
      arMod <- try(auto.arima(TS,xreg = seasonaldummy(TS)))
      if (class(arMod)[1] == "try-error") next
      Forecast <- forecast(arMod, h=Forecasting_Period,xreg=seasonaldummyf(TS, Forecasting_Period))
      Forecast <- as.data.frame(round(Forecast$mean,4)) 
      X_Vars_All1[which(X_Vars_All1$Date >= F_1 & X_Vars_All1$Date <= F_2),y[i]] <- Forecast
      #setTkProgressBar(pb, i, sprintf("ShipUtility Forecasting (%s)", (sprintf("%d%% done", round((i/q)*100))), (sprintf("%d%% done", round((i/q)*100)))))
      if (i==1 ) {print("ShipUtility Forecasting")}
      setTxtProgressBar(pb, i )
    }
    #rm(Forecast,Util_Forecast,x,y,F_1,F_2)
    close(pb)
    options(show.error.messages=T)
    
    #########Creating Lag Variables For ShipUtility and oil Related Varaibles#########
    
    shift<-function(x,shift_by){ 
      stopifnot(is.numeric(shift_by)) 
      stopifnot(is.numeric(x)) 
      
      if (length(shift_by)>1) 
        return(sapply(shift_by,shift, x=x)) 
      
      out<-NULL
      abs_shift_by=abs(shift_by) 
      if (shift_by > 0 ) 
        out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by)) 
      else if (shift_by < 0 ) 
        out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by)) 
      else 
        out<-x 
      out 
    }
    #Lags For oil Related Variables
    X_Vars1 <- X_Vars_All1[,c("Report_MonthYr","Brent_future","WTI_price","Brent_Premium","Crude_Price")]
    for (k in c(2,3,4,5)){
      for (i in c(1:4)){   
        X<-shift(X_Vars1[,k],-i)
        X_Vars1 <- cbind(X_Vars1, X)
        colnames(X_Vars1)[length(colnames(X_Vars1))] <- paste((colnames(X_Vars1[k])), i, sep="_Lag_")
      }}
    rm(i,k,X)
    #Lags For Ship Utility Related Variables
    Vars_Names <- which(grepl("Ship_Util",colnames(X_Vars_All1)))
    X_Vars2 <- X_Vars_All1[,c(1,Vars_Names)]
    for (k in c(2:ncol(X_Vars2))){
      for (i in c(1:2)){
        X<-shift(X_Vars2[,k],-i)
        X_Vars2 <- cbind(X_Vars2, X)
        colnames(X_Vars2)[length(colnames(X_Vars2))] <- paste((colnames(X_Vars2[k])), i, sep="_Lag_")
      }}
    rm(i,k,X)
    
    Var_Names <- unique(c(names(X_Vars1),names(X_Vars2)))
    X_Vars_All <- cbind(X_Vars1,X_Vars2[,-1],X_Vars_All1[, !names(X_Vars_All1) %in% c(Var_Names)])
    
    ##### Functional Froms Of Variables #####
    X_Vars_All$dlog_WTI_L1_Brent_L1 <- round(c(NA,diff(log(X_Vars_All$Brent_future_Lag_1/X_Vars_All$WTI_price_Lag_1))),3)
    X_Vars_All$dlog_WTI_L2_Brent_L2 <- round(c(NA,diff(log(X_Vars_All$Brent_future_Lag_2/X_Vars_All$WTI_price_Lag_2))),3)
    #X_Vars_All$SMA3_Diff_WTI_Brent  <- SMA(c(NA,diff(X_Vars_All$WTI_price - X_Vars_All$Brent_future)),3)
    X_Vars_All$Diff_Brent_L1_WTI_L1 <- round(c(NA,diff(X_Vars_All$Brent_future_Lag_1 - X_Vars_All$WTI_price_Lag_1)),3)
    X_Vars_All$Diff_Brent_L2_WTI_L2 <- round(c(NA,diff(X_Vars_All$Brent_future_Lag_2 - X_Vars_All$WTI_price_Lag_2)),3)
    X_Vars_All$Diff_Brent_Premium   <- round(c(NA,diff(X_Vars_All$Brent_Premium)),3)
    X_Vars_All$Diff_Brent_Premium_L2   <- round(c(NA,diff(X_Vars_All$Brent_Premium_Lag_2)),3)
    X_Vars_All$Diff_Brent_Premium_L3   <- round(c(NA,diff(X_Vars_All$Brent_Premium_Lag_3)),3)
    a <- which(grepl("IPN",colnames(X_Vars_All)))
    b <- which(grepl("WPI",colnames(X_Vars_All)))
    a <- c(a,b)
    for (k in a){
      for (m in c(1,2,3)){   
        X <- shift(X_Vars_All[,k],-m)
        X <- round(c(NA,diff(log(X))),3)
        X_Vars_All <- cbind(X_Vars_All, X)
        colnames(X_Vars_All)[length(colnames(X_Vars_All))] <- paste0("Diff_Log_",(colnames(X_Vars_All[k])),"_Lag_",m)   
        X_Vars_All <- cbind(X_Vars_All,SMA(X,3))
        colnames(X_Vars_All)[length(colnames(X_Vars_All))] <- paste0("SMA_Diff_Log_",(colnames(X_Vars_All[k])),"_Lag_",m)   
      }
      X <- log(X_Vars_All[,k])
      X_Vars_All <- cbind(X_Vars_All, X)
      colnames(X_Vars_All)[length(colnames(X_Vars_All))] <- paste("Log",(colnames(X_Vars_All[k])), sep="_")   
    }
    rm(m,k,X)
    
    a <- which(grepl("SRTR",colnames(X_Vars_All)))
    for (k in a){
      for (m in c(1,2,3)){
        X <- shift(X_Vars_All[,k],-m)  
        X_Vars_All <- cbind(X_Vars_All, X)
        colnames(X_Vars_All)[length(colnames(X_Vars_All))] <- paste((colnames(X_Vars_All[k])),m, sep="_Lag_")   
      }
      X_Vars_All <- cbind(X_Vars_All,SMA(X_Vars_All[,k],3))
      colnames(X_Vars_All)[length(colnames(X_Vars_All))] <- paste0("SMA_",(colnames(X_Vars_All[k])))   
    }
    
    X_Vars_All$Diff_LongTerm_BondYields_EU_UK <- c(NA,diff(X_Vars_All$LongTerm_BondYields_EU - X_Vars_All$LongTerm_BondYields_GBR))
    X_Vars_All$Diff_Lag_1_LongTerm_BondYields_EU_UK <- c(NA,diff((shift(X_Vars_All$LongTerm_BondYields_EU,-1))-(shift(X_Vars_All$LongTerm_BondYields_GBR,-1))))
    X_Vars_All$Diff_Log_Lag_1_LongTerm_BondYields_US_EU_Ratio <- SMA(c(NA,diff(log((shift(X_Vars_All$LongTerm_BondYields_USA,-1))/(shift(X_Vars_All$LongTerm_BondYields_EU,-1))))),3)
    X_Vars_All$Diff_Log_Lag_1_LongTerm_BondYields_UK_US_Ratio <- SMA(c(NA,diff(log((shift(X_Vars_All$LongTerm_BondYields_GBR,-1))/(shift(X_Vars_All$LongTerm_BondYields_USA,-1))))),3)
    
    a <- which(colnames(X_Vars_All) %in% "ShortTerm_IR_USA")
    for (k in a){
      for (m in c(1,2,3)){   
        X <- shift(X_Vars_All[,k],-m)
        X <- round(c(NA,diff(X)),3)
        X_Vars_All <- cbind(X_Vars_All, X)
        colnames(X_Vars_All)[length(colnames(X_Vars_All))] <- paste0("Diff_",(colnames(X_Vars_All[k])),"_Lag_",m)   
        X_Vars_All <- cbind(X_Vars_All,SMA(X,2))
        colnames(X_Vars_All)[length(colnames(X_Vars_All))] <- paste0("SMA_Diff_",(colnames(X_Vars_All[k])),"_Lag_",m)   
      }
    }
    
    a <- which(colnames(X_Vars_All) %in% "World_Exchange_Rate")
    for (k in a){
      for (m in c(1,2,3)){   
        X <- shift(X_Vars_All[,k],-m)
        X <- round(c(NA,diff(log(X))),3)
        X_Vars_All <- cbind(X_Vars_All, X)
        colnames(X_Vars_All)[length(colnames(X_Vars_All))] <- paste0("Diff_Log_",(colnames(X_Vars_All[k])),"_Lag_",m)   
      }
    }
    
    X_Vars_All$SMA3_Diff_Log_World_Exchange_Rate <- SMA(c(NA,diff(log(X_Vars_All$World_Exchange_Rate))),3)
    
    a <- which(grepl("Ship_Util",colnames(X_Vars_All)))
    for (i in a){
      X <- round(log(X_Vars_All[,i]),3)
      X_Vars_All <- cbind(X_Vars_All, X)
      colnames(X_Vars_All)[length(colnames(X_Vars_All))] <- paste0("Log_",(colnames(X_Vars_All[i])))   
    }
    
    X_Vars_All$Date <- as.Date(X_Vars_All$Report_MonthYr, format="%m/%d/%Y")
    X_Vars_All <- subset(X_Vars_All, X_Vars_All$Date >= Training_Dates[1] & X_Vars_All$Date <= End_Date)
    X_Vars_All <- data.frame(X_Vars_All[order(X_Vars_All$Date), ],row.names=NULL)
  }
  
####################################################################################
########                                                                   #########
########                         Model Bulding Loop                        #########
########                                                                   #########
####################################################################################
  
  if (S_Loop == "WSL")         {Best_Models <- Best_Models_Base_WSL}
  if (S_Loop == "LumpSum")     {Best_Models <- Best_Models_Base_LumpSum }
  
  Best_Models$Var_list <- as.character(Best_Models$Var_list)
  Best_Models$Region <- as.character(Best_Models$Region)
  
  q  <- nrow(Best_Models)
  pb = txtProgressBar(min = 0, max = q, initial = 0, char = ">", width = NA, style = 3)
  
  for (i in c(1:nrow(Best_Models))){ 
    if (i == 1){print(S_Loop)}
    if (i == 1){
      Model_Forecast <- as.data.frame(cbind(Region=1, SizeClass=1, Model_Order =1,AIC =1, BIC=1,Var_list=1, Training_MAPE =1, Training_Min_Max = 1))
      Temp <- as.data.frame(matrix(1,ncol =(Forecasting_Period)))
      Model_Forecast <- cbind(Model_Forecast,Temp)
      a <- (which(colnames(Model_Forecast) == "Training_Min_Max")+1):((which(colnames(Model_Forecast) == "Training_Min_Max"))+Forecasting_Period)
      colnames(Model_Forecast)[a] <- paste(gsub(paste("F_",1:length(a)),pattern = "\\s",replacement = ""))
      Temp2 <- as.data.frame(matrix(1,ncol =length(Validation_Period)))
      colnames(Temp2) <- paste(gsub(paste("A_",1:ncol(Temp2)),pattern = "\\s",replacement = ""))
      Model_Forecast <- cbind(Model_Forecast,Temp2)
    }
    
    Var_list1 <- unlist(strsplit(Best_Models$Var_list[i], " + ", fixed = TRUE))
    Var_list <- Var_list1[!Var_list1 %in% c("Fourier","Seasonal")]
    
    SizeClass <- as.character(Best_Models$SizeClass[i])
    temp      <- unlist(strsplit(Best_Models$Region[i], " - ", fixed = TRUE))
    Org_Reg   <- temp[1]
    Dest_Reg  <- temp[2]
    rm(temp)
    
    if ((Best_Models$Region[i])== "Global"){
      Org_Reg  <- SizeClass
      Dest_Reg <- SizeClass
    }
    
    Fixture_Aggr <- subset(Fixture_Aggr_All_1, Trade_Region_origin == Org_Reg & Trade_Region_destination == Dest_Reg & Size_Class == SizeClass)
    TS <- ts(Fixture_Aggr$WSL[Training_Period_1],frequency=12)    
    
    X_Vars_D <- as.data.frame(X_Vars_All[Training_Period_1,Var_list])
    X_Vars_V <- as.data.frame(X_Vars_All[Forecasting_Period_1,Var_list])
    
    if (grepl("Fourier",Best_Models$Var_list[i])){
      X_Vars_D <- as.data.frame(cbind(fourier(TS,3),X_Vars_All[Training_Period_1,Var_list]))
      X_Vars_V <- as.data.frame(cbind(fourierf(TS,3,Forecasting_Period),X_Vars_All[Forecasting_Period_1,Var_list]))
    }
    
    if (grepl("Seasonal",Best_Models$Var_list[i])){
      X_Vars_D <- as.data.frame(cbind(seasonaldummy(TS),X_Vars_All[Training_Period_1,Var_list]))
      X_Vars_V <- as.data.frame(cbind(seasonaldummyf(TS, Forecasting_Period),X_Vars_All[Forecasting_Period_1,Var_list]))
    }
    
    if (grepl("Fourier(TS,3)",Best_Models$Var_list[i])){
      X_Vars_D <- as.data.frame(cbind(fourier(TS,3)))
      X_Vars_V <- as.data.frame(cbind(fourierf(TS,3,Forecasting_Period)))
    }
    
    if (grepl("Seasonal_Dummy",Best_Models$Var_list[i])){
      X_Vars_D <- as.data.frame(cbind(seasonaldummy(TS)))
      X_Vars_V <- as.data.frame(cbind(seasonaldummyf(TS, Forecasting_Period)))
    }
    options(warn=-1)
    options(show.error.messages=F)
    arMod <- try(auto.arima(TS, xreg = X_Vars_D))
    if (class(arMod)[1] == "try-error") next
    Forecast <- forecast(arMod, h=Forecasting_Period,xreg=X_Vars_V)
    Forecast1 <- as.data.frame(Forecast$mean)
    AIC <- round(arMod$aic,2)
    BIC <- round(arMod$bic,2)
    Training_MAPE <- round(100 - (accuracy(arMod)[5]),2)        
    Model_Fit <- residuals(arMod,xreg=X_Vars_D)+TS
    a <- numeric(length(Model_Fit))
    for (m in c(1:length(a))){
      a[m] <-  round(min(Model_Fit[m],TS[m]) / max(Model_Fit[m],TS[m]), 3)}
    Training_Min_Max <- round(mean(a)*100,1)        
    Model_Order <- paste("ARIMA(", arMod$arma[1], ",", arMod$arma[length(arMod$arma)-1], ",", arMod$arma[2], ") with ",intersect("drift", names(arMod$coef)), sep = "")
    
    act <- as.data.frame(cbind(rbind(Fixture_Aggr$WSL[Validation_Period])))
    colnames(act) <- paste(gsub(paste("A_",1:ncol(act)),pattern = "\\s",replacement = ""))
    
    Model_Forecast1 <- as.data.frame(cbind(Region = Best_Models$Region[i], SizeClass = as.character(Best_Models$SizeClass[i]), Model_Order = Model_Order, AIC = AIC, BIC =BIC,Var_list=Best_Models$Var_list[i], Training_MAPE = Training_MAPE, Training_Min_Max = Training_Min_Max, t(Forecast1)))
    a <- (which(colnames(Model_Forecast1) == "Training_Min_Max")+1):((which(colnames(Model_Forecast1) == "Training_Min_Max"))+Forecasting_Period)
    colnames(Model_Forecast1)[a] <- paste(gsub(paste("F_",1:length(a)),pattern = "\\s",replacement = ""))
    Model_Forecast1 <- cbind(Model_Forecast1,act)
    Model_Forecast   <- rbind(Model_Forecast,Model_Forecast1) 
    options(show.error.messages=T)
    options(warn=1)
    
    if (i == nrow(Best_Models)){
      Model_Forecast <- Model_Forecast[-1,]
      a <- which(colnames(Model_Forecast)== "F_1"):ncol(Model_Forecast)
      Model_Forecast_1 <- as.data.frame(sapply(Model_Forecast[,a], FUN = function(x){round((as.numeric(as.character(x))),2)}))
      Model_Forecast <- cbind(Model_Forecast[, !names(Model_Forecast) %in% names(Model_Forecast_1)], Model_Forecast_1)
      rownames(Model_Forecast) <- NULL
      rm(Model_Forecast_1,Model_Forecast1)
    }
    setTxtProgressBar(pb, i )
  }
  if (S_Loop == "WSL"){ 
    Region_Size_Models <- Region_Size_Models[!Region_Size_Models$Combo %in% "ARABIAN GULF - ARABIAN GULF",]
    #Region_Size_Models <- Region_Size_Models[!Region_Size_Models$Combo %in% "ARABIAN GULF - INDIA",]
  }
  
  Final_Models <- Model_Forecast
  for (R in c(1:nrow(Region_Size_Models))){
    if (paste0(Region_Size_Models$Combo[R],Region_Size_Models$Size_Class[R]) %in% paste0(Final_Models$Region,Final_Models$SizeClass)){
      a <- subset(Final_Models,Region == Region_Size_Models$Combo[R] & SizeClass == Region_Size_Models$Size_Class[R])  
      
      if (nrow(a)==4){
        b <- a[1,]
        c <- a[1,c("F_1","F_2","F_3")]
        b[1,c("F_1","F_2","F_3")] <- c
        
        c <- a[2,c("F_4","F_5","F_6")]
        b[1,c("F_4","F_5","F_6")] <- c
        
        c <- a[3,c("F_7","F_8","F_9")]
        b[1,c("F_7","F_8","F_9")] <- c
        
        col <- colnames(a)
        col <- col[which(col %in% "F_10") : length(col)]
        
        c <- a[4,col]
        b[1,col] <- c
      }
      
      if (nrow(a)==3){
        b <- a[1,]
        c <- a[1,c("F_1","F_2","F_3")]
        b[1,c("F_1","F_2","F_3")] <- c
        
        c <- a[2,c("F_4","F_5","F_6")]
        b[1,c("F_4","F_5","F_6")] <- c
        
        col <- colnames(a)
        col <- col[which(col %in% "F_7") : length(col)]
        
        c <- a[3,col]
        b[1,col] <- c
      }
      
      ifelse(R==1 ,Final_Models_1 <- b, Final_Models_1<-rbind(Final_Models_1,b))
    }
  }
  #write.csv(Final_Models_1,(paste0(SOURCE_DIR,"/results/","Out_Sample_Forecast_",S_Loop,".csv")),row.names=F)
  if (S_Loop == "WSL")    {Best_Models_WSL     <- Final_Models_1}
  if (S_Loop == "LumpSum"){Best_Models_LumpSum <- Final_Models_1}
  
  ###################################################################################################
  #############                            Dollar Per Ton Models                    #################
  ###################################################################################################
  
  if (S_Loop == "WSL"){ 
    Dollar_Conversion$Size_Class <- as.character(Dollar_Conversion$Size_Class)
    Dollar_Conversion$Region <- paste(Dollar_Conversion$Trade_Region_origin,Dollar_Conversion$Trade_Region_destination,sep = " - ")
    Final_DLR_Per_Ton <- merge(x = Final_Models_1, y = Dollar_Conversion,
                               by.x = c("Region","SizeClass"), 
                               by.y = c("Region","Size_Class"), all.x=TRUE)
    Final_DLR_Per_Ton <- Final_DLR_Per_Ton[!is.na(Final_DLR_Per_Ton$Load_Port),]
    Final_DLR_Per_Ton <- Final_DLR_Per_Ton[,!colnames(Final_DLR_Per_Ton) %in% c("Load_Port","Discharge_Port")]
    
    Final_DLR_Per_Ton <- Final_DLR_Per_Ton[,!colnames(Final_DLR_Per_Ton) %in% c("Trade_Region_origin","Trade_Region_destination","YR2009","YR2010","YR2011","YR2012","YR2013","YR2014","YR2016")]
    #Final_DLR_Per_Ton <- Final_DLR_Per_Ton[,!colnames(Final_DLR_Per_Ton) %in% c("Trade_Region_origin","Trade_Region_destination","YR2009","YR2010","YR2011","YR2012","YR2013","YR2014","YR2015")]
    
    Final_DLR_Per_Ton[,grepl("F_",colnames(Final_DLR_Per_Ton))] <- (Final_DLR_Per_Ton[,grepl("F_",colnames(Final_DLR_Per_Ton))]*Final_DLR_Per_Ton[,"YR2015"])/100
    Final_DLR_Per_Ton[,grepl("A_",colnames(Final_DLR_Per_Ton))] <- (Final_DLR_Per_Ton[,grepl("A_",colnames(Final_DLR_Per_Ton))]*Final_DLR_Per_Ton[,"YR2015"])/100
    
    #Final_DLR_Per_Ton[,grepl("F_",colnames(Final_DLR_Per_Ton))] <- (Final_DLR_Per_Ton[,grepl("F_",colnames(Final_DLR_Per_Ton))]*Final_DLR_Per_Ton[,"YR2016"])/100
    #Final_DLR_Per_Ton[,grepl("A_",colnames(Final_DLR_Per_Ton))] <- (Final_DLR_Per_Ton[,grepl("A_",colnames(Final_DLR_Per_Ton))]*Final_DLR_Per_Ton[,"YR2016"])/100
    
    Final_DLR_Per_Ton <- Final_DLR_Per_Ton[,!colnames(Final_DLR_Per_Ton) %in% "YR2015"]
    #Final_DLR_Per_Ton <- Final_DLR_Per_Ton[,!colnames(Final_DLR_Per_Ton) %in% "YR2016"]
    
    #write.csv(Final_DLR_Per_Ton,(paste0(SOURCE_DIR,"/results/","Out_Sample_Forecast_","DLR_Per_Ton",".csv")),row.names=F)   
    Best_Models_DLR_Per_Ton <- Final_DLR_Per_Ton
    }
  
  
  ##################################################################################################
  ##################                      Dollar Per Day Models                   ##################
  ##################################################################################################
  
  if (S_Loop ==  "WSL"){
    rownames(Final_DLR_Per_Ton) <- NULL
    Forecast <- Final_DLR_Per_Ton
    Lookup <- Lookup_1
    Lookup$Region      <- as.character(Lookup$Region)
    Lookup$SizeClass   <- as.character(Lookup$SizeClass)
    Forecast$Region    <- as.character(Forecast$Region)
    Forecast$SizeClass <- as.character(Forecast$SizeClass)
    
    names <- c(colnames(Forecast)[grepl("A_",colnames(Forecast))],colnames(Forecast)[grepl("F_",colnames(Forecast))])
    #Calculating the 
    for (i in c(1:nrow(Forecast))){
      x <- round(Forecast[i,names] *Lookup[Lookup$Region == Forecast$Region[i] & Lookup$SizeClass == Forecast$SizeClass[i], "averagecommodityweight"])
      Forecast[i,names] <- x[1,]
    }
    Lookup[,c(6:ncol(Lookup))] <- Lookup$Distance /(Lookup[,c(6:ncol(Lookup))]*24)
    
    Forecast <- Forecast[order(Forecast$Region,Forecast$SizeClass),]
    Lookup <- Lookup[order(Lookup$Region,Lookup$SizeClass),]
    
    st <- add.months(Training_Dates[2],1)
    en <- add.months(Training_Dates[2],Forecasting_Period)
    st_en <- seq(st, en, "month")
    st_en <- format(st_en, format="%b%Y")
    
    names <- colnames(Forecast)[grepl("A_",colnames(Forecast))]
    colnames(Forecast)[colnames(Forecast) %in% names] <- paste(names,st_en[1:length(names)],sep = "_")
    names <- colnames(Forecast)[grepl("F_",colnames(Forecast))]
    colnames(Forecast)[colnames(Forecast) %in% names] <- paste(names,st_en,sep = "_")
    
    Months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    for (j in c(1:nrow(Forecast))){
      for (i in c(1:length(Months))){
        a <- colnames(Forecast)[which(grepl(Months[i],colnames(Forecast)))]
        b <- colnames(Lookup)[which(grepl(Months[i],colnames(Lookup)))]
        M <- which(Forecast$Region[j] == Lookup$Region  & Forecast$SizeClass[j]== Lookup$SizeClass)
        x <- round(Forecast[j,a]/((2*(Lookup[M,b]))+Lookup[M,"turnaroundtime"]))
        Forecast[j,a] <- x[1,]
      }
    }
    
    names <- colnames(Forecast)[grepl("A_",colnames(Forecast))]
    names1 <- sub("_+$", "",substr(names, 1, 4))
    colnames(Forecast)[colnames(Forecast) %in% names] <- names1
    names <- colnames(Forecast)[grepl("F_",colnames(Forecast))]
    names1 <- sub("_+$", "",substr(names, 1, 4))
    colnames(Forecast)[colnames(Forecast) %in% names] <- names1
    
    #write.csv(Forecast,(paste0(SOURCE_DIR,"/results/Out_Sample_Forecast_DLR_Per_Day.csv")),row.names=F)
    Best_Models_DLR_Per_Day <- Forecast
  }
}
Sys.time() - Start_Time 

#######################################################################################################
###################                        Production Format Code                   ###################
#######################################################################################################

#Best_Models_WSL         <- read.csv(paste0(SOURCE_DIR,"/results/Out_Sample_Forecast_WSL.csv"), header= TRUE)
#Best_Models_DLR_Per_Ton <- read.csv(paste0(SOURCE_DIR,"/results/Out_Sample_Forecast_DLR_Per_Ton.csv"), header= TRUE)
#Best_Models_DLR_Per_Day <- read.csv(paste0(SOURCE_DIR,"/results/Out_Sample_Forecast_DLR_Per_Day.csv"), header= TRUE)
#Best_Models_LumpSum     <- read.csv(paste0(SOURCE_DIR,"/results/Out_Sample_Forecast_LumpSum.csv"), header= TRUE)
#master            <- read.csv(paste0(SOURCE_DIR,"/data/MasterCalculationLumpsumtoDollarperdayConversion.csv"), header= TRUE)

LumpsumModels <- Best_Models_LumpSum
Lumpsum       <- LumpsumModels
Lumpsum1      <- LumpsumModels
Lookup        <- master
Lookup$Region      <- as.character(Lookup$Region)
Lookup$SizeClass   <- as.character(Lookup$SizeClass)
Lumpsum$Region     <- as.character(Lumpsum$Region)
Lumpsum$SizeClass  <- as.character(Lumpsum$SizeClass)
Lumpsum1$Region    <- as.character(Lumpsum1$Region)
Lumpsum1$SizeClass <- as.character(Lumpsum1$SizeClass)

Lumpsum <- Lumpsum[order(Lumpsum$Region,Lumpsum$SizeClass),]
Lumpsum1 <- Lumpsum1[order(Lumpsum1$Region,Lumpsum1$SizeClass),]
Lookup <- Lookup[order(Lookup$Region,Lookup$SizeClass),]

names <- colnames(Lumpsum)[c(which(grepl("F_",colnames(Lumpsum))),which(grepl("A_",colnames(Lumpsum))))]
for (j in c(1:nrow(Lumpsum))){
  a <- colnames(Lumpsum)[c(which(grepl("F_",colnames(Lumpsum))),which(grepl("A_",colnames(Lumpsum))))]
  b <- colnames(Lookup)[which(colnames(Lookup)=="Days_At_Sea")]
  
  x <- round(Lumpsum[j,a] /((2*(Lookup[Lumpsum$Region[j] == Lookup$Region  & Lumpsum$SizeClass[j]== Lookup$SizeClass,b]))+3))
  Lumpsum[j,a] <- x[1,]
}

#Dollar per tonn conversion 
for (j in c(1:nrow(Lumpsum))){
  a <- colnames(Lumpsum)[c(which(grepl("F_",colnames(Lumpsum))),which(grepl("A_",colnames(Lumpsum))))]
  b <- colnames(Lookup)[which(colnames(Lookup)=="averagecommodityweight")]
  
  x <- round(Lumpsum1[j,a] /(((Lookup[Lumpsum1$Region[j] == Lookup$Region  & Lumpsum1$SizeClass[j]== Lookup$SizeClass,b]))),2)
  Lumpsum1[j,a] <- x[1,]
}
Best_Models_LumpSum <- rbind(cbind(Commodity ="Crude",Family = "Tanker",LumpsumModels ,ValueType ="Lumpsum", View = "Forecast"),
                             cbind(Commodity ="Crude",Family = "Tanker",Lumpsum1     ,ValueType ="$ Tonne",  View = "Forecast"),
                             cbind(Commodity ="Crude",Family = "Tanker",Lumpsum       ,ValueType ="$ Day",   View = "Forecast")
)

#write.csv(Best_Models_LumpSum,(paste0(SOURCE_DIR,"/results/","Out_Sample_Forecast","_Lumpsum",".csv")),row.names=F)

n2 <- colnames(Best_Models_WSL)[c(which(grepl("A_",colnames(Best_Models_WSL))),which(grepl("F_",colnames(Best_Models_WSL))))]
n3 <- c("Region","SizeClass")

Best_Models_WSL_F         <- Best_Models_WSL[,c(n3,n2)]
Best_Models_DLR_Per_Ton_F <- Best_Models_DLR_Per_Ton[,c(n3,n2)]
Best_Models_DLR_Per_Day_F <- Best_Models_DLR_Per_Day[,c(n3,n2)]
Best_Models_LumpSum_F     <- Best_Models_LumpSum[,c("Commodity","Family",n3,n2,"ValueType","View")]
Forecast <- rbind(cbind(Commodity ="Crude",Family = "Tanker",Best_Models_WSL_F        ,ValueType ="Worldscale", View = "Forecast"),
                  cbind(Commodity ="Crude",Family = "Tanker",Best_Models_DLR_Per_Ton_F,ValueType ="$ Tonne",    View = "Forecast"),
                  cbind(Commodity ="Crude",Family = "Tanker",Best_Models_DLR_Per_Day_F,ValueType ="$ Day",      View = "Forecast"),
                  Best_Models_LumpSum_F)
#Forecast <- Best_Models_LumpSum_F

Final_All <- Forecast
n1 <- colnames(Final_All)[grepl("F_",colnames(Final_All))]
n2 <- colnames(Final_All)[grepl("A_",colnames(Final_All))]
Final_All_F <- Final_All[,c("Family","Commodity","Region","SizeClass","ValueType","View",n1)]
Final_All_A <- Final_All[,c("Family","Commodity","Region","SizeClass","ValueType","View",n2)]

st <- add.months(Training_Dates[2],1)
en <- add.months(Training_Dates[2],Forecasting_Period)
st_en <- seq(st, en, "month")
st_en <- substr(as.character(st_en),1,7)

colnames(Final_All_F)[colnames(Final_All_F) %in% n1] <- st_en
colnames(Final_All_A)[colnames(Final_All_A) %in% n2] <- st_en[1:length(n2)]
Final_All_A$View <- "Actual"


Final_All_F <- Final_All_F[,c("Family","Commodity","Region","SizeClass","ValueType","View",st_en)]
Final_All_A <- Final_All_A[,c("Family","Commodity","Region","SizeClass","ValueType","View",st_en[1:length(n2)])]

Final_All_F <- melt(Final_All_F, id.vars=c("Family","Commodity","Region","SizeClass","ValueType","View"))
Final_All_A <- melt(Final_All_A, id.vars=c("Family","Commodity","Region","SizeClass","ValueType","View"))

Final_All_1 <- rbind(Final_All_A,Final_All_F)

Final_All_1$Origin      <- lapply(strsplit(as.character(Final_All_1$Region), "-"), "[", 1)
Final_All_1$Destination <- lapply(strsplit(as.character(Final_All_1$Region), "-"), "[", 2)
Final_All_1$Year        <- lapply(strsplit(as.character(Final_All_1$variable), "-"), "[", 1)
Final_All_1$Month       <- lapply(strsplit(as.character(Final_All_1$variable), "-"), "[", 2)

Final_All_1$Origin      <- gsub("^\\s+|\\s+$", "", Final_All_1$Origin)
Final_All_1$Destination <- gsub("^\\s+|\\s+$", "", Final_All_1$Destination)
Final_All_1$Year        <- gsub("^\\s+|\\s+$", "", Final_All_1$Year)
Final_All_1$Month       <- gsub("^\\s+|\\s+$", "", Final_All_1$Month)

Final_All_1 <- Final_All_1[,c("Region","Family","Commodity","Origin","Destination","SizeClass","ValueType","View","Year","Month","value")]
Final_All_1 <- unique(Final_All_1)

test <- read.csv(paste0(SOURCE_DIR,"/results/Master.csv"))
test <- test[,c("Origin","Destination","SizeClass","ValueType")]
test <- unique(test)
Final_All_2 <- merge(x =test,y =Final_All_1,
                     by.x = c("Origin","Destination","SizeClass","ValueType"), 
                     by.y = c("Origin","Destination","SizeClass","ValueType"), all.x=TRUE)

New <- Final_All_2
New <- New[order(New$ValueType,New$Origin,New$Destination,New$SizeClass,New$View,New$Year,New$Month),]

write.csv(New,(paste0(SOURCE_DIR,"/results/OutSample_Forecast_All_",format(End_Date_1, format="%b%Y"),".csv")),row.names=F)


##############################################################################################
###########                              Outlier Code                              ###########
##############################################################################################
Old <- read.csv(paste0(SOURCE_DIR,"/results/Master.csv"))
New <- read.csv(paste0(SOURCE_DIR,"/results/OutSample_Forecast_All_",format(End_Date_1, format="%b%Y"),".csv"))

Old <- unique(Old)
New <- unique(New)

i <- sapply(Old, is.factor)
Old[i] <- lapply(Old[i], as.character)
i <- sapply(New, is.factor)
New[i] <- lapply(New[i], as.character)

Old$Year <- as.character(Old$Year)
Old$Month <- as.character(Old$Month)


colnames(New)[colnames(New) %in% "value"] <- paste0("Trained_Till_",format(End_Date_1, format="%b%Y"))

All <- merge(x =Old,y =New,
             by.x = c("Region","Family","Commodity","Origin","Destination","SizeClass","ValueType","View","Year","Month"), 
             by.y = c("Region","Family","Commodity","Origin","Destination","SizeClass","ValueType","View","Year","Month"), all.x=TRUE,all.y=TRUE)

#####################################################################
for (i in c(1:nrow(All))){
  ifelse (is.na(All[i,ncol(All)]) | is.na(All[i,(ncol(All)-1)]),
          R <- All[i,(ncol(All)-1)], 
          R <- abs(All[i,ncol(All)] - All[i,(ncol(All)-1)])/All[i,(ncol(All)-1)])
  
  ifelse(R <=.20 , All[i,ncol(All)] <- All[i,ncol(All)], All[i,ncol(All)] <- All[i,(ncol(All)-1)])
}

######################################################################

All_A <- All[All$View == "Actual",]
All   <- All[All$View == "Forecast",]

test <- unique(All[,c("Origin","Destination","SizeClass","ValueType")])

for (i in c(1:nrow(test))){
  test1 <-All[All$Origin == test$Origin[i] & All$Destination == test$Destination[i] & All$SizeClass == test$SizeClass[i] & All$ValueType == test$ValueType[i],]
  test1 <- data.frame(test1[order(test1$Year,test1$Month), ],row.names=NULL)
  
  for (m in c(1:(nrow(test1)-1))){
    R <- abs(test1[m,ncol(test1)] - test1[m+1,ncol(test1)])/test1[m,ncol(test1)]
    a <- test1[m,ncol(test1)]
    b <- test1[m+1,ncol(test1)]
    
    if(sign(a-b) == -1 & R > .20) {
      test1[m+1,ncol(test1)] <- min(a,b)*120/100
    }else if(sign(a-b) ==  1 & R > .20) {
      test1[m+1,ncol(test1)] <- max(a,b)*80/100
    }else{
      test1[m+1,ncol(test1)] <-test1[m+1,ncol(test1)]
    }
    
  }
  ifelse (i==1, chk <- test1, chk <- rbind(chk,test1))
}
chk <- rbind(All_A,chk)
chk$Year <- as.numeric(chk$Year)
chk$Month <- as.numeric(chk$Month)
chk <- chk[order(chk$ValueType,chk$Region,chk$Family,chk$Commodity,chk$Origin,chk$Destination,chk$SizeClass,chk$View,chk$Year,chk$Month),]

chk[is.na(chk)] <- ""

write.csv(chk,(paste0(SOURCE_DIR,"/results/Master1.csv")),row.names=F)

#######################################################################
All <- chk
All$ValueType <- as.character(All$ValueType)
Production <- All[,c("Family","Commodity","Origin","Destination","SizeClass","Year","Month",colnames(All)[ncol(All)-1],colnames(All)[ncol(All)],"ValueType","View")]
colnames(Production)[8] <- "Previous"
colnames(Production)[9] <- "Value"
Production$Primary <- ifelse(Production$ValueType %in% c("Worldscale","Lumpsum"), 1, 0)

New <- New[,c("Family","Commodity","Origin","Destination","SizeClass","ValueType","View","Year","Month")]

Production <- merge(x =Production,y =New,
                    by.x = c("Family","Commodity","Origin","Destination","SizeClass","ValueType","View","Year","Month"), 
                    by.y = c("Family","Commodity","Origin","Destination","SizeClass","ValueType","View","Year","Month"),all.y = TRUE)

Production <- Production[,c("Family","Commodity","Origin","Destination","SizeClass","Year","Month","Value","Previous","ValueType","Primary","View")]

Production <- merge(x =Production,y =Avg_fixtures_Final,
                    by.x = c("Family","Commodity","Origin","Destination","SizeClass","ValueType","Month"), 
                    by.y = c("Family","Commodity","Origin","Destination","SizeClass","ValueType","Month"),all.x = TRUE)
Production <- Production[order(Production$ValueType,Production$Origin,Production$Destination,Production$SizeClass,Production$View, Production$Year,Production$Month),]

Production <- Production[,!colnames(Production) %in% "Region"]

Production <- Production[,c("Family","Commodity","Origin","Destination","SizeClass","Year","Month","Value","Previous","ValueType","Primary","View","Avg_5Years")]

write.csv(Production,(paste0(SOURCE_DIR,"/results/Production.csv")),row.names=F)

#writeWorksheetToFile(paste0(SOURCE_DIR,"/results/Production.xls"),data=Production,sheet="Production",header = TRUE)

X_Vars_Raw <- melt(X_Vars_All,id= c("Report_MonthYr"))

write.csv(X_Vars_Raw,(paste0(SOURCE_DIR,"/results/X_Vars_Raw.csv")),row.names=F)

      
# ----  Accuracy Dashbaord Code ----

i <- colnames(chk)[11:ncol(chk)]
chk[i] <- lapply(chk[i], as.numeric)

Accuracy <- chk[chk$Year == year(End_Date_1) & chk$Month == month(End_Date_1),]

#Accuracy <- chk[chk$Year == year(End_Date_1) & chk$Month == 2,]
#Accuracy <- Accuracy[,c(-21)]
for (i in 1:nrow(Accuracy)){
  if (sum(is.na(Accuracy[i,1:ncol(Accuracy)])) > 0) {
    Accuracy[i,which(is.na(Accuracy[i,1:ncol(Accuracy)]))] <- Accuracy[i,max(which(is.na(Accuracy[i,1:ncol(Accuracy)])))+1]
  }
}
Accuracy[,11:ncol(Accuracy)] <- round(Accuracy[,11:ncol(Accuracy)])

a <- which(grepl("Trained_Till_", colnames(Accuracy)))
colnames(Accuracy)[a] <- gsub("Trained_Till_","",colnames(Accuracy)[a])
colnames(Accuracy)[a] <- paste(colnames(Accuracy)[a],"Forecast")

Accuracy[,ncol(Accuracy)+1] <- Accuracy[,ncol(Accuracy)]
colnames(Accuracy)[ncol(Accuracy)] <- gsub("Forecast", "Actual",colnames(Accuracy)[ncol(Accuracy)-1])
Accuracy[,ncol(Accuracy)][Accuracy$View == "Forecast"] <- NA

Accuracy[,ncol(Accuracy)][which(is.na(Accuracy[,ncol(Accuracy)]))] <-  Accuracy[,ncol(Accuracy)][which(is.na(Accuracy[,ncol(Accuracy)]))-1]

Accuracy_1 <- melt(Accuracy, id.vars=c("Region","Family","Commodity","Origin","Destination","SizeClass","ValueType","View","Year","Month"))
Accuracy_1 <- dcast(Accuracy_1,Region+Family+Commodity+Origin+Destination+SizeClass+ValueType+variable+Year+Month~View)

Accuracy_1$Month <-format(End_Date_1, format="%B %y")

#Accuracy_1$Month <-"February 16"

colnames(Accuracy_1)[colnames(Accuracy_1) %in% "variable"] <- "Value"
Accuracy_1 <- Accuracy_1[,c("Month","ValueType","Region","SizeClass","Value","Actual","Forecast")]

Accuracy_1$Region_Size <- paste(Accuracy_1$Region,Accuracy_1$SizeClass,sep = " - ")
write.csv(Accuracy_1,(paste0(SOURCE_DIR,"/results/Monthly_Accuracy_Mar.csv")),row.names=F)


# -----  New Production Format -----

Production <- Production[!Production$SizeClass == "All",]

library(XLConnect)
#Production_Old <- readWorksheetFromFile(paste0(SOURCE_DIR,"/results/Production.xls"),sheet = "Production")

Production_Old <- read.csv(paste0(SOURCE_DIR,"/results/Production_New.csv"))

Production_Old <- Production_Old[Production_Old$Commodity == "Crude",]
Production_Old <- Production_Old[Production_Old$View == "Actual",]
Production_Old <- Production_Old[-1*which(Production_Old$Year == year(add.months(sys_date,-2)) & 
                                            Production_Old$Month == month(add.months(sys_date,-2))),]

Production <- Production[!Production$SizeClass == "All",]

act <- Production[Production$View == "Actual",] 
act <- act[act$Year %in% unique(c(year(add.months(sys_date,-1)),year(add.months(sys_date,-2)))) &
      act$Month %in%  c(month(add.months(sys_date,-1)), month(add.months(sys_date,-2))),]
act <- rbind(Production_Old,act)

fct <- Production[Production$View == "Forecast",] 

fct[fct$Year <= year(sys_date) & fct$Month <= month(add.months(sys_date,-1)),c("Value","Previous")] <- NA
fct[fct$Year < year(sys_date),c("Value","Previous")] <- NA 
fct <- fct[-1*which(fct$Year == year(add.months(sys_date,-1)) & 
                      fct$Month == month(add.months(sys_date,-1))),]
tst <- act[act$Year == year(add.months(sys_date,-1)) & act$Month == month(add.months(sys_date,-1)),]
tst$View <- "Forecast" 

Production_New <- rbind(act,tst,fct)

Production_New <- Production_New[order(Production_New$ValueType,Production_New$Origin,Production_New$Destination,Production_New$SizeClass,Production_New$View, Production_New$Year,Production_New$Month),]

write.csv(Production_New,(paste0(SOURCE_DIR,"/results/Production_New.csv")),row.names=F)








  