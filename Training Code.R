

##############################Summary and Version ################################### 
#######                                                                       #######
#######        Objective       : Freigth Rate Forecasting Training Code       #######
#######        Author          : Rajesh Vaddi                                 #######
#######        R Version       : 3.1.2                                        #######
#######        Version(V0)     : Base code                                    #######
#######        Version - V1    : CHANGES                                      #######
#######                                                                       #######
#####################################################################################

# Start/Stop Nodes
system("./statusnode.sh ALL")

#system("./startnode.sh ALL")

#system("./stopnode.sh ALL")

#system("./CleanHostFile.sh")

#### Load all necessary libraries ########
wants <- c("TTR", "Hmisc", "data.table", "DataCombine", "reshape2", "zoo", "plyr", "forecast",
           "reshape", "gtools", "car", "sqldf", "foreach", "DMwR", "moments", "RODBC", "gdata","doSNOW")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, require, character.only = TRUE)
rm("wants","has")

###########  Clusters To Run on Cloud ##############
stopCluster(cl)
coresPerNode <- 15
Sys.setenv(PATH = paste0(Sys.getenv('PATH'), ':/usr/lib/rstudio-server/bin/postback'))
namelist <- c(rep('ip-10-45-89-23.us-west-2.compute.internal', coresPerNode), 
              rep('ip-10-45-89-24.us-west-2.compute.internal', coresPerNode), 
              rep('ip-10-45-89-25.us-west-2.compute.internal', coresPerNode),
              rep('ip-10-45-89-26.us-west-2.compute.internal', coresPerNode))
cl <- makeCluster(namelist, type="SOCK", user="snow")
registerDoSNOW(cl)


connect_RA <- odbcDriverConnect("Driver=SQL Server Native Client 11.0; Server=10.45.88.171; Database=RA; Uid=maritime; Pwd=trial,123")

connect_IDDS <- odbcDriverConnect("Driver=SQL Server Native Client 11.0; Server=10.45.88.171; Database=IDDS03; Uid=maritime; Pwd=trial,123")


###### cloud Directory ######
SOURCE_DIR <- "/home/ihs.internal.corp/rkj49482/Freight Rate Forecast Crude Oil" 

#########  Clusters For Local ###########
# coresPerNode <- 3
# cl <- makeCluster(coresPerNode)
# registerDoSNOW(cl)

###### Local Directory ######

#SOURCE_DIR <- "E:\\2.Projects\\Forecasting V3"

X_Vars_All_LookUp_1  <- read.csv(paste0(SOURCE_DIR,"/data/X_Vars_All_LookUP_Route.csv"), sep=",", header=T)
X_Vars_All_LookUp_2  <- read.csv(paste0(SOURCE_DIR,"/data/X_Vars_All_LookUP_Region.csv"), sep=",", header=T)
Indicator_Vars       <- read.csv(paste0(SOURCE_DIR,"/data/Indicator Vars.csv"), sep=",", header=T)

Brent_Future             <- sqlQuery(connect_RA, "SELECT * From  FR_Brent_Future")
TradeRegion              <- sqlQuery(connect_RA, "SELECT * From  FR_TradeRegion")
portToCountry            <- sqlQuery(connect_RA, "SELECT * From  FR_PortToCountry")
Dollar_Conversion        <- sqlQuery(connect_RA, "SELECT * From  FR_Dollar_Conversion")
Lookup_1                 <- sqlQuery(connect_RA, "SELECT * From  FR_MasterCalculation")
Region_Size_Combos_1     <- sqlQuery(connect_RA, "SELECT * From  FR_Region_Size_Combos_Route")

#master                   <- sqlQuery(connect_RA, "SELECT * From  FR_MasterCalculation_Lumpsum")

Forecasting_Period <- 12 
Training_Dates <- c(as.Date("2010-01-01"), as.Date("2014-12-01"))

add.months= function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]
sys_date <- as.Date(as.character(Sys.time()))
sys_date <- as.Date(paste(year(sys_date), month(sys_date),"01",sep="-"))
Data_Pull <- sys_date-1

Start_Time <- Sys.time()
date1 <- as.POSIXlt(Training_Dates[1])
date2 <- as.POSIXlt(Training_Dates[2])
Training_Period <- abs((date1$year*12 + date1$mon) - (date2$year*12 + date2$mon))+1
Training_Period_1 <- as.numeric(c(1:(Training_Period)))
Forecasting_Period_1 <- as.numeric(c(((Training_Period)+1):((Training_Period)+Forecasting_Period)))
End_Date <- add.months(Training_Dates[2],Forecasting_Period)
rm(date1,date2)

########################################################################
#######                                                          #######
#######              Fixture Data Extraction                     #######
#######                                                          #######
########################################################################

Fixture_2010_11_Data <- sqlQuery(connect_RA, "SELECT * From  FR_Fixture_2010_11_Clean_Data" )

fRate <- sqlQuery(connect_RA, sprintf("SELECT
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

#####################################################################
#######                                                       #######
#######             X Vars Data Extraction                    #######
#######                                                       #######
#####################################################################

## Running extracting the data from table for specific variables.
## Purpose is to get the series id details for each mnemonic

names <-  c('POILBNT$@WOR.M', 'POILWTI$@WOR.M',  'RX@WOR.M',  
            'IP@GBR.M',  'WPI$@BRA.M',  'WPI$@CHN.M',  'WPI$@IND.M',	
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

PURCHDE1 <- sqlQuery(connect_IDDS, sprintf("SELECT series_id, mnemonic, Dri_mnemonic, Wefa_mnemonic, startdate, enddate 
                                           FROM series_attr where mnemonic in (%s)",names1))

series_id <- PURCHDE1[,1]
series_id <- paste0(series_id, collapse = ",")

## Extracting data from SQL table based on series id.
PURCHDE2 <- sqlQuery(connect_IDDS, sprintf("select * from series_data where series_id in (%s) and (Date >= Convert(date, '2009-01-01'))",series_id))

## Preparing final data. getting variable names & mnemonic based on series id.
rawDataMonthly <- data.frame(Report_MonthYr = as.Date(PURCHDE2$date, format = "%m/%d/%Y") ,series_id=PURCHDE2$series_id, 
                             VariableName=PURCHDE1[match(PURCHDE2$series_id, PURCHDE1$series_id), "Wefa_mnemonic"],
                             mnemonic = PURCHDE1[match(PURCHDE2$series_id, PURCHDE1$series_id), "Dri_mnemonic"],
                             datavalue = PURCHDE2$datavalue)

### Extracting Yearly data.
names2 <- gsub("\\.\\M","\\.\\A",names1)
PURCHDEYear1 <- sqlQuery(connect_IDDS, sprintf("SELECT series_id, mnemonic, Dri_mnemonic, Wefa_mnemonic, startdate, enddate 
                                               FROM series_attr  where mnemonic in (%s)",names2))
PURCHDEYear1 <- PURCHDEYear1[order(PURCHDEYear1$mnemonic),]
PURCHDEYear1 <-do.call(rbind, lapply(split(PURCHDEYear1, PURCHDEYear1$mnemonic), head, 1))

series_id_A <- PURCHDEYear1[,1]
series_id_A <- paste0(series_id_A, collapse = ",")
## Extracting data from SQL table based on series id.
PURCHDEYear2 <- sqlQuery(connect_IDDS, sprintf("select * from series_data where series_id in (%s) and (Date > Convert(date, '2015-01-01') and Date <= convert(date, '2025-12-01'))
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
                 "RMSHORT_USA","WPI$_AUS","WPI$_BRA","WPI$_CHN","WPI$_IDN","WPI$_IND","WPI$_USA","WPI$_ZAF",
                 "RMSPREAD_AUS","RMSPREAD_ITA","RMSPREAD_JPN","RMSPREAD_NGA","RMSPREAD_USA","RMSPREAD_ZAF",
                 "SRT%_CHN","SRTR%_AUS","SRTR%_CHN","SRTR%_DEU","SRTR%_ESP","SRTR%_FRA","SRTR%_GBR","SRTR%_ITA","SRTR%_JPN", 
                 "SRTR%_USA","SRTR%_ZAF","POILWTI$_WOR","RX_WOR","RMSPREAD_IND","SRT%_BRA","RMSPREAD_BRA",
                 "POILBNT$_WOR"), 
         
         new = c("IP_CHN","Eurozone_IPI","EuropeanUnion_IPI","IP_GBR","IPN_IDN","IP_IND",
                 "IPN_NGA","IPN_USA","IPN_WOR","IPN_ZAF","LongTerm_BondYields_DEU","LongTerm_BondYields_ESP",
                 "LongTerm_BondYields_FRA","LongTerm_BondYields_GBR","LongTerm_BondYields_ITA","LongTerm_BondYields_USA",
                 "ShortTerm_IR_USA","WPI_AUS","WPI_BRA","WPI_CHN","WPI_IDN","WPI_IND","WPI_USA","WPI_ZAF",
                 "RMSPREAD_AUS","RMSPREAD_ITA","RMSPREAD_JPN","RMSPREAD_NGA","RMSPREAD_USA","RMSPREAD_ZAF",
                 "SRT_CHN","SRTR_AUS","SRTR_CHN","SRTR_DEU","SRTR_ESP","SRTR_FRA","SRTR_GBR","SRTR_ITA","SRTR_JPN",
                 "SRTR_USA","SRTR_ZAF","WTI_price","World_Exchange_Rate","RMSPREAD_IND","SRT_BRA","RMSPREAD_BRA",
                 "Crude_Price"))

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

Ship_Utility <- sqlQuery(connect_RA, "SELECT * From  FR_X_Vars_ShipUtility" )
Ship_Utility$Report_MonthYr <- as.Date(as.character(Ship_Utility$Report_MonthYr),"%Y-%m-%d")
finalOut <- merge(finalOut, Ship_Utility, by = "Report_MonthYr", all.x= T)
X_Vars_All1 <- finalOut 
X_Vars_All1$Date <- as.Date(X_Vars_All1$Report_MonthYr, format="%m/%d/%Y")
rm("Brent_Future" ,"finalOut","i","maxYear","names","names1","names2","output","PURCHDE1","PURCHDE2","PURCHDEYear1","PURCHDEYear2",
   "rawDataMonthly","rawDataYearly","rawDataYearlyTemp","series_id","series_id_A","Ship_Utility","years","ym","ym_temp")          

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

#fRate_raw_WSL <- fRate_raw
fRate_raw_WSL <- rbind(fRate_raw,fRate_India)

rm(fRate_raw_LumpSum_1,fRate_raw_LumpSum_2,fRate_raw_10_11)

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

#################################################################################
###########################   Fixture Data Preparation ##########################
#################################################################################

for (S_Loop in c("WSL","LumpSum")){
  if (S_Loop == "WSL"){
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
  
  #####################################################################################
  ####Step : 1 Ship Size Classification and selecting Traing Data For Forecasting #####
  #####################################################################################
  
  Fixture_Data$Size_Class <- cut(Fixture_Data$Size, breaks=c(1,40000,55000,80000,125000,200000,1000000)-1, labels=paste('=',c(0,40,55,80,125,200),"to <",c(40,55,80,125,200,999)))
  Fixture_Data$Size_Class <- as.character(Fixture_Data$Size_Class)
  Fixture_Data$Size_Class[which(Fixture_Data$Size_Class == "= 0 to < 40")]    <- "Handy tanker + Handysize"
  Fixture_Data$Size_Class[which(Fixture_Data$Size_Class == "= 40 to < 55")]   <- "Handymax"
  Fixture_Data$Size_Class[which(Fixture_Data$Size_Class == "= 55 to < 80")]   <- "Panamax"
  Fixture_Data$Size_Class[which(Fixture_Data$Size_Class == "= 80 to < 125")]  <- "Aframax"
  Fixture_Data$Size_Class[which(Fixture_Data$Size_Class == "= 125 to < 200")] <- "Suezmax"
  Fixture_Data$Size_Class[which(Fixture_Data$Size_Class == "= 200 to < 999")] <- "VLCC + ULCC"
  
  a <- c("Aframax","Suezmax","VLCC + ULCC") #unlist(strsplit(svalue(lyt[11,1]), "[\n]"))
  b <- c("Oil Dirty") #unlist(strsplit(svalue(lyt[10,1]), "[\n]"))
  Comm <- paste(c(b),collapse = " + ")
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
  Fixture_Aggr_All_1 <- subset(Fixture_Aggr_All_1,Fixture_Aggr_All_1$Date >= Training_Dates[1] & Fixture_Aggr_All_1$Date <= End_Date)
  Fixture_Aggr_All_1 <- data.frame(Fixture_Aggr_All_1[order(Fixture_Aggr_All_1$Date), ],row.names=NULL)
  
  #### Imputation Code
  Temp_Freq <- as.data.frame(table(Fixture_Aggr_All_1$Trade_Region_origin, Fixture_Aggr_All_1$Trade_Region_destination,Fixture_Aggr_All_1$Size_Class))
  Temp_Freq <- rename(Temp_Freq, c(Var1="Trade_Region_origin",Var2="Trade_Region_destination",Var3="Size_Class"))
  Temp_Freq <- Temp_Freq[which((Temp_Freq$Freq >= (max(Forecasting_Period_1)-6)) & (Temp_Freq$Freq < max(Forecasting_Period_1))) ,]
  
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
  Region_Size_Models <- Region_Size_Models[(which(Region_Size_Models$Freq==max(Forecasting_Period_1))),]
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
  #Fixture_Aggr_T_1 <- subset(Fixture_Aggr_T,MissPer <= '25' )
  #Fixture_Aggr_T_2 <- subset(Fixture_Aggr_T,MissPer > '25' )
  
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
  rm(Fixture_Aggr_T_1,Fixture_Aggr_T_2,Fixture_Aggr_T)
  
  #####Region and Size Class Summary
  Region_Size_Models <- as.data.frame(table(Fixture_Aggr_All_1$Trade_Region_origin, Fixture_Aggr_All_1$Trade_Region_destination,Fixture_Aggr_All_1$Size_Class))
  Region_Size_Models <- rename(Region_Size_Models, c(Var1="Trade_Region_origin",Var2="Trade_Region_destination",Var3="Size_Class"))
  Region_Size_Models <- Region_Size_Models[(which(Region_Size_Models$Freq==max(Forecasting_Period_1))),]
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
  
  #####################################################################################################
  ###################################  Preparing X Vars Data   ########################################
  #####################################################################################################
  
  #if (S_Loop == "LumpSum"){
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
  
  T_Dates <- paste(as.character(Training_Dates),collapse= " - ")
  All_Models <- as.data.frame(cbind(Region=1, SizeClass=1, Commodity =1, Training_Dates=1, Model=1, Model_Order =1,AIC =1, BIC=1,Var_list=1, Training_MAPE =1, Training_Min_Max = 1, MAPE=1, Min_Max=1))
  Temp <- as.data.frame(matrix(1,ncol =(Forecasting_Period+Forecasting_Period)))
  All_Models <- cbind(All_Models,Temp)
  a <- (which(colnames(All_Models) == "Min_Max")+1):((which(colnames(All_Models) == "Min_Max"))+Forecasting_Period)
  colnames(All_Models)[a] <- paste(gsub(paste("A_",1:length(a)),pattern = "\\s",replacement = ""))
  a <- ((which(colnames(All_Models) == "Min_Max"))+Forecasting_Period+1):length(All_Models)
  colnames(All_Models)[a] <- paste(gsub(paste("F_",1:length(a)),pattern = "\\s",replacement = ""))
  rm(Temp,a)
  
  ###########################################################################
  #######               ParallelForecastCombinations                 ########
  ###########################################################################
  print(S_Loop)
  q  <- nrow(Region_Size_Models)
  pb <- txtProgressBar(min = 0, max = q, initial = 0, char = ">", width = NA, title, label, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  
  TM <- Sys.time()
  listCombinations <- foreach (R = 1:nrow(Region_Size_Models),.options.snow=opts, .packages = c("plyr", "forecast", "reshape","gtools","car","sqldf")) %dopar% {
    
    #q  <- nrow(Region_Size_Models)
    #pb <- tkProgressBar(max =q)
    #for (R in c(1:nrow(Region_Size_Models))){    
    Fixture_Aggr_All <- subset(Fixture_Aggr_All_1,Trade_Region_origin==as.character(Region_Size_Models$Trade_Region_origin[R]) & Trade_Region_destination==as.character(Region_Size_Models$Trade_Region_destination[R]) & Size_Class == as.character(Region_Size_Models$Size_Class[R]))
    
    if (Region_Size_Models$Region_Route[R] == "Route") {
      X_Vars_All_LookUp  <- X_Vars_All_LookUp_1
      X_Vars_All_LookUp1 <- subset(X_Vars_All_LookUp,X_Vars_All_LookUp$Origin_Region == as.character(Region_Size_Models[R,1]) & X_Vars_All_LookUp$Destination == as.character(Region_Size_Models[R,2]))
      columnNames <- unique(na.omit(sapply(X_Vars_All_LookUp1[3:ncol(X_Vars_All_LookUp1)], as.character)))
      columnNames <- columnNames[!columnNames %in% ""]
      X_Vars_All_1 <- X_Vars_All[, names(X_Vars_All) %in% c("Report_MonthYr",columnNames)]
    }
    
    if (Region_Size_Models$Region_Route[R] == "Region") {
      X_Vars_All_LookUp  <- X_Vars_All_LookUp_2
      X_Vars_All_LookUp1 <- subset(X_Vars_All_LookUp,X_Vars_All_LookUp$Region == as.character(Region_Size_Models[R,1]))
      X_Vars_All_LookUp2 <- subset(X_Vars_All_LookUp,X_Vars_All_LookUp$Region == as.character(Region_Size_Models[R,2]))
      namesCombined <- union(X_Vars_All_LookUp1[!names(X_Vars_All_LookUp1) %in% "Region"], X_Vars_All_LookUp2[!names(X_Vars_All_LookUp2) %in% "Region"])
      columnNames <- unique(na.omit(sapply(namesCombined, as.character)))
      columnNames <- columnNames[!columnNames %in% ""]
      X_Vars_All_1 <- X_Vars_All[, names(X_Vars_All) %in% c("Report_MonthYr",columnNames)]
      rm(X_Vars_All_LookUp1,X_Vars_All_LookUp2)
    }
    
    ########Economic Variables Creation
    X_Vars1 <- X_Vars_All_1[Training_Period_1,]
    X_Vars2 <- X_Vars_All_1[Forecasting_Period_1,]
    
    names(X_Vars1) <- gsub(names(X_Vars1),pattern = "\\s",replacement = "_")
    names(X_Vars2) <- gsub(names(X_Vars2),pattern = "\\s",replacement = "_")
    
    #for (K in c(1:nrow(A))){
    Fixture_Aggr <- Fixture_Aggr_All
    Fixture_Aggr$Date <- as.POSIXlt(Fixture_Aggr$Report_MonthYr, format="%m/%d/%Y")
    Fixture_Aggr <- data.frame(Fixture_Aggr[order(Fixture_Aggr$Date), ],row.names=NULL)
    
    TS <- ts(Fixture_Aggr$WSL[Training_Period_1],frequency=12)
    
    # Defining the Cloumn Names and Temp Dataset
    a <- colnames(X_Vars1)
    T_Data <- as.data.frame(cbind(Var=numeric(ncol(X_Vars1)-1),T_Value=numeric(ncol(X_Vars1)-1),P_Value=numeric(ncol(X_Vars1)-1)))
    
    ###### Step-2 Variable selection Using T-Value and P-Value From ARIMA Model
    
    for (i in c(2:ncol(X_Vars1))){
      TS_Ex_D <- X_Vars1[,i]
      arMod <- try(auto.arima(TS, xreg = TS_Ex_D))
      if ( class(arMod)[1] == 'try-error') next
      T_Value <- as.data.frame(round((arMod$coef/sqrt(diag(arMod$var.coef))),2))
      P_Value <- as.data.frame(round((1-pnorm(abs(arMod$coef)/sqrt(diag(arMod$var.coef))))*2,4))
      T_Value <- T_Value[nrow(T_Value),1]
      P_Value <- P_Value[nrow(P_Value),1]
      T_Data[i-1,] <- cbind(a[i],abs(T_Value),P_Value)
    }    
    #Selecting Variables with T-Value >= 2
    T_Data1 <- subset(T_Data,T_Data$T_Value >= 2)
    tempNames <- T_Data1$Var
    if(length(tempNames) == 0) {tempNames  <- c("Brent_future")}
    
    ####   Variable Reduction Using Baruta
    #     Bor.son <- Boruta(TS~.,data=X_Vars1[,-1],doTrace=2)
    #     stats <- subset(attStats(Bor.son),decision == "Confirmed")
    #     tempNames <- rownames(stats, do.NULL = TRUE, prefix = "row")
    
    ######Step-3 Multicollinearity Check Using VIF for the Significant Variables From Step-2
    #     (Droping the Variables having Multicollinearity)
    
    tempNames <- tempNames[!grepl("RMSPREAD",tempNames)]
    tempNames <- tempNames[!grepl("LongTerm_BondYields",tempNames)]
    
    Formula <- formula(paste("TS ~ ", paste(tempNames, collapse=" + ")))
    fit <- lm(Formula, data=X_Vars1)
    n <- rownames(alias(fit)$Complete)
    tempNames <- tempNames[!tempNames %in% n]
    
    temp_Names <- tempNames
    if (length(temp_Names) >1) {
      for (i in c(1:length(temp_Names))){
        varlist <- tempNames
        Formula <- formula(paste("TS ~ ", paste(varlist, collapse=" + ")))
        fit <- lm(Formula, data=X_Vars1)
        VIF_Data <- as.data.frame(vif(fit))
        VIF_Data$Vars <- rownames(VIF_Data, do.NULL = TRUE, prefix = "row")
        VIF_Data <- data.frame(VIF_Data[order(-VIF_Data[,1]), ],row.names=NULL)
        if(VIF_Data[1,1] <= 5)break
        #if(VIF_Data[1,1] <= 100)break
        tempNames = VIF_Data[-1,2]
        if(length(tempNames) == 1)break
      }
    }
    #rm(i,fit,Formula,varlist,temp_Names)
    
    if (length(tempNames) >5) {
      temp_Names <- tempNames
      for (i in c(1:length(temp_Names))){
        varlist <- tempNames
        Formula <- formula(paste("TS ~", paste(varlist, collapse=" + ")))
        fit <- lm(Formula, data=X_Vars1)
        VIF_Data <- as.data.frame(vif(fit))
        VIF_Data$Vars <- rownames(VIF_Data, do.NULL = TRUE, prefix = "row")
        VIF_Data <- data.frame(VIF_Data[order(-VIF_Data[,1]), ],row.names=NULL)
        if(VIF_Data[1,1] <= 2)break
        #if(VIF_Data[1,1] <= 50)break
        tempNames = VIF_Data[-1,2]
        if(length(tempNames) == 1)break
      }
    }
    if(length(tempNames) == 0) {tempNames  <- c("Brent_future")}
    
    
    listy1 <- list()
    i <- 1
    for (m in c(1:length(tempNames))){
      #for (m in c(12:12)){
      y <- combinations(length(tempNames),m,tempNames,repeats=FALSE)
      
      n <- 2
      for (n in c(1:length(y[,1]))){
        #for (n in c(1:1)){
        y1 <- c(y[n,])
        
        listComb <- list()
        listComb[[1]] <- R
        listComb[[2]] <- y1
        
        listy1[[i]] <- listComb
        i <- i + 1
      }
    }
    
    listy1
    #rm(i,fit,Formula,varlist,temp_Names)
    #setTkProgressBar(pb, R, sprintf("Forecasting (%s)", (sprintf("%d%% done", round((R/q)*100))), (sprintf("%d%% done", round((R/q)*100)))))
  }
  
  # Consolidate the list
  k <- 1
  combinedlist <- list()
  regs <- length(listCombinations)
  for (i in c(1:regs)) {
    elements <- listCombinations[[i]]
    eachreg <- length(elements)
    for (j in c(1:eachreg)) {
      eachregele <- elements[[j]]
      combinedlist[[k]] <- eachregele
      k <- k + 1
    }
  }
  # Total number of variable permutations to process
  print(length(combinedlist))
  Sys.time() - TM
  #stopCluster(cl)
  
  ###########################################################################
  #######            ParallelForecastCombinationsModels             ########
  ###########################################################################
  
  #if (S_Loop == "LumpSum") {rm(All_Models)}
  
  listlen <- length(combinedlist)
  #listlen <- 10
  q  <- listlen
  pb <- txtProgressBar(min = 0, max = q, initial = 0, char = ">", width = NA, title, label, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  
  TM <- Sys.time()
  output <- foreach (ele=1:listlen, .options.snow=opts, .combine = "rbind",  
                     .packages = c("plyr", "forecast", "reshape","gtools","car","sqldf")) %dopar% {
                       #for (ele in c(1:listlen)) {
                       
                       R <- combinedlist[[ele]][[1]]
                       y1 <- combinedlist[[ele]][[2]]
                       
                       Fixture_Aggr_All <- subset(Fixture_Aggr_All_1,Trade_Region_origin==as.character(Region_Size_Models$Trade_Region_origin[R]) & Trade_Region_destination==as.character(Region_Size_Models$Trade_Region_destination[R]) & Size_Class == as.character(Region_Size_Models$Size_Class[R]))
                       
                       if (Region_Size_Models$Region_Route[R] == "Route") {
                         X_Vars_All_LookUp  <- X_Vars_All_LookUp_1
                         X_Vars_All_LookUp1 <- subset(X_Vars_All_LookUp,X_Vars_All_LookUp$Origin_Region == as.character(Region_Size_Models[R,1]) & X_Vars_All_LookUp$Destination == as.character(Region_Size_Models[R,2]))
                         columnNames <- unique(na.omit(sapply(X_Vars_All_LookUp1[3:ncol(X_Vars_All_LookUp1)], as.character)))
                         columnNames <- columnNames[!columnNames %in% ""]
                         X_Vars_All_1 <- X_Vars_All[, names(X_Vars_All) %in% c("Report_MonthYr",columnNames)]
                       }
                       
                       if (Region_Size_Models$Region_Route[R] == "Region") {
                         X_Vars_All_LookUp  <- X_Vars_All_LookUp_2
                         X_Vars_All_LookUp1 <- subset(X_Vars_All_LookUp,X_Vars_All_LookUp$Region == as.character(Region_Size_Models[R,1]))
                         X_Vars_All_LookUp2 <- subset(X_Vars_All_LookUp,X_Vars_All_LookUp$Region == as.character(Region_Size_Models[R,2]))
                         namesCombined <- union(X_Vars_All_LookUp1[!names(X_Vars_All_LookUp1) %in% "Region"], X_Vars_All_LookUp2[!names(X_Vars_All_LookUp2) %in% "Region"])
                         columnNames <- unique(na.omit(sapply(namesCombined, as.character)))
                         columnNames <- columnNames[!columnNames %in% ""]
                         X_Vars_All_1 <- X_Vars_All[, names(X_Vars_All) %in% c("Report_MonthYr",columnNames)]
                         rm(X_Vars_All_LookUp1,X_Vars_All_LookUp2)
                       }
                       
                       ########Economic Variables Creation
                       X_Vars1 <- X_Vars_All_1[Training_Period_1,]
                       X_Vars2 <- X_Vars_All_1[Forecasting_Period_1,]
                       
                       names(X_Vars1) <- gsub(names(X_Vars1),pattern = "\\s",replacement = "_")
                       names(X_Vars2) <- gsub(names(X_Vars2),pattern = "\\s",replacement = "_")
                       
                       #for (K in c(1:nrow(A))){
                       Fixture_Aggr <- Fixture_Aggr_All
                       Fixture_Aggr$Date <- as.POSIXlt(Fixture_Aggr$Report_MonthYr, format="%m/%d/%Y")
                       Fixture_Aggr <- data.frame(Fixture_Aggr[order(Fixture_Aggr$Date), ],row.names=NULL)
                       
                       TS <- ts(Fixture_Aggr$WSL[Training_Period_1],frequency=12)
                       
                       # Defining the Cloumn Names and Temp Dataset
                       a <- colnames(X_Vars1)
                       T_Data <- as.data.frame(cbind(Var=numeric(ncol(X_Vars1)-1),T_Value=numeric(ncol(X_Vars1)-1),P_Value=numeric(ncol(X_Vars1)-1)))
                       
                       tempColumn <- NA
                       if(length(y1) > 0) {
                         for(i in 1:length(y1)) {
                           tempColumn[i] <- which(colnames(X_Vars1) == y1[i])
                         }
                       }
                       tempNames1 <- c("NA",y1)
                       
                       Var_List <- paste(tempNames1,collapse=" + ")
                       
                       #### Step-5 Input Each Model Into ARIMAX Models and Calculating the Accuracy
                       
                       TS_Ex_D <- as.data.frame(cbind(fourier(TS,3),seasonaldummy(TS),X_Vars1[,tempColumn]))
                       TS_Ex_V <- as.data.frame(cbind(fourierf(TS,3,Forecasting_Period),seasonaldummyf(TS, Forecasting_Period),X_Vars2[,tempColumn]))
                       
                       if(length(y1)==1){
                         ####Fourier Model
                         arMod <- try(auto.arima(TS, xreg = TS_Ex_D[1:6]))
                         if (class(arMod)[1] != 'try-error') {
                           Forecast <- forecast(arMod, h=Forecasting_Period,xreg=TS_Ex_V[1:6])
                           Forecast1 <- as.data.frame(Forecast$mean)
                           AIC <- round(arMod$aic,2)
                           BIC <- round(arMod$bic,2)
                           Training_MAPE <- round(100 - (accuracy(arMod)[5]),2)
                           Model_Fit <- residuals(arMod,xreg=TS_Ex_D[1:6])+TS
                           a <- numeric(length(Model_Fit))
                           for (i in c(1:length(a))){a[i] <-  round(min(Model_Fit[i],Fixture_Aggr$WSL[i]) / max(Model_Fit[i],Fixture_Aggr$WSL[i]), 3)}
                           Training_Min_Max <- round(mean(a)*100,1)
                           Model_Order <- paste("ARIMA(", arMod$arma[1], ",", arMod$arma[length(arMod$arma)-1], ",", arMod$arma[2], ") with ",intersect("drift", names(arMod$coef)), sep = "")
                           Accuracy_MAPE_Fourier <- 100-(round((100*sum(abs(Fixture_Aggr$WSL[Forecasting_Period_1]- Forecast$mean)/Fixture_Aggr$WSL[Forecasting_Period_1],na.rm=TRUE))/length(Forecast$mean),1))
                           #Min Max Calculation
                           a <- numeric(Forecasting_Period)
                           for (i in c(1:length(a))){a[i] <-  round(min(Forecast$mean[i],Fixture_Aggr$WSL[(max(Training_Period_1))+i]) / max(Forecast$mean[i],Fixture_Aggr$WSL[(max(Training_Period_1))+i]), 3)}
                           Accuracy_Min_Max_Fourier <- round(mean(a)*100,1)
                           
                           Fourier <- as.data.frame(cbind(Model= "Fourier", Model_Order = Model_Order, AIC = AIC, BIC =BIC,Var_list="Fourier(TS,3)", Training_MAPE = Training_MAPE, Training_Min_Max = Training_Min_Max, MAPE = Accuracy_MAPE_Fourier,Min_Max=Accuracy_Min_Max_Fourier, cbind(rbind(Fixture_Aggr$WSL[Forecasting_Period_1]),rbind(Forecast1[,1]))))
                           a <- (which(colnames(Fourier) == "Min_Max")+1):((which(colnames(Fourier) == "Min_Max"))+Forecasting_Period)
                           colnames(Fourier)[a] <- paste(gsub(paste("A_",1:length(a)),pattern = "\\s",replacement = ""))
                           a <- ((which(colnames(Fourier) == "Min_Max"))+Forecasting_Period+1):length(Fourier)
                           colnames(Fourier)[a] <- paste(gsub(paste("F_",1:length(a)),pattern = "\\s",replacement = ""))
                         }
                         
                         #### Seasonal Model
                         arMod <- try(auto.arima(TS, xreg = TS_Ex_D[7:17]))
                         if (class(arMod)[1] != 'try-error') {
                           Forecast <- forecast(arMod, h=Forecasting_Period,xreg=TS_Ex_V[7:17])
                           Forecast1 <- as.data.frame(Forecast$mean)
                           AIC <- round(arMod$aic,2)
                           BIC <- round(arMod$bic,2)
                           Training_MAPE <- round(100 - (accuracy(arMod)[5]),2)
                           Model_Fit <- residuals(arMod,xreg=TS_Ex_D[7:17])+TS
                           a <- numeric(length(Model_Fit))
                           for (i in c(1:length(a))){a[i] <-  round(min(Model_Fit[i],Fixture_Aggr$WSL[i]) / max(Model_Fit[i],Fixture_Aggr$WSL[i]), 3)}
                           Training_Min_Max <- round(mean(a)*100,1)
                           Model_Order <- paste("ARIMA(", arMod$arma[1], ",", arMod$arma[length(arMod$arma)-1], ",", arMod$arma[2], ") with ",intersect("drift", names(arMod$coef)), sep = "")
                           Accuracy_MAPE_Seasonal <- 100-(round((100*sum(abs(Fixture_Aggr$WSL[Forecasting_Period_1]- Forecast$mean)/Fixture_Aggr$WSL[Forecasting_Period_1],na.rm=TRUE))/length(Forecast$mean),1))
                           #Min Max Calculation
                           a <- numeric(Forecasting_Period)
                           for (i in c(1:length(a))){a[i] <-  round(min(Forecast$mean[i],Fixture_Aggr$WSL[(max(Training_Period_1))+i]) / max(Forecast$mean[i],Fixture_Aggr$WSL[(max(Training_Period_1))+i]), 3)}
                           Accuracy_Min_Max_Seasonal <- round(mean(a)*100,1)
                           
                           Seasonal <- as.data.frame(cbind(Model= "Seasonal", Model_Order = Model_Order, AIC = AIC, BIC =BIC,Var_list="Seasonal_Dummy", Training_MAPE = Training_MAPE, Training_Min_Max = Training_Min_Max, MAPE = Accuracy_MAPE_Seasonal,Min_Max=Accuracy_Min_Max_Seasonal,cbind(rbind(Fixture_Aggr$WSL[Forecasting_Period_1]),rbind(Forecast1[,1]))))
                           a <- (which(colnames(Seasonal) == "Min_Max")+1):((which(colnames(Seasonal) == "Min_Max"))+Forecasting_Period)
                           colnames(Seasonal)[a] <- paste(gsub(paste("A_",1:length(a)),pattern = "\\s",replacement = ""))
                           a <- ((which(colnames(Seasonal) == "Min_Max"))+Forecasting_Period+1):length(Seasonal)
                           colnames(Seasonal)[a] <- paste(gsub(paste("F_",1:length(a)),pattern = "\\s",replacement = ""))
                         }
                         
                         Final_Models <- try(as.data.frame(rbind(Fourier,Seasonal)))
                         if (class(Final_Models) == "try-error") {Final_Models <- try(as.data.frame(rbind(Seasonal)))}
                         if (class(Final_Models) == "try-error") {Final_Models <- try(as.data.frame(rbind(Fourier)))}
                         Final_Models <- cbind(Region = Region_Size_Models$Combo[R],SizeClass = Region_Size_Models$Size_Class[R], Commodity = Comm, Training_Dates =T_Dates, Final_Models)
                         All_Models   <- rbind(All_Models,Final_Models)
                       }
                       
                       ####Fourier Model + X Reg Models
                       arMod <- try(auto.arima(TS, xreg = cbind(TS_Ex_D[1:6],X_Vars1[,tempColumn])))
                       if (class(arMod)[1] != 'try-error') {
                         Forecast <- forecast(arMod, h=Forecasting_Period ,xreg=cbind(TS_Ex_V[1:6],X_Vars2[,tempColumn]))
                         Forecast1 <- as.data.frame(Forecast$mean)
                         AIC <- round(arMod$aic,2)
                         BIC <- round(arMod$bic,2)
                         Training_MAPE <- round(100 - (accuracy(arMod)[5]),2)
                         Model_Fit <- residuals(arMod,xreg=cbind(TS_Ex_D[1:6],X_Vars1[,tempColumn]))+TS
                         a <- numeric(length(Model_Fit))
                         for (i in c(1:length(a))){
                           a[i] <-  round(min(Model_Fit[i],Fixture_Aggr$WSL[i]) / max(Model_Fit[i],Fixture_Aggr$WSL[i]), 3)}
                         Training_Min_Max <- round(mean(a)*100,1)
                         Model_Order <- paste("ARIMA(", arMod$arma[1], ",", arMod$arma[length(arMod$arma)-1], ",", arMod$arma[2], ") with ",intersect("drift", names(arMod$coef)), sep = "")
                         Accuracy_MAPE_Fourier <- 100-(round((100*sum(abs(Fixture_Aggr$WSL[Forecasting_Period_1]- Forecast$mean)/Fixture_Aggr$WSL[Forecasting_Period_1],na.rm=TRUE))/length(Forecast$mean),1))
                         #Min Max Calculation
                         a <- numeric(Forecasting_Period)
                         for (i in c(1:length(a))){
                           a[i] <-  round(min(Forecast$mean[i],Fixture_Aggr$WSL[(max(Training_Period_1))+i]) / max(Forecast$mean[i],Fixture_Aggr$WSL[(max(Training_Period_1))+i]), 3)}
                         Accuracy_Min_Max_Fourier <- round(mean(a)*100,1)
                         r <-  c("Fourier",Var_List)
                         
                         Fourier_XReg <- as.data.frame(cbind(Model= "Fourier+XReg", Model_Order = Model_Order, AIC = AIC, BIC =BIC,Var_list=paste(r,collapse=" + "), Training_MAPE = Training_MAPE, Training_Min_Max = Training_Min_Max, MAPE = Accuracy_MAPE_Fourier,Min_Max=Accuracy_Min_Max_Fourier, cbind(rbind(Fixture_Aggr$WSL[Forecasting_Period_1]),rbind(Forecast1[,1]))))
                         a <- (which(colnames(Fourier_XReg) == "Min_Max")+1):((which(colnames(Fourier_XReg) == "Min_Max"))+Forecasting_Period)
                         colnames(Fourier_XReg)[a] <- paste(gsub(paste("A_",1:length(a)),pattern = "\\s",replacement = ""))
                         a <- ((which(colnames(Fourier_XReg) == "Min_Max"))+Forecasting_Period+1):length(Fourier_XReg)
                         colnames(Fourier_XReg)[a] <- paste(gsub(paste("F_",1:length(a)),pattern = "\\s",replacement = ""))
                       }
                       
                       #### Seasonal Model + X Reg Models
                       arMod <- try(auto.arima(TS, xreg = cbind(TS_Ex_D[7:17],X_Vars1[,tempColumn])))
                       if (class(arMod)[1] != 'try-error') {
                         Forecast <- forecast(arMod, h=Forecasting_Period,xreg= cbind(TS_Ex_V[7:17],X_Vars2[,tempColumn]))
                         Forecast1 <- as.data.frame(Forecast$mean)
                         AIC <- round(arMod$aic,2)
                         BIC <- round(arMod$bic,2)
                         Training_MAPE <- round(100 - (accuracy(arMod)[5]),2)
                         Model_Fit <- residuals(arMod,xreg=cbind(TS_Ex_D[7:17],X_Vars1[,tempColumn]))+TS
                         a <- numeric(length(Model_Fit))
                         for (i in c(1:length(a))){
                           a[i] <-  round(min(Model_Fit[i],Fixture_Aggr$WSL[i]) / max(Model_Fit[i],Fixture_Aggr$WSL[i]), 3)}
                         Training_Min_Max <- round(mean(a)*100,1)
                         Model_Order <- paste("ARIMA(", arMod$arma[1], ",", arMod$arma[length(arMod$arma)-1], ",", arMod$arma[2], ") with ",intersect("drift", names(arMod$coef)), sep = "")
                         Accuracy_MAPE_Seasonal <- 100-(round((100*sum(abs(Fixture_Aggr$WSL[Forecasting_Period_1]- Forecast$mean)/Fixture_Aggr$WSL[Forecasting_Period_1],na.rm=TRUE))/length(Forecast$mean),1))
                         #Min Max Calculation
                         a <- numeric(Forecasting_Period)
                         for (i in c(1:length(a))){
                           a[i] <-  round(min(Forecast$mean[i],Fixture_Aggr$WSL[(max(Training_Period_1))+i]) / max(Forecast$mean[i],Fixture_Aggr$WSL[(max(Training_Period_1))+i]), 3)}
                         Accuracy_Min_Max_Seasonal <- round(mean(a)*100,1)
                         r <-  c("Seasonal",Var_List)
                         
                         Seasonal_XReg <- as.data.frame(cbind(Model="Seasonal+XReg", Model_Order = Model_Order, AIC = AIC, BIC =BIC,Var_list=paste(r,collapse=" + "), Training_MAPE = Training_MAPE, Training_Min_Max = Training_Min_Max, MAPE = Accuracy_MAPE_Seasonal,Min_Max=Accuracy_Min_Max_Seasonal,cbind(rbind(Fixture_Aggr$WSL[Forecasting_Period_1]),rbind(Forecast1[,1]))))
                         a <- (which(colnames(Seasonal_XReg) == "Min_Max")+1):((which(colnames(Seasonal_XReg) == "Min_Max"))+Forecasting_Period)
                         colnames(Seasonal_XReg)[a] <- paste(gsub(paste("A_",1:length(a)),pattern = "\\s",replacement = ""))
                         a <- ((which(colnames(Seasonal_XReg) == "Min_Max"))+Forecasting_Period+1):length(Seasonal_XReg)
                         colnames(Seasonal_XReg)[a] <- paste(gsub(paste("F_",1:length(a)),pattern = "\\s",replacement = ""))
                       }
                       
                       #### X Reg Model
                       arMod <- try(auto.arima(TS, xreg = X_Vars1[,tempColumn]))
                       if (class(arMod)[1] != 'try-error') {
                         Forecast <- forecast(arMod, h=Forecasting_Period ,xreg=X_Vars2[,tempColumn])
                         Forecast1 <- as.data.frame(Forecast$mean)
                         AIC <- round(arMod$aic,2)
                         BIC <- round(arMod$bic,2)
                         Training_MAPE <- round(100 - (accuracy(arMod)[5]),2)
                         Model_Fit <- residuals(arMod,xreg=X_Vars1[,tempColumn])+TS
                         a <- numeric(length(Model_Fit))
                         for (i in c(1:length(a))){
                           a[i] <-  round(min(Model_Fit[i],Fixture_Aggr$WSL[i]) / max(Model_Fit[i],Fixture_Aggr$WSL[i]), 3)}
                         Training_Min_Max <- round(mean(a)*100,1)
                         Model_Order <- paste("ARIMA(", arMod$arma[1], ",", arMod$arma[length(arMod$arma)-1], ",", arMod$arma[2], ") with ",intersect("drift", names(arMod$coef)), sep = "")
                         #MAPE
                         Accuracy_MAPE_X_Reg <- 100-(round((100*sum(abs(Fixture_Aggr$WSL[Forecasting_Period_1]- Forecast$mean)/Fixture_Aggr$WSL[Forecasting_Period_1],na.rm=TRUE))/length(Forecast$mean),1))
                         #Min Max Calculation
                         a <- numeric(Forecasting_Period)
                         for (i in c(1:length(a))){
                           a[i] <-  round(min(Forecast$mean[i],Fixture_Aggr$WSL[(max(Training_Period_1))+i]) / max(Forecast$mean[i],Fixture_Aggr$WSL[(max(Training_Period_1))+i]), 3)}
                         Accuracy_Min_Max_X_Reg <- round(mean(a)*100,1)
                         
                         X_Reg <- as.data.frame(cbind(Model= "X_Reg", Model_Order = Model_Order, AIC = AIC, BIC =BIC,Var_list=Var_List, Training_MAPE = Training_MAPE,  Training_Min_Max = Training_Min_Max, MAPE = Accuracy_MAPE_X_Reg, Min_Max=Accuracy_Min_Max_X_Reg,cbind(rbind(Fixture_Aggr$WSL[Forecasting_Period_1]),rbind(Forecast1[,1]))))
                         a <- (which(colnames(X_Reg) == "Min_Max")+1):((which(colnames(X_Reg) == "Min_Max"))+Forecasting_Period)
                         colnames(X_Reg)[a] <- paste(gsub(paste("A_",1:length(a)),pattern = "\\s",replacement = ""))
                         a <- ((which(colnames(X_Reg) == "Min_Max"))+Forecasting_Period+1):length(X_Reg)
                         colnames(X_Reg)[a] <- paste(gsub(paste("F_",1:length(a)),pattern = "\\s",replacement = ""))
                       }
                       
                       Final_Models <- try(as.data.frame(rbind(Fourier_XReg,Seasonal_XReg,X_Reg)))
                       if (class(Final_Models) == "try-error") {Final_Models <- try(as.data.frame(rbind(Fourier_XReg,Seasonal)))}
                       if (class(Final_Models) == "try-error") {Final_Models <- try(as.data.frame(rbind(Seasonal,X_Reg)))}
                       if (class(Final_Models) == "try-error") {Final_Models <- try(as.data.frame(rbind(Fourier_XReg,X_Reg)))}
                       if (class(Final_Models) == "try-error") {Final_Models <- try(as.data.frame(rbind(Fourier_XReg)))}
                       if (class(Final_Models) == "try-error") {Final_Models <- try(as.data.frame(rbind(Seasonal)))}
                       if (class(Final_Models) == "try-error") {Final_Models <- try(as.data.frame(rbind(X_Reg)))}
                       Final_Models <- cbind(Region = Region_Size_Models$Combo[R],SizeClass = Region_Size_Models$Size_Class[R],Commodity = Comm,Training_Dates =T_Dates,Final_Models)
                       All_Models <- rbind(All_Models,Final_Models)
                     }
  output <- unique(output)
  Sys.time() - TM
  
  #stopCluster(cl)
  
  ###########################################################################
  #######            Best Models Identification Code                 ########
  ###########################################################################
  
  All_Models <- unique(output)
  All_Models <- All_Models[-1,]
  All_Models$Var_list <- gsub(("NA\\s\\+\\s"),"",All_Models$Var_list)
  
  Fixture_Aggr_All_1$id <- paste0(Fixture_Aggr_All_1$Trade_Region_origin,Fixture_Aggr_All_1$Trade_Region_destination,Fixture_Aggr_All_1$Size_Class)
  Temp_Freq          <- as.data.frame(paste0(Region_Size_Models$Trade_Region_origin,Region_Size_Models$Trade_Region_destination,Region_Size_Models$Size_Class))
  colnames(Temp_Freq)[1] <- "id"
  Fixture_Aggr_Subset_1 <- Fixture_Aggr_All_1[which(!Fixture_Aggr_All_1$id %in% Temp_Freq$id),]
  Fixture_Aggr_Subset <- Fixture_Aggr_All_1[which(Fixture_Aggr_All_1$id %in% Temp_Freq$id),]
  Fixture_Aggr_Subset <- Fixture_Aggr_Subset[,!names(Fixture_Aggr_Subset) %in% c("Date","id") ]
  Fixture_Aggr_T <- dcast(melt(Fixture_Aggr_Subset, id.vars=c("Trade_Region_origin","Trade_Region_destination","Report_MonthYr","Size_Class")),Trade_Region_origin+Trade_Region_destination+Size_Class~Report_MonthYr)
  Fixture_Aggr_T <- Fixture_Aggr_T[,c(1:3,(ncol(Fixture_Aggr_T)-Forecasting_Period+1):ncol(Fixture_Aggr_T))]
  Fixture_Aggr_T <- cbind(Fixture_Aggr_T[1:3],round((Fixture_Aggr_T[,4:ncol(Fixture_Aggr_T)]),2))
  
  names <- colnames(All_Models)[10:ncol(All_Models)]
  All_Models[names] <- lapply(All_Models[names], as.numeric)
  All_Models[names] <- lapply(All_Models[names], function(x) round(x, 2))
  
  q  <- nrow(Fixture_Aggr_T)
  pb = txtProgressBar(min = 0, max = q, initial = 0, char = ">", width = NA, style = 3)
  for (i in c(1:nrow(Fixture_Aggr_T))){
    if (i ==1){print("All_Models_Check")}
    temp <- paste(Fixture_Aggr_T$Trade_Region_origin[i],Fixture_Aggr_T$Trade_Region_destination[i],sep=" - ")
    All_Models_1 <- subset(All_Models,Region==temp & SizeClass==Fixture_Aggr_T$Size_Class[i])
    for (m in c(1:nrow(All_Models_1))){
      if (sum(All_Models_1[m,grepl("A_",colnames(All_Models_1))] %in% Fixture_Aggr_T[i,c(4:(3+Forecasting_Period))]) ==Forecasting_Period){
        a <- All_Models_1[m,]
      }
      ifelse (m==1, a_1 <- a, a_1 <- rbind(a_1,a))
    }
    ifelse (i==1, All_Models_2 <- a_1, All_Models_2 <- rbind(All_Models_2,a_1))
    All_Models_2 <- unique(All_Models_2)
    setTxtProgressBar(pb, i )
  }
  All_Models <- All_Models_2
  rm(i,m,All_Models_2,All_Models_1,a,a_1)
  
  a <- which(colnames(All_Models)== "A_1"):ncol(All_Models)
  All_Models_1 <- as.data.frame(sapply(All_Models[,a], FUN = function(x){round((as.numeric(as.character(x))),2)}))
  All_Models_2 <- cbind(All_Models[, !names(All_Models) %in% names(All_Models_1)], All_Models_1)
  All_Models <- All_Models_2
  rownames( All_Models) <- NULL 
  colnames(All_Models)[colnames(All_Models)=="MAPE"]    <- "Forecast_MAPE"
  colnames(All_Models)[colnames(All_Models)=="Min_Max"] <- "Forecast_Min_Max"
  All_Models$Training_Min_Max <- as.numeric(All_Models$Training_Min_Max)
  All_Models <- transform(All_Models, Training_Min_Max_Rank = ave(Training_Min_Max, Region,SizeClass, FUN = function(x) rank(-x, ties.method = "first")))
  All_Models <- cbind(All_Models[,1:7],Training_Min_Max_Rank = All_Models$Training_Min_Max_Rank, All_Models[,8:(ncol(All_Models)-1)])
  All_Models <- data.frame(All_Models[order(All_Models$Region,All_Models$SizeClass,All_Models$Training_Min_Max_Rank), ],row.names=NULL)
  rm(All_Models_1,All_Models_2)
  
  s1 <- as.character(unlist(Indicator_Vars[1]))
  s1 <- s1[which(!s1 %in% "")] 
  All_Models$Oil_Indicator <- 0 
  for (r in 1:nrow(All_Models)){
    for (i in 1:length(s1)){
      indicator <-  grepl(s1[i],All_Models$Var_list[r])
      All_Models$Oil_Indicator[r] <- ifelse(indicator == "TRUE", 1, 0)
      if (All_Models$Oil_Indicator[r] == 1) break
    }}
  
  s2 <- as.character(unlist(Indicator_Vars[2]))
  s2 <- s2[which(!s2 %in% "")]
  All_Models$Supply_Indicator <- 0 
  for (r in 1:nrow(All_Models)){
    for (i in 1:length(s2)){
      indicator <-  grepl(s2[i],All_Models$Var_list[r])
      All_Models$Supply_Indicator[r] <- ifelse(indicator == "TRUE", 1, 0)
      if (All_Models$Supply_Indicator[r] == 1) break
    }}
  
  s3 <- c("Seasonal")
  All_Models$Seasonality_Indicator <- 0 
  for (r in 1:nrow(All_Models)){
    for (i in 1:length(s3)){
      indicator <-  grepl(s3[i],All_Models$Var_list[r])
      All_Models$Seasonality_Indicator[r] <- ifelse(indicator == "TRUE", 1, 0)
      if (All_Models$Seasonality_Indicator[r] == 1) break
    }}
  
  s4 <- as.character(unlist(Indicator_Vars[3]))
  s4 <- s4[which(!s4 %in% "")]
  All_Models$Economic_Indicator <- 0
  q  <- nrow(All_Models)
  pb = txtProgressBar(min = 0, max = q, initial = 0, char = ">", width = NA, style = 3)
  for (r in 1:nrow(All_Models)){
    if (r == 1){print("Economic_Indicator_Classification")}
    for (i in 1:length(s4)){
      indicator <-  grepl(s4[i],All_Models$Var_list[r])
      All_Models$Economic_Indicator[r] <- ifelse(indicator == "TRUE", 1, 0)
      if (All_Models$Economic_Indicator[r] == 1) break
    }  
    setTxtProgressBar(pb, r) 
  }
  
  All_Models$Indicator <- All_Models$Economic_Indicator + All_Models$Seasonality_Indicator + All_Models$Oil_Indicator + All_Models$Supply_Indicator
  
  write.csv(All_Models,(paste0(SOURCE_DIR,"/results/","All_Models_",S_Loop,".csv")),row.names=F)
  
  ######Step-6 Selecting Models With Weight Logic (Prashant Code)
  All_Models$Region                <- as.character(All_Models$Region )
  All_Models$SizeClass             <- as.character(All_Models$SizeClass )
  All_Models$Commodity             <- as.character(All_Models$Commodity )
  All_Models$Training_Dates        <- as.Date(All_Models$Training_Dates,format="%Y-%m-%d")
  All_Models$Model                 <- as.character(All_Models$Model )
  All_Models$Training_Min_Max_Rank <- as.numeric(All_Models$Training_Min_Max_Rank )
  All_Models$Var_list              <- as.character(All_Models$Var_list )
  
  alldata <- subset(All_Models,Training_Min_Max > 70 & Forecast_Min_Max > 70)
  alldata$X3.months.accuracy  <- 0
  alldata$X6.months.accuracy  <- 0
  alldata$X9.months.accuracy  <- 0
  alldata$X12.months.accuracy <- 0
  
  Region_Size_Models$Size_Class <- as.character(Region_Size_Models$Size_Class)
  
  for (R in c(1:nrow(Region_Size_Models))){
    if (Region_Size_Models$Combo[R] %in% alldata$Region){
      if (Region_Size_Models$Region_Route[R] == "Route") {
        X_Vars_All_LookUp  <- X_Vars_All_LookUp_1
        X_Vars_All_LookUp1 <- subset(X_Vars_All_LookUp,X_Vars_All_LookUp$Origin_Region == as.character(Region_Size_Models[R,1]) & X_Vars_All_LookUp$Destination == as.character(Region_Size_Models[R,2]))
        columnNames <- unique(na.omit(sapply(X_Vars_All_LookUp1[3:ncol(X_Vars_All_LookUp1)], as.character)))
        columnNames <- columnNames[!columnNames %in% ""]
      }
      
      if (Region_Size_Models$Region_Route[R] == "Region") {
        X_Vars_All_LookUp  <- X_Vars_All_LookUp_2
        X_Vars_All_LookUp1 <- subset(X_Vars_All_LookUp,X_Vars_All_LookUp$Region == as.character(Region_Size_Models[R,1]))
        X_Vars_All_LookUp2 <- subset(X_Vars_All_LookUp,X_Vars_All_LookUp$Region == as.character(Region_Size_Models[R,2]))
        namesCombined <- union(X_Vars_All_LookUp1[!names(X_Vars_All_LookUp1) %in% "Region"], X_Vars_All_LookUp2[!names(X_Vars_All_LookUp2) %in% "Region"])
        columnNames <- unique(na.omit(sapply(namesCombined, as.character)))
        columnNames <- columnNames[!columnNames %in% ""]
      }
      
      Route <- as.character(Region_Size_Models$Combo[R])
      Size <- as.character(Region_Size_Models$Size_Class[R])
      sea <- 11
      uti <- length(which(grepl("Ship_Util",columnNames)))
      oil <- length(which(grepl("Brent_future", columnNames))) +
        length(which(grepl("WTI_price",    columnNames))) +
        length(which(grepl("Brent_Premium",columnNames))) +
        length(which(grepl("Crude_Price",  columnNames))) 
      eco <- length(columnNames) - (uti+oil)
      limitrows <- 10
      
      subsetalldata <- subset(alldata,Region==Route & SizeClass==Size)
      
      #Accuracy Calculation
      b <- c(which(colnames(subsetalldata)== "A_1"):which(colnames(subsetalldata)== "A_3"))
      c <- c(which(colnames(subsetalldata)== "F_1"):which(colnames(subsetalldata)== "F_3"))
      for (r in c(1:nrow(subsetalldata))){
        a <- numeric(3)
        for (i in c(1:length(a))){a[i] <-  round((min(subsetalldata[r,b[i]],subsetalldata[r,c[i]])/max(subsetalldata[r,b[i]],subsetalldata[r,c[i]])), 3)}
        Accuracy_Min_Max <- round(mean(a)*100,1)
        subsetalldata$X3.months.accuracy[r] <- Accuracy_Min_Max
      }
      
      b <- c(which(colnames(subsetalldata)== "A_4"):which(colnames(subsetalldata)== "A_6"))
      c <- c(which(colnames(subsetalldata)== "F_4"):which(colnames(subsetalldata)== "F_6"))
      for (r in c(1:nrow(subsetalldata))){
        a <- numeric(3)
        for (i in c(1:length(a))){a[i] <-  round((min(subsetalldata[r,b[i]],subsetalldata[r,c[i]])/max(subsetalldata[r,b[i]],subsetalldata[r,c[i]])), 3)}
        Accuracy_Min_Max <- round(mean(a)*100,1)
        subsetalldata$X6.months.accuracy[r] <- Accuracy_Min_Max
      }
      
      b <- c(which(colnames(subsetalldata)== "A_7"):which(colnames(subsetalldata)== "A_9"))
      c <- c(which(colnames(subsetalldata)== "F_7"):which(colnames(subsetalldata)== "F_9"))
      for (r in c(1:nrow(subsetalldata))){
        a <- numeric(3)
        for (i in c(1:length(a))){a[i] <-  round((min(subsetalldata[r,b[i]],subsetalldata[r,c[i]])/max(subsetalldata[r,b[i]],subsetalldata[r,c[i]])), 3)}
        Accuracy_Min_Max <- round(mean(a)*100,1)
        subsetalldata$X9.months.accuracy[r] <- Accuracy_Min_Max
      }
      
      b <- c(which(colnames(subsetalldata)== "A_10"):which(colnames(subsetalldata)== "A_12"))
      c <- c(which(colnames(subsetalldata)== "F_10"):which(colnames(subsetalldata)== "F_12"))
      for (r in c(1:nrow(subsetalldata))){
        a <- numeric(3)
        for (i in c(1:length(a))){a[i] <-  round((min(subsetalldata[r,b[i]],subsetalldata[r,c[i]])/max(subsetalldata[r,b[i]],subsetalldata[r,c[i]])), 3)}
        Accuracy_Min_Max <- round(mean(a)*100,1)
        subsetalldata$X12.months.accuracy[r] <- Accuracy_Min_Max
      }
      
      # ordering months based on the accuracy
      sort12months <- subsetalldata[order(-subsetalldata$X12.months.accuracy),] 
      sort9months <- subsetalldata[order(-subsetalldata$X9.months.accuracy),] 
      sort6months <- subsetalldata[order(-subsetalldata$X6.months.accuracy),] 
      sort3months <- subsetalldata[order(-subsetalldata$X3.months.accuracy),] 
      
      countvector <- 1:length(subsetalldata[,1])
      
      # creating serial no in dataset
      sort12months <- data.frame(cbind(countvector,sort12months))
      sort9months <- data.frame(cbind(countvector,sort9months))
      sort6months <- data.frame(cbind(countvector,sort6months))
      sort3months <- data.frame(cbind(countvector,sort3months))
      
      #limiting the no of rows to aggregate
      limitrowssort12months <- sort12months[sort12months$countvector<=limitrows,]
      limitrowssort9months <- sort9months[sort9months$countvector<=limitrows,]
      limitrowssort6months <- sort6months[sort6months$countvector<=limitrows,]
      limitrowssort3months <- sort3months[sort3months$countvector<=limitrows,]
      
      #calculating average accuracies of values present in firat selected rows
      averageacc12monoil  <- ifelse(is.nan(mean(limitrowssort12months$X12.months.accuracy[limitrowssort12months$Oil_Indicator > 0])),0,mean(limitrowssort12months$X12.months.accuracy[limitrowssort12months$Oil_Indicator >0]) )
      averageacc12monuti  <- ifelse(is.nan(mean(limitrowssort12months$X12.months.accuracy[limitrowssort12months$Supply_Indicator > 0])),0,mean(limitrowssort12months$X12.months.accuracy[limitrowssort12months$Supply_Indicator > 0]))
      averageacc12monsea  <- ifelse(is.nan(mean(limitrowssort12months$X12.months.accuracy[limitrowssort12months$Seasonality_Indicator > 0])),0,mean(limitrowssort12months$X12.months.accuracy[limitrowssort12months$Seasonality_Indicator > 0]))
      averageacc12moneco  <- ifelse(is.nan(mean(limitrowssort12months$X12.months.accuracy[limitrowssort12months$Economic_Indicator > 0])),0,mean(limitrowssort12months$X12.months.accuracy[limitrowssort12months$Economic_Indicator > 0]))
      
      # capturing no of counts in each category for later use
      month12oilcount  <- length(limitrowssort12months$Oil_Indicator[limitrowssort12months$Oil_Indicator>0])
      month12uticount  <- length(limitrowssort12months$Supply_Indicator[limitrowssort12months$Supply_Indicator>0])
      month12seacount  <- length(limitrowssort12months$Seasonality_Indicator[limitrowssort12months$Seasonality_Indicator>0])
      month12ecocount  <- length(limitrowssort12months$Economic_Indicator[limitrowssort12months$Economic_Indicator>0])
      
      # calculating ratio of initial input variables to present in selected few
      Ratioforoccuranceoil12month <- month12oilcount/oil
      Ratioforoccuranceuti12month <- month12uticount/uti
      Ratioforoccurancesea12month <- month12seacount/sea
      Ratioforoccuranceeco12month <- month12ecocount/eco
      
      # renaming initial variables to be inline with others, this is first score
      # score4, product of initial to final presence to accuracy
      score4month12oil <- Ratioforoccuranceoil12month * averageacc12monoil
      score4month12uti <- Ratioforoccuranceuti12month * averageacc12monuti
      score4month12sea <- Ratioforoccurancesea12month * averageacc12monsea
      score4month12eco <- Ratioforoccuranceeco12month * averageacc12moneco
      
      #Weight4
      weightscore4month12oil <-  score4month12oil/sum(score4month12oil,score4month12uti,score4month12sea,score4month12eco) 
      weightscore4month12uti <-  score4month12uti/sum(score4month12oil,score4month12uti,score4month12sea,score4month12eco) 
      weightscore4month12sea <-  score4month12sea/sum(score4month12oil,score4month12uti,score4month12sea,score4month12eco) 
      weightscore4month12eco <-  score4month12eco/sum(score4month12oil,score4month12uti,score4month12sea,score4month12eco) 
      
      # Same comments for all other months variable derieved below
      averageacc9monoil  <- ifelse(is.nan(mean(limitrowssort9months$X9.months.accuracy[limitrowssort9months$Oil_Indicator > 0])),0,mean(limitrowssort9months$X9.months.accuracy[limitrowssort9months$Oil_Indicator >0]) )
      averageacc9monuti  <- ifelse(is.nan(mean(limitrowssort9months$X9.months.accuracy[limitrowssort9months$Supply_Indicator > 0])),0,mean(limitrowssort9months$X9.months.accuracy[limitrowssort9months$Supply_Indicator > 0]))
      averageacc9monsea  <- ifelse(is.nan(mean(limitrowssort9months$X9.months.accuracy[limitrowssort9months$Seasonality_Indicator > 0])),0,mean(limitrowssort9months$X9.months.accuracy[limitrowssort9months$Seasonality_Indicator > 0]))
      averageacc9moneco  <- ifelse(is.nan(mean(limitrowssort9months$X9.months.accuracy[limitrowssort9months$Economic_Indicator > 0])),0,mean(limitrowssort9months$X9.months.accuracy[limitrowssort9months$Economic_Indicator > 0]))
      
      month9oilcount  <- length(limitrowssort9months$Oil_Indicator[limitrowssort9months$Oil_Indicator>0])
      month9uticount  <- length(limitrowssort9months$Supply_Indicator[limitrowssort9months$Supply_Indicator>0])
      month9seacount  <- length(limitrowssort9months$Seasonality_Indicator[limitrowssort9months$Seasonality_Indicator>0])
      month9ecocount  <- length(limitrowssort9months$Economic_Indicator[limitrowssort9months$Economic_Indicator>0])
      
      Ratioforoccuranceoil9month <- month9oilcount/oil
      Ratioforoccuranceuti9month <- month9uticount/uti
      Ratioforoccurancesea9month <- month9seacount/sea
      Ratioforoccuranceeco9month <- month9ecocount/eco
      
      score4month9oil <- Ratioforoccuranceoil9month * averageacc9monoil
      score4month9uti <- Ratioforoccuranceuti9month * averageacc9monuti
      score4month9sea <- Ratioforoccurancesea9month * averageacc9monsea
      score4month9eco <- Ratioforoccuranceeco9month * averageacc9moneco
      
      weightscore4month9oil <-  score4month9oil/sum(score4month9oil,score4month9uti,score4month9sea,score4month9eco) 
      weightscore4month9uti <-  score4month9uti/sum(score4month9oil,score4month9uti,score4month9sea,score4month9eco) 
      weightscore4month9sea <-  score4month9sea/sum(score4month9oil,score4month9uti,score4month9sea,score4month9eco) 
      weightscore4month9eco <-  score4month9eco/sum(score4month9oil,score4month9uti,score4month9sea,score4month9eco) 
      
      averageacc6monoil  <- ifelse(is.nan(mean(limitrowssort6months$X6.months.accuracy[limitrowssort6months$Oil_Indicator > 0])),0,mean(limitrowssort6months$X6.months.accuracy[limitrowssort6months$Oil_Indicator >0]) )
      averageacc6monuti  <- ifelse(is.nan(mean(limitrowssort6months$X6.months.accuracy[limitrowssort6months$Supply_Indicator > 0])),0,mean(limitrowssort6months$X6.months.accuracy[limitrowssort6months$Supply_Indicator > 0]))
      averageacc6monsea  <- ifelse(is.nan(mean(limitrowssort6months$X6.months.accuracy[limitrowssort6months$Seasonality_Indicator > 0])),0,mean(limitrowssort6months$X6.months.accuracy[limitrowssort6months$Seasonality_Indicator > 0]))
      averageacc6moneco  <- ifelse(is.nan(mean(limitrowssort6months$X6.months.accuracy[limitrowssort6months$Economic_Indicator > 0])),0,mean(limitrowssort6months$X6.months.accuracy[limitrowssort6months$Economic_Indicator > 0]))
      
      month6oilcount  <- length(limitrowssort6months$Oil_Indicator[limitrowssort6months$Oil_Indicator>0])
      month6uticount  <- length(limitrowssort6months$Supply_Indicator[limitrowssort6months$Supply_Indicator>0])
      month6seacount  <- length(limitrowssort6months$Seasonality_Indicator[limitrowssort6months$Seasonality_Indicator>0])
      month6ecocount  <- length(limitrowssort6months$Economic_Indicator[limitrowssort6months$Economic_Indicator>0])
      
      Ratioforoccuranceoil6month <- month6oilcount/oil
      Ratioforoccuranceuti6month <- month6uticount/uti
      Ratioforoccurancesea6month <- month6seacount/sea
      Ratioforoccuranceeco6month <- month6ecocount/eco
      
      score4month6oil <- Ratioforoccuranceoil6month * averageacc6monoil
      score4month6uti <- Ratioforoccuranceuti6month * averageacc6monuti
      score4month6sea <- Ratioforoccurancesea6month * averageacc6monsea
      score4month6eco <- Ratioforoccuranceeco6month * averageacc6moneco
      
      weightscore4month6oil <-  score4month6oil/sum(score4month6oil,score4month6uti,score4month6sea,score4month6eco) 
      weightscore4month6uti <-  score4month6uti/sum(score4month6oil,score4month6uti,score4month6sea,score4month6eco) 
      weightscore4month6sea <-  score4month6sea/sum(score4month6oil,score4month6uti,score4month6sea,score4month6eco) 
      weightscore4month6eco <-  score4month6eco/sum(score4month6oil,score4month6uti,score4month6sea,score4month6eco) 
      
      averageacc3monoil  <- ifelse(is.nan(mean(limitrowssort3months$X3.months.accuracy[limitrowssort3months$Oil_Indicator > 0])),0,mean(limitrowssort3months$X3.months.accuracy[limitrowssort3months$Oil_Indicator >0]) )
      averageacc3monuti  <- ifelse(is.nan(mean(limitrowssort3months$X3.months.accuracy[limitrowssort3months$Supply_Indicator > 0])),0,mean(limitrowssort3months$X3.months.accuracy[limitrowssort3months$Supply_Indicator > 0]))
      averageacc3monsea  <- ifelse(is.nan(mean(limitrowssort3months$X3.months.accuracy[limitrowssort3months$Seasonality_Indicator > 0])),0,mean(limitrowssort3months$X3.months.accuracy[limitrowssort3months$Seasonality_Indicator > 0]))
      averageacc3moneco  <- ifelse(is.nan(mean(limitrowssort3months$X3.months.accuracy[limitrowssort3months$Economic_Indicator > 0])),0,mean(limitrowssort3months$X3.months.accuracy[limitrowssort3months$Economic_Indicator > 0]))
      
      month3oilcount  <- length(limitrowssort3months$Oil_Indicator[limitrowssort3months$Oil_Indicator>0])
      month3uticount  <- length(limitrowssort3months$Supply_Indicator[limitrowssort3months$Supply_Indicator>0])
      month3seacount  <- length(limitrowssort3months$Seasonality_Indicator[limitrowssort3months$Seasonality_Indicator>0])
      month3ecocount  <- length(limitrowssort3months$Economic_Indicator[limitrowssort3months$Economic_Indicator>0])
      
      Ratioforoccuranceoil3month <- month3oilcount/oil
      Ratioforoccuranceuti3month <- month3uticount/uti
      Ratioforoccurancesea3month <- month3seacount/sea
      Ratioforoccuranceeco3month <- month3ecocount/eco
      
      score4month3oil <- Ratioforoccuranceoil3month * averageacc3monoil
      score4month3uti <- Ratioforoccuranceuti3month * averageacc3monuti
      score4month3sea <- Ratioforoccurancesea3month * averageacc3monsea
      score4month3eco <- Ratioforoccuranceeco3month * averageacc3moneco
      
      weightscore4month3oil <-  score4month3oil/sum(score4month3oil,score4month3uti,score4month3sea,score4month3eco) 
      weightscore4month3uti <-  score4month3uti/sum(score4month3oil,score4month3uti,score4month3sea,score4month3eco) 
      weightscore4month3sea <-  score4month3sea/sum(score4month3oil,score4month3uti,score4month3sea,score4month3eco) 
      weightscore4month3eco <-  score4month3eco/sum(score4month3oil,score4month3uti,score4month3sea,score4month3eco) 
      
      # Aggregating rows below to be in one table
      month12score4weight <- cbind(weightscore4month12oil,weightscore4month12uti,weightscore4month12sea,weightscore4month12eco)
      month9score4weight <- cbind(weightscore4month9oil,weightscore4month9uti,weightscore4month9sea,weightscore4month9eco)
      month6score4weight <- cbind(weightscore4month6oil,weightscore4month6uti,weightscore4month6sea,weightscore4month6eco)
      month3score4weight <- cbind(weightscore4month3oil,weightscore4month3uti,weightscore4month3sea,weightscore4month3eco)
      
      Final <- data.frame(rbind(month12score4weight,month9score4weight,month6score4weight,month3score4weight))
      
      Final$Total <- Final[,1]+Final[,2]+Final[,3]+Final[,4]
      
      rowname <- data.frame(rbind("month12score4weight","month9score4weight","month6score4weight","month3score4weight"))
      Final <- cbind(rowname,Final)
      
      names(Final)[names(Final) == names(Final[1])] <- "Weightname"
      names(Final)[names(Final) == names(Final[2])] <- "Oil"
      names(Final)[names(Final) == names(Final[3])] <- "Uti"
      names(Final)[names(Final) == names(Final[4])] <- "Sea"
      names(Final)[names(Final) == names(Final[5])] <- "Eco"
      
      a <- which(Final[1,] == max(Final[1,2:5]))
      month12score4weight_ind <- names(Final)[a]
      a <- which(Final[2,] == max(Final[2,2:5]))
      month9score4weight_ind <- names(Final)[a]
      a <- which(Final[3,] == max(Final[3,2:5]))
      month6score4weight_ind <- names(Final)[a]
      a <- which(Final[4,] == max(Final[4,2:5]))
      month3score4weight_ind <- names(Final)[a]
      
      #####12 Months Models 
      if (month12score4weight_ind == "Oil"){
        for (i in 1:nrow(limitrowssort12months)){
          a <- as.data.frame(limitrowssort12months[i,])
          if (limitrowssort12months$Oil_Indicator[1] == 1) break
        }
      }
      if (month12score4weight_ind == "Uti"){
        for (i in 1:nrow(limitrowssort12months)){
          a <- as.data.frame(limitrowssort12months[i,])
          if (limitrowssort12months$Supply_Indicator[i] == 1) break
        }
      }
      
      if (month12score4weight_ind == "Eco"){
        for (i in 1:nrow(limitrowssort12months)){
          a <- as.data.frame(limitrowssort12months[i,])
          if (limitrowssort12months$Economic_Indicator[1] == 1) break
        }
      }
      if (month12score4weight_ind == "Sea"){
        for (i in 1:nrow(limitrowssort12months)){
          a <- as.data.frame(limitrowssort12months[i,])
          if (limitrowssort12months$Seasonality_Indicator[1] == 1) break
        }
      }
      
      #####9 Months Models 
      if (month9score4weight_ind == "Oil"){
        for (i in 1:nrow(limitrowssort9months)){
          b <- as.data.frame(limitrowssort9months[i,])
          if (limitrowssort9months$Oil_Indicator[1] == 1) break
        }
      }
      if (month9score4weight_ind == "Uti"){
        for (i in 1:nrow(limitrowssort9months)){
          b <- as.data.frame(limitrowssort9months[i,])
          if (limitrowssort9months$Supply_Indicator[i] == 1) break
        }
      }
      
      if (month9score4weight_ind == "Eco"){
        for (i in 1:nrow(limitrowssort9months)){
          b <- as.data.frame(limitrowssort9months[i,])
          if (limitrowssort9months$Economic_Indicator[1] == 1) break
        }
      }
      if (month9score4weight_ind == "Sea"){
        for (i in 1:nrow(limitrowssort9months)){
          b <- as.data.frame(limitrowssort9months[i,])
          if (limitrowssort9months$Seasonality_Indicator[1] == 1) break
        }
      }
      
      #####6 Months Models 
      if (month6score4weight_ind == "Oil"){
        for (i in 1:nrow(limitrowssort6months)){
          c <- as.data.frame(limitrowssort6months[i,])
          if (limitrowssort6months$Oil_Indicator[1] == 1) break
        }
      }
      if (month6score4weight_ind == "Uti"){
        for (i in 1:nrow(limitrowssort6months)){
          c <- as.data.frame(limitrowssort6months[i,])
          if (limitrowssort6months$Supply_Indicator[i] == 1) break
        }
      }
      
      if (month6score4weight_ind == "Eco"){
        for (i in 1:nrow(limitrowssort6months)){
          c <- as.data.frame(limitrowssort6months[i,])
          if (limitrowssort6months$Economic_Indicator[1] == 1) break
        }
      }
      if (month6score4weight_ind == "Sea"){
        for (i in 1:nrow(limitrowssort6months)){
          c <- as.data.frame(limitrowssort6months[i,])
          if (limitrowssort6months$Seasonality_Indicator[1] == 1) break
        }
      }
      
      #####3 Months Models 
      if (month3score4weight_ind == "Oil"){
        for (i in 1:nrow(limitrowssort3months)){
          d <- as.data.frame(limitrowssort3months[i,])
          if (limitrowssort3months$Oil_Indicator[1] == 1) break
        }
      }
      if (month3score4weight_ind == "Uti"){
        for (i in 1:nrow(limitrowssort3months)){
          d <- as.data.frame(limitrowssort3months[i,])
          if (limitrowssort3months$Supply_Indicator[i] == 1) break
        }
      }
      
      if (month3score4weight_ind == "Eco"){
        for (i in 1:nrow(limitrowssort3months)){
          d <- as.data.frame(limitrowssort3months[i,])
          if (limitrowssort3months$Economic_Indicator[1] == 1) break
        }
      }
      if (month3score4weight_ind == "Sea"){
        for (i in 1:nrow(limitrowssort3months)){
          d <- as.data.frame(limitrowssort3months[i,])
          if (limitrowssort3months$Seasonality_Indicator[1] == 1) break
        }
      }
      Final_Models_1 <- rbind(d,c,b,a)
      ifelse(R==1 ,Final_Models <- Final_Models_1, Final_Models<-rbind(Final_Models,Final_Models_1))
    }
  }
  
  for (R in c(1:nrow(Region_Size_Models))){
    if (Region_Size_Models$Combo[R] %in% Final_Models$Region){
      a <- subset(Final_Models,Region == Region_Size_Models$Combo[R] & SizeClass == Region_Size_Models$Size_Class[R])
      b <- a[1,]
      
      c <- a[1,c("F_1","F_2","F_3")]
      b[1,c("F_1","F_2","F_3")] <- c
      
      c <- a[2,c("F_4","F_5","F_6")]
      b[1,c("F_4","F_5","F_6")] <- c
      
      c <- a[3,c("F_7","F_8","F_9")]
      b[1,c("F_7","F_8","F_9")] <- c
      
      c <- a[4,c("F_10","F_11","F_12")]
      b[1,c("F_10","F_11","F_12")] <- c
      
      x <- c(which(colnames(b)=="A_1"):which(colnames(b)=="A_12"))
      y <- c(which(colnames(b)=="F_1"):which(colnames(b)=="F_12"))
      z <- numeric(12)
      for (i in c(1:length(z))){z[i] <- round((min(b[1,x[i]],b[1,y[i]])/max(b[1,x[i]],b[1,y[i]])),3)}
      Accuracy_Min_Max <- round(mean(z)*100,1)
      b$Forecast_Min_Max[1] <- Accuracy_Min_Max
      
      ifelse(R==1 ,Final_Models_1 <- b, Final_Models_1<-rbind(Final_Models_1,b))
    }
  }
  
  write.csv(Final_Models,(paste0(SOURCE_DIR,"/results/","Best_Models_Base_",S_Loop,".csv")),row.names=F)
  write.csv(Final_Models_1,(paste0(SOURCE_DIR,"/results/","Best_Models_",S_Loop,".csv")),row.names=F)
  
}
stopCluster(cl)

####  Production Format Code ####
Best_Models_WSL         <- read.csv(paste0(SOURCE_DIR,"/results/Best_Models_WSL.csv"), header= TRUE)
Best_Models_LumpSum     <- read.csv(paste0(SOURCE_DIR,"/results/Best_Models_LumpSum.csv"), header= TRUE)

n1 <- colnames(Best_Models_WSL)[grepl("A_",colnames(Best_Models_WSL))]
n2 <- colnames(Best_Models_WSL)[grepl("F_",colnames(Best_Models_WSL))]
n3 <- c("Commodity","Region","SizeClass")

Best_Models_WSL_F         <- Best_Models_WSL[,c(n3,n2)]
Best_Models_DLR_Per_Ton_F <- Best_Models_DLR_Per_Ton[,c(n3,n2)]
Best_Models_DLR_Per_Day_F <- Best_Models_DLR_Per_Day[,c(n3,n2)]
Best_Models_LumpSum_F     <- Best_Models_LumpSum[,c(n3,n2)]
Forecast <- rbind(cbind(Family = "Tanker", Best_Models_WSL_F         ,ValueType ="Worldscale", View = "Forecast"),
                  cbind(Family = "Tanker", Best_Models_LumpSum_F     ,ValueType ="LumpSum",    View = "Forecast"))
colnames(Forecast)[colnames(Forecast) %in% n2] <- n1

Best_Models_WSL_A         <- Best_Models_WSL[,c(n3,n1)]
Best_Models_DLR_Per_Ton_A <- Best_Models_DLR_Per_Ton[,c(n3,n1)]
Best_Models_DLR_Per_Day_A <- Best_Models_DLR_Per_Day[,c(n3,n1)]
Best_Models_LumpSum_A     <- Best_Models_LumpSum[,c(n3,n1)]
Actual <- rbind(cbind(Family = "Tanker", Best_Models_WSL_A         ,ValueType ="Worldscale", View = "Actual"),
                cbind(Family = "Tanker", Best_Models_LumpSum_A     ,ValueType ="LumpSum",    View = "Actual"))

Final_All <- rbind(Actual,Forecast)
n1 <- colnames(Final_All)[grepl("A_",colnames(Final_All))]

st <- add.months(Training_Dates[2],1)
en <- add.months(Training_Dates[2],Forecasting_Period)
st_en <- seq(st, en, "month")
st_en <- substr(as.character(st_en),1,7)

colnames(Final_All)[colnames(Final_All) %in% n1] <- st_en

Final_All <- Final_All[,c("Family","Commodity","Region","SizeClass","ValueType","View",st_en)]
Final_All_1 <- melt(Final_All, id.vars=c("Family","Commodity","Region","SizeClass","ValueType","View"))

Final_All_1$Origin      <- lapply(strsplit(as.character(Final_All_1$Region), "-"), "[", 1)
Final_All_1$Destination <- lapply(strsplit(as.character(Final_All_1$Region), "-"), "[", 2)
Final_All_1$Year        <- lapply(strsplit(as.character(Final_All_1$variable), "-"), "[", 1)
Final_All_1$Month       <- lapply(strsplit(as.character(Final_All_1$variable), "-"), "[", 2)

Final_All_1$Origin      <- gsub("^\\s+|\\s+$", "", Final_All_1$Origin)
Final_All_1$Destination <- gsub("^\\s+|\\s+$", "", Final_All_1$Destination)
Final_All_1$Year        <- gsub("^\\s+|\\s+$", "", Final_All_1$Year)
Final_All_1$Month       <- gsub("^\\s+|\\s+$", "", Final_All_1$Month)

Final_All_1$Commodity <-  "Crude"
Final_All_1 <- Final_All_1[,c("Family","Commodity","Origin","Destination","SizeClass","ValueType","View","Year","Month","value")]

write.csv(Final_All_1,(paste0(SOURCE_DIR,"/results/Final_All.csv")),row.names=F)

