# William Dean
# MA 415 Final Project

# This script reads in all the data, cleans it and saves it to two csv files to be used later

library(foreign)
library(dplyr)
library(lubridate)
library(ggplot2)

path <- function(year,file){return(paste0("Fire/",year,"/NFIRS_",year,"/",file))}


CleanData <- function(FireInc, Codes, Basic) {
    ### This Function Takes the Fire Incident Table, the look-up table and basic Incident table from a certain year
    ### and returns a single data frame with intention in mind to explore Dispatch Time and fatal and non-fatal accidents
    
    ### SELECT ATTRIBUTES ###
    FireInc <- FireInc %>% select(STATE,INC_DATE,INC_NO,BLDG_INVOL, CAUSE_IGN, AREA_ORIG,HEAT_SOURC,FIRST_IGN,TOT_SQ_FT,HUM_FAC_1,FIRE_SPRD)
    
    Basic <- Basic %>% select(STATE,FDID,INC_DATE,INC_NO,ALARM,ARRIVAL,LU_CLEAR,PROP_LOSS,PROP_VAL,CONT_LOSS,CONT_VAL,FF_DEATH,OTH_DEATH,FF_INJ,OTH_INJ)
    
    names(Codes) <- c("FIELDID", "CODE_VALUE", "CODE_DESCR")
    FireInc$FIRE_SPRD <- as.factor(FireInc$FIRE_SPRD)
    
    ### Labels ###
    # Ignition Cause
    CI <- Codes %>% filter(FIELDID == "CAUSE_IGN") %>% filter(!is.na(CODE_VALUE)) %>% select(-FIELDID)
    names(CI) <- c("CAUSE_IGN", "IGN_CAUSE")
    CI <- CI[-1,]
    FireInc <- left_join(FireInc, CI, by = "CAUSE_IGN")
    FireInc <- FireInc %>% select(-CAUSE_IGN)
    FireInc$IGN_CAUSE <- droplevels(FireInc$IGN_CAUSE)
    
    # Area Origin
    AO <- Codes %>% filter(FIELDID == "AREA_ORIG") %>% filter(!is.na(CODE_VALUE)) %>% select(-FIELDID)
    names(AO) <- c("AREA_ORIG", "AREAORIG")
    AO <- AO[-1,]
    FireInc <- left_join(FireInc, AO, by = "AREA_ORIG")
    FireInc <- FireInc %>% select(-AREA_ORIG)
    FireInc$AREAORIG <- droplevels(FireInc$AREAORIG)
    
    # Heat Source
    HS <- Codes %>% filter(FIELDID == "HEAT_SOURC") %>% filter(!is.na(CODE_VALUE)) %>% select(-FIELDID)
    names(HS) <- c("HEAT_SOURC", "HEATSOURC")
    HS <- HS[-1,]
    FireInc <- left_join(FireInc, HS, by = "HEAT_SOURC")
    FireInc <- FireInc %>% select(-HEAT_SOURC)
    FireInc$HEATSOURC <- droplevels(FireInc$HEATSOURC)
    
    # First Ignition
    FI <- Codes %>% filter(FIELDID == "FIRST_IGN") %>% filter(!is.na(CODE_VALUE)) %>% select(-FIELDID)
    names(FI) <- c("FIRST_IGN", "FIRSTIGN")
    FI <- FI[-1,]
    FireInc <- left_join(FireInc, FI, by = "FIRST_IGN")
    FireInc <- FireInc %>% select(-FIRST_IGN)
    FireInc$FIRSTIGN <- droplevels(FireInc$FIRSTIGN)
    
    # Human Factor
    HF <- Codes %>% filter(FIELDID == "HUM_FAC_1") %>% filter(!is.na(CODE_VALUE)) %>% select(-FIELDID)
    names(HF) <- c("HUM_FAC_1", "HUMFAC")
    HF <- HF[-1,]
    FireInc <- left_join(FireInc, HF, by = "HUM_FAC_1")
    FireInc <- FireInc %>% select(-HUM_FAC_1)
    FireInc$HUMFAC <- droplevels(FireInc$HUMFAC)
    
    # Fire Spread
    FS <- Codes %>% filter(FIELDID == "FIRE_SPRD") %>% filter(!is.na(CODE_VALUE)) %>% select(-FIELDID)
    names(FS) <- c("FIRE_SPRD", "FIRESPRD")
    FS <- FS[-1,]
    FireInc <- left_join(FireInc, FS, by = "FIRE_SPRD")
    FireInc <- FireInc %>% select(-FIRE_SPRD)
    FireInc$FIRESPRD <- droplevels(FireInc$FIRESPRD)
    
    ### Distinct ###
    Basic <- distinct(Basic)
    FireInc <- distinct(FireInc)
    
    ### Join Tables ###
    Fires <- left_join(FireInc, Basic, by = c("STATE", "INC_DATE", "INC_NO"))
    
    
    
    ### Adjust Attributes ###
    Fires$INC_DATE <- mdy(Fires$INC_DATE)
    Fires$ALARM <- mdy_hm(Fires$ALARM)
    Fires$ARRIVAL <- mdy_hm(Fires$ARRIVAL)
    Fires$LU_CLEAR <- mdy_hm(Fires$LU_CLEAR)
    
    Fires <- Fires %>% mutate(TIME = as.numeric(difftime(Fires$ARRIVAL, Fires$ALARM, units = "mins")))
    Fires <- Fires %>% mutate(MONTH = as.numeric(month(Fires$INC_DATE)))
    Fires <- Fires %>% mutate(HOUR = as.numeric(hour(Fires$ALARM)))
    Fires <- Fires %>% mutate(CLEAR = as.numeric(difftime(Fires$LU_CLEAR, Fires$ALARM, units = "mins")))
    
    Fires$HOUR <- as.factor(Fires$HOUR)
    Fires$MONTH <- as.factor(Fires$MONTH)
    
    Fires <- Fires %>% mutate(PROP = ifelse(PROP_VAL != 0, PROP_LOSS / PROP_VAL, NA))
    Fires <- Fires %>% mutate(DEATH = ifelse(FF_DEATH != 0 | OTH_DEATH != 0, "Death", "NoDeath"))
    Fires <- Fires %>% mutate(NUM_INJ = FF_DEATH + FF_INJ + OTH_DEATH + OTH_INJ)
    
    Fires$DEATH <- as.factor(Fires$DEATH)
    
    return(Fires)
}

### 2007 Data
FireInc <- read.dbf(path(2007, "fireincident.dbf"))
Codes <- read.dbf(path(2007,"codelookup.dbf"))
Basic <- read.dbf(path(2007, "basicincident.dbf"))

Fires2007 <- CleanData(FireInc, Codes, Basic)
write.csv(Fires2007, "Fires2007.csv")

### 2015 Data
FireInc <- read.table(path(2015, "fireincident.txt"), sep = "^", header = TRUE)
Codes <- read.table(path(2015, "codelookup.txt"), sep = "^", header = TRUE)
Basic <- read.table(path(2015, "basicincident.txt"), sep = "^", header = TRUE)

Fires2015 <- CleanData(FireInc, Codes, Basic)
write.csv(Fires2015, "Fires2015.csv")

rm(Basic, Codes, FireInc, CleanData, path)

