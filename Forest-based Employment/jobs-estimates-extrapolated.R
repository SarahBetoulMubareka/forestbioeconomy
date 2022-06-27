library(eurostat)
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)

# ---- Functions ----
# -- function binding dataframes with different numbers of columns --
rbind.all.columns <- function(x, y) {
  out <- y
  if(!is.null(ncol(x))){
    
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    
    x[, c(as.character(y.diff))] <- NA
    
    y[, c(as.character(x.diff))] <- NA
    
    out <- rbind(x, y)}
  return(out)
}

# -- function fill time --
fillValueTime <- function(inputSet, value, year, dependentVars){ # -- inputSet: data frame to expand, value: name of the column with the value (as character), year: name of the time variable, dependentVars: list of the dependent variables to use for expanding(vector of characters), NB do not use variables that are linked by definition (e.g. code and label)
  completeSet <- inputSet[,c(value, year, dependentVars)]
  colnames(completeSet)[1:2] <- c("value", "year")
  completeSet[, c(dependentVars, "year")] <- apply(completeSet[, c(dependentVars, "year")],2, FUN=as.character)
  completeSet <- merge(apply((data.frame(expand.grid(apply(completeSet[,c(dependentVars, "year")], 2, unique)))), 2, FUN=as.character), completeSet, all.x=TRUE)
  completeSet[, c(dependentVars, "year")] <- apply(completeSet[, c(dependentVars, "year")],2, FUN=as.character)
  completeSet$year <- as.numeric(as.character(completeSet$year))
  
  completeSet <- merge(completeSet[is.na(completeSet$value)|is.nan(completeSet$value), ], completeSet[!is.na(completeSet$value)&!is.nan(completeSet$value), ], by.x =dependentVars, by.y = dependentVars)
  completeSet$yeardif <-  completeSet$year.x - completeSet$year.y
  completeSetp <- merge(completeSet, aggregate(as.formula(paste("yeardif ~ year.x +", paste(dependentVars, collapse=" + "))), FUN = min, data=completeSet[completeSet$yeardif>0, ]))[, c(dependentVars, "year.x", "year.y",  "value.y")]
  colnames(completeSetp)[(ncol(completeSetp)-2):ncol(completeSetp)] <- c("year", "year_p", "value_p")
  
  completeSetn <- merge(completeSet, aggregate(as.formula(paste("yeardif ~ year.x +", paste(dependentVars, collapse=" + "))), FUN = max, data=completeSet[completeSet$yeardif<0, ]))[, c(dependentVars, "year.x", "year.y",  "value.y")]
  colnames(completeSetn)[(ncol(completeSetn)-2):ncol(completeSetn)] <- c("year", "year_n", "value_n")
  
  completeSet <- merge(completeSetp, completeSetn, all=TRUE)
  
  completeSet$value_est <- with(completeSet, ifelse(is.na(value_n), value_p, ifelse(is.na(value_p), value_n, (value_p-value_n)/(year_p-year_n)*(year-year_n)+value_n  )))
  
  completeSet <- merge(inputSet, completeSet, by.x = c(year, dependentVars), by.y = c("year", dependentVars), all=TRUE)
  completeSet$value_est <- ifelse(!is.na(completeSet[, value]), completeSet[, value], completeSet$value_est)
  
  return(completeSet)
}

# ---- Employment by sex, age and detailed economic activity (from 2008 onwards, NACE Rev. 2 two digit level) - 1 000 ----
lfsa_egan22d <- get_eurostat("lfsa_egan22d",  time_format = "num", stringsAsFactors=FALSE)
lfsa_egan22d <- label_eurostat(lfsa_egan22d, code = names(lfsa_egan22d)[c(-6,-7)])

lfsa <- lfsa_egan22d[lfsa_egan22d$unit_code=="THS" &
  lfsa_egan22d$age_code=="Y_GE15" & # Y15, 20, 25 40, 50, 55, 65, 75
  lfsa_egan22d$sex_code=="T" &
  lfsa_egan22d$nace_r2_code %in% c("A02", "C16", "C17", "C18", "C31", "D35", "F41", "F42", "F43", "J58") &
  lfsa_egan22d$geo_code %in% c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "EU28", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK"),]

lfsa_t <- aggregate(values~unit_code+age_code+sex_code+geo_code+time, data=lfsa_egan22d[lfsa_egan22d$unit_code=="THS" &
                         lfsa_egan22d$age_code=="Y_GE15" & # Y15, 20, 25 40, 50, 55, 65, 75
                         lfsa_egan22d$sex_code=="T" &
                         lfsa_egan22d$geo_code %in% c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "EU28", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK"),], FUN = sum)  
# rm(lfsa_egan22d)

## - Introduce the values of LU and MT in the labor force table
# - Source for A02: SoEF questionnaire, calculated for Eurostat using the Labour force survey
#lfsa_complement <- rbind(cbind(unique(lfsa[lfsa$geo_code=="LU",c(1:4, 6:9)]), nace_r2_code="A02", time=2010, values=0.22), cbind(unique(lfsa[lfsa$geo_code=="MT",c(1:4, 6:9)]), nace_r2_code="A02", time=2010, values=0) )

lfsa <- lfsa[order(lfsa$geo_code, lfsa$unit_code, lfsa$age_code, lfsa$sex_code, lfsa$nace_r2_code, lfsa$time),]

lfsa[lfsa$unit_code=="THS" & lfsa$age_code=="Y_GE15" & lfsa$sex_code=="T" & lfsa$nace_r2_code=="A02" & lfsa$geo_code=="LU" & lfsa$time==2010, ]$values <- 0.22
lfsa <- rbind(lfsa, merge(data.frame(unit_code="THS", age_code="Y_GE15", sex_code="T", nace_r2_code="A02", geo_code="MT", geo="Malta", time=c(2008:2015), values=0), unique(lfsa[,c(1:4, 6:9)])))


# LU C16, C17 & C18 (source STATEC 2018)
# https://statistiques.public.lu/stat/TableViewer/tableView.aspx?ReportId=13161&IF_Language=fra&MainTheme=5&FldrName=2&RFPath=21
# https://adem.public.lu/dam-assets/fr/marche-emploi-luxembourg/faits-et-chiffres/statistiques/igss/Tableaux-interactifs-stock-emploi/Salaries-sectAct2Pos.xlsx

#lfsa[lfsa$unit_code=="THS" & lfsa$age_code=="Y_GE15" & lfsa$sex_code=="T" & lfsa$nace_r2_code=="C16" & lfsa$geo_code=="LU" & lfsa$time==2010, ]$values <- 0.6
lfsa <- rbind(lfsa[!(lfsa$unit_code=="THS" & lfsa$age_code=="Y_GE15" & lfsa$sex_code=="T" & lfsa$nace_r2_code=="C16" & lfsa$geo_code=="LU" & lfsa$time%in%c(2009:2018)), ], merge(data.frame(unit_code="THS", age_code="Y_GE15", sex_code="T", nace_r2_code="C16", geo_code="LU", geo="Luxembourg", time=c(2009:2018), values=c(610,	590,	600,	620,	620,	610,	570,	580,	580,	610)/1000), unique(lfsa[,c(1:4, 6:9)])) ) 

lfsa <- rbind(lfsa[!(lfsa$unit_code=="THS" & lfsa$age_code=="Y_GE15" & lfsa$sex_code=="T" & lfsa$nace_r2_code=="C17" & lfsa$geo_code=="LU" & lfsa$time%in%c(2009:2018)), ], merge(data.frame(unit_code="THS", age_code="Y_GE15", sex_code="T", nace_r2_code="C17", geo_code="LU", geo="Luxembourg", time=c(2009:2018), values=c(370,	380,	410,	410,	410,	410,	400,	390,	420,	460)/1000), unique(lfsa[,c(1:4, 6:9)])) )

lfsa <- rbind(lfsa[!(lfsa$unit_code=="THS" & lfsa$age_code=="Y_GE15" & lfsa$sex_code=="T" & lfsa$nace_r2_code=="C18" & lfsa$geo_code=="LU" & lfsa$time%in%c(2009:2018)), ], merge(data.frame(unit_code="THS", age_code="Y_GE15", sex_code="T", nace_r2_code="C18", geo_code="LU", geo="Luxembourg", time=c(2009:2018), values=c(940,	800,	770,	730,	720,	680,	670,	600,	590,	570)/1000), unique(lfsa[,c(1:4, 6:9)])) )




# LU official statistics of the enterprises: http://www.statistiques.public.lu/stat/TableViewer/tableView.aspx?ReportId=13333&sCS_ChosenLang=fr 
# update 2019.09.03
lfsa <- rbind(lfsa[!(lfsa$unit_code=="THS" & lfsa$age_code=="Y_GE15" & lfsa$sex_code=="T" & lfsa$nace_r2_code=="C31" & lfsa$geo_code=="LU" & lfsa$time%in%c(2008:2016)), ], merge(data.frame(unit_code="THS", age_code="Y_GE15", sex_code="T", nace_r2_code="C31", geo_code="LU", geo="Luxembourg", time=c(2008:2016), values=c(189, 180, 183, 187, 173, 169, 159, 163, 166)/1000), unique(lfsa[,c(1:4, 6:9)])))

# -- Possible error in the table, corrected by the member state in Forest Europe
lfsa[lfsa$unit_code=="THS" & lfsa$age_code=="Y_GE15" & lfsa$sex_code=="T" & lfsa$nace_r2_code=="A02" & lfsa$geo_code=="BE" & lfsa$time==2010, ]$values <- 2.8 # Value in the Forest Europe questionnaire

lfsa[lfsa$unit_code=="THS" & lfsa$age_code=="Y_GE15" & lfsa$sex_code=="T" & lfsa$nace_r2_code=="A02" & lfsa$geo_code=="IE" & lfsa$time==2010, ]$values <- 2.41 # Value in the Forest Europe questionnaire

## - remove the distinction between F41, F42 and F43 -
lfsa$nace_r2_code <- as.character(lfsa$nace_r2_code)
lfsa$geo_code <- as.character(lfsa$geo_code)
lfsa[lfsa$nace_r2_code %in% c("F41", "F42", "F43"),]$nace_r2_code <- "F"


# lfsa2 <- lfsa %>%
#   group_by(unit_code , age_code , sex_code , nace_r2_code , geo_code , unit , age , sex , geo , time) %>%
#   summarise(values = sum (values)) %>% 
#   filter(!is.na(values))


lfsa <- aggregate(values ~ unit_code + age_code + sex_code + nace_r2_code + geo_code + unit + age + sex + geo + time, data=lfsa, FUN=sum)

# Employment data from the national accounts: to see the eventual connections
# nama_10_a64_e <- get_eurostat("nama_10_a64_e",  time_format = "num", stringsAsFactors=FALSE)
# nama_10_a64_e <- label_eurostat(nama_10_a64_e, code = names(nama_10_a64_e)[-c(5:6)])
# 
# nama_10_a64_eF <- nama_10_a64_e[nama_10_a64_e$nace_r2_code%in%c("A02", "C16", "C17", "C18", "C31_C32", "C31-C33", "D", "F") & nama_10_a64_e$na_item_code=="EMP_DC" & nama_10_a64_e$unit_code%in%c("THS_PER", "THS_HW", "THS_JOB"),]
# rm("nama_10_a64_e")
# 
# nama_10_a64_eF[nama_10_a64_eF$nace_r2_code=="D",]$nace_r2_code <- "D35"
# nama_10_a64_eF[nama_10_a64_eF$nace_r2_code=="C31_C32",]$nace_r2_code <- "C31"
# compareLFSA_NAMA <- (merge(nama_10_a64_eF, lfsa, by=c("nace_r2_code", "geo_code", "time"))[,c(1:4,10,18)])
# 
# compareLFSA_NAMA$ratio.x.y <- compareLFSA_NAMA$values.x/compareLFSA_NAMA$values.y
# compareLFSA_NAMA[compareLFSA_NAMA$unit_code.x=="THS_PER", ] %>% group_by(nace_r2_code, geo_code) %>% summarise(mean=mean(ratio.x.y), sd=sd(ratio.x.y)) %>% View
# write.csv(compareLFSA_NAMA, "clipboard-1024")

for_awu <- get_eurostat("for_awu",  time_format = "num", stringsAsFactors=FALSE)



# -- country codes --
countryCodes <- unique(data.frame(lfsa$geo, geo_code=lfsa$geo_code))
countryCodes[,1] <- as.character(countryCodes[,1])
countryCodes[grep('Germany', countryCodes[,1]),1] <- 'Germany'


# ---- Use table at purchaser's price ----
naio_10_cp16 <- get_eurostat("naio_10_cp16",  time_format = "num", stringsAsFactors=FALSE)
naio_10_cp16 <- label_eurostat(naio_10_cp16, code = names(naio_10_cp16)[c(-6,-7)])

naio <- naio_10_cp16[naio_10_cp16$unit_code=="MIO_EUR"
  & naio_10_cp16$induse_code %in% c("D", "F", "J58")
  & naio_10_cp16$prod_na_code %in% c("CPA_A01", "CPA_A02", "CPA_A03", "CPA_B", "CPA_C10-12", "CPA_C13-15", "CPA_C16", "CPA_C17", "CPA_C18", "CPA_C19", "CPA_C20", "CPA_C21", "CPA_C22", "CPA_C23", "CPA_C24", "CPA_C25", "TOTAL") , ]

# rm(naio_10_cp16)


# ---- Annual detailed enterprise statistics for industry (NACE Rev. 2, B-E) ----
sbs_na_ind_r2 <- get_eurostat("sbs_na_ind_r2",  time_format = "num", stringsAsFactors=FALSE)
sbs_na_ind_r2 <- label_eurostat(sbs_na_ind_r2, code = names(sbs_na_ind_r2)[c(1:3)], fix_duplicated = TRUE)


sbs <- sbs_na_ind_r2[sbs_na_ind_r2$nace_r2_code %in% c("C181", "C1811", "C1812", "C1813", "C1814", "C182", # -- details on the printing sector --
  "C3101", "C3102", "C3103", "C3109", # -- details on the furniture sector --
  "D3511", "D3512", "D3513", "D3514", "D3521", "D3522", "D3523", "D3530") # -- details on the energy sector, probably useful to extract the transport of the energy 
  & sbs_na_ind_r2$indic_sb_code %in% c("V16110") # -- V16110 	Persons employed - number
# -- V16120 	Unpaid persons employed - number
# -- V16130 	Employees - number
# -- V16140 	Employees in full time equivalent units - number
# -- V16150 	Hours worked by employees - number 
  ,]

# rm(sbs_na_ind_r2)
sbsC <- sbs_na_ind_r2[sbs_na_ind_r2$nace_r2_code %in% c("C16", "C17", "C18", "C31", "C32", "D35") 
                                  & sbs_na_ind_r2$indic_sb_code %in% c("V16110"),]

# Crosscheck the connections between the NAMA and SBS
# compareSBS_NAMA <- (merge(nama_10_a64_eF[nama_10_a64_eF$unit_code=="THS_PER",c(1:4,9:10)], sbsC[,c(1:3,7:8)], by=c("nace_r2_code", "geo_code", "time")))
# 
# compareSBS_NAMA$ratio.x.y <- compareSBS_NAMA$values.x/compareSBS_NAMA$values.y*1000
# compareSBS_NAMA %>% group_by(nace_r2_code, geo_code) %>% summarise(mean=mean(ratio.x.y), sd=sd(ratio.x.y)) %>% View
# write.csv(compareSBS_NAMA, "clipboard-1024")


# ---- ProdCom data for the C31 sector ----

# -- old download method, from xlsx files --
# load the files from http://ec.europa.eu/eurostat/web/prodcom/data/excel-files-nace-rev.2
# 
# ProdCom2008 <- read_excel("./ProdCom/Download_20180717/Website_snapshot_2008_N2.xlsx", sheet = "Value", 
#                           col_names = as.character(read_excel("ProdCom/Download_20180717/Website_snapshot_2008_N2.xlsx", col_names = FALSE, skip = 2, n_max = 1)[1,]),
#                           na = c(":C", ":E", ":CE", "CE", "-"), skip = 6)
# 
# ProdCom <- cbind(time=2008, ProdCom2008[,c(1:11)], (lapply(ProdCom2008[,c(12:42)], as.numeric)))
# rm(ProdCom2008)
# 
# for (i in 2009:2017) ProdCom <- rbind.all.columns(ProdCom, cbind(time=i, 
#                                                                  read_excel(paste("./ProdCom/Download_20180717/Website_snapshot_",i,"_N2.xlsx", sep = ""), sheet = "Value", 
#                                                                             col_names = as.character(read_excel(paste("./ProdCom/Download_20180717/Website_snapshot_",i,"_N2.xlsx", sep = ""), col_names = FALSE, skip = 2, n_max = 1)[1,]),
#                                                                             na = c(":C", ":E", ":CE", "CE", "-"), skip = 6))) 
# 
# ProdComData <- NULL
# for (i in c(5,8,11, 13:ncol(ProdCom))) {
#   ProdComData <- rbind(ProdComData, data.frame(ProdCom[,c(1:3)], soldProductionValue=ProdCom[,i], country=colnames(ProdCom)[i]))}  

# Table DS-066341 from https://ec.europa.eu/eurostat/web/prodcom/data/database/ (download with PERIOD and PRCCODE in lines and DECL in rows)
# with PERIOD since 2008, PRCCODE in 31 - 32, INDICATORS=PRODVAL, as csv

ProdComData <- read.csv("./ProdCom/DS-066341_1_Data.Prodval.csv", header = TRUE, sep = ",", na.strings=c(":",""), colClasses='character' , stringsAsFactors = FALSE)
ProdComData$Value <- as.numeric(gsub(",", "", ProdComData$Value))
ProdComData[!is.na(ProdComData$Value) & ProdComData$Value==0,]$Value <- NA #-- most 0 are not real zeros.

ProdComData$DECL <- sub('Luxemburg', 'Luxembourg', ProdComData$DECL)
ProdComData$DECL <- sub('Value.', '', ProdComData$DECL)
ProdComData$DECL <- sub('The.', '', ProdComData$DECL)
ProdComData$DECL <- chartr('.', ' ', ProdComData$DECL)
ProdComData <- merge(ProdComData, countryCodes, by.x='DECL', by.y=1, all.x=TRUE, sort=FALSE)
ProdComData$geo_code <- as.character(ProdComData$geo_code)
ProdComData[is.na(ProdComData$geo_code),]$geo_code <- ProdComData[is.na(ProdComData$geo_code),]$country


# ---- wood content in C31 products ----
# -- aligned with the NOVA coefficients --

woodContentC31 <- data.frame( rbind(
c(31001150,0,"Swivel seats with variable height adjustments (excluding medical, surgical, dental or veterinary, and barbers’ chairs)"),
c(31001170,0,"Upholstered seats with metal frames (excluding swivel seats, medical, surgical, dental or veterinary seats, barbers’ or similar chairs, for motor, vehicles, for aircraft)"),
c(31001190,0,"Non-upholstered seats with metal frames (excluding medical, surgical, dental or veterinary seats, barbers’ or similar chairs, swivel seats)"),
c(31001210,0,"Seats convertible into beds (excluding garden seats or camping equipment)"),
c(31001230,0,"Seats of cane, osier, bamboo or similar materials"),
c(31001250,0.2,"Upholstered seats with wooden frames (including three piece suites) (excluding swivel seats)"),
c(31001290,0.2,"Non-upholstered seats with wooden frames (excluding swivel seats)"),
c(31001300,0,"Other seats, of HS 9401, n.e.c."),
c(31001400,0,"Parts of seats"),
c(31002030,0,"Parts of furniture, of metal, n.e.s. (excl. of seats and medical, surgical, dental or veterinary furniture)"),
c(31002050,1,"Parts of furniture, of wood, n.e.s. (excl. seats)"),
c(31002090,0,"Parts of furniture, n.e.s. (excl. of metal or wood, and of seats and medical, surgical, dental or veterinary furniture)"),
c(31011100,0,"Metal furniture for offices"),
c(31011200,.8,"Wooden furniture of a kind used in offices"),
c(31011300,.8,"Wooden furniture for shops"),
c(31021000,0.5,"Kitchen furniture"),
c(31031100,0.3,"Mattress supports (including wooden or metal frames fitted with springs or steel wire mesh, upholstered mattress bases, with wooden slats, divans)"),
c(31031230,0,"Mattresses of cellular rubber (including with a metal frame) (excluding water-mattresses, pneumatic mattresses)"),
c(31031250,0,"Mattresses of cellular plastics (including with a metal frame) (excluding water-mattresses, pneumatic mattresses)"),
c(31031270,0,"Mattresses with spring interiors (excluding of cellular rubber or plastics)"),
c(31031290,0,"Mattresses (excluding with spring interiors, of cellular rubber or plastics)"),
c(31091100,0,"Metal furniture (excluding office, medical, surgical, dental or veterinary furniture; barbers’ chairs - cases and cabinets specially designed for hi-fi systems, videos or televisions)"),
c(31091230,.8,"Wooden bedroom furniture (excluding builders’ fittings for cupboards to be built into walls, mattress supports, lamps and lighting fittings, floor standing mirrors, seats)"),
c(31091250,.8,"Wooden furniture for the dining-room and living-room (excluding floor standing mirrors, seats)"),
c(31091300,.8,"Other wooden furniture (excluding bedroom, dining-, living-room, kitchen, office, shop, medical, surgical, dental/veterinary furniture, cases and cabinets designed for hi-fi, videos and televisions)"),
c(31091430,0,"Furniture of plastics (excluding medical, surgical, dental or veterinary furniture - cases and cabinets specially designed for hi-fi systems, videos and televisions)"),
c(31091450,0,"Furniture of materials other than metal, wood or plastic (excluding seats, cases and cabinets specially designed for hi-fi systems, videos and televisions)")), stringsAsFactors = FALSE)
colnames(woodContentC31) <- c("PRCCODE","Share.of.Wood","Product.definition")
woodContentC31$Share.of.Wood <- as.numeric(woodContentC31$Share.of.Wood)

## ---- Calculations ----

# ---- Forest share in A02, C16 and C17 of 100% ----
woodShare <- data.frame(unique(data.frame(geo_code = lfsa$geo_code, time = lfsa$time, stringsAsFactors = FALSE)), woodShare = 1, nace_r2_code = "A02", stringsAsFactors = FALSE)
woodShare <- rbind(woodShare, data.frame(unique(data.frame(geo_code = lfsa$geo_code, time = lfsa$time, stringsAsFactors = FALSE)), woodShare = 1, nace_r2_code = "C16", stringsAsFactors = FALSE))
woodShare <- rbind(woodShare, data.frame(unique(data.frame(geo_code = lfsa$geo_code, time = lfsa$time, stringsAsFactors = FALSE)), woodShare = 1, nace_r2_code = "C17", stringsAsFactors = FALSE))

# ---- C18 coefficients: share of employment in C181 (printing and binding) compared to C18 as a whole (sbs) ----

sbsC181 <- aggregate(values~indic_sb_code + geo_code + time, FUN=sum, data=sbs[sbs$nace_r2_code %in% c("C1811", "C1812", "C1813", "C1814"), ])
names(sbsC181)[4] <- "C181Xvalue"
sbsC181 <- merge(sbsC181, sbs[sbs$nace_r2_code %in% c("C181"), c("indic_sb_code", "geo_code", "time", "values")], all=TRUE)
sbsC181 <- sbsC181[!(is.na(sbsC181$values) & is.na(sbsC181$C181Xvalue)),]
sbsC181$C181value <- apply(sbsC181[,c("C181Xvalue", "values")], 1, FUN=max, na.rm = TRUE)


sbsC181 <- merge(sbsC181[, c("indic_sb_code", "geo_code", "time", "C181value")], sbs[sbs$nace_r2_code %in% c("C182"), ], all=TRUE)[,c("indic_sb_code", "geo_code", "time", "C181value", "values")]
# Add zeros in C182 in LU where the sector is not represented.
sbsC181[sbsC181$geo_code=="LU" & sbsC181$indic_sb_code=="V16110" & sbsC181$time %in% c(2008:2015), ]$values <- 0

sbsC181$C181share <- ifelse((sbsC181$C181value + sbsC181$values)==0, NA, sbsC181$C181value / (sbsC181$C181value + sbsC181$values))

sbsC181 <- merge(unique(data.frame(geo_code = lfsa$geo_code, time = lfsa$time, stringsAsFactors = FALSE)), sbsC181)

woodShare <- rbind(woodShare, data.frame(geo_code = sbsC181$geo_code, time = sbsC181$time, woodShare = sbsC181$C181share, nace_r2_code = "C18", stringsAsFactors = FALSE))

# ---- C31 coefficient: wood content in the various furniture products listed in the prodcom database; wood content from expert say ----

ProdComC31 <- ProdComData[substring(ProdComData$PRCCODE, 1, 2)=="31", ]
ProdComC31$time <- as.numeric(substr(ProdComC31$PERIOD, nchar(ProdComC31$PERIOD)-3, nchar(ProdComC31$PERIOD)))

# -- clean the product details available only for one year (merging with the more general category) --
# merge 31001155 and 31001159 in 31001150 (swivel seats)
# merge 31011110 31011140 31011170 in 31011100 (metal furniture for office)
ProdComC31[ProdComC31$PRCCODE %in% c("31001155", "31001159"), ]$PRCCODE <- "31001150"
ProdComC31[ProdComC31$PRCCODE %in% c("31011110", "31011140", "31011170"), ]$PRCCODE <- "31011100"
ProdComC31 <- aggregate(Value~PRCCODE+DECL+PERIOD+INDICATORS+time+geo_code, data=ProdComC31, FUN=sum)

# drop all figures that are not in EU28 and not in the periods where data from LFSA is available
#ProdComC31 <- merge(apply((data.frame(expand.grid(apply(lfsa[,c("geo_code","time")], 2, unique)))), 2, FUN=as.character), ProdComC31)
ProdComC31 <- merge(data.frame(expand.grid(geo_code=unique(lfsa$geo_code), time=unique(lfsa$time), stringsAsFactors=FALSE)), ProdComC31)

ProdComC31 <- fillValueTime(ProdComC31, "Value", "time", c("PRCCODE", "geo_code"))

ProdComC31.t <- merge(ProdComC31, woodContentC31[,c(1:2)], all = TRUE)
ProdComC31.t$soldProductionWood <- ProdComC31.t$value_est * ProdComC31.t$Share.of.Wood


ProdComC31Wood <- merge(aggregate(soldProductionWood~time+geo_code, data=ProdComC31.t, FUN=sum), aggregate(value_est~time+geo_code, data=ProdComC31.t, FUN=sum))
#ProdComC31Wood[,c(1,2)] <- lapply(ProdComC31Wood[,c(1,2)], as.character)


# -- No data for CY, LU  and MT --
# -- Use the average for EU28-(CY, LU, MT). NB: employment values are very low or null in these countries for this sector. With these factors, we avoid NA. --
ProdComC31WoodEU28 <- data.frame(geo_code="EU28", merge(aggregate(soldProductionWood~time, data=ProdComC31Wood, FUN=sum), aggregate(value_est~time, data=ProdComC31Wood, FUN=sum)), stringsAsFactors = FALSE)
#ProdComC31WoodEU28[,c(1,2)] <- lapply(ProdComC31WoodEU28[,c(1,2)], as.character)

ProdComC31Wood <- rbind(ProdComC31Wood, ProdComC31WoodEU28)
#ProdComC31Wood <- merge(apply((data.frame(expand.grid(apply(lfsa[,c("geo_code","time")], 2, unique)))), 2, FUN=as.character), ProdComC31Wood, all=TRUE)
ProdComC31Wood <- merge(data.frame(expand.grid(geo_code=unique(lfsa$geo_code), time=unique(lfsa$time), stringsAsFactors=FALSE)), ProdComC31Wood, all=TRUE)


ProdComC31Wood$shareWoodC31 <- ProdComC31Wood$soldProductionWood/ProdComC31Wood$value_est

ProdComC31Wood[is.na(ProdComC31Wood$shareWoodC31),]$shareWoodC31 <- merge(ProdComC31Wood[is.na(ProdComC31Wood$shareWoodC31),c(1:2)], ProdComC31Wood[ProdComC31Wood$geo_code=="EU28",c(2,5)])$shareWoodC31


# controls
# ggplot(data=ProdComC31Wood, aes(y=shareWoodC31, , x=factor(time), group=paste(geo_code), colour=geo_code)) +geom_line() + geom_point()
# ggplot(data=ProdComC31.t[ProdComC31.t$geo_code=="RO" & substr(ProdComC31.t$PRCCODE, 1, 6) %in% c("310912", "310012"),], aes(y=soldProductionWood, , x=factor(time), group=paste(PRCCODE), colour=PRCCODE)) +geom_line() + geom_point()
# note a switch between 3109250 and 31001250 between 2015 and 2016

woodShare <- rbind(woodShare, data.frame(geo_code = ProdComC31Wood$geo_code, time = ProdComC31Wood$time, woodShare = ProdComC31Wood$shareWoodC31, nace_r2_code = "C31"))


# ---- D35 coefficient: share of the cost of wood in the total material inputs (naio, CPA_A01 to CPA_C25) ----
naioD <- as.data.frame(naio[naio$induse_code=="D", ])
naioDWood <- merge(setNames(aggregate(values ~ unit_code + stk_flow_code + induse_code + geo_code + time, data=naioD[naioD$prod_na_code %in% c("CPA_A02", "CPA_C16", "CPA_C17"),], FUN=sum), c("unit_code", "stk_flow_code", "induse_code", "geo_code", "time", "valuesWood")), aggregate(values ~ unit_code + stk_flow_code + induse_code + geo_code + time, data=naioD[naioD$prod_na_code !="TOTAL",], FUN=sum))

rm(naioD)

naioDWood$shareWoodD <- naioDWood$valuesWood/naioDWood$values

# add the dataset and complete for MT (no information available in NAIO, but no harvest locally and no import for energy: to be confirmed)
woodShare <- rbind(woodShare, data.frame(geo_code = naioDWood$geo_code, time = naioDWood$time, woodShare = naioDWood$shareWoodD, nace_r2_code = "D35"), data.frame(geo_code = "MT", time = unique(naioDWood$time), woodShare = 0, nace_r2_code = "D35"))


# ---- F coefficient: share of the cost of wood in the total inputs (naio) ----
# -- NB: the construction sector F is not detailed in the naio statistics. The category used is therefore F. However, it wold be more realistic to restrict 

naioF <- as.data.frame(naio[naio$induse_code=="F", ])
naioFWood <- merge(setNames(aggregate(values ~ unit_code + stk_flow_code + induse_code + geo_code + time, data=naioF[naioF$prod_na_code %in% c("CPA_A02", "CPA_C16", "CPA_C17"),], FUN=sum), c("unit_code", "stk_flow_code", "induse_code", "geo_code", "time", "valuesWood")), naioF[naioF$prod_na_code =="TOTAL",])

rm(naioF)

naioFWood$shareWoodF <- naioFWood$valuesWood/naioFWood$values
woodShare <- rbind(woodShare, data.frame(geo_code = naioFWood$geo_code, time = naioFWood$time, woodShare = naioFWood$shareWoodF, nace_r2_code = "F", stringsAsFactors = FALSE))

# ---- J58 coefficient: share of the cost of wood in the total inputs (naio) ----

naioJ58 <- as.data.frame(naio[naio$induse_code=="J58"&naio$time>2007, ])
naioJ58Wood <- merge(setNames(aggregate(values ~ unit_code + stk_flow_code + induse_code + geo_code + time, data=naioJ58[naioJ58$prod_na_code %in% c("CPA_A02", "CPA_C16", "CPA_C17", "CPA_C18"),], FUN=sum), c("unit_code", "stk_flow_code", "induse_code", "geo_code", "time", "valuesWood")), naioJ58[naioJ58$prod_na_code =="TOTAL",])

rm(naioJ58)

# No data available for Ireland. National data are only available for the sectors J58-60. We use the average share from all other EU countries (NB data partial after 2015 and unreliable for 2017).
naioJ58Wood <- naioJ58Wood[naioJ58Wood$time<2017, ] %>% group_by(unit_code, stk_flow_code, induse_code, time, prod_na_code, unit, stk_flow, induse, prod_na) %>% summarise(valuesWood = sum(valuesWood), values = sum(values)) %>% data.frame(., geo_code="IE", geo="Ireland") %>% rbind(naioJ58Wood, .)

naioJ58Wood$shareWoodJ58 <- naioJ58Wood$valuesWood/naioJ58Wood$values

woodShare <- rbind(woodShare, data.frame(geo_code = naioJ58Wood$geo_code, time = naioJ58Wood$time, woodShare = naioJ58Wood$shareWoodJ58, nace_r2_code = "J58", stringsAsFactors = FALSE))


# ---- Estimation of number of jobs in the forest-based and forest-related sectors ----

ForestEmployment <- merge(lfsa, woodShare)
ForestEmployment$ThousandEmployedPersons <- ForestEmployment$values * ForestEmployment$woodShare
ForestEmployment$geo_code <- as.character(ForestEmployment$geo_code)

plot(ForestEmployment$time, ForestEmployment$ThousandEmployedPersons, type = "p", col = as.factor(ForestEmployment$geo_code))

# compile a table with the sectors in columns
FBemployment <- unique(ForestEmployment[, c(1, 3:10)])
for (i in unique(ForestEmployment$nace_r2_code)) {
  tempEmp <- ForestEmployment[ForestEmployment$nace_r2_code==i,][,-c(2, 11,12)]
  colnames(tempEmp)[ncol(tempEmp)] <- paste("THDEmployedPersons", i, sep = "")
  FBemployment <- merge(FBemployment, tempEmp, all=TRUE)
}

# for an unknown reason, it generates duplicates
FBemployment <- unique(FBemployment)


FBemployment$THDEmployedPersons <- apply(FBemployment[,c(10:16)], 1, sum)
FBemployment$ShareEmployedPersonsA02 <- FBemployment[,c(10)]/FBemployment$THDEmployedPersons

FBemployment$ShareEmployedPersonsC16C17 <- apply(FBemployment[,c(11:12)], 1, sum)/FBemployment$THDEmployedPersons

FBemployment$ShareEmployedPersonsC18C31D35F <- apply(FBemployment[,c(13:16)], 1, sum)/FBemployment$THDEmployedPersons

# write.csv(FBemployment, "FBemployment.csv")
# 
# write.csv(FBemployment[FBemployment$time==2010,], "FBemployment2010.csv", na="")

# library(rworldmap)
# newmap <- getMap(resolution = "low")
# plot(newmap, xlim = range(c(-32,60)), ylim = range(c(34,81)), asp = 1)
# plot(newmap, xlim = range(c(-5,40)), ylim = range(c(35,70)), asp = 1)
# str(newmap)
# 
# # -- looking for details
# View(cbind(as.character(unique(naio_10_cp16$prod_na)), as.character(unique(naio_10_cp16$prod_na_code))))
# write.csv(naio_10_cp16[naio_10_cp16$induse_code=="A02" & naio_10_cp16$prod_na_code %in% c("EMP_DC", "D1", "P1", "B2A3G", "B2A3N", "B1G") & naio_10_cp16$unit_code %in% c("MIO_EUR", "HW") , ], "clipboard-1024")
# nb: in DK, the hours worked are wrong for 2005-2009 and 2014 (multiplied by 1000)


# write.csv(aggregate(values ~ time + geo_code + nace_r2_code, data=lfsa, FUN = function(x){(!is.na(x))}), "clipboard-1024")

# write.csv(lfsa[,c(4,5,11,12)], "clipboard-1024")

# View(lfsa[lfsa$nace_r2_code %in% c("A02", "C16", "C17", "C18", "C31", "D35", "F41", "F42", "F43") & lfsa$geo_code %in% c("LU", "MT"),c(1,3,4,5,11,12)])

# write.csv(aggregate(values ~ time + geo_code + induse_code, data=naio_10_cp16[naio_10_cp16$induse_code %in% c("A02", "C16", "C17", "C18", "C31_32", "D", "F") & naio_10_cp16$prod_na_code == "EMP_DC",], FUN = function(x){sum(!is.na(x))}), "clipboard-1024")

# write.csv(naio_10_cp16[naio_10_cp16$induse_code %in% c("A02", "C16", "C17", "C18", "C31_32", "D", "F") & naio_10_cp16$prod_na_code == "EMP_DC",c(1,3,4,5,11,12)], "clipboard-1024")


# View(naio_10_cp16[naio_10_cp16$induse_code %in% c("A02", "C16", "C17", "C18", "C31_32", "D", "F") & naio_10_cp16$prod_na_code == "EMP_DC" & naio_10_cp16$geo_code %in% c("LU", "MT"),c(1,3,4,5,11,12)])

# ------
# ---- Modelling the correlation with other datasets ----
# - To produce a dataset merging the LFS, NAIO and SBS datasets requires adaptations of the different categories -
tempLfsa <- lfsa[,c("nace_r2_code","geo_code","time","values")]


tempLfsa$induse_code <- as.character(tempLfsa$nace_r2_code)
tempLfsa$geo_code <- as.character(tempLfsa$geo_code)
tempLfsa[tempLfsa$induse_code=="C31", ]$induse_code <- "C31_32"
tempLfsa[tempLfsa$induse_code=="D35", ]$induse_code <- "D"
tempLfsa[substring(tempLfsa$induse_code, 1, 1) == "F", ]$induse_code <- "F"

tempLfsa <- aggregate(values ~ time + geo_code + induse_code, data=tempLfsa, FUN=sum)

tempNaio <- naio_10_cp16[naio_10_cp16$induse_code %in% c("A02", "C16", "C17", "C18", "C31_32", "D", "F", "J58") & naio_10_cp16$prod_na_code == "EMP_DC" & naio_10_cp16$unit_code=="HW" ,c(3,5,11,12)]

tempNaio[,c(1:2)] <- apply(tempNaio[,c(1:2)], 2, FUN=as.character)
colnames(tempNaio)[4] <- "valuesNaio"
tempNaio <- tempNaio[tempNaio$valuesNaio>0 & !is.na(tempNaio$valuesNaio),]

# filterNaio <- aggregate(valuesNaio ~ geo_code + induse_code, data=tempLNS, FUN = function(x){c(max(x)/min(x), min(x)/median(x))})
filterNaio <- aggregate(valuesNaio ~ geo_code + induse_code, data=tempNaio, FUN = function(x){c(max(x)/min(x), min(x)/median(x))})
filterNaio$maxMin <- filterNaio$valuesNaio[,1]
filterNaio$minMed <- filterNaio$valuesNaio[,2]
filterNaio <- filterNaio[, -3]
tempNaio <- merge(tempNaio, filterNaio, all=TRUE)

# - Calculate the average number of employees in lfsa and compare it to the Naio statistics. If higher than 10*5, then there is a problem of order of magnitude (thousand); 
# if there is a ratio close to 1, then it is likely a problem of unit: full time equivalent reported instead of working hours
tempLN <- merge(aggregate(values ~ geo_code + induse_code, data=tempLfsa, FUN=mean), tempNaio, all = TRUE)
tempLN$workingTimePerEmployee <- tempLN$valuesNaio / tempLN$values
tempLN <- merge(tempLN[!names(tempLN) %in% "values"], tempLfsa, all = TRUE)
tempLN$valuesNaioCor <- tempLN$valuesNaio
tempLN[tempLN$maxMin>10 & !is.na(tempLN$maxMin) & !is.na(tempLN$workingTimePerEmployee)& tempLN$workingTimePerEmployee>10**5,]$valuesNaioCor <- tempLN[tempLN$maxMin>10 & !is.na(tempLN$maxMin) & !is.na(tempLN$workingTimePerEmployee) & tempLN$workingTimePerEmployee>10**5,]$valuesNaio / 1000

tempLN[tempLN$maxMin>10 & !is.na(tempLN$maxMin) & !is.na(tempLN$workingTimePerEmployee) & tempLN$workingTimePerEmployee<5,]$valuesNaioCor <- tempLN[tempLN$maxMin>10 & !is.na(tempLN$maxMin) & !is.na(tempLN$workingTimePerEmployee) & tempLN$workingTimePerEmployee<5,]$valuesNaio * 1000

tempLN <- merge ( aggregate((valuesNaio/values) ~ geo_code + induse_code, data=subset(merge(unique(tempLN[tempLN$maxMin>1300 & !is.na(tempLN$maxMin) & tempLN$workingTimePerEmployee<3,c(1,2)]), tempLN), workingTimePerEmployee>3), FUN=mean) , tempLN, all=TRUE)  # controll function: FUN=function(x){c(mean(x), sqrt(var(x)/length(na.omit(x))))}

tempLN[tempLN$maxMin>1300 & !is.na(tempLN$maxMin) & !is.na(tempLN$workingTimePerEmployee) & tempLN$workingTimePerEmployee<3,]$valuesNaioCor <- tempLN[tempLN$maxMin>1300 & !is.na(tempLN$maxMin) & !is.na(tempLN$workingTimePerEmployee) & tempLN$workingTimePerEmployee<3,]$valuesNaio * tempLN[tempLN$maxMin>1300 & !is.na(tempLN$maxMin) & !is.na(tempLN$workingTimePerEmployee) & tempLN$workingTimePerEmployee<3,]$'(valuesNaio/values)'

# manual additional corrections
tempLN[tempLN$geo_code=="MT"&tempLN$induse_code=="D"& tempLN$time==2015, ]$valuesNaioCor = 100*tempLN[tempLN$geo_code=="MT"&tempLN$induse_code=="D"& tempLN$time==2015, ]$valuesNaio

# remove errors impossible to correct
tempLN[tempLN$geo_code=="FR" & tempLN$time %in% c(2014,2015) & tempLN$induse_code %in% c("C31_32", "D"), ]$valuesNaioCor <- NA


# -- connection to SBS

tempSbs <- sbs <- sbs_na_ind_r2[sbs_na_ind_r2$nace_r2_code %in% c("C16", "C17", "C18", "C31", "D") & sbs_na_ind_r2$indic_sb_code %in% c("V16110"),c(1,3,7,8)]
tempSbs$induse_code <- as.character(tempSbs$nace_r2_code)
tempSbs$geo_code <- as.character(tempSbs$geo_code)
tempSbs[tempSbs$induse_code=="C31", ]$induse_code <- "C31_32"

tempSbs <- aggregate(values ~ time + geo_code + induse_code, data=tempSbs, FUN=sum)
colnames(tempSbs)[4] <- "valuesSbs"

tempLNS <- merge(tempLN, tempSbs, all = TRUE)



# -- correlations between datasets--
tempLNS$valuesDIVvaluesNaioCor <- tempLNS$values/ tempLNS$valuesNaioCor

summary(lm(values ~ valuesNaioCor, data=tempLNS[tempLNS$geo_code!="EU28",]))
summary(lm(values ~ valuesNaioCor:as.factor(induse_code=="C31_32")-1, data=tempLNS[tempLNS$geo_code!="EU28",]))
summary(lm(values ~ valuesNaioCor + valuesDIVvaluesNaioCor , data=tempLNS[tempLNS$geo_code!="EU28",]))
summary(lm(log10(tempLNS$values)~log10(tempLNS$valuesSbs)))

summary(lm(values ~ valuesSbs:geo_code:induse_code + geo_code:induse_code, data=tempLNS[tempLNS$geo_code!="EU28",]))


# --- model prediction ---

#View(cbind(tempLNS, predict(lm(values ~ valuesNaioCor:as.factor(induse_code=="C31_32")+as.factor(geo_code)-1, data=tempLNS[tempLNS$geo_code!="EU28",]),tempLNS)))


# ---- modelling with standardized numbers using the average for the country and industry ----
tempLNSmean <- aggregate(cbind(values, valuesNaioCor) ~ geo_code + induse_code, data=tempLNS, mean)
tempLNSmean <- merge(tempLNSmean, aggregate(valuesSbs ~ geo_code + induse_code, data=tempLNS, mean), all=TRUE)
colnames(tempLNSmean)[3:5] <- c("meanValues", "meanValuesNaioCor", "meanValuesSbs")
tempLNS <- merge(tempLNS, tempLNSmean)
tempLNS$stdValues <- tempLNS$values/tempLNS$meanValues
tempLNS$stdValuesNaioCor <- tempLNS$valuesNaioCor/tempLNS$meanValuesNaioCor
tempLNS$stdValuesSbs <- tempLNS$valuesSbs/tempLNS$meanValuesSbs

plot(stdValues~ stdValuesNaioCor, col=as.factor(induse_code), data=tempLNS[tempLNS$geo_code!="EU28"& tempLNS$induse_code!="C31_32",])

summary(lm(stdValues~ 0+stdValuesNaioCor:as.factor(induse_code=="C31_32"), data=tempLNS[tempLNS$geo_code!="EU28" ,]))

summary(lm(stdValues~ stdValuesNaioCor, data=tempLNS[tempLNS$geo_code!="EU28" ,]))

summary(lm(stdValues~ 0+stdValuesNaioCor, data=tempLNS[tempLNS$geo_code!="EU28" & tempLNS$induse_code!="C31_32",])) # no effect of the induse_code, except the C31_32 in NAIO which does not correspond to the C31 in the LFS
summary(lm(tempLNS$stdValues ~ tempLNS$stdValuesSbs))

summary(lm(stdValues~ 0+stdValuesNaioCor, data=tempLNS[tempLNS$geo_code!="EU28" & tempLNS$induse_code=="C31_32",]))
with(tempLNS[tempLNS$geo_code!="EU28" & tempLNS$induse_code!="C31_32",], t.test(stdValues-stdValuesNaioCor))

summary(lm(stdValues~ stdValuesSbs, data=tempLNS[tempLNS$geo_code!="EU28" ,]))

# ---- modeling using relative changes in both sources ----
tempLNShist <- tempLNS[,c("geo_code", "induse_code", "time", "values", "valuesNaioCor", "valuesSbs")]
names(tempLNShist)[4:6] <- c("valuesPrevYear", "valuesNaioCorPrevYear", "valuesSbsPrevYear")
tempLNShist$time <- tempLNShist$time + 1
tempLNShist <- merge(tempLNS[,c("geo_code", "induse_code", "time", "values", "valuesNaioCor", "valuesSbs")], tempLNShist, all = TRUE)
tempLNShist$changeValues <- tempLNShist$values / tempLNShist$valuesPrevYear -1
tempLNShist$changeValuesNaioCor <- tempLNShist$valuesNaioCor / tempLNShist$valuesNaioCorPrevYear -1
tempLNShist$changeValuesSbs <- tempLNShist$valuesSbs / tempLNShist$valuesSbsPrevYear -1

summary(lm(changeValues~changeValuesNaioCor-1, data=tempLNShist[tempLNShist$geo_code!="EU28"& tempLNShist$induse_code!="C31_32",]))
summary(lm(changeValues~changeValuesNaioCor-1, data=tempLNShist[tempLNShist$geo_code!="EU28",]))
summary(lm(changeValues~changeValuesNaioCor:induse_code + changeValuesNaioCor-1, data=tempLNShist[tempLNShist$geo_code!="EU28",]))
with(data=tempLNShist[tempLNShist$geo_code!="EU28",], t.test(changeValues-changeValuesNaioCor))
plot(changeValues~changeValuesNaioCor, data=tempLNShist[tempLNS$geo_code!="EU28"& tempLNS$induse_code!="C31_32",])

t.test(tempLNShist[tempLNShist$geo_code!="EU28"& tempLNShist$induse_code!="C31_32",]$changeValues, tempLNShist[tempLNShist$geo_code!="EU28"& tempLNShist$induse_code!="C31_32",]$changeValuesNaioCor)

summary(lm(changeValues~changeValuesSbs-1, data=tempLNShist[tempLNShist$geo_code!="EU28"& tempLNShist$induse_code!="C31_32",]))
summary(lm(changeValues~changeValuesSbs-1, data=tempLNShist[tempLNShist$geo_code!="EU28",]))
with(data=tempLNShist[tempLNShist$geo_code!="EU28",], t.test(changeValues-changeValuesSbs))


aggregate(values ~ geo_code + induse_code, data=tempLNS, FUN = function(x){c(max(x)/mean(x), min(x)/mean(x), max(x)/min(x))})
write.csv(aggregate(valuesNaio ~ geo_code + induse_code, data=tempLNS, FUN = function(x){c(max(x)/mean(x), min(x)/mean(x), max(x)/min(x))}), "clipboard-1024")
write.csv(tempLNS, "clipboard-1024")


# ---- Complement dataset ----
# ---------- tests fill-in missing data ---------
# -- lfsa_c --

lfsa_c <- merge(data.frame(expand.grid(apply(lfsa[,c(1:5, 10)], 2, unique))), lfsa[,c(1:5, 10:11)], all=TRUE)
lfsa_c$time <- as.numeric(as.character(lfsa_c$time))

# -- Use the variations in the NAIO or SBS tables as a driver of change when possible --
# -- This is supposed to give a better representation of the economic changes --
# -- NAIO --
lfsa_c <- merge(lfsa_c, tempLN[, c("geo_code", "induse_code", "time", "valuesNaioCor")], by.x = c("nace_r2_code", "geo_code", "time"), by.y =  c("induse_code", "geo_code", "time"), all.x=TRUE)

lfsa_c1 <- merge(lfsa_c[is.na(lfsa_c$values) & !is.na(lfsa_c$valuesNaioCor),], lfsa_c[!is.na(lfsa_c$values) & !is.na(lfsa_c$valuesNaioCor),], by.x = c("unit_code", "age_code", "sex_code", "nace_r2_code", "geo_code"), by.y = c("unit_code", "age_code", "sex_code", "nace_r2_code", "geo_code"))
lfsa_c1$values.x <- lfsa_c1$values.y / lfsa_c1$valuesNaioCor.y * lfsa_c1$valuesNaioCor.x

lfsa_c1$timedif <-  lfsa_c1$time.x - lfsa_c1$time.y
lfsa_c1p <- merge(lfsa_c1, aggregate(timedif ~ unit_code + age_code + sex_code + nace_r2_code + geo_code + time.x, FUN = max, data=lfsa_c1[lfsa_c1$timedif<0, ]))[,c(1:6,8)]
colnames(lfsa_c1p)[6:7] <- c("time", "value_p")
lfsa_c1n <- merge(lfsa_c1, aggregate(timedif ~ unit_code + age_code + sex_code + nace_r2_code + geo_code + time.x, FUN = min, data=lfsa_c1[lfsa_c1$timedif>0, ]))[,c(1:6,8)]
colnames(lfsa_c1n)[6:7] <- c("time", "value_n")

lfsa_c1 <- merge(lfsa_c1p, lfsa_c1n, all=TRUE)

lfsa_c1$value_eNAIO <- ifelse(is.na(lfsa_c1$value_p), lfsa_c1$value_n, ifelse(is.na(lfsa_c1$value_n), lfsa_c1$value_p, (lfsa_c1$value_p+lfsa_c1$value_n)/2))

# -- SBS --
lfsa_c2 <- merge(lfsa_c[,-8], tempSbs, by.x = c("nace_r2_code", "geo_code", "time"), by.y =  c("induse_code", "geo_code", "time"), all.x=TRUE)

lfsa_c2 <- merge(lfsa_c2[is.na(lfsa_c2$values) & !is.na(lfsa_c2$valuesSbs),], lfsa_c2[!is.na(lfsa_c2$values) & !is.na(lfsa_c2$valuesSbs),], by.x = c("unit_code", "age_code", "sex_code", "nace_r2_code", "geo_code"), by.y = c("unit_code", "age_code", "sex_code", "nace_r2_code", "geo_code"))
lfsa_c2$values.x <- lfsa_c2$values.y / lfsa_c2$valuesSbs.y * lfsa_c2$valuesSbs.x

lfsa_c2$timedif <-  lfsa_c2$time.x - lfsa_c2$time.y
lfsa_c2p <- merge(lfsa_c2, aggregate(timedif ~ unit_code + age_code + sex_code + nace_r2_code + geo_code + time.x, FUN = max, data=lfsa_c2[lfsa_c2$timedif<0, ]))[,c(1:6,8)]
colnames(lfsa_c2p)[6:7] <- c("time", "value_p")
lfsa_c2n <- merge(lfsa_c2, aggregate(timedif ~ unit_code + age_code + sex_code + nace_r2_code + geo_code + time.x, FUN = min, data=lfsa_c2[lfsa_c2$timedif>0, ]))[,c(1:6,8)]
colnames(lfsa_c2n)[6:7] <- c("time", "value_n")

lfsa_c2 <- merge(lfsa_c2p, lfsa_c2n, all=TRUE)

lfsa_c2$value_eSBS <- ifelse(is.na(lfsa_c2$value_p), lfsa_c2$value_n, ifelse(is.na(lfsa_c2$value_n), lfsa_c2$value_p, (lfsa_c2$value_p+lfsa_c2$value_n)/2))


# -- In case there is no other information, use a linear regression if significant --
# EstimateMissingValues <- function(dataset, Value, Year, variables){
#   dataset <- dataset[,c(Value, Year, variables)]
#   colnames(dataset)[c(1,2)] <- c("Value", "Year")
#   
#   # -- Linear regression returning parameters and test --
#   ## - clean for regression and
#   dataset <- dataset[!is.na(dataset$Value),]
#   ## - regression -
#   s.dataset <- split(dataset, dataset[,c(3:(ncol(dataset)))], drop=TRUE)
#   lm.dataset <- lapply(s.dataset, FUN=function(x){(summary(lm(formula=Value~Year, x))$coefficients)[c(1,2,8)]})
#   
#   tlm.dataset <- as.data.frame(t(data.frame(lm.dataset)))
#   colnames(tlm.dataset) <- c("Intercept","coeff.Year", "Pr_t")
#   
#   # - Remove non significant results -
#   tlm.dataset <- tlm.dataset[tlm.dataset$Pr_t < 0.1 & !is.na(tlm.dataset$Pr_t),]
#   tlm.dataset$fields <- rownames(tlm.dataset)
#   
#   # -- Expand dataset to represent all years for every combination of variables, filling in with NA values for the missing years 
#   # (except if no information at all in the time frame) --
# 
# 
#   expandedDataset <- data.frame(expand.grid(apply(dataset[,-c(1)], 2, unique)))
#   expandedDataset$Year <- as.numeric(as.character(expandedDataset$Year))
#   expandedDataset <- merge(expandedDataset, dataset, all.x=TRUE)
#   dataset <- merge(expandedDataset, tidyr::separate(tlm.dataset, fields, colnames(dataset)[-c(1:2)], sep="\\."), all.x=TRUE)
#   
#   # dataset <- dataset[,c(ncol(dataset), 1:ncol(dataset)-1)]
#   # if(is.numeric(dataset[dataset$Value==0 & !is.na(dataset$Value),]$Value>0)) {dataset[dataset$Value==0 & !is.na(dataset$Value),]$Value <- NA}
#   return(dataset)
# }
# 
# EstimateMissingValues(lfsa_c, "values", "time", c("nace_r2_code", "geo_code", "unit_code", "age_code", "sex_code"))

## It appears that there is othing to replace with a significative model.



# -- add the new results to the dataset --
lfsa_c <- merge(merge(lfsa_c, lfsa_c1[,c(-7,-8)], all.x = TRUE), lfsa_c2[,c(-7,-8)], all.x = TRUE)
lfsa_c$values_c <- ifelse(!is.na(lfsa_c$values), lfsa_c$values, ifelse(!is.na(lfsa_c$value_eNAIO), lfsa_c$value_eNAIO, ifelse(!is.na(lfsa_c$value_eSBS), lfsa_c$value_eSBS, NA)))


# -- Specific calculations for Malta, where some information is available in NAIO for the sector C16, using the coefficient between LFS and NAIO from the other sectors (quite constant)
lfsa_c[lfsa_c$geo_code=="MT" & lfsa_c$nace_r2_code=="C16",]$values_c <- lfsa_c[lfsa_c$geo_code=="MT" & lfsa_c$nace_r2_code=="C16",]$valuesNaioCor * mean(with(lfsa_c[lfsa_c$geo_code=="MT",], values/valuesNaioCor), na.rm = TRUE)

# -- Specific calculations for Malta, where some information is available in NAIO for the sector C17, and one point is available in LFS for 2017: use of a coefficient corresponding to the ratio of the averages
lfsa_c[lfsa_c$geo_code=="MT" & lfsa_c$nace_r2_code=="C17",]$values_c <- lfsa_c[lfsa_c$geo_code=="MT" & lfsa_c$nace_r2_code=="C17",]$valuesNaioCor * mean(lfsa_c[lfsa_c$geo_code=="MT" & lfsa_c$nace_r2_code=="C17",]$values, na.rm=TRUE)/mean(lfsa_c[lfsa_c$geo_code=="MT" & lfsa_c$nace_r2_code=="C17",]$valuesNaioCor, na.rm=TRUE)
# sum(lfsa_egan22d[lfsa_egan22d$geo_code=="MT" & lfsa_egan22d$age_code=="Y_GE15" & lfsa_egan22d$sex_code=="T" & lfsa_egan22d$time==2010,]$values, na.rm=TRUE)

# -- If no data is available, then use the linear interpolation if there are information before and after. Use the closest (in time) known value in other cases --
# lfsa_c3 <- merge(lfsa_c[is.na(lfsa_c$values_c),c(1:6,11)], lfsa_c[!is.na(lfsa_c$values_c),c(1:6,11)], by.x = c("unit_code", "age_code", "sex_code", "nace_r2_code", "geo_code"), by.y = c("unit_code", "age_code", "sex_code", "nace_r2_code", "geo_code"))
# lfsa_c3$timedif <-  lfsa_c3$time.x - lfsa_c3$time.y
# lfsa_c3p <- merge(lfsa_c3, aggregate(timedif ~ unit_code + age_code + sex_code + nace_r2_code + geo_code + time.x, FUN = min, data=lfsa_c3[lfsa_c3$timedif>0, ]))[,c(1:6,9:10)]
# colnames(lfsa_c3p)[6:8] <- c("time", "time_p", "value_p")
# lfsa_c3n <- merge(lfsa_c3, aggregate(timedif ~ unit_code + age_code + sex_code + nace_r2_code + geo_code + time.x, FUN = max, data=lfsa_c3[lfsa_c3$timedif<0, ]))[,c(1:6,9:10)]
# colnames(lfsa_c3n)[6:8] <- c("time", "time_n", "value_n")
# 
# lfsa_c3 <- merge(lfsa_c3p, lfsa_c3n, all=TRUE)
# lfsa_c3$value_est <- ifelse(is.na(lfsa_c3$value_n), lfsa_c3$value_p, ifelse(is.na(lfsa_c3$value_p), lfsa_c3$value_n, lfsa_c3$value_n+lfsa_c3$value_p))
# 
# lfsa_c3$value_est <- with(lfsa_c3, ifelse(is.na(value_n), value_p, ifelse(is.na(value_p), value_n, (value_p-value_n)/(time_p-time_n)*(time-time_n)+value_n  )))
# 
# # -- Final dataset on employment --
# lfsa_c <- merge(lfsa_c, lfsa_c3[, c(1:6,11)], all.x=TRUE)
# lfsa_c[is.na(lfsa_c$values_c),]$values_c <- lfsa_c[is.na(lfsa_c$values_c),]$value_est
lfsa_c3 <- fillValueTime(lfsa_c, "values_c", "time", c("unit_code", "age_code", "sex_code", "nace_r2_code", "geo_code"))

# ---- Filtering and complementing the woodshare table ----

# -- Create a sataset with the same countries, dates and sectors as lfsa_c --
woodShare <- merge(apply((data.frame(expand.grid(apply(lfsa[,c("nace_r2_code","geo_code","time")], 2, unique)))), 2, FUN=as.character), woodShare)

woodShare_c <- fillValueTime(woodShare, "woodShare", "time", c("nace_r2_code","geo_code"))[c("time", "nace_r2_code", "geo_code", "value_est")]
colnames(woodShare_c)[4] <- "woodShare"

lfsa_cWood <- merge(lfsa_c3, woodShare_c, all.x=TRUE)
lfsa_cWood$employment <- lfsa_cWood$value_est*lfsa_cWood$woodShare

lfsa_cWood[lfsa_cWood$geo_code=="EU28" & is.na(lfsa_cWood$employment),]$employment <- with(merge(lfsa_cWood[lfsa_cWood$geo_code=="EU28" & is.na(lfsa_cWood$employment),c(1:7)], merge(aggregate(employment~time+nace_r2_code+unit_code+age_code+sex_code, data=lfsa_cWood[lfsa_cWood$geo_code!="EU28",], FUN=sum), aggregate(value_est~time+nace_r2_code+unit_code+age_code+sex_code, data=lfsa_cWood[lfsa_cWood$geo_code!="EU28",], FUN=sum))), values*employment/value_est)
  

write.csv(lfsa_cWood, "lfsa_cwood.csv")

write.csv(merge(reshape2::dcast(lfsa_cWood, unit_code+age_code+sex_code+geo_code+time~nace_r2_code, value.var="employment"), lfsa_t), "lfsa_c_wood.csv")

# -- plots --
library(ggplot2)
library(dplyr)
Employment_sectors_plot <- 
    aggregate(employment~time+nace_r2_code, FUN=sum, data=lfsa_cWood[lfsa_cWood$geo_code!="EU28" & lfsa_cWood$geo_code!="UK" & lfsa_cWood$time<2020,]) %>% 
    ggplot(data=., aes(y=employment, x=factor(time), group=nace_r2_code, colour=nace_r2_code)) +
    geom_line() +
    geom_point() + 
    scale_y_continuous(breaks=c(0:8*200)) +
    expand_limits(y=0) + 
    #  ggtitle("Employment in forestry, the wood-based and wood related sectors in the EU27") +
    xlab("Year") + ylab("Thousands of people employed in the EU27") +
    annotate("text", x = c(.9), y=(aggregate(employment~time+nace_r2_code, FUN=sum, data=lfsa_cWood[lfsa_cWood$geo_code!="EU28" & lfsa_cWood$geo_code!="UK"  & lfsa_cWood$time==2008,])$employment+c(0,0,0,0,0,0,0,0)), label = aggregate(employment~time+nace_r2_code, FUN=sum, data=lfsa_cWood[lfsa_cWood$geo_code!="EU28" & lfsa_cWood$geo_code!="UK" & lfsa_cWood$time==2008,])$nace_r2_code, hjust=1, colour=c("#F8766D", "#CD9600", "#7CAE00", "#00BE67", "#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC")) +
    theme_bw()

Employment_sectors_plot + theme(legend.position="none")

Employment_sectors_plot + guides(title="Sector",
#                    breaks=unique(Employment_sectors_plot[["data"]][["nace_r2_code"]]),
                    labels=c("Forestry and logging",                                                                                                           
                             "Manufacture of wood and of products of wood and cork, except furniture; manufacture of articles of straw and plaiting materials",
                             "Manufacture of paper and paper products",
                             "Printing and reproduction of recorded media",
                             "Manufacture of furniture",
                             "Electricity, gas, steam and air conditioning supply",
                             "Publishing activities",
                             "Construction"))


aggregate(employment~time+nace_r2_code, FUN=sum, data=lfsa_cWood[lfsa_cWood$geo_code!="EU28",]) %>% .[c("employment")]

ggplot(data=aggregate(woodShare~time+nace_r2_code+geo_code, FUN=sum, data=lfsa_cWood[lfsa_cWood$geo_code!="EU28" & lfsa_cWood$nace_r2_code %in% c("C18", "C31", "D35", "F", "J58"),]), aes(y=woodShare, x=factor(time), group=paste(geo_code, nace_r2_code), colour=geo_code)) +
  geom_line() +
  geom_point() + 
  expand_limits(y=0) + 
  xlab("Year") + ylab("Employment") +
  ggtitle("Changes in employment") +
  theme_bw()

ggplot(data=aggregate(woodShare~time+nace_r2_code, FUN=mean, data=lfsa_cWood[lfsa_cWood$geo_code!="EU28" & lfsa_cWood$nace_r2_code %in% c("C18", "C31", "D35", "F", "J58"),]), aes(y=woodShare, x=factor(time), group=paste(nace_r2_code), colour=nace_r2_code)) +
  geom_line() +
  geom_point() + 
  expand_limits(y=0) + 
  xlab("Year") + ylab("Share of employment") +
  ggtitle("Changes in share of employment") +
  theme_bw()

ggplot(data=aggregate(employment~time+geo_code, FUN=sum, data=lfsa_cWood[lfsa_cWood$geo_code!="EU28",]), aes(y=employment, x=factor(time), group=geo_code, colour=geo_code)) +
  geom_line() +
  geom_point() + 
  expand_limits(y=0) + 
  xlab("Year") + ylab("Employment") +
  ggtitle("Changes in employment") +
  theme_bw()



selectCountries <- lfsa_cWood %>% group_by(time, geo_code) %>% summarise(val=sum(employment)) %>% group_by(geo_code) %>%  summarise(Max=max(val, na.rm=TRUE)/min(val, na.rm=TRUE)) %>% .[.$Max>1.5 & .$geo_code!="ES", "geo_code"] %>% apply(2,FUN = as.character)
  aggregate(employment~time+geo_code, FUN=sum, data=lfsa_cWood[lfsa_cWood$geo_code%in%selectCountries, ]) %>% 
  ggplot(aes(y=employment, x=factor(time), group=geo_code, colour=geo_code)) +
  geom_line() +
  geom_point() + 
  expand_limits(y=0) + 
  xlab("Year") + ylab("Employment") +
  ggtitle("Changes in employment") +
  theme_bw()

  #write.csv(reshape2::dcast(nace_r2_code~time, value.var="employment", data=aggregate(employment~time+nace_r2_code, FUN=sum, data=lfsa_cWood[lfsa_cWood$geo_code!="EU28",])), "clipboard")
  
#write.csv(lfsa_cWood[lfsa_cWood$time<2017, ] %>% reshape2::dcast(unit_code+age_code+sex_code+geo_code+time~nace_r2_code, value.var="employment") %>% data.frame(., WoodBased=.$C16+.$C17, WoodRelated= .$C18 + .$C31 + .$D35+ .$F + .$J58, Total = .$A02+.$C16+.$C17 + .$C18 + .$C31 + .$D35+ .$F + .$J58) %>% data.frame(.,ShareForestry=.$A02/.$Total, ShareWoodBased = .$WoodBased/.$Total, ShareWoodRelated=.$WoodRelated/.$Total ) %>% group_by(unit_code, age_code, sex_code, geo_code) %>% summarise(ShareWoodBased=mean(ShareWoodBased), ShareWoodRelated=mean(ShareWoodRelated)), "clipboard-1024")

lfsa_cWood %>% reshape2::dcast(unit_code+age_code+sex_code+geo_code+time~nace_r2_code, value.var="employment") %>% 
  data.frame(., WoodBased=.$C16+.$C17, WoodRelated= .$C18 + .$C31 + .$D35+ .$F + .$J58, Total = .$A02+.$C16+.$C17 + .$C18 + .$C31 + .$D35+ .$F + .$J58) %>% 
  data.frame(.,ShareForestry=.$A02/.$Total, ShareWoodBased = .$WoodBased/.$Total, ShareWoodRelated=.$WoodRelated/.$Total ) %>% group_by(unit_code, age_code, sex_code, geo_code) %>% summarise(ShareWoodBased=mean(ShareWoodBased), ShareWoodRelated=mean(ShareWoodRelated)) %>% 
ggplot(aes(y=ShareWoodRelated, x=ShareWoodBased, group=geo_code, colour=geo_code))+
  geom_text(aes(label=geo_code))+
  annotate("text", x = c(0.6), y=c(0.55, 0.45, 0.25), label = c("Share of\nemployment\nin forestry", "0%", "20%"), hjust=1, color=c("darkgreen", "lightgreen", "darkgreen")) +
  coord_cartesian(xlim = c(0.2, 0.6), ylim = c(0.2, 0.8))+
  coord_fixed(ratio = 1/2)+
#  geom_line() +
#  geom_point() + 
  xlab("Share of wood-based employment") + ylab("Share of wood-related employment") +
  theme_bw()+theme(legend.position="none")+
  geom_abline(intercept = 1, slope = -1, color="lightgreen", size=1) +
  geom_abline(intercept = .8, slope = -1, color="darkgreen", size=1) 


library(ggtern)

lfsa_cWood %>% reshape2::dcast(unit_code+age_code+sex_code+geo_code+time~nace_r2_code, value.var="employment") %>% 
  data.frame(., WoodBased=.$C16+.$C17, WoodRelated= .$C18 + .$C31 + .$D35+ .$F + .$J58, Total = .$A02+.$C16+.$C17 + .$C18 + .$C31 + .$D35+ .$F + .$J58) %>% 
  data.frame(.,ShareForestry=.$A02/.$Total, ShareWoodBased = .$WoodBased/.$Total, ShareWoodRelated=.$WoodRelated/.$Total ) %>% group_by(unit_code, age_code, sex_code, geo_code) %>% summarise(ShareWoodBased=mean(ShareWoodBased), ShareWoodRelated=mean(ShareWoodRelated), ShareForestry=mean(ShareForestry)) %>% 
  ggtern(aes(y=ShareWoodRelated, z=ShareWoodBased, x= ShareForestry, group=geo_code)) +
  labs( x       = "Forestry",
        xarrow  = "Share in forestry",
        y       = "Forest-related sectors",
        yarrow  = "Share in forest-related sectors",
        z       = "Forest-based sectors",
        zarrow  = "Share in forest-based sectors")+
  geom_text(aes(label=geo_code), hjust=-.2)+
  ggtitle("Repartition of forest-related employment between sectors (average 2008-2017)") +
    theme_custom(base_size = 12, base_family = "", tern.panel.background="white",
                 col.T = "cornsilk3", col.L = "chartreuse3", col.R = "darkgoldenrod",
                 col.grid.minor = "gray95") +
    theme_showarrows() +
#      theme_rgbw()+
  theme(legend.position="none") -> A
A+scale_L_continuous(limits = c(.0, .6)) +
  scale_R_continuous(limits = c(.2, .8)) +
  scale_T_continuous(limits = c(.2, .8)) +
  geom_point()
  
  
#  geom_label_repel(aes(label = geo_code), box.padding   = 0.35, point.padding = 0.5, segment.color = 'grey50') +

aggregate(employment~time+nace_r2_code, FUN=sum, data=lfsa_cWood[lfsa_cWood$geo_code=="FI",]) %>% 
  ggplot(data=., aes(y=employment, x=factor(time), group=nace_r2_code, colour=nace_r2_code)) +
  geom_line() +
  geom_point() + 
  expand_limits(y=0) + 
  ggtitle("Employment in forestry, the wood-based and wood related sectors in the EU28") +
  xlab("Year") + ylab("Thousand people employed") +
  annotate("text", x = c(.9), y=(aggregate(employment~time+nace_r2_code, FUN=sum, data=lfsa_cWood[lfsa_cWood$geo_code=="FI" & lfsa_cWood$time==2008,])$employment+c(0,0,0,0,0,0,0,0)), label = aggregate(employment~time+nace_r2_code, FUN=sum, data=lfsa_cWood[lfsa_cWood$geo_code=="FI" & lfsa_cWood$time==2008,])$nace_r2_code, hjust=1, colour=c("#F8766D", "#CD9600", "#7CAE00", "#00BE67", "#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC")) +
  theme_bw()+theme(legend.position="none")

# --------------------------------------
# ---- Gender ratio and age classes ----
lfsa_age_gender <- lfsa_egan22d[lfsa_egan22d$unit_code=="THS" &
                                  #                       lfsa_egan22d$age_code=="Y_GE15" & # Y15, 20, 25 40, 50, 55, 65, 75
                                  #                       lfsa_egan22d$sex_code=="T" &
                                  lfsa_egan22d$nace_r2_code %in% c("A02", "C16", "C17", "C18", "C31", "D35", "F41", "F42", "F43", "J58") &
                                  lfsa_egan22d$geo_code %in% c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "EU28", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK"),]

lfsa_age_gender[substr(lfsa_age_gender$nace_r2_code,1,1)=="F",]$nace_r2_code <- "F"

lfsa_age_gender <- aggregate(values~unit_code+geo_code+nace_r2_code+sex_code+age_code+time, data=lfsa_age_gender, FUN=sum)

lfsa_age_gender_shares <- reshape2::dcast(lfsa_age_gender[lfsa_age_gender$sex_code%in%c("T", "M") & lfsa_age_gender$age_code%in%c("Y15-64", "Y25-64", "Y25-49", "Y50-64", "Y_GE15", "Y_GE25", "Y_GE50", "Y_GE65"), ], nace_r2_code+geo_code+time~age_code+sex_code, value.var="values")

lfsa_age_gender_shares[is.na(lfsa_age_gender_shares$Y_GE65_T),]$Y_GE65_T <- lfsa_age_gender_shares[is.na(lfsa_age_gender_shares$Y_GE65_T),]$Y_GE15_T-lfsa_age_gender_shares[is.na(lfsa_age_gender_shares$Y_GE65_T),]$`Y15-64_T`

lfsa_age_gender_shares[is.na(lfsa_age_gender_shares$`Y50-64_T`),]$`Y50-64_T` <- lfsa_age_gender_shares[is.na(lfsa_age_gender_shares$`Y50-64_T`),]$`Y25-64_T`-lfsa_age_gender_shares[is.na(lfsa_age_gender_shares$`Y50-64_T`),]$`Y25-49_T`
lfsa_age_gender_shares[is.na(lfsa_age_gender_shares$`Y25-49_T`),]$`Y25-49_T` <- lfsa_age_gender_shares[is.na(lfsa_age_gender_shares$`Y25-49_T`),]$Y_GE25_T-lfsa_age_gender_shares[is.na(lfsa_age_gender_shares$`Y25-49_T`),]$Y_GE50_T


# -- check the missing values for women
sum(with(lfsa_age_gender_shares[!is.na(lfsa_age_gender_shares$Y_GE15_M),], Y_GE15_M==Y_GE15_T))
lfsa_age_gender_shares[!is.na(lfsa_age_gender_shares$Y_GE15_M)& lfsa_age_gender_shares$Y_GE15_M>=lfsa_age_gender_shares$Y_GE15_T,]

lfsa_age_gender_shares$Y_GE15_M_T <- lfsa_age_gender_shares$Y_GE15_M/lfsa_age_gender_shares$Y_GE15_T

lfsa_age_gender_shares %>% group_by(nace_r2_code, geo_code) %>% summarize(mean(Y_GE15_M_T, na.rm=TRUE)) %>% View

#plot(lm(Y_GE15_M_T~time+geo_code, data=lfsa_age_gender_shares))
boxplot(Y_GE15_M_T~time, lfsa_age_gender_shares[lfsa_age_gender_shares$nace_r2_code=="A02", ], xlab="Year", ylab="Share of men in the forestry sector (nace A02)")

lfsa_age_gender_shares$`Y15-24_T` <- lfsa_age_gender_shares$`Y15-64_T`-lfsa_age_gender_shares$`Y25-64_T`

sum(with(lfsa_age_gender_shares, `Y15-24_T`+ `Y25-49_T` + `Y50-64_T` + Y_GE65_T < Y_GE15_T-.3), na.rm = TRUE)

lfsa_age_gender_shares$total <- lfsa_age_gender_shares$`Y15-24_T` + lfsa_age_gender_shares$`Y25-49_T` + lfsa_age_gender_shares$`Y50-64_T` + lfsa_age_gender_shares$`Y_GE65_T`

lfsa_age_shares <- cbind(lfsa_age_gender_shares[, c("nace_r2_code", "geo_code", "time")], lfsa_age_gender_shares[,c("Y15-24_T", "Y25-49_T", "Y50-64_T", "Y_GE65_T")]/lfsa_age_gender_shares$total)

lfsa_age_shares <- lfsa_age_shares[!is.na(lfsa_age_shares$`Y15-24_T`+lfsa_age_shares$`Y25-49_T`+lfsa_age_shares$`Y50-64_T`+lfsa_age_shares$Y_GE65_T),]

lfsa_age_shares <- fillValueTime(lfsa_age_shares, "Y15-24_T", "time", c("nace_r2_code", "geo_code"))
colnames(lfsa_age_shares)[ncol(lfsa_age_shares)] <- "Y15-24_Te"
lfsa_age_shares <- lfsa_age_shares[, !(colnames(lfsa_age_shares) %in% c( "year_p", "value_p", "year_n", "value_n"))]
lfsa_age_shares <- fillValueTime(lfsa_age_shares, "Y25-49_T", "time", c("nace_r2_code", "geo_code"))
colnames(lfsa_age_shares)[ncol(lfsa_age_shares)] <- "Y25-49_Te"
lfsa_age_shares <- lfsa_age_shares[, !(colnames(lfsa_age_shares) %in% c( "year_p", "value_p", "year_n", "value_n"))]
lfsa_age_shares <- fillValueTime(lfsa_age_shares, "Y50-64_T", "time", c("nace_r2_code", "geo_code"))
colnames(lfsa_age_shares)[ncol(lfsa_age_shares)] <- "Y50-64_Te"
lfsa_age_shares <- lfsa_age_shares[, !(colnames(lfsa_age_shares) %in% c( "year_p", "value_p", "year_n", "value_n"))]
lfsa_age_shares <- fillValueTime(lfsa_age_shares, "Y_GE65_T", "time", c("nace_r2_code", "geo_code"))
colnames(lfsa_age_shares)[ncol(lfsa_age_shares)] <- "Y_GE65_Te"
lfsa_age_shares <- lfsa_age_shares[, !(colnames(lfsa_age_shares) %in% c( "year_p", "value_p", "year_n", "value_n"))]

#reshape2::dcast(unique(lfsa_age_shares[,c(2,3)]), geo_code ~ nace_r2_code)

# --- Any figure missing for a specific sector in a country is replaced by the EU average ---
lfsa_age_shares <- merge(apply((data.frame(expand.grid(apply(lfsa_age_shares[,c('time', 'nace_r2_code', 'geo_code')], 2, unique)))), 2, FUN=as.character), lfsa_age_shares, all.x=TRUE)
lfsa_age_shares <- rbind(lfsa_age_shares[!is.na(lfsa_age_shares$`Y15-24_Te`),], merge(lfsa_age_shares[is.na(lfsa_age_shares$`Y15-24_Te`),][,c(1:3)], lfsa_age_shares[lfsa_age_shares$geo_code=="EU28",][,-c(3)]))

lfsa_cWood_age <- merge(lfsa_cWood, lfsa_age_shares[, c('time', 'nace_r2_code', 'geo_code', 'Y15-24_Te', 'Y25-49_Te', 'Y50-64_Te', 'Y_GE65_Te')], all.x = TRUE)
lfsa_cWood_age <- cbind(lfsa_cWood_age[, -c(19:22)], lfsa_cWood_age[, c('Y15-24_Te', 'Y25-49_Te', 'Y50-64_Te', 'Y_GE65_Te')]*lfsa_cWood_age$employment)

library(ggplot2)

reshape2::melt(lfsa_cWood_age[lfsa_cWood_age$geo_code!="EU28",], id=c("nace_r2_code", "geo_code", "time"), measure=c('Y15-24_Te', 'Y25-49_Te', 'Y50-64_Te', 'Y_GE65_Te'), variable="employment_age_class") %>% group_by(nace_r2_code, employment_age_class, time) %>% summarise(value=sum(value)) %>% 
  ggplot(aes(y=value, x=as.factor(time), group=paste(employment_age_class, nace_r2_code), colour=employment_age_class)) +
  facet_wrap(~nace_r2_code, nrow=2, scales="free_y") +
  geom_line() +
  geom_point() + 
  expand_limits(y=0) + 
  xlab("Year") + ylab("Employment (1000 people)") +
  ggtitle("Employment in forest-based and forest related sectors by age classes") +
  theme_bw()


# ---- Complementary analysis ----
# --- Correlations between the value addedd gross (B1G) and the compensation of employees (D1) ---
naioAssess <- merge(naio_10_cp16[naio_10_cp16$induse_code %in% c("A02", "C16", "C17", "C18", "C31_32", "D", "F") & naio_10_cp16$prod_na_code == "B1G" & naio_10_cp16$unit_code=="MIO_EUR",c(3,5,11,12)], naio_10_cp16[naio_10_cp16$induse_code %in% c("A02", "C16", "C17", "C18", "C31_32", "D", "F") & naio_10_cp16$prod_na_code == "D1" & naio_10_cp16$unit_code=="MIO_EUR",c(3,5,11,12)], by.x = c(1:3), by.y = c(1:3), all=TRUE)

#naioAssess[naioAssess$induse_code=="C17"&naioAssess$geo_code=="IE"&naioAssess$time==2012,]$values.x <- 172.15 #error in the data

summary(lm(values.y~values.x:geo_code-1,  data=naioAssess))
plot(values.y~values.x, col=as.factor(geo_code),  data=naioAssess)
plot(values.y~time, col=as.factor(geo_code), data=naioAssess)

t <-aggregate(values.y~geo_code+induse_code, data=naioAssess, FUN=function(x){c(max(x)/min(x))})
View(aggregate(values.x~geo_code+induse_code, data=naioAssess, FUN=function(x){c(max(x)/min(x))}))
plot(values.y~values, merge(naioAssess, tempLNS))

naioAssess$timePrev <- naioAssess$time -1
names(naioAssess)[c(4,5)] <- c("B1G", "D1")
naioAssess <- merge(naioAssess[,c(1:5)], naioAssess[,c(1:2,4:6)], by.x= c("induse_code", "geo_code", "time"), by.y=c("induse_code", "geo_code", "timePrev"))
names(naioAssess)[c(4:7)] <- c("B1G", "D1", "B1Gnext", "D1next")
naioAssess$changeB1G <- naioAssess$B1Gnext / naioAssess$B1G
naioAssess$changeD1 <- naioAssess$D1next / naioAssess$D1
summary(lm(changeB1G~changeD1:as.factor(induse_code)-1, data=naioAssess[naioAssess$time>2007,]))
plot(changeB1G~changeD1:time, data=naioAssess)
with(naioAssess[naioAssess$time %in% c(2008:2015) &naioAssess$induse_code!="C31_32",], t.test(changeB1G-changeD1))

summary(lm(changeB1G~changeD1:as.factor(time)-1, data=naioAssess[naioAssess$time>2007,]))
summary(lm(changeValues~as.factor(time), data=tempLNShist[tempLNShist$time>2007,]))


# ---- Quantities processed and connection to labour ----

# data on inputs to industries in SWE
BiomassInputs <- read_excel("~/Biomass/Data Sources/IO_info_v12102018.xlsx", sheet = "000 m3 SWE", na = "NA")
BiomassInputs$Year <- as.numeric(BiomassInputs$Year)
names(BiomassInputs)[c(1,4)] <- c("geo_code", "Value_1000m3_SWE")
unique(BiomassInputs$geo_code)[!(unique(BiomassInputs$geo_code) %in% unique(lfsa_cWood$geo_code))]
BiomassInputs[BiomassInputs$geo_code=="GR",]$geo_code <- "EL"
BiomassInputs[BiomassInputs$geo_code=="GB",]$geo_code <- "UK"
BiomassInputs$Item <- substring(BiomassInputs$Item, 4)

# write.csv(unique(BiomassInputs$Item), "clipboard")

Item_Sector <- read.csv("Item_Sector.csv", stringsAsFactors = FALSE)
BiomassInputs <- merge(Item_Sector, BiomassInputs, all.y=TRUE)

SAMwood_m3 <- reshape2::dcast(BiomassInputs, geo_code+Year+Supply~Use, value.var = "Value_1000m3_SWE", fun.aggregate = sum)

SAMwood_m3[SAMwood_m3$Supply!="Total inputs",]

BiomassInputsConsumption <- BiomassInputs[BiomassInputs$Use%in%c("C16",	"C17_Pulp",	"C17_Paper", "D35"),]

input_Industry <- naio_10_cp16[naio_10_cp16$induse_code%in%c("C16","C17") & naio_10_cp16$prod_na_code%in%c("CPA_A02") & naio_10_cp16$unit_code=="MIO_EUR",]
#View(naio_10_cp16[naio_10_cp16$prod_na_code%in%c("CPA_A02") & substr(naio_10_cp16$induse_code, 1,1)=="P" & naio_10_cp16$induse_code!="P",])
input_Industry$values


Round2Industries <- merge(input_Industry, BiomassInputsConsumption[substr(BiomassInputsConsumption$Item, 1, 3)=="Rou", ], by.x=c("geo_code","induse_code","time"), by.y=c("geo_code","induse_code","Year"))
Round2Industries$valuePerm3_SWE = Round2Industries$values/Round2Industries$Value_1000m3_SWE*1000

# filtering data: drop all non-consistent zeros
Round2Industries <- Round2Industries[!(Round2Industries$valuePerm3_SWE==0 | is.infinite(Round2Industries$valuePerm3_SWE) | is.na(Round2Industries$valuePerm3_SWE) ),]

Round2Industries[Round2Industries$induse_code=="C16", ] %>% 
ggplot(aes(time, valuePerm3_SWE, group=paste(geo_code), colour = paste(geo_code)))+
  geom_line() +
  geom_point()

summary(lm( valuePerm3_SWE ~ geo_code + as.factor(time), Round2Industries[Round2Industries$induse_code=="C16", ]))


Round2Industries[Round2Industries$induse_code=="C17", ] %>% 
  ggplot(aes(time, valuePerm3_SWE, group=paste(geo_code), colour = paste(geo_code)))+
  geom_line() +
  geom_point()

summary(lm( valuePerm3_SWE ~ geo_code + as.factor(time), Round2Industries[Round2Industries$induse_code=="C17", ]))

merge(Round2Industries[Round2Industries$induse_code=="C17", ] %>% group_by(induse_code, geo_code) %>% summarise(std=sd(values), mean=mean(values)) %>% with(., data.frame(induse_code, geo_code, std.mean=std/mean)), Round2Industries) %>% .[.$`std.mean`>.3 &!is.na(.$`std.mean`), ] %>% 
  ggplot(aes(time, values, group=paste(geo_code), colour = paste(geo_code)))+
  geom_line() +
  geom_point()


# ---- Analysis of the production functions ----- 
naio_10_cp16[naio_10_cp16$induse_code%in%c("A02", "C16", "C17", "C18", "C31_32", "D", "F") & naio_10_cp16$prod_na_code %in% c("P1", "D1", "TOTAL", "EMP_DC") , ] %>% View

naio_10r <- naio_10_cp16[naio_10_cp16$induse_code%in%c("A02", "C16", "C17", "C18", "C31_32", "D", "F") & substr(naio_10_cp16$prod_na_code, 1, 5) %in% c("CPA_A", "CPA_B", "CPA_C", "CPA_D", "CPA_F") & naio_10_cp16$unit_code=="MIO_EUR", ] %>% group_by(unit_code, stk_flow_code, induse_code, geo_code, time) %>% summarize(values=sum(values, na.rm=TRUE)) %>% data.frame(., prod_na_code="CPA_CI", stringsAsFactors=FALSE) %>% bind_rows(naio_10_cp16[naio_10_cp16$induse_code%in%c("A02", "C16", "C17", "C18", "C31_32", "D", "F") & naio_10_cp16$prod_na_code %in% c("P1", "D1", "TOTAL")  & naio_10_cp16$unit_code=="MIO_EUR", c("unit_code", "stk_flow_code", "induse_code", "prod_na_code", "geo_code", "time", "values")])
colnames(naio_10r)[colnames(naio_10r)=="values"] <- unique(naio_10r$unit_code)

naio_10r <- naio_10_cp16[naio_10_cp16$induse_code%in%c("A02", "C16", "C17", "C18", "C31_32", "D", "F") & naio_10_cp16$prod_na_code %in% c("EMP_DC") , c("geo_code", "time", "induse_code", "values")] %>% left_join(naio_10r[, c("geo_code", "time", "induse_code", "prod_na_code", "MIO_EUR")])

naio_10r$THS_EURperHW <- naio_10r$MIO_EUR / naio_10r$values
naio_10r[!is.na(naio_10r$THS_EURperHW) & naio_10r$THS_EURperHW>0 & !is.infinite(naio_10r$THS_EURperHW),] %>% reshape2::dcast(geo_code+time+induse_code ~ prod_na_code, value.var = "THS_EURperHW") %>% lm(log(P1)~log(D1)+log(TOTAL), .) %>% summary

naio_10r[!is.na(naio_10r$THS_EURperHW) & naio_10r$THS_EURperHW>0 & !is.infinite(naio_10r$THS_EURperHW),] %>% reshape2::dcast(geo_code+time+induse_code ~ prod_na_code, value.var = "THS_EURperHW") %>% lm(log(P1)~log(D1)+log(CPA_CI), .) %>% summary

naio_10r[!is.na(naio_10r$THS_EURperHW) & naio_10r$THS_EURperHW>0 & !is.infinite(naio_10r$THS_EURperHW),] %>% reshape2::dcast(geo_code+time+induse_code+values ~ prod_na_code, value.var = "THS_EURperHW") %>% lm(log(P1)~log(D1), .) %>% summary

naio_10r[!is.na(naio_10r$THS_EURperHW) & naio_10r$THS_EURperHW>0 & !is.infinite(naio_10r$THS_EURperHW),] %>% reshape2::dcast(geo_code+time+induse_code+values ~ prod_na_code, value.var = "MIO_EUR") %>% lm(log(CPA_CI)~log(values):induse_code, .) %>% summary


# ---- Confrontation of the INPUT and the PRODUCT approaches in the C31 sector ----

naio_C31_32_woodShares <- naio_10_cp16[naio_10_cp16$unit_code=="MIO_EUR" & 
                 naio_10_cp16$induse_code %in% c("C31_32") &
                 naio_10_cp16$prod_na_code %in% c("CPA_A01", "CPA_A02", "CPA_A03", "CPA_B", "CPA_C10-12", "CPA_C13-15", "CPA_C16", "CPA_C17", "CPA_C18", "CPA_C19", "CPA_C20", "CPA_C21", "CPA_C22", "CPA_C23", "CPA_C24", "CPA_C25", "TOTAL") , ] %>% 
    group_by(geo_code, time) %>%  
    summarise(totalMaterialInput=sum(ifelse(prod_na_code %in% c("TOTAL"), 0, values)), totalWoodBasedInput=sum(ifelse(prod_na_code %in% c("CPA_A02", "CPA_C16", "CPA_C17", "CPA_C18"), values, 0)), totalInputs=sum(ifelse(prod_na_code %in% c("TOTAL"), values, 0)))

naio_C31_32_woodShares$woodShareMaterials <- naio_C31_32_woodShares$totalWoodBasedInput / naio_C31_32_woodShares$totalMaterialInput

sbs_C31_32 <- sbs_na_ind_r2[sbs_na_ind_r2$indic_sb_code%in%c("V13110", "V16110") & sbs_na_ind_r2$nace_r2_code %in% c("C31", "C32"),] %>% reshape2::dcast(geo_code+time~nace_r2_code+indic_sb_code, value.var="values")
sbs_C31_32$C31_32_V13110 <- sbs_C31_32$C31_V13110 + sbs_C31_32$C32_V13110
sbs_C31_32$C31_32_V16110 <- sbs_C31_32$C31_V16110 + sbs_C31_32$C32_V16110
(sbs_C31_32$C31_V16110 / sbs_C31_32$C31_32_V16110) / (sbs_C31_32$C31_V13110 / sbs_C31_32$C31_32_V13110)

C31_32_woodShares <- naio_C31_32_woodShares %>% 
    left_join(sbs_C31_32) %>% 
    left_join(lfsa_egan22d[lfsa_egan22d$age_code=="Y_GE15" & lfsa_egan22d$sex_code=="T" & lfsa_egan22d$nace_r2_code %in% c("C31", "C32"), ] %>% reshape2::dcast(geo_code+time~nace_r2_code, value.var="values")) %>% 
    left_join(ProdComC31Wood)
C31_32_woodShares$totalWoodBasedInput / (C31_32_woodShares$totalMaterialInput * C31_32_woodShares$C31_V13110 / C31_32_woodShares$C31_32_V13110)
C31_32_woodShares$C31_32_woodShares <- C31_32_woodShares$totalWoodBasedInput / C31_32_woodShares$totalMaterialInput



# dataset to test imputation algorithms

# labour force survey
lfsa_egan22d <- get_eurostat("lfsa_egan22d",  time_format = "num", stringsAsFactors=FALSE)
lfsa_egan22d <- label_eurostat(lfsa_egan22d, code = names(lfsa_egan22d)[c(-6,-7)])

# national accounts: employment
nama_10_a64_e <- get_eurostat("nama_10_a64_e",  time_format = "num", stringsAsFactors=FALSE)
nama_10_a64_e <- label_eurostat(nama_10_a64_e, code = names(nama_10_a64_e)[c(-5, -6)])

# national accounts: economic indicators
nama_10_a64 <- get_eurostat("nama_10_a64",  time_format = "num", stringsAsFactors=FALSE)
nama_10_a64 <- label_eurostat(nama_10_a64, code = names(nama_10_a64)[c(-5, -6)])

# business statistics
sbs_na_ind_r2 <- get_eurostat("sbs_na_ind_r2",  time_format = "num", stringsAsFactors=FALSE)
sbs_na_ind_r2 <- label_eurostat(sbs_na_ind_r2, code = names(sbs_na_ind_r2)[c(-4,-5)], fix_duplicated = TRUE)


sectors <- c("A02", "C16", "C17", "C18", "C31", "D35", "F41", "F42", "F43", "J58")
EU27 <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "EU27_2020", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")

# Building the full dataset and merging the sectors that cannot be reported separately (F41, F42 and F43 -> F)


employment <- lfsa_egan22d %>% filter(unit_code=="THS", 
                        lfsa_egan22d$age_code %in% c("Y15-64", "Y25-64", "Y25-49", "Y50-64", "Y_GE15", "Y_GE25", "Y_GE50", "Y_GE65"),
                        lfsa_egan22d$nace_r2_code %in% sectors,
                        lfsa_egan22d$geo_code %in% EU27) %>%
  select(-sex, -age) %>% 
  pivot_wider(names_from = sex_code, values_from = values) %>% 
  mutate(F=ifelse(is.na(F), T-M, F)) %>%
  pivot_longer(cols=c("M", "F", "T"), names_to = "sex_code", values_to = "values" ) %>% 
  pivot_wider(names_from = age_code, values_from = values) %>% 
  mutate(`Y50-64`=ifelse(is.na(`Y50-64`), `Y25-64` - `Y25-49`, `Y50-64`), `Y25-49`=ifelse(is.na(`Y25-49`), `Y_GE25` - `Y_GE50`, `Y25-49`), `Y15-24`=ifelse(!is.na(`Y_GE15`- `Y_GE25`), `Y_GE15`- `Y_GE25`, `Y15-64` - `Y25-64`)) %>% 
  select(unit_code, nace_r2_code, geo_code, unit, time, sex_code, `Y15-24`, `Y25-49`, `Y50-64`, Y_GE65, Y_GE15) %>% 
  mutate(Y_GE65=ifelse(is.na(Y_GE65), Y_GE15-(`Y15-24`+`Y25-49`+`Y50-64`), Y_GE65)) %>% 
  pivot_longer(cols=c("Y15-24", "Y25-49", "Y50-64", "Y_GE65", "Y_GE15"), names_to = "age_code", values_to = "values" ) %>% 
  mutate(values=ifelse(values<0,0, values)) %>% # I checked that negative values come from rounding, not from erroneous values. 
  mutate(nace_r2_code=ifelse(nace_r2_code %in% c("F41", "F42",  "F43"), "F", nace_r2_code) ) %>%  # merging the construction sector
  group_by(unit_code, geo_code, nace_r2_code, sex_code, age_code, time) %>%
  summarise(values=sum(values)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = c(sex_code, age_code), values_from = values) %>% 
  left_join(nama_10_a64_e %>% 
              filter(na_item_code %in% c("EMP_DC"), unit_code %in% c("THS_PER", "THS_HW")) %>% 
              select(nace_r2_code, geo_code, time, unit_code, values) %>% 
              pivot_wider(names_from = unit_code, values_from = values)) %>% 
  left_join(sbs_na_ind_r2 %>% select(nace_r2_code, indic_sb_code, geo_code, time, values) %>% 
              filter(time>2007, indic_sb_code %in% c("V16110", "V16150", "V13320")) %>%
              pivot_wider(names_from = indic_sb_code, values_from = values)) %>% 
  left_join(nama_10_a64 %>% 
              filter(na_item_code %in% c("D1", "D11", "B1G"), unit_code %in% c("CP_MEUR")) %>% # -- could also explore "CLV10_MEUR" with chained prices, but only with the value added.
              select(nace_r2_code, geo_code, time, na_item_code, unit_code, values) %>% 
              pivot_wider(names_from = c(na_item_code, unit_code), values_from = values))


# remove the 0 hours worked then a number of workers is reported (accounting data)
employment$THS_HW <- ifelse(employment$THS_HW==0 & employment$THS_PER>0, NA,  employment$THS_HW) 

employment %>% mutate(ratio_NA_LFS = T_Y_GE15/THS_PER, ratio_SBS_LFS = V16110/THS_PER, employ_cost_NA=D11_CP_MEUR/THS_PER, employ_cost_SBS = V13320/V16110*1000) %>% View


# unit_code     unit used in LFS data
# geo_code      country code
# nace_r2_code  code of activity (NACE rev2)
# time          year
# F_Y_GE15      number of women employed
# F_Y_GE65      number of women over 65 employed
# F_Y15-24      number of 15 to 24 year-old women employed
# F_Y25-49      number of 25 to 49 year-old women employed
# F_Y50-64      number of 50 to 64 year-old women employed
# M_Y_GE15      number of men employed
# M_Y_GE65      number of men over 65 employed
# M_Y15-24      number of 15 to 24 year-old men employed
# M_Y25-49      number of 25 to 49 year-old men employed
# M_Y50-64      number of 50 to 64 year-old men employed
# T_Y_GE15      number of persons employed
# T_Y_GE65      number of persons over 65 employed
# T_Y15-24      number of 15 to 24 year-old persons employed
# T_Y25-49      number of 25 to 49 year-old persons employed
# T_Y50-64      number of 50 to 64 year-old persons employed
# THS_HW        Thousand hours worked (from the national accounts)
# THS_PER       Thousand persons employed (from the national accounts)
# V16110        Persons employed - number (from the structural business statistics)
# V13320        Wages and Salaries - million euro (from the structural business statistics)
# V16150        Hours worked by employees - number (from the structural business statistics)
# B1G_CP_MEUR   Gross value added in million euros (from the national accounts)
# D1_CP_MEUR    Compensation of employees in million euros (from the national accounts)
# D11_CP_MEUR   Wages and salaries in million euros (from the national accounts)


# NB: SBS only reports manufacturing sectors (no data for forestry and construction)
# NB2: the sectors C31 and C32 are agregated in the national accounts
