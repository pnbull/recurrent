#Project: 514 Project - Recurrent Preterm Birth
#Author: Matthew Dekker
#Updated: 5/6/18 by JEC

#Notes: 
# 1. Copied code from Jessica's "01 - Clean Data.R" file
# 2. Applied rules dictated in "Variable Description" in the shared google drive

library(tidyverse)

#Load in raw datapt <- readRDS("/Users/apple/Downloads/P23_recurrent_preterm_04_24.rds")
#pt <- read.csv("C:/Users/Llamaface/Desktop/EPI 514/Data/P23_recurrent_preterm_04_24.csv")
pt <- readRDS("/Users/pnbullard/Documents/UW/Epi514/P23 Recurrent Preterm/P23_recurrent_preterm_04_24.rds")


# keep only variables we're possibly interested in
# 292 variables remaining
pt <- subset(pt, select = c(exposed, mbgap_p2, momage_p1, momedu03_p1, 
                            momrace_p1, cigs_1st_p1, cigs_2nd_p1, cigs_3rd_p1, cigs_bef_p1, 
                            delivpay_p1, cmpayid1_p1, cmpayid2_p1, diabetes_p1, hyper_p1, 
                            laboth2_p1, preclamp_p1, chlamyd_p1, gherpes_p1, gonorrh_p1, 
                            minfect_p1, othinfec_p1, rubella_p1, strep_p1, syphil_p1, 
                            antibiot_p1, antiseps_p1, hepatit_p1, hepatitb_p1, 
                            hepatitc_p1, parity_p1, gestest_p1, spontdel_p1, labonset_p1, 
                            rupmem_p1, indlab_p1, precipl_p1, longlab_p1, forcfail_p1, 
                            vacuum_p1, vacfail_p1, stimlab_p1, csection_p1, dmethfin_p1, 
                            fet_pres_p1, breech_p1, placprev_p1, otforcep_p1,
                            cmdiag1_p1:cmdiag25_p1, cbdiag1_p1:cbdiag25_p1, 
                            cmproc1_p1:cmproc25_p1, cbproc1_p1:cbproc25_p1,
                            momage_p2, momedu03_p2, momrace_p2, cigs_1st_p2, cigs_2nd_p2, 
                            cigs_3rd_p2, cigs_bef_p2, delivpay_p2, cmpayid1_p2, cmpayid2_p2, 
                            diabetes_p2, hyper_p2, laboth2_p2, preclamp_p2, chlamyd_p2, 
                            gherpes_p2, gonorrh_p2, minfect_p2, othinfec_p2, rubella_p2, 
                            strep_p2, syphil_p2, antibiot_p2, antiseps_p2, hepatit_p2, 
                            hepatitb_p2, hepatitc_p2, parity_p2, gestest_p2, spontdel_p2, 
                            labonset_p2, rupmem_p2, indlab_p2, precipl_p2, longlab_p2, 
                            forcfail_p2, vacuum_p2, vacfail_p2, stimlab_p2, csection_p2, 
                            dmethfin_p2, fet_pres_p2, breech_p2, placprev_p2, otforcep_p2,
                            cmdiag1_p2:cmdiag25_p2, cbdiag1_p2:cbdiag25_p2, 
                            cmproc1_p2:cmproc25_p2, cbproc1_p2:cbproc25_p2, prewght_p1,
                            prewght_p2, momhgt_p1, momhgt_p2, bmi_p1, bmi_p2))

########
#Start data cleaning
########

### MOMAGE
#Set NA's if equals 99 or is less than 13 or is greater than 57
pt$momage_p1[pt$momage_p1 == 99 | 
               pt$momage_p1 < 13|
               pt$momage_p1 > 57] <- NA

pt$momage_p2[pt$momage_p2 == 99 | 
               pt$momage_p2 < 13|
               pt$momage_p2 > 57] <- NA 

#create factor variables
pt$momage_p1_fac <- NA
pt$momage_p2_fac <- NA

pt$momage_p1_fac[pt$momage_p1 <= 20] <- "Under 21"
pt$momage_p1_fac[pt$momage_p1 > 20 & pt$momage_p1 <= 35] <- "21-35"
pt$momage_p1_fac[pt$momage_p1 > 35] <- "Over 35"

pt$momage_p2_fac[pt$momage_p2 <= 20] <- "Under 21"
pt$momage_p2_fac[pt$momage_p2 > 20 & pt$momage_p2 <= 35] <- "21-35"
pt$momage_p2_fac[pt$momage_p2 > 35] <- "Over 35"

### MOMEDU03
#Set NA's if equals ".", 99, 09, or 0 

pt$momedu03_p1[pt$momedu03_p1 == "." |
                 pt$momedu03_p1 == 99 |
                 pt$momedu03_p1 == 09 |
                 pt$momedu03_p1 == 0] <- NA

pt$momedu03_p2[pt$momedu03_p2 == "." |
                 pt$momedu03_p2 == 99 |
                 pt$momedu03_p2 == 09 |
                 pt$momedu03_p2 == 0] <- NA

#create factor variable based off provided criteria (see: Variable Description)
pt$momedu03_p1_fac <- factor(pt$momedu03_p1,
                             labels = c("8th or Less",
                                        "9th to 12th - No Dip",
                                        "HS or GED",
                                        "Some College",
                                        "Associate's",
                                        "Bachelor's",
                                        "Master's",
                                        "Doctorate or Professional"))

#View(with(pt, table(momedu03_p1, momedu03_p1_fac)))

pt$momedu03_p2_fac <- factor(pt$momedu03_p2,
                             labels = c("8th or Less",
                                        "9th to 12th - No Dip",
                                        "HS or GED",
                                        "Some College",
                                        "Associate's",
                                        "Bachelor's",
                                        "Master's",
                                        "Doctorate or Professional"))

#View(with(pt, table(momedu03_p2, momedu03_p2_fac)))

#create broader education factor variable  
pt$momedu03_p1_fac2[pt$momedu03_p1 <= 2] <- "Less than high school"
pt$momedu03_p1_fac2[pt$momedu03_p1 == 3] <- "High school / GED"
pt$momedu03_p1_fac2[pt$momedu03_p1 > 3] <- "Some College"
with(pt, table(momedu03_p1_fac, momedu03_p1_fac2))

pt$momedu03_p2_fac2[pt$momedu03_p2 <= 2] <- "Less than high school"
pt$momedu03_p2_fac2[pt$momedu03_p2 == 3] <- "High school / GED"
pt$momedu03_p2_fac2[pt$momedu03_p2 > 3] <- "Some College"
with(pt, table(momedu03_p2_fac, momedu03_p2_fac2))


###Race/ethnicity
#Set NA's if 98 or 99

pt$momrace_p1[pt$momrace_p1 == 98 |
                pt$momrace_p1 == 99] <- NA

pt$momrace_p2[pt$momrace_p2 == 98 |
                pt$momrace_p2 == 99] <- NA

table(pt$momrace_p1)
table(pt$momrace_p2)

#create factor variable
pt$momrace_p1_fac <- NA

pt$momrace_p1_fac[pt$momrace_p1 == 1] <- "White"
pt$momrace_p1_fac[pt$momrace_p1 == 2] <- "Black"
pt$momrace_p1_fac[pt$momrace_p1 == 3] <- "Native American"
pt$momrace_p1_fac[pt$momrace_p1 == 9] <- "Hispanic"

#create vector of races to include in "API"
API <- c(4,5,6,7,8,11,12,13,14,15)

pt$momrace_p1_fac[pt$momrace_p1 %in% API] <- "API"


pt$momrace_p2_fac <- NA

pt$momrace_p2_fac[pt$momrace_p2 == 1] <- "White"
pt$momrace_p2_fac[pt$momrace_p2 == 2] <- "Black"
pt$momrace_p2_fac[pt$momrace_p2 == 3] <- "Native American"
pt$momrace_p2_fac[pt$momrace_p2 == 9] <- "Hispanic"
pt$momrace_p2_fac[pt$momrace_p2 %in% API] <- "API"


###tobacco use
#replace 99 with NA

#find cigarette variables
cigs <- grep("^cig", names(pt))

pt[,cigs] <- sapply(pt[,cigs],function(x) ifelse(x==99, NA, x))

pt$smoked_p1 <- ifelse(pt$cigs_1st_p1>0 | pt$cigs_2nd_p1>0 | pt$cigs_3rd_p1>0,
                       "Yes", "No")

pt$smoked_p2 <- ifelse(pt$cigs_1st_p2>0 | pt$cigs_2nd_p2>0 | pt$cigs_3rd_p2>0,
                       "Yes", "No")

###payer
#replace 9's with NA
pt$delivpay_p1[pt$delivpay_p1 == 9] <- NA
pt$delivpay_p2[pt$delivpay_p2 == 9] <- NA

#create factor variable with levels defined in Variable Description
pt$delivpay_p1_fac <- factor(pt$delivpay_p1, labels = c("Medicaid",
                                                        "Self-Pay",
                                                        "Private Insurance",
                                                        "Indian Health Insurance",
                                                        "Champus/Tricare",
                                                        "Other Gov't",
                                                        "Charity Care",
                                                        "Other"))

pt$delivpay_p2_fac <- factor(pt$delivpay_p2, labels = c("Medicaid",
                                                        "Self-Pay",
                                                        "Private Insurance",
                                                        "Indian Health Insurance",
                                                        "Champus/Tricare",
                                                        "Other Gov't",
                                                        "Charity Care",
                                                        "Other"))

###primary payer and secondary payer
#replace 999 with NA

payid <- grep("^cmpayid", names(pt))

pt[,payid] <- sapply(pt[,payid],function(x) ifelse(x==999, NA, x))

#create factors with levels from Variable Description

pt$cmpayid1_p1_fac <- factor(pt$cmpayid1_p1, labels = c("Medicare",
                                                        "Medicaid",
                                                        "HMO",
                                                        "Commercial Insurance",
                                                        "Labor & Industries",
                                                        "Self-Pay",
                                                        "Health Care Services Contractor",
                                                        "Other Sponsored Patients",
                                                        "Charity Care"))

pt$cmpayid1_p2_fac <- factor(pt$cmpayid1_p2, labels = c("Medicare",
                                                        "Medicaid",
                                                        "HMO",
                                                        "Commercial Insurance",
                                                        "Labor & Industries",
                                                        "Self-Pay",
                                                        "Health Care Services Contractor",
                                                        "Other Sponsored Patients",
                                                        "Charity Care"))

pt$cmpayid2_p1_fac <- factor(pt$cmpayid2_p1, labels = c("Medicare",
                                                        "Medicaid",
                                                        "HMO",
                                                        "Commercial Insurance",
                                                        "Labor & Industries",
                                                        "Self-Pay",
                                                        "Health Care Services Contractor",
                                                        "Other Sponsored Patients",
                                                        "Charity Care"))

pt$cmpayid2_p2_fac <- factor(pt$cmpayid2_p2, labels = c("Medicare",
                                                        "Medicaid",
                                                        "HMO",
                                                        "Commercial Insurance",
                                                        "Labor & Industries",
                                                        "Self-Pay",
                                                        "Health Care Services Contractor",
                                                        "Other Sponsored Patients",
                                                        "Charity Care"))

with(pt,table(delivpay_p1_fac, cmpayid1_p1_fac))

###Diabetes
#replace 9's with NA's
pt$diabetes_p1[pt$diabetes_p1 == 9] <- NA
pt$diabetes_p2[pt$diabetes_p2 == 9] <- NA

pt$diabetes_p1_fac <- factor(pt$diabetes_p1, labels = c("No",
                                                        "Established",
                                                        "Gestational"))

pt$diabetes_p2_fac <- factor(pt$diabetes_p2, labels = c("No",
                                                        "Established",
                                                        "Gestational"))

###hypertension
#replace "unknown" with NA

pt$hyper_p1[pt$hyper_p1 == "Unknown"] <- NA
pt$hyper_p2[pt$hyper_p2 == "Unknown"] <- NA

###pre-eclampsia
#replace unknown with NA

levels(pt$preclamp_p1)[3] <- NA
levels(pt$preclamp_p2)[3] <- NA

###Infections
#recode unknown as NA

#get column numbers for strep, antibiot, and antiseps
inf_facs <- c(grep("^strep", names(pt)), grep("^anti", names(pt)))

pt[,inf_facs] <- lapply(pt[,inf_facs], function(x) factor(ifelse(x == "Unknown", NA, x),
                                                          labels = c("No",
                                                                     "Yes")))

#get column numbers for other infections
oth_inf <- c(grep("^chlam", names(pt)),
             grep("^gherp", names(pt)),
             grep("^gonor", names(pt)),
             grep("^hepat", names(pt)),
             grep("^minf", names(pt)),
             grep("^othin", names(pt)),
             grep("^syph", names(pt)))

pt[,oth_inf][pt[,oth_inf] == 9] <- NA
pt[,oth_inf][pt[,oth_inf] == 0] <- "No"
pt[,oth_inf][pt[,oth_inf] == 1] <- "Yes"
pt[,oth_inf][pt[,oth_inf] == 2] <- "Established"

pt$any_inf_p1 <- NA
pt$any_inf_p1 <-  with(pt, ifelse(chlamyd_p1=="Yes" | gherpes_p1=="Yes" | gonorrh_p1=="Yes" |  
                                    hepatitb_p1=="Yes" | hepatitc_p1=="Yes" | minfect_p1=="Yes" | 
                                    othinfec_p1=="Yes" | strep_p1=="Yes" | syphil_p1=="Yes" | 
                                    antibiot_p1=="Yes" | antiseps_p1=="Yes", "Yes", "No"))

pt$any_inf_p2 <- NA
pt$any_inf_p2 <-  with(pt, ifelse(chlamyd_p2=="Yes" | gherpes_p2=="Yes" | gonorrh_p2=="Yes" |  
                                    hepatitb_p2=="Yes" | hepatitc_p2=="Yes" | minfect_p2=="Yes" | 
                                    othinfec_p2=="Yes" | strep_p2=="Yes" | syphil_p2=="Yes" | 
                                    antibiot_p2=="Yes" | antiseps_p2=="Yes", "Yes", "No"))


###exposure status
#verify gestest_p1 < 37 for exposed = 1

by(pt$gestest_p1, pt$exposed, summary)


###gestation (age) estimate
#replace 99 and 999 with NA
#also limit gestest to between 22 and 42 weeks

pt$gestest_p1[pt$gestest_p1 == 99 |
                pt$gestest_p1 == 999 |
                pt$gestest_p1 < 22 |
                pt$gestest_p1 > 42] <- NA

pt$gestest_p2[pt$gestest_p2 == 99 |
                pt$gestest_p2 == 999 |
                pt$gestest_p2 < 22 |
                pt$gestest_p2 > 42] <- NA
table(pt$gestest_p1, pt$gestest_p2, useNA = "ifany")

###spontaneous delivery, any onset of labor, ruptured membranes, induction of labor,
### precipitous labor, prolonged labor, attempted labor, attempted forceps, attempted 
### vacuum, augmentation

lots_vars <- c(grep("^spontd", names(pt)),
               grep("^labons", names(pt)),
               grep("^rupmem", names(pt)),
               grep("^indl", names(pt)),
               grep("^precipl", names(pt)),
               grep("^longlab", names(pt)),
               grep("^forcf", names(pt)),
               grep("^vac", names(pt)),
               grep("^stimlab", names(pt)))

pt[,lots_vars][pt[,lots_vars] == "Unknown"] <- NA
pt[,lots_vars][pt[,lots_vars] == "U"] <- NA


###trial of labor (csection)
#change 9's to NA

pt$csection_p1[pt$csection_p1 == 9] <- NA
pt$csection_p2[pt$csection_p2 == 9] <- NA


#create factor variables with levels defined in Variable Description
pt$csection_p1_fac <- factor(pt$csection_p1, labels = c("Normal Vaginal",
                                                        "Primary C-Sec",
                                                        "Vaginal Following C",
                                                        "Repeat C No Lab",
                                                        "Repeat C Try Lab"))


pt$csection_p2_fac <- factor(pt$csection_p2, labels = c("Normal Vaginal",
                                                        "Primary C-Sec",
                                                        "Vaginal Following C",
                                                        "Repeat C No Lab",
                                                        "Repeat C Try Lab"))

###Final route of delivery (dmethfin)
#change 9 to NA
pt$dmethfin_p1[pt$dmethfin_p1 == 9] <- NA
pt$dmethfin_p2[pt$dmethfin_p2 == 9] <- NA

#create factor variables with levels defined in Variable Description
pt$dmethfin_p1_fac <- factor(pt$dmethfin_p1, labels = c("Spontaneous Vaginal",
                                                        "Forceps",
                                                        "Vacuum",
                                                        "Cesarean with Trial of Labor",
                                                        "Cesarean WITHOUT Trial of Labor"))

pt$dmethfin_p2_fac <- factor(pt$dmethfin_p2, labels = c("Spontaneous Vaginal",
                                                        "Forceps",
                                                        "Vacuum",
                                                        "Cesarean with Trial of Labor",
                                                        "Cesarean WITHOUT Trial of Labor"))


###Breech (fet_pres, breech)
#Recode U and Unknown as NA
pt$fet_pres_p1[pt$fet_pres_p1 == "U"] <- NA
pt$fet_pres_p2[pt$fet_pres_p2 == "U"] <- NA

pt$breech_p1[pt$breech_p1 == "Unknown"] <- NA
pt$breech_p2[pt$breech_p2 == "Unknown"] <- NA


#create factor variable with levels defined in Variable Description

pt$fet_pres_p1_fac <- factor(pt$fet_pres_p1, labels = c("Breech",
                                                        "Cephalic",
                                                        "Other"))

pt$fet_pres_p2_fac <- factor(pt$fet_pres_p2, labels = c("",
                                                        "Breech",
                                                        "Cephalic",
                                                        "Other"))

levels(pt$fet_pres_p2_fac)[1] <- NA


#note: breech_p1 and breech_p2 already factors with appropriate levels

###Interpregnancy interval
#create variable based on mbgap_2 and gestest
pt$ipi <- round(pt$mbgap_p2/(7*4.33) - pt$gestest_p2/4.33, 0)

pt$ipi[pt$ipi < 1] <- NA

pt$ipi_fac <- NA
pt$ipi_fac[pt$ipi <= 5] <- "1-5"
pt$ipi_fac[pt$ipi > 5 & pt$ipi <= 18] <- "6-18"
pt$ipi_fac[pt$ipi > 18] <- "19+"
table(pt$ipi_fac)

###BMI
#clean height and weight variable to recalculate BMI
hgt_na <- c(900, 909, 999)
pt$momhgt_p1[pt$momhgt_p1 %in% hgt_na] <- NA
pt$momhgt_p2[pt$momhgt_p2 %in% hgt_na] <- NA

pt$momhgt_p1_in <- as.numeric(substr(pt$momhgt_p1, start = 2, stop = 3)) +
  as.numeric(substr(pt$momhgt_p1, start = 1, stop = 1)) * 12

pt$momhgt_p2_in <- as.numeric(substr(pt$momhgt_p2, start = 2, stop = 3)) +
  as.numeric(substr(pt$momhgt_p2, start = 1, stop = 1)) * 12


pt$prewght_p1[pt$prewght_p1 == 999 |
                pt$prewght_p1 < 70] <- NA
pt$prewght_p2[pt$prewght_p2 == 999 |
                pt$prewght_p2 < 70] <- NA


#recalculate bmi with cleaned height and weight
pt$bmi_new_p1 <- (pt$prewght_p1 * 0.454) / ((pt$momhgt_p1_in * 0.0254)*(pt$momhgt_p1_in * 0.0254))
pt$bmi_new_p2 <- (pt$prewght_p2 * 0.454) / ((pt$momhgt_p2_in * 0.0254)*(pt$momhgt_p2_in * 0.0254))


#bmi categories based on CDC guidelines
pt$bmi_cut_p1 <- cut(pt$bmi_new_p1, 
                     breaks = c(0, 18.5, 25, 30, 100),
                     right = FALSE)
levels(pt$bmi_cut_p1) <- c("Underweight", "Normal", "Overweight", "Obese")

pt$bmi_cut_p2 <- cut(pt$bmi_new_p2, 
                     breaks = c(0, 18.5, 25, 30, 100),
                     right = FALSE)
levels(pt$bmi_cut_p2) <- c("Underweight", "Normal", "Overweight", "Obese")

#CHARS
#p2 diagnosisvariables
diag_p2 <- grep("^cmdiag", names(pt), value = T)
diag_p2 <- grep("p2", diag_p2, value = T)

#established hypertension
est_ht <- sapply(pt[,diag_p2], function(x) grep("^401|^402|^403|^404|^405|^4372", x, value = F))

est_ht_rows <- unique(paste(unlist(est_ht)))

#gestational hypertension
gest_ht <- sapply(pt[,diag_p2], function(x) grep("^642", x, value = F))

gest_ht_rows <- unique(paste(unlist(gest_ht)))

#create new ht variable
pt$ht_chars_p2 <- "No"
pt[gest_ht_rows, "ht_chars_p2"] <- "Gestational"
pt[est_ht_rows, "ht_chars_p2"] <- "Established"


#preeclampsia 6424-6427
preeclamp <- sapply(pt[,diag_p2], function(x) grep("^6424|^6425|^6426|^6427", x, value = F))

preeclamp_rows <- unique(paste(unlist(preeclamp)))

#create new preeclampsia code based on CHARS
pt$preeclamp_p2 <- "No"
pt[preeclamp_rows, "preeclamp_p2"] <- "Preeclampsia" 

#placenta previa 641.0, 641.1
plac_prev <- sapply(pt[,diag_p2], function(x) grep("^6410|^6411", x, value = F))

plac_prev_rows <- unique(paste(unlist(plac_prev)))


#create placenta previa variable based on CHARS
pt$placprev_chars_p2 <- "No"
pt[plac_prev_rows, "placprev_chars_p2"] <- "Placenta Previa"

#p1 diagnosisvariables
diag_p1 <- grep("^cmdiag", names(pt), value = T)
diag_p1 <- grep("p1", diag_p1, value = T)

#established hypertension
est_ht <- sapply(pt[,diag_p1], function(x) grep("^401|^402|^403|^404|^405|^4372", x, value = F))

est_ht_rows <- unique(paste(unlist(est_ht)))

#gestational hypertension
gest_ht <- sapply(pt[,diag_p1], function(x) grep("^642", x, value = F))

gest_ht_rows <- unique(paste(unlist(gest_ht)))

#create new ht variable
pt$ht_chars_p1 <- "No"
pt[gest_ht_rows, "ht_chars_p1"] <- "Gestational"
pt[est_ht_rows, "ht_chars_p1"] <- "Established"


#preeclampsia 6424-6427
preeclamp <- sapply(pt[,diag_p1], function(x) grep("^6424|^6425|^6426|^6427", x, value = F))

preeclamp_rows <- unique(paste(unlist(preeclamp)))

#create new preeclampsia code based on CHARS
pt$preeclamp_p1 <- "No"
pt[preeclamp_rows, "preeclamp_p1"] <- "Preeclampsia" 



#placenta previa 641.0, 641.1
plac_prev <- sapply(pt[,diag_p1], function(x) grep("^6410|^6411", x, value = F))

plac_prev_rows <- unique(paste(unlist(plac_prev)))


#create placenta previa variable based on CHARS

pt$placprev_chars_p1 <- "No"
pt[plac_prev_rows, "placprev_chars_p1"] <- "Placenta Previa"

### Clinical subtype algorithm ###

# if there was PROM, then birth is spontaneous regardless of other codes
# if no prom, then continue
pt$subtype_node_p1 <- ifelse(pt$gestest_p1 < 37,
                             ifelse(!is.na(pt$rupmem_p1) & pt$rupmem_p1 == "Yes", "SPONT-S1",
                                    # if labor was induced and there was no prom, then birth is indicated
                                    ifelse(!is.na(pt$rupmem_p1) & pt$rupmem_p1 == "No", 
                                           ifelse(!is.na(pt$indlab_p1) & pt$indlab_p1 == "Yes", "INDIC-I1",
                                                  # If labor was not induced and there was a code implying labor existed, then birth was spontaneous 
                                                  ifelse(!is.na(pt$indlab_p1) & pt$indlab_p1 == "No", 
                                                         ifelse((!is.na(pt$precipl_p1) & pt$precipl_p1 == "Yes") | 
                                                                (!is.na(pt$longlab_p1) & pt$longlab_p1 == "Yes") | 
                                                                (!is.na(pt$forcfail_p1) & pt$forcfail_p1 == "Y") | 
                                                                (!is.na(pt$vacfail_p1) & pt$vacfail_p1 == "Y")   | 
                                                                (!is.na(pt$stimlab_p1) & pt$stimlab_p1 == "Yes") | 
                                                                (!is.na(pt$dmethfin_p1) & pt$dmethfin_p1 == 4),
                                                                "SPONT-S2",
                                                                # among remaining records, if vaginal delivery then assume spontaneous labor
                                                                # if not vaginal delivery then assume indicated birth
                                                                ifelse((!is.na(pt$spontdel_p1) & pt$spontdel_p1 == "Yes") |
                                                                       (!is.na(pt$vacuum_p1) & pt$vacuum_p1 == "Yes") | 
                                                                       (!is.na(pt$otforcep_p1) & pt$otforcep_p1 == "Yes"), 
                                                                       "SPONT-S3", "INDIC-I2")
                                                         ),
                                                         NA)
                                           ),
                                           NA)
                             ),
                             NA)


pt$subtype_p1 <- ifelse(is.na(pt$subtype_node_p1), NA, substr(pt$subtype_node_p1, 1, 5))
pt$node_p1 <- ifelse(is.na(pt$subtype_node_p1), NA, substr(pt$subtype_node_p1, 7, 8))


### REPEAT FOR P2 ###  (Probably better, loopier way to do this)

# if there was PROM, then birth is spontaneous regardless of other codes
# if no prom, then continue
pt$subtype_node_p2 <- ifelse(pt$gestest_p2 < 37,
                             ifelse(!is.na(pt$rupmem_p2) & pt$rupmem_p2 == "Yes", "SPONT-S1",
                                    # if labor was induced and there was no prom, then birth is indicated
                                    ifelse(!is.na(pt$rupmem_p2) & pt$rupmem_p2 == "No", 
                                           ifelse(!is.na(pt$indlab_p2) & pt$indlab_p2 == "Yes", "INDIC-I1",
                                                  # If labor was not induced and there was a code implying labor existed, then birth was spontaneous 
                                                  ifelse(!is.na(pt$indlab_p2) & pt$indlab_p2 == "No", 
                                                         ifelse((!is.na(pt$precipl_p2) & pt$precipl_p2 == "Yes") | 
                                                                  (!is.na(pt$longlab_p2) & pt$longlab_p2 == "Yes") | 
                                                                  (!is.na(pt$forcfail_p2) & pt$forcfail_p2 == "Y") | 
                                                                  (!is.na(pt$vacfail_p2) & pt$vacfail_p2 == "Y")   | 
                                                                  (!is.na(pt$stimlab_p2) & pt$stimlab_p2 == "Yes") | 
                                                                  (!is.na(pt$dmethfin_p2) & pt$dmethfin_p2 == 4),
                                                                "SPONT-S2",
                                                                # among remaining records, if vaginal delivery then assume spontaneous labor
                                                                # if not vaginal delivery then assume indicated birth
                                                                ifelse((!is.na(pt$spontdel_p2) & pt$spontdel_p2 == "Yes") |
                                                                         (!is.na(pt$vacuum_p2) & pt$vacuum_p2 == "Yes") | 
                                                                         (!is.na(pt$otforcep_p2) & pt$otforcep_p2 == "Yes"), 
                                                                       "SPONT-S3", "INDIC-I2")
                                                         ),
                                                         NA)
                                           ),
                                           NA)
                             ),
                             NA)

pt$subtype_p2 <- ifelse(is.na(pt$subtype_node_p2), NA, substr(pt$subtype_node_p2, 1, 5))
pt$node_p2 <- ifelse(is.na(pt$subtype_node_p2), NA, substr(pt$subtype_node_p2, 7, 8))

#create binary variables for each exposure and outcome and code for epiR::epi.2by2()
# (i.e. 0 = Yes and 1 = No), where 0 is the group with the exposure or outcome in question
# (spontaneous pt or indicated pt), 1 is the comparison group (term births), and the other
# exposure/outcome (spontaneous or indicated) NOT of interest is coded as NA

spont_lbl <- c("Spontaneous PT", "Term")

pt$spont_p1 <- ifelse(pt$subtype_p1 == "SPONT", 0, NA)
pt[!is.na(pt$gestest_p1) & pt$gestest_p1 >= 37 , "spont_p1"] <- 1
pt$spont_p1 <- factor(pt$spont_p1, labels = c("SPONT_p1", "TERM_p1"))

pt$spont_p2 <- ifelse(pt$subtype_p2 == "SPONT", 0, NA)
pt[!is.na(pt$gestest_p2) & pt$gestest_p2 >= 37, "spont_p2"] <- 1
pt$spont_p2 <- factor(pt$spont_p2, labels = c("SPONT_p2", "TERM_p2"))

pt$indic_p1 <- ifelse(pt$subtype_p1 == "INDIC", 0, NA)
pt[!is.na(pt$gestest_p1) & pt$gestest_p1 >= 37, "indic_p1"] <- 1
pt$indic_p1 <- factor(pt$indic_p1, labels = c("INDIC_p1", "TERM_p1"))

pt$indic_p2 <- ifelse(pt$subtype_p2 == "INDIC", 0, NA)
pt[!is.na(pt$gestest_p2) & pt$gestest_p2 >= 37, "indic_p2"] <- 1
pt$indic_p2 <- factor(pt$indic_p2, labels = c("INDIC_p2", "TERM_p2"))
                    
                    
### EXCLUSIONS ###
#subset to those where p1 is first birth
# reduces data from 101,088 to 86,554 women
pt <- pt[pt$parity_p1 == 0, ]

#subset to those with non-missing gestest in both pregnancies
# reduces data from 86,554 to 86,453
pt <- pt[!is.na(pt$gestest_p1) & !is.na(pt$gestest_p1), ]
table(pt$exposed)

#subset to term and classified preterm
pt <- pt[(!is.na(pt$indic_p1) | !is.na(pt$spont_p1)) 
        & (!is.na(pt$indic_p2) | !is.na(pt$spont_p2)), ]
table(pt$subtype_p1, useNA = "ifany")
                    

write.csv(pt, file = "ptFinal2.csv")
