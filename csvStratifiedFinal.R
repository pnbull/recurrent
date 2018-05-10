library(epiR)
require(fastmatch)

pt <- read.csv("ptFinal2.csv")

my_strata <- c("ipi_fac", "momrace_p1_fac", "momage_p1_fac", "momedu03_p1_fac2", 
               "smoked_p1", "diabetes_p1_fac", "any_inf_p1", 
               "bmi_cut_p1", "placprev_chars_p1", "preeclamp_p1", "ht_chars_p1")

stratified_SS <- function(x=pt, byvar, filePath){
  SS <- with(x, table(spont_p1, spont_p2, x[[fmatch(byvar,names(x))]]))
  ss <- epi.2by2(SS, method = "cohort.count", homogeneity = "breslow.day")
  
  r1 <- rbind(
        ss$massoc$RR.crude.wald,
        ss$massoc$RR.strata.wald,
        ss$massoc$RR.mh.wald)
  
  rows <- c("crude", levels(x[[fmatch(byvar,names(x))]]), "MH")
  
  r2 <- cbind(rows, r1)
  r2[,-1] <-round(r2[,-1],2)

  write.table(byvar, file = filePath, append = T)
  write.table(r2, file = filePath, append = T, sep = ",")
  write.table(round(ss$massoc$RR.homog, 3), file = filePath, append = T, sep = ",")
}

stratified_IS <- function(x=pt, byvar, filePath){
  SS <- with(x, table(indic_p1, spont_p2, x[[fmatch(byvar,names(x))]]))
  ss <- epi.2by2(SS, method = "cohort.count", homogeneity = "breslow.day")
  
  r1 <- rbind(
    ss$massoc$RR.crude.wald,
    ss$massoc$RR.strata.wald,
    ss$massoc$RR.mh.wald)
  
  rows <- c("crude", levels(x[[fmatch(byvar,names(x))]]), "MH")
  
  r2 <- cbind(rows, r1)
  r2[,-1] <-round(r2[,-1],2)
  
  write.table(byvar, file = filePath, append = T)
  write.table(r2, file = filePath, append = T, sep = ",")
  write.table(round(ss$massoc$RR.homog, 3), file = filePath, append = T, sep = ",")
}

stratified_II <- function(x=pt, byvar, filePath){
  SS <- with(x, table(indic_p1, indic_p2, x[[fmatch(byvar,names(x))]]))
  ss <- epi.2by2(SS, method = "cohort.count", homogeneity = "breslow.day")
  
  r1 <- rbind(
    ss$massoc$RR.crude.wald,
    ss$massoc$RR.strata.wald,
    ss$massoc$RR.mh.wald)
  
  rows <- c("crude", levels(x[[fmatch(byvar,names(x))]]), "MH")
  
  r2 <- cbind(rows, r1)
  r2[,-1] <-round(r2[,-1],2)
  
  write.table(byvar, file = filePath, append = T)
  write.table(r2, file = filePath, append = T, sep = ",")
  write.table(round(ss$massoc$RR.homog, 3), file = filePath, append = T, sep = ",")
}

stratified_SI <- function(x=pt, byvar, filePath){
  SS <- with(x, table(spont_p1, indic_p2, x[[fmatch(byvar,names(x))]]))
  ss <- epi.2by2(SS, method = "cohort.count", homogeneity = "breslow.day")
  
  r1 <- rbind(
    ss$massoc$RR.crude.wald,
    ss$massoc$RR.strata.wald,
    ss$massoc$RR.mh.wald)
  
  rows <- c("crude", levels(x[[fmatch(byvar,names(x))]]), "MH")
  
  r2 <- cbind(rows, r1)
  r2[,-1] <-round(r2[,-1],2)
  
  write.table(byvar, file = filePath, append = T)
  write.table(r2, file = filePath, append = T, sep = ",")
  write.table(round(ss$massoc$RR.homog, 3), file = filePath, append = T, sep = ",")
}

for(i in 1:length(my_strata)){
  stratified_SS(pt, my_strata[i], "/Users/pnbullard/Documents/UW/SSstrtP1.csv")
}
for(i in 1:length(my_strata)){
  stratified_IS(pt, my_strata[i], "/Users/pnbullard/Documents/UW/ISstrtP1.csv")
}
for(i in 1:length(my_strata)){
  stratified_II(pt, my_strata[i], "/Users/pnbullard/Documents/UW/IIstrtP1.csv")
}
for(i in 1:length(my_strata)){
  stratified_SI(pt, my_strata[i], "/Users/pnbullard/Documents/UW/SIstrtP1.csv")
}

