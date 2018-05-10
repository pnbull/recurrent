library(epiR)
require(fastmatch)

my_strata <- c("ipi_fac", "momrace_p2_fac", "momage_p2_fac", "momedu03_p2_fac2", 
               "smoked_p2", "diabetes_p2_fac", "any_inf_p2", 
               "bmi_cut_p2", "placprev_chars_p2", "preeclamp_p2", "ht_chars_p2")

stratified_SS <- function(x=pt, byvar, filePath){
  SS <- with(x, table(sponP1.b, sponP2.b, x[[fmatch(byvar,names(x))]]))
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
  SS <- with(x, table(indcP1.b, sponP2.b, x[[fmatch(byvar,names(x))]]))
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
  SS <- with(x, table(indcP1.b, indcP2.b, x[[fmatch(byvar,names(x))]]))
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
  SS <- with(x, table(sponP1.b, indcP2.b, x[[fmatch(byvar,names(x))]]))
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
  stratified_SS(pt, my_strata[i], "/Users/pnbullard/Documents/UW/SSstratas.csv")
}
for(i in 1:length(my_strata)){
  stratified_IS(pt, my_strata[i], "/Users/pnbullard/Documents/UW/ISstratas.csv")
}
for(i in 1:length(my_strata)){
  stratified_II(pt, my_strata[i], "/Users/pnbullard/Documents/UW/IIstratas.csv")
}
for(i in 1:length(my_strata)){
  stratified_SI(pt, my_strata[i], "/Users/pnbullard/Documents/UW/SIstratas.csv")
}

