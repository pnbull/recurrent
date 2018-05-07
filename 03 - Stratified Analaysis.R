
pt <- read.csv("/Users/pnbullard/Documents/GitHub/recurrent/pt_cleanWsubtype.csv")
pt <- read.csv("C:/Users/Llamaface/Documents/GitHub/recurrent/pt_cleanWsubtype.csv")

#Exposure == P1 spontaneous preterm
pt$sponP1.b <- rep(NA, nrow(pt))

for (i in 1:nrow(pt)){
  if (!is.na(pt[i, "subtype_p1"]) & pt[i, "subtype_p1"] == "SPONT") {pt$sponP1.b[i] <- 0}
  if (!is.na(pt[i, "gestest_p1"]) & pt[i, "gestest_p1"] >= 37){pt$sponP1.b[i] <- 1}
}

#Exposure == P1 indicated preterm 
pt$indcP1.b <- rep(NA, nrow(pt))

for (i in 1:nrow(pt)){
  if (!is.na(pt[i, "subtype_p1"]) & pt[i, "subtype_p1"] == "INDIC") {pt$indcP1.b[i] <- 0}
  if (!is.na(pt[i, "gestest_p1"]) & pt[i, "gestest_p1"] >= 37){pt$indcP1.b[i] <- 1}
}

#Outcome == P2 spontaneous preterm 
pt$sponP2.b <- rep(NA, nrow(pt))

for (i in 1:nrow(pt)){
  if (!is.na(pt[i, "subtype_p2"]) & pt[i, "subtype_p2"] == "SPONT") {pt$sponP2.b[i] <- 0}
  if (!is.na(pt[i, "gestest_p2"]) & pt[i, "gestest_p2"] >= 37){pt$sponP2.b[i] <- 1}
}

#Outcome == P2 indicated preterm 
pt$indcP2.b <- rep(NA, nrow(pt))

for (i in 1:nrow(pt)){
  if (!is.na(pt[i, "subtype_p2"]) & pt[i, "subtype_p2"] == "INDIC") {pt$indcP2.b[i] <- 0}
  if (!is.na(pt[i, "gestest_p2"]) & pt[i, "gestest_p2"] >= 37){pt$indcP2.b[i] <- 1}
}

#Outcome == P2 term 
pt$termP2.b <- rep(NA, nrow(pt))

for (i in 1:nrow(pt)){
  if (!is.na(pt[i, "gestest_p2"]) & pt[i, "gestest_p2"] < 37) {pt$termP2.b[i] <- 1}
  if (!is.na(pt[i, "gestest_p2"]) & pt[i, "gestest_p2"] >= 37){pt$termP2.b[i] <- 0}
}


library(epiR)

#Crude Spontaneous P1 with Spontaneous P2
t1 <- table(pt$sponP1.b, pt$sponP2.b, dnn = c("p1S", "p2S"))
t2 <- epi.2by2(dat=t1, method="cohort.count", conf.level=0.95, units=100, 
               outcome = "as.columns")

#Crude Indicated P1 with Spontaneous P2
t3 <- table(pt$indcP1.b, pt$sponP2.b, dnn = c("p1I", "p2S"))
t4 <- epi.2by2(dat=t3, method="cohort.count", conf.level=0.95, units=100, 
               outcome = "as.columns")

#Crude Indicated P1 with Indicated P2
t5 <- table(pt$indcP1.b, pt$indcP2.b, dnn = c("p1I", "p2I"))
t6 <- epi.2by2(dat=t5, method="cohort.count", conf.level=0.95, units=100, 
               outcome = "as.columns")

#Crude Spontaneous P1 with Indicated P2
t7 <- table(pt$sponP1.b, pt$indcP2.b, dnn = c("p1S", "p2I"))
t8 <- epi.2by2(dat=t7, method="cohort.count", conf.level=0.95, units=100, 
               outcome = "as.columns")

#Crude Indicated P1 with Term P2
t9 <- table(pt$indcP1.b, pt$termP2.b, dnn = c("p1I", "p2T"))
t10 <- epi.2by2(dat=t9, method="cohort.count", conf.level=0.95, units=100, 
               outcome = "as.columns")

#Crude Spontaneous P1 with Term P2
t11 <- table(pt$sponP1.b, pt$termP2.b, dnn = c("p1S", "p2T"))
t12 <- epi.2by2(dat=t11, method="cohort.count", conf.level=0.95, units=100, 
               outcome = "as.columns")


##Run analysis by different levels of potential effect modifiers
#Create function to create table and run epi.2by2

ss_fx <- function(x){
  SS <- with(x, table(sponP1.b, sponP2.b))
  print("SS")
  print(epi.2by2(SS, method = "cohort.count", homogeneity = "breslow.day"))
}

is_fx <- function(x){
  IS <- with(x, table(indcP1.b, sponP2.b))
  print("IS")
  print(epi.2by2(IS, method = "cohort.count", homogeneity = "breslow.day"))
}

ii_fx <- function(x){
  II <- with(x, table(indcP1.b, indcP2.b))
  print("II")
  print(epi.2by2(II, method = "cohort.count", homogeneity = "breslow.day"))
}

si_fx <- function(x){
  SI <- with(x, table(sponP1.b, indcP2.b))
  print("SI")
  print(epi.2by2(SI, method = "cohort.count", homogeneity = "breslow.day"))
}
  
it_fx <- function(x){
  IT <- with(x, table(indcP1.b, termP2.b))
  print("IT")
  print(epi.2by2(IT, method = "cohort.count", homogeneity = "breslow.day"))
}
  
st_fx <- function(x){
  ST <- with(x, table(sponP1.b, termP2.b))
  print("ST")
  print(epi.2by2(ST, method = "cohort.count", homogeneity = "breslow.day"))
}

#Run above function by different levels of potential effect modifiers
stratified_fx <- function(x){
  by(pt, x, ss_fx)
  by(pt, x, si_fx)
  by(pt, x, ii_fx)
  by(pt, x, is_fx)
  by(pt, x, it_fx)
  by(pt, x, st_fx)
}
stratified_fx(pt$ipi_fac)
stratified_fx(pt$momage_p2_fac)
stratified_fx(pt$momrace_p2_fac)
stratified_fx(pt$smoked_p2)
stratified_fx(pt$delivpay_p2_fac)
stratified_fx(pt$diabetes_p2_fac)
stratified_fx(pt$htnICDp2.b) #from CHARS
stratified_fx(pt$preclampICDp2.b) #from CHARS
stratified_fx(pt$placentaPrevICDp2.b) #from CHARS
stratified_fx(pt$any_inf_p1)
stratified_fx(pt$bmi_cut_p1)

# It doesn't look like we can get the breslow-day statistic using
# this "by" code, since it thinks of each strata separately and
# wouldn't know what to compare to.  Is that right?
