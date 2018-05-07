library(epiR)

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

write.csv(pt, "pt_cleanWsubtype.csv")

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
stratified_fx <- function(x, byvar){
  SS <- with(x, table(sponP1.b, sponP2.b, byvar))
  ss2 <- epi.2by2(SS, method = "cohort.count", homogeneity = "breslow.day")
  print("SS")
  print(ss2$massoc$RR.crude.wald)
  print(ss2$massoc$RR.strata.wald)
  print(ss2$massoc$RR.homog)

  IS <- with(x, table(indcP1.b, sponP2.b, byvar))
  is2 <- epi.2by2(IS, method = "cohort.count", homogeneity = "breslow.day")
  print("IS")
  print(is2$massoc$RR.crude.wald)
  print(is2$massoc$RR.strata.wald)
  print(is2$massoc$RR.homog)

  II <- with(x, table(indcP1.b, indcP2.b, byvar))
  ii2 <- epi.2by2(II, method = "cohort.count", homogeneity = "breslow.day")
  print("II")
  print(ii2$massoc$RR.crude.wald)
  print(ii2$massoc$RR.strata.wald)
  print(ii2$massoc$RR.homog)

  SI <- with(x, table(sponP1.b, indcP2.b, byvar))
  si2 <-epi.2by2(SI, method = "cohort.count", homogeneity = "breslow.day")
  print("SI")
  print(si2$massoc$RR.crude.wald)
  print(si2$massoc$RR.strata.wald)
  print(si2$massoc$RR.homog)

  IT <- with(x, table(indcP1.b, termP2.b, byvar))
  it2 <- epi.2by2(IT, method = "cohort.count", homogeneity = "breslow.day")
  print("IT")
  print(it2$massoc$RR.crude.wald)
  print(it2$massoc$RR.strata.wald)
  print(it2$massoc$RR.homog)

  ST <- with(x, table(sponP1.b, termP2.b, byvar))
  st2 <- epi.2by2(ST, method = "cohort.count", homogeneity = "breslow.day")
  print("ST")
  print(st2$massoc$RR.crude.wald)
  print(st2$massoc$RR.strata.wald)
  print(st2$massoc$RR.homog)
}

#Run above function by different levels of potential effect modifiers
stratified_fx(pt, pt$ipi_fac)
stratified_fx(pt, pt$momrace_p2_fac)
stratified_fx(pt, pt$momage_p2_fac)
stratified_fx(pt, pt$momedu03_p2_fac)
stratified_fx(pt, pt$smoked_p2)
stratified_fx(pt, pt$delivpay_p2_fac)
stratified_fx(pt, pt$diabetes_p2_fac)
stratified_fx(pt, pt$htnICDp2.b) #from CHARS
stratified_fx(pt, pt$preclampICDp2.b) #from CHARS
stratified_fx(pt, pt$placentaPrevICDp2.b) #from CHARS
stratified_fx(pt, pt$any_inf_p2)
stratified_fx(pt, pt$bmi_cut_p2)

