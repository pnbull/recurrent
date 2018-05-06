
pt <- read.csv("/Users/pnbullard/Documents/GitHub/recurrent/pt_cleanWsubtype.csv")

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


library(epiR)

#Crude Spontaneous P1 with Spontaneous P2
t1 <- table(pt$sponP1.b, pt$sponP2.b, dnn = c("p1S", "p2S"))
t2 <- epi.2by2(dat=t1, method="cohort.count", conf.level=0.95, units=100, 
               outcome = "as.columns")

#Crude Indicaated P1 with Spontaneous P2
t3 <- table(pt$indcP1.b, pt$sponP2.b, dnn = c("p1I", "p2S"))
t4 <- epi.2by2(dat=t3, method="cohort.count", conf.level=0.95, units=100, 
               outcome = "as.columns")

#Crude Indicaated P1 with Indicaated P2
t5 <- table(pt$indcP1.b, pt$indcP2.b, dnn = c("p1I", "p2I"))
t6 <- epi.2by2(dat=t5, method="cohort.count", conf.level=0.95, units=100, 
               outcome = "as.columns")

#Crude Spontaneous P1 with Indicaated P2
t7 <- table(pt$sponP1.b, pt$indcP2.b, dnn = c("p1S", "p2I"))
t8 <- epi.2by2(dat=t7, method="cohort.count", conf.level=0.95, units=100, 
               outcome = "as.columns")



