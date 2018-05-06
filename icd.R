pt <- read.csv("/Users/pnbullard/Documents/GitHub/recurrent/pt_cleanWsubtype.csv")

diagVarsP1 <- c("cmdiag1_p1", "cmdiag2_p1", "cmdiag3_p1", "cmdiag4_p1", "cmdiag5_p1", 
                "cmdiag6_p1", "cmdiag7_p1", "cmdiag8_p1", "cmdiag9_p1", "cmdiag10_p1",
                "cmdiag11_p1", "cmdiag12_p1", "cmdiag13_p1", "cmdiag14_p1", "cmdiag15_p1",
                "cmdiag16_p1", "cmdiag17_p1", "cmdiag18_p1", "cmdiag19_p1", "cmdiag20_p1",
                "cmdiag21_p1", "cmdiag22_p1", "cmdiag23_p1", "cmdiag24_p1", "cmdiag25_p1")

diagVarsP2 <- c("cmdiag1_p2", "cmdiag2_p2", "cmdiag3_p2", "cmdiag4_p2", "cmdiag5_p2", 
                "cmdiag6_p2", "cmdiag7_p2", "cmdiag8_p2", "cmdiag9_p2", "cmdiag10_p2",
                "cmdiag11_p2", "cmdiag12_p2", "cmdiag13_p2", "cmdiag14_p2", "cmdiag15_p2",
                "cmdiag16_p2", "cmdiag17_p2", "cmdiag18_p2", "cmdiag19_p2", "cmdiag20_p2",
                "cmdiag21_p2", "cmdiag22_p2", "cmdiag23_p2", "cmdiag24_p2", "cmdiag25_p2")

pt$placentaPrevICDp1.b <- rep(0, nrow(pt))
pt$placentaPrevICDp2.b <- rep(0, nrow(pt))

pt$htnICDp1.b <- rep(0, nrow(pt))
pt$htnICDp2.b <- rep(0, nrow(pt))

pt$preclampICDp1.b <- rep(0, nrow(pt))
pt$preclampICDp2.b <- rep(0, nrow(pt))

#ICD codes for palcenta previa
placPrevICD <- c("64101", "64111")

#ICD codes for hypertension
htnICD <- as.character(seq(4010, 4050, by = 1))

#ICD codes for preeclampsia
preclamICD <- c("64240", "64241", "64242", "64243", "64244", "64250", "64251", "64252", 
                "64253", "64254", "64270", "64271", "64272", "64273", "64274")

for(i in 1:25){

pt$placentaPrevICDp1.b[pt[[diagVarsP1[i]]] %in% placPrevICD] <- 1
pt$placentaPrevICDp2.b[pt[[diagVarsP2[i]]] %in% placPrevICD] <- 1

pt$htnICDp1.b[pt[[diagVarsP1[i]]] %in% htnICD] <- 1
pt$htnICDp2.b[pt[[diagVarsP2[i]]] %in% htnICD] <- 1  

pt$preclampICDp1.b[pt[[diagVarsP1[i]]] %in% preclamICD] <- 1
pt$preclampICDp2.b[pt[[diagVarsP2[i]]] %in% preclamICD] <- 1
}

#Totals
sum(pt$placentaPrevICDp1.b)
sum(pt$placentaPrevICDp2.b)

sum(pt$htnICDp1.b)
sum(pt$htnICDp2.b)

sum(pt$preclampICDp1.b)
sum(pt$preclampICDp2.b)


