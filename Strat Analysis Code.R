#Stratify for Analysis
#

library(epiR)

pt

#create binary variables for each exposure and outcome and code for epiR::epi.2by2()
# (i.e. 0 = Yes and 1 = No), where 0 is the group with the exposure or outcome in question
# (spontaneous pt or indicated pt), 1 is the comparison group (term births), and the other
# exposure/outcome (spontaneous or indicated) NOT of interest is coded as NA

spont_lbl <- c("Spontaneous PT", "Term")

pt$spont_p1 <- ifelse(pt$subtype_p1 == "SPONT", 0, NA)
pt[is.na(pt$subtype_p1), "spont_p1"] <- 1
pt$spont_p1 <- factor(pt$spont_p1, labels = c("SPONT_p1", "TERM_p1"))


pt$spont_p2 <- ifelse(pt$subtype_p2 == "SPONT", 0, NA)
pt[is.na(pt$subtype_p2), "spont_p2"] <- 1
pt$spont_p2 <- factor(pt$spont_p2, labels = c("SPONT_p2", "TERM_p2"))







#Run initial (crude) analysis of Spont_p1 vs Spont_p2

q1_tbl <- table(pt$spont_p1, pt$spont_p2)

epi.2by2(q1_tbl, method = "cohort.count")


##!!!Run analysis by different levels of potential effect modifiers
#Create function to create table and run epi.2by2

q1_fx <- function(x){
  x_tbl <- with(x, table(spont_p1, spont_p2))
  epi.2by2(x_tbl, method = "cohort.count")
}

#Run above function by different levels of potential effect modifiers
by(pt, pt$ipi_fac, q1_fx)
by(pt, pt$momrace_p1_fac, q1_fx)
by(pt, pt$momedu03_p1_fac, q1_fx)

#end stratification code









#re-stratify education by smaller categories
pt$edu_cat_p1 <- NULL
pt$edu_cat_p1 <- ifelse(pt$momedu03_p1 == 1 | pt$momedu03_p1 ==2, "Less than High-School",
                        ifelse(pt$momedu03_p1 == 3, "High-School or GED",
                               ifelse(pt$momedu03_p1 == 4 | pt$momedu03_p1 == 5, "Some College",
                                      ifelse(pt$momedu03_p1 %in% 6:8, "Bachelor's or Higher", NA))))


#create age stratification 
pt$age_cat_p1 <- cut(pt$momage_p1, breaks = c(0, 20, 35, 80))


#rerun 2by2 analysis by new education levels
by(pt, pt$edu_cat_p1, q1_fx)



