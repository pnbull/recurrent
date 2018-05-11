#Stratify for Analysis
#

library(epiR)


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






#Set up tables for each crude analysis

#Q1: Spont-Spont
q1_tbl <- table(pt$spont_p1, pt$spont_p2)

#Q2: Spont-Indic
q2_tbl <- table(pt$spont_p1, pt$indic_p2)

#Q3: Indic-Spont
q3_tbl <- table(pt$indic_p1, pt$spont_p2)

#Q4: Indic-Indic
q4_tbl <- table(pt$indic_p1, pt$indic_p2)

q1 <- epi.2by2(q1_tbl, method = "cohort.count")
q2 <- epi.2by2(q2_tbl, method = "cohort.count")
q3 <- epi.2by2(q3_tbl, method = "cohort.count")
q4 <- epi.2by2(q4_tbl, method = "cohort.count")

#get proportions for each analysis
q1_prop <- matrix(as.numeric(unlist(q1[6])), nrow = 3)[1:2, 1:2] %>%
          prop.table(1)

q2_prop <- matrix(as.numeric(unlist(q2[6])), nrow = 3)[1:2, 1:2] %>%
            prop.table(1)

q3_prop <- matrix(as.numeric(unlist(q3[6])), nrow = 3)[1:2, 1:2] %>%
  prop.table(1)

q4_prop <- matrix(as.numeric(unlist(q4[6])), nrow = 3)[1:2, 1:2] %>%
  prop.table(1)
###
#Run analysis by different levels of potential effect modifiers
###

#Create function to create table and run epi.2by2

q1_fx <- function(x){
  x_tbl <- with(x, table(spont_p1, spont_p2))
  epi.2by2(x_tbl, method = "cohort.count")
}


q2_fx <- function(x){
  x_tbl <- with(x, table(spont_p1, indic_p2))
  epi.2by2(x_tbl, method = "cohort.count")
}


q3_fx <- function(x){
  x_tbl <- with(x, table(indic_p1, spont_p2))
  epi.2by2(x_tbl, method = "cohort.count")
}


q4_fx <- function(x){
  x_tbl <- with(x, table(indic_p1, indic_p2))
  epi.2by2(x_tbl, method = "cohort.count")
}


#Run above function by different levels of potential effect modifiers
#IPI
by(pt, pt$ipi_fac, q1_fx)
by(pt, pt$ipi_fac, q2_fx)


by(pt, pt$diabetes_p1_fac, q1_fx)
by(pt, pt$diabetes_p1_fac, q2_fx)
by(pt, pt$diabetes_p1_fac, q3_fx)
by(pt, pt$diabetes_p1_fac, q4_fx)

by(pt, pt$preeclamp_p1, q1_fx)
by(pt, pt$preeclamp_p1, q2_fx)
by(pt, pt$preeclamp_p1, q3_fx)
by(pt, pt$preeclamp_p1, q4_fx)

by(pt, pt$ht_chars_p1, q1_fx)
by(pt, pt$ht_chars_p1, q2_fx)
by(pt, pt$ht_chars_p1, q3_fx)
by(pt, pt$ht_chars_p1, q4_fx)


by(pt, pt$ipi_fac, q3_fx)
by(pt, pt$ipi_fac, q4_fx)





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



