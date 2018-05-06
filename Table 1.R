#EPI 514 - Group Project
#Table 1 (Descriptives)

  
library(tidyverse)

age <- pt %>%
  group_by(subtype_p1) %>%
  summarise("Age (Mean)" = round(mean(momage_p1), 1), "Age (SD)" = round(sd(momage_p1), 1)) %>%
  t() %>%
  as.data.frame()


race <- pt %>%
  group_by(momrace_p1_fac, subtype_p1) %>%
  summarise("N" = n()) %>%
  spread(key = momrace_p1_fac, value = N) %>% 
  data.frame()

race$sum <- rowSums(race[,-1])



smoke <- pt %>%
  group_by(smoked_p1, subtype_p1) %>%
  summarise("N" = n()) %>%
  spread(key = smoked_p1, value = N) %>%
  data.frame()

smoke$sum <- rowSums(smoke[,-1])

table(pt$smoked_p1, useNA = "ifany")


by(pt$bmi_new_p1, pt$subtype_p1, summary)

sd_x <- function(x){
  sd(x, na.rm = T)
}

by(pt$bmi_new_p1, pt$subtype_p1, sd_x)

mean(pt[is.na(pt$subtype_p1), "bmi_new_p1"], na.rm = T)
sd(pt[is.na(pt$subtype_p1), "bmi_new_p1"], na.rm = T)

table(is.na(pt$bmi_new_p1), pt$subtype_p1, useNA = "ifany")


ipi <- pt %>%
  group_by(subtype_p1) %>%
  summarise("IPI (Mean)" = round(mean(ipi, na.rm = T), 1), 
            "IPI (SD)" = round(sd(ipi, na.rm = T), 1)) %>%
  t() %>%
  as.data.frame()



pt$edu_cat_p1 <- NULL
pt$edu_cat_p1 <- ifelse(pt$momedu03_p1 == 1 | pt$momedu03_p1 ==2, "Less than High-School",
                        ifelse(pt$momedu03_p1 == 3, "High-School or GED",
                               ifelse(pt$momedu03_p1 == 4 | pt$momedu03_p1 == 5, "Some College",
                                      ifelse(pt$momedu03_p1 %in% 6:8, "Bachelor's or Higher", NA))))


edu_cat <- pt %>%
  group_by(edu_cat_p1, subtype_p1) %>%
  summarise("N" = n()) %>%
  spread(key = edu_cat_p1, value = N) %>% 
  data.frame()

edu_cat$sum <- rowSums(edu_cat[,-1])

edu_cat <- as.data.frame(t(edu_cat))