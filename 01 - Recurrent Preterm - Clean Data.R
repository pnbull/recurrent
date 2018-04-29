### 514 Preterm Births ###
### Clean data ###

library(uwIntroStats)

pt_raw <- read.csv("C:/Users/Llamaface/Desktop/EPI 514/Data/P23_recurrent_preterm_04_24.csv")

names(pt_raw)

# keep only variables we're possibly interested in
pt <- subset(pt_raw, select = c(exposed, mbgap_p2, momage_p1, momedu03_p1, 
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
                                cmproc1_p2:cmproc25_p2, cbproc1_p2:cbproc25_p2))

# quick look at all values for each variable (other than ICD9)
for(i in c(1:47, 148:192)) {
  print(names(pt[i]))
  print(table(pt[, i]))
}

descrip(pt)

table(pt$gestest_p1, pt$exposed) # OK - all exposed have gestest_p1 < 37

pt$gestest_p2[pt$gestest_p2>=99] <-NA
pt$ipi = round(pt$mbgap_p2/7 - pt$gestest_p2, 0)
table(pt$ipi) # Range from -10 to 489 weeks.  Sparse under 5 weeks.

# investigate "labonset" variable
table(pt$labonset_p1, pt$rupmem_p1)
table(pt$labonset_p1, pt$precipl_p1)
table(pt$labonset_p1, pt$longlab_p1)
# "labonset" appears to be set to "Yes" if any of the other "onset of labor" box
# vars are checked, and also for "none of the above" onsets of labor 
# Klebanoff ignored this "none of the above" onset of labor checkbox.

table(pt$csection_p1, pt$dmethfin_p1)
table(pt$csection_p2, pt$dmethfin_p2)
# csection type and final method of delivery are consistent.
# use "dmethfin" for csection with trial of labor (provides more info than csection)

table(pt$parity_p1, pt$parity_p2)
# 80,842 2nd births with parity=1 or 999, of 86,554 1st births with parity= (93%)
