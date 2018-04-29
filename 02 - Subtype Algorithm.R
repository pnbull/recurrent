### 514 Preterm Births ###
### Clinical subtype algorithm ###
### (See bottom for variable definitions!) ###


### Restrict to those without prior births ###
# reduces data from 101,088 to 86,554 women
pt <- pt[pt$parity_p1 == 0, ]


### Clinical subtype algorithm ###

# if there was PROM, then birth is spontaneous regardless of other codes
# if no prom, then continue
pt$subtype_node_p1 <- ifelse(pt$gestest_p1 < 37,
  ifelse(pt$rupmem_p1 == "Yes", "SPONT-S1",
    # if labor was induced and there was no prom, then birth is indicated
    ifelse(pt$rupmem_p1 == "No", 
      ifelse(pt$indlab_p1 == "Yes", "INDIC-I1",
        # If labor was not induced and there was a code implying labor existed, then birth was spontaneous 
        ifelse(pt$indlab_p1 == "No", 
          ifelse(pt$precipl_p1 == "Yes" | pt$longlab_p1 == "Yes" | pt$forcfail_p1 == "Y" | 
                 pt$vacfail_p1 == "Y"   | pt$stimlab_p1 == "Yes" | pt$dmethfin_p1 == 4,
                 "SPONT-S2",
                 # among remaining records, if vaginal delivery then assume spontaneous labor
                 # if not vaginal delivery then assume indicated birth
                 ifelse(pt$spontdel_p1 == "Yes" |  pt$vacuum_p1 == "Yes" | pt$otforcep_p1 == "Yes", 
                        "SPONT-S3", "INDIC-I2")
          ),
        NA)
      ),
    NA)
  ),
NA)

pt$subtype_p1 <- ifelse(is.na(pt$subtype_node_p1), NA, substr(pt$subtype_node_p1, 1, 5))
pt$node_p1 <- ifelse(is.na(pt$subtype_node_p1), NA, substr(pt$subtype_node_p1, 7, 8))


### REPEAT FOR P2 ###  (Probably better, loopier way to do this)

# if there was PROM, then birth is spontaneous regardless of other codes
# if no prom, then continue
pt$subtype_node_p2 <- ifelse(pt$gestest_p2 < 37,
  ifelse(pt$rupmem_p2 == "Yes", "SPONT-S1",
    # if labor was induced and there was no prom, then birth is indicated
    ifelse(pt$rupmem_p2 == "No", 
      ifelse(pt$indlab_p2 == "Yes", "INDIC-I1",
        # If labor was not induced and there was a code implying labor existed, then birth was spontaneous 
        ifelse(pt$indlab_p2 == "No", 
          ifelse(pt$precipl_p2 == "Yes" | pt$longlab_p2 == "Yes" | pt$forcfail_p2 == "Y" | 
                 pt$vacfail_p2 == "Y"   | pt$stimlab_p2 == "Yes" | pt$dmethfin_p2 == 4,
                 "SPONT-S2",
                 # among remaining records, if vaginal delivery then assume spontaneous labor
                 # if not vaginal delivery then assume indicated birth
                 ifelse(pt$spontdel_p2 == "Yes" |  pt$vacuum_p2 == "Yes" | pt$otforcep_p2 == "Yes", 
                        "SPONT-S3", "INDIC-I2")
          ),
        NA)
      ),
    NA)
  ),
NA)

pt$subtype_p2 <- ifelse(is.na(pt$subtype_node_p2), NA, substr(pt$subtype_node_p2, 1, 5))
pt$node_p2 <- ifelse(is.na(pt$subtype_node_p2), NA, substr(pt$subtype_node_p2, 7, 8))


### Review algorithm assignment ###
prop.table(table(pt$subtype_p1)) # 33% indicated, 67% spontaneous
prop.table(table(pt$subtype_p2)) # 42% indicated, 58% spontaneous
prop.table(table(pt$node_p1)) # 15% I1, 19% I2, 20% S1, 19% S2, 28% S3
prop.table(table(pt$node_p2)) # 9% I1, 33% I2, 14% S1, 18% S2, 26% S3

# further investigate catch-all category of I2s (33%!) 
pt_i2 <- subset(pt, node_p2=="I2", select=-c(momage_p1:cbproc25_p1))
table(pt_i2$dmethfin_p2) # method of delivery
# 99 were 1= Spontaneous vaginal
# 3 were 2= Forceps
# 8 were 3= Vacuum
# 0 were 4= Cesarean with Trial of Labor
# 1,719 were 5= Cesarean without Trial Labor
# OK - these look (for the  most part) like true I2s

subsettable(pt$subtype_p1, pt$subtype_p2, useNA='ifany') 
# 2,338 total recurrent preterms (Ananth started with 3,559)



### Data dictionary for algorithm variables ###

  # rupmem - Onset of labor; ‘Premature rupture of the membranes (prolonged, >12 h)’ 
  # precipl – Onset of labor; ‘Precipitous labor’ 
  # longlab - Onset of labor; ‘Prolonged labor’ 
  # forcfail - Method of delivery; ‘Was delivery with forceps attempted but unsuccessful?’ 
  # vacfail - Method of delivery; ‘Was delivery with vacuum extraction attempted but unsuccessful?’ 
  # indlab - Characteristics of labor and delivery ‘Induction of labor’ 
  # stimlab - Characteristics of labor and delivery; ‘Augmentation of labor’ 
  # csection - Final route and method of delivery ‘If cesarean, was a trial of labor attempted?’ 
  # spontdel – Final route and method of delivery ‘Vaginal/Spontaneous’ 
  # vacuum – Final route and method of delivery ‘Vaginal/Vacuum’ 
  # otforcep – Final route and method of delivery ‘Vaginal/Forceps’
  # csection
  #   0= Normal Vaginal
  #   1= Primary C-Sec
  #   2= Vaginal Following C
  #   3= Repeat C No Lab
  #   4= Repeat C Try Lab
  #   5= Repeat C NOS
  #   9= Unknown
  # dmethfin - method of delivery 
  #   1= Spontaneous vaginal
  #   2= Forceps
  #   3= Vacuum
  #   4= Cesarean with Trial of Labor
  #   5= Cesarean without Trial Labor
  #   9= Unknown
