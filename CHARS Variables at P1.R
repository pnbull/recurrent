pt <- read.csv("Recurrent CLEANED Updated 5_2 MD.csv")

#p1 diagnosisvariables
diag_p1 <- grep("^cmdiag", names(pt), value = T)
diag_p1 <- grep("p1", diag_p1, value = T)

#established hypertension
est_ht <- sapply(pt[,diag_p1], function(x) grep("^401|^402|^403|^404|^405|^4372", x, value = F))

est_ht_rows <- unique(paste(unlist(est_ht)))

#gestational hypertension
gest_ht <- sapply(pt[,diag_p1], function(x) grep("^642", x, value = F))

gest_ht_rows <- unique(paste(unlist(gest_ht)))

#create new ht variable
pt$ht_chars_p1 <- "No"
pt[gest_ht_rows, "ht_chars_p1"] <- "Gestational"
pt[est_ht_rows, "ht_chars_p1"] <- "Established"


#preeclampsia 6424-6427
preeclamp <- sapply(pt[,diag_p1], function(x) grep("^6424|^6425|^6426|^6427", x, value = F))

preeclamp_rows <- unique(paste(unlist(preeclamp)))

#create new preeclampsia code based on CHARS
pt$preeclamp_p1 <- "No"
pt[preeclamp_rows, "preeclamp_p1"] <- "Preeclampsia" 



#placenta previa 641.0, 641.1
plac_prev <- sapply(pt[,diag_p1], function(x) grep("^6410|^6411", x, value = F))

plac_prev_rows <- unique(paste(unlist(plac_prev)))


#create placenta previa variable based on CHARS

pt$placprev_chars_p1 <- "No"
pt[plac_prev_rows, "placprev_chars_p1"] <- "Placenta Previa"
