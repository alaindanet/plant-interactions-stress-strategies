#'#'#########################################
#'  Functions to manipulate the data sets  #'
#'  Date: 22-03-2016
#'  Author: Alain Danet
#'##########################################'

library(zoo)
library(lubridate)
library(tidyverse)
library(stringr)
library(testthat)
library(magrittr)

# Load data
january <- read_csv2("data/raw/data_january_plant.csv") %>%
    mutate(date = "jan")
march <- read_csv2("data/raw/data_mars_plant.csv") %>%
    mutate(date = "mar")
june <- read_csv2("data/raw/data_juin_plant.csv") %>%
    mutate(date = "jun")
november <- read_csv2("data/raw/data_nov_plant.csv") %>%
    mutate(date = "nov") # The table are quite different

# Which table do we want ?
## 1. Survival table
## 2. Quantitative data table (height and diameter)

####################
#  Survival table  #
####################

# In january, there is no dead because it was the starting point

var_joined_by <- c("ter", "plot", "label", "ms", "com", "watering", "ind", "date") # , "notes"
## species table
january_sp <- january %>%
    select(-h1, -h2, -h3, -d1, -d2, -d3, -e1, -e2, -e3, -f1, -f2, -f3) %>%
    gather(sp1, sp2, sp3, key = "ind", value = "species") %>%
    mutate(ind = str_replace(ind, "sp", ""))
## height table
january_h <- january %>%
    select(-sp1, -sp2, -sp3, -d1, -d2, -d3, -e1, -e2, -e3, -f1, -f2, -f3) %>%
    gather(h1, h2, h3, key = "ind", value = "h") %>%
    mutate(ind = str_replace(ind, "h", ""))
## state table
january_e <- january %>%
    select(-sp1, -sp2, -sp3, -d1, -d2, -d3, -h1, -h2, -h3, -f1, -f2, -f3) %>%
    gather(e1, e2, e3, key = "ind", value = "e") %>%
    mutate(ind = str_replace(ind, "e", ""))
## diameter table
january_d <- january %>%
    select(-sp1, -sp2, -sp3, -h1, -h2, -h3, -e1, -e2, -e3, -f1, -f2, -f3) %>%
    gather(d1, d2, d3, key = "ind", value = "d") %>%
    mutate(ind = str_replace(ind, "d", ""))
## flower table
january_f <- january %>%
    select(-sp1, -sp2, -sp3, -h1, -h2, -h3, -e1, -e2, -e3, -d1, -d2, -d3) %>%
    gather(f1, f2, f3, key = "ind", value = "f") %>%
    mutate(ind = str_replace(ind, "f", ""))
## full join
january_tot <- full_join(january_sp, january_h, by = var_joined_by) %>%
    full_join(., january_d, by = var_joined_by) %>%
    full_join(., january_e, by = var_joined_by) %>%
    full_join(., january_f, by = var_joined_by)

january_surv <- january_tot %>%
    rename(notes = e) %>%
    mutate(
	notes = ifelse(notes %in% c(1,2), ifelse(notes == 1, "bad", "very_bad"), "NA")
	)
january_surv2 <- january_surv %>%
    mutate(survival = 1,
	broken = 0,
	missing = 0,
	hm = NA,
	hm = as.numeric(hm)) %>%
select(-f)

#################
#  March table  #
#################

# How are noticed the dead individuals in March ?
march %>% filter(h1 == "Dead")
unique(c(march$h1, march$h2, march$h3)) # Dead are noticed "Dead" and broken are
#noticed "C"
unique(c(march$d1, march$d2, march$d3))
unique(c(march$hm1, march$hm2, march$hm3))

# Transform data
var_joined_by <- c("ter", "plot", "label", "ms", "com", "watering", "ind", "date", "notes")
## species table
march_sp <- march %>%
    select(-h1, -h2, -h3, -d1, -d2, -d3, -hm1, -hm2, -hm3) %>%
    gather(sp1, sp2, sp3, key = "ind", value = "species") %>%
    mutate(ind = str_replace(ind, "sp", ""))
## height table
march_h <- march %>%
    select(-sp1, -sp2, -sp3, -d1, -d2, -d3, -hm1, -hm2, -hm3) %>%
    gather(h1, h2, h3, key = "ind", value = "h") %>%
    mutate(ind = str_replace(ind, "h", ""))
## maximum height table
march_hm <- march %>%
    select(-sp1, -sp2, -sp3, -d1, -d2, -d3, -h1, -h2, -h3) %>%
    gather(hm1, hm2, hm3, key = "ind", value = "hm") %>%
    mutate(ind = str_replace(ind, "hm", ""))
## diameter table
march_d <- march %>%
    select(-sp1, -sp2, -sp3, -h1, -h2, -h3, -hm1, -hm2, -hm3) %>%
    gather(d1, d2, d3, key = "ind", value = "d") %>%
    mutate(ind = str_replace(ind, "d", ""))
## full join
march_tot <- full_join(march_sp, march_h, by = var_joined_by) %>%
    full_join(., march_d, by = var_joined_by) %>%
    full_join(., march_hm, by = var_joined_by)

# Are dead correctly recorded in h, d, hm ?
sort_survival <- function(x, dead = "Dead", casse = "C"){
ifelse(x %in% c(dead, casse), ifelse(x == dead, "D", "C"), 1)
}
mar_surv <- march_tot %>%
    mutate(
	eh = sort_survival(h),
	ed = sort_survival(d),
	ehm = sort_survival(hm))

mar_surv %>% filter(eh == "D", ed == "D", ehm == "D") %>% nrow(.)
mar_surv %>% filter(eh == "D") %>% nrow(.)
mar_surv %>% filter(ehm == "D") %>% nrow(.)
mar_surv %>% filter(ed == "D") %>% nrow(.) # Missing registered death in diameter columns
# Check that dead recorded are the same in h and hm:
mar_surv %>% filter(eh == "D", ehm == "D") %>% nrow(.) ## There are, nice!
## Take death from the eh or ehm column
replace_numerise <- function(x, pattern = "Dead|C", replacement = "NA", dec = ","){
    temp <- str_replace(x, pattern, replacement)
    temp2 <- str_replace(temp, dec, ".")
    res <- as.numeric(temp2)
    # Check that no extra NA are introduced
    expect_equal(sum(temp == "NA", is.na(temp), na.rm = T), sum(is.na(res)))
    return(res)
}
mar_surv2 <- mar_surv %>%
    rename(survival = eh) %>%
    mutate(
	broken = ifelse(survival == "C", 1, 0), # Separate natural death from accidental breaking
	survival = ifelse(survival == "D", 0, 1),
	h = replace_numerise(h),
	d = replace_numerise(d),
	hm = replace_numerise(hm)
	) %>%
    select(-ed, -ehm) %>% # Do not need anymore
    mutate(missing = 0)
## March okay now =)

################
#  June table  #
################

june %>% filter(h1 == "D")
unique(c(june$h1, june$h2, june$h3))
unique(c(june$e1, june$e2, june$e3))

# Transform data
var_joined_by <- c("ter", "plot", "label", "ms", "com", "watering", "ind",
    "date") #, "notes" not in june dataset
## species table
june_sp <- june %>%
    select(-h1, -h2, -h3, -d1, -d2, -d3, -hm1, -hm2, -hm3, -e1, -e2, -e3) %>%
    gather(sp1, sp2, sp3, key = "ind", value = "species") %>%
    mutate(ind = str_replace(ind, "sp", ""))
## height table
june_h <- june %>%
    select(-sp1, -sp2, -sp3, -d1, -d2, -d3, -hm1, -hm2, -hm3, -e1, -e2, -e3) %>%
    gather(h1, h2, h3, key = "ind", value = "h") %>%
    mutate(ind = str_replace(ind, "h", ""))
## maximum height table
june_hm <- june %>%
    select(-sp1, -sp2, -sp3, -d1, -d2, -d3, -h1, -h2, -h3, -e1, -e2, -e3) %>%
    gather(hm1, hm2, hm3, key = "ind", value = "hm") %>%
    mutate(ind = str_replace(ind, "hm", ""))
## diameter table
june_d <- june %>%
    select(-sp1, -sp2, -sp3, -h1, -h2, -h3, -hm1, -hm2, -hm3, -e1, -e2, -e3) %>%
    gather(d1, d2, d3, key = "ind", value = "d") %>%
    mutate(ind = str_replace(ind, "d", ""))
## state table
june_e <- june %>%
    select(-sp1, -sp2, -sp3, -h1, -h2, -h3, -hm1, -hm2, -hm3, -d1, -d2, -d3) %>%
    gather(e1, e2, e3, key = "ind", value = "e") %>%
    mutate(ind = str_replace(ind, "e", ""))
## full join
june_tot <- full_join(june_sp, june_h, by = var_joined_by) %>%
    full_join(., june_d, by = var_joined_by) %>%
    full_join(., june_hm, by = var_joined_by) %>%
    full_join(., june_e, by = var_joined_by)

# Are dead correctly recorded in h, d, hm ?
sort_survival <- function(x, dead = "Dead", casse = "C", missing = "M"){
ifelse(x %in% c(dead, casse, missing), 
    ifelse(x == dead, "D", 
	ifelse(x == missing, "M", "C")), 1)
}
june_surv <- june_tot %>%
    mutate(
	eh = sort_survival(h, dead = "D"),
	ed = sort_survival(d, dead = "D"),
	ehm = sort_survival(hm, dead = "D"))
june_surv

## Dead recording
june_surv %>% filter(eh == "D", ed == "D", ehm == "D") %>% nrow(.)
june_surv %>% filter(eh == "D") %>% nrow(.)
june_surv %>% filter(ehm == "D") %>% nrow(.)
june_surv %>% filter(ed == "D") %>% nrow(.) # Missing registered death in diameter columns
# Check that dead recorded are the same in h and hm:
june_surv %>% filter(eh == "D", ehm == "D") %>% nrow(.) ## One miss! 
# Which is missing ?
june_surv %>% filter(eh != "D", ehm == "D") # One is recorded C in h and D in hm, should be recorded in C in hm


## Broken recording
june_surv %>% filter(eh == "C", ed == "C", ehm == "C") %>% nrow(.)
june_surv %>% filter(eh == "C") %>% nrow(.)
june_surv %>% filter(ehm == "C") %>% nrow(.)
june_surv %>% filter(ed == "C") %>% nrow(.) # Recorded by 
# Check that dead recorded are the same in h and hm:
june_surv %>% filter(eh == "C", ehm != "C") ## One miss! 
june_surv[which(june_surv$eh == "C" & june_surv$ehm != "C"), c("hm", "ehm")] <- c("C", "C")

## Missing recording
june_surv %>% filter(eh == "M" | ed == "M" | ehm == "M")
# En 833, replace M by D
june_surv[which(june_surv$label == 833 & june_surv$ind == "1"), c("h", "hm", "eh", "ehm")] <- rep("D", 4)
# En 445 et 938, replace D by M
june_surv[which(june_surv$label %in% c(445, 938) & june_surv$ind == "1"), c("h", "hm", "eh", "ehm")] <- rep("M", 8) 
# en 803, replace NA by M
june_surv[which(june_surv$label == 803 & june_surv$ind == "3"), c("hm", "ehm")] <- rep("M", 2)
# replace NA to 3 in 803
june_surv[which(june_surv$label == 803 & june_surv$ind == "3"), c("ter")] <- 3

replace_numerise <- function(x, pattern = "Dead|C", replacement = "NA", dec = ","){
    temp <- str_replace(x, pattern, replacement)
    temp2 <- str_replace(temp, dec, ".")
    res <- as.numeric(temp2)
    # Check that no extra NA are introduced
    expect_equal(sum(temp == "NA", is.na(temp), na.rm = T), sum(is.na(res)))
    return(res)
}
june_surv2 <- june_surv %>%
    rename(survival = eh, notes = e) %>%
    mutate(
	broken = ifelse(survival == "C", 1, 0), # Separate natural death from accidental breaking
	survival = ifelse(survival == "D", 0, 1),
	missing = ifelse(survival == "M", 1, 0),
	h = replace_numerise(h, pattern = "D|C|M"),
	d = replace_numerise(d, pattern = "D|C|M"),
	hm = replace_numerise(hm, pattern = "D|C|M")
	) %>%
    select(-ed, -ehm) # Do not need anymore

####################
#  November table  #
####################

november %>% filter(h1 == "D")
unique(c(november$h1, november$h2, november$h3))
unique(c(november$e1, november$e2, november$e3))
## rename columns
november <- november %>%
    rename(hm2 = `hm²`)

# Transform data
var_joined_by <- c("ter", "plot", "label", "ms", "com", "watering", "ind", "date") #, "notes" not in november dataset


## species table
november_sp <- november %>%
    select(-h1, -h2, -h3, -d1, -d2, -d3, -hm1, -hm2, -hm3, -e1, -e2, -e3) %>%
    gather(sp1, sp2, sp3, key = "ind", value = "species") %>%
    mutate(ind = str_replace(ind, "sp", ""))
## height table
november_h <- november %>%
    select(-sp1, -sp2, -sp3, -d1, -d2, -d3, -hm1, -hm2, -hm3, -e1, -e2, -e3) %>%
    select(-long_nurse, -larg_nurse, -nurse_dead) %>%
    gather(h1, h2, h3, key = "ind", value = "h") %>%
    mutate(ind = str_replace(ind, "h", ""))
## maximum height table
november_hm <- november %>%
    select(-sp1, -sp2, -sp3, -d1, -d2, -d3, -h1, -h2, -h3, -e1, -e2, -e3) %>%
    select(-long_nurse, -larg_nurse, -nurse_dead) %>%
    gather(hm1, hm2, hm3, key = "ind", value = "hm") %>%
    mutate(ind = str_replace(ind, "hm", ""))
## diameter table
november_d <- november %>%
    select(-sp1, -sp2, -sp3, -h1, -h2, -h3, -hm1, -hm2, -hm3, -e1, -e2, -e3) %>%
    select(-long_nurse, -larg_nurse, -nurse_dead) %>%
    gather(d1, d2, d3, key = "ind", value = "d") %>%
    mutate(ind = str_replace(ind, "d", ""))
## state table
november_e <- november %>%
    select(-sp1, -sp2, -sp3, -h1, -h2, -h3, -hm1, -hm2, -hm3, -d1, -d2, -d3) %>%
    select(-long_nurse, -larg_nurse, -nurse_dead) %>%
    gather(e1, e2, e3, key = "ind", value = "e") %>%
    mutate(ind = str_replace(ind, "e", ""))
## full join
november_tot <- full_join(november_sp, november_h, by = var_joined_by) %>%
    full_join(., november_d, by = var_joined_by) %>%
    full_join(., november_hm, by = var_joined_by) %>%
    full_join(., november_e, by = var_joined_by)

## Survival
november_surv <- november_tot %>%
    mutate(
	eh = sort_survival(h, dead = "D"),
	ed = sort_survival(d, dead = "D"),
	ehm = sort_survival(hm, dead = "D"))
november_surv

## Dead recording
november_surv %>% filter(eh == "D") %>% nrow(.)
november_surv %>% filter(ehm == "D") %>% nrow(.)
november_surv %>% filter(ed == "D") %>% nrow(.) # Dead has not been registered as dead in diameter columns

# Check that dead recorded are the same in h and hm:
# Which is missing ?
november_surv %>% filter(eh != "D", ehm == "D") # Status not recorded in hm and ehm,
# so we keep only h and eh

## Broken recording
november_surv %>% filter(eh == "C", ed == "C", ehm == "C") %>% nrow(.)
november_surv %>% filter(eh == "C") %>% nrow(.)
november_surv %>% filter(ehm == "C") %>% nrow(.)
november_surv %>% filter(ed == "C") %>% nrow(.) # Recorded by 
# Check that dead recorded are the same in h and hm:
november_surv %>% filter(eh == "C", ehm != "C") ## One miss! 
november_surv[which(november_surv$eh == "C" & november_surv$ehm != "C"), c("hm", "ehm")] <- c("C", "C")

## Missing recording
november_surv %>% filter(eh == "M" | ed == "M" | ehm == "M")
november_surv[which(november_surv$eh == "M" | november_surv$ed == "M" | november_surv$ehm == "M"), c("h", "hm", "eh", "ehm")] <- rep("M", 55*4)
november_surv %>% filter(eh == "M" | ed == "M" | ehm == "M")

## Resprout and NF (No Feuilles) recording
november_surv %>% filter(h == "RS", e == "resprout")
november_surv %>% filter(h == "RS" | e == "resprout") # Let put resprout status in notes
november_surv[which(november_surv$h == "RS"), "e"] <- rep("resprout", 5)
### NF
november_surv %>% filter(h == "NF", e == "NF")
november_surv %>% filter(h == "NF" | e == "NF") # Let put NF from h to notes 
november_surv %>% filter(h == "NF") # Let put NF from h to notes 
november_surv[which(november_surv$h == "NF"), "e"] <- rep("NF", 6)
november_surv %>% filter(h == "NF", e != "NF") %>% nrow(.) # Good 
#### Those that was recorded NF in notes but not in h
november_surv %>% filter(h != "NF", e == "NF") 
november_surv[which(november_surv$h != "NF" & november_surv$e == "NF"), "h"] <- "NF"

## Re-run sort survival!
november_surv3 <- november_surv %>%
    mutate(
	eh = sort_survival(h, dead = "D"),
	ed = sort_survival(d, dead = "D"),
	ehm = sort_survival(hm, dead = "D"))

november_surv2 <- november_surv3 %>%
    rename(survival = eh, notes = e) %>%
    mutate(
	broken = ifelse(survival == "C", 1, 0), # Separate natural death from accidental breaking
	survival = ifelse(survival == "D", 0, 1),
	missing = ifelse(survival == "M", 1, 0),
	h = replace_numerise(h, pattern = "D|C|M|RS|NF"),
	d = replace_numerise(d, pattern = "D|C|M|RS|NF"),
	hm = replace_numerise(hm, pattern = "D|C|M|RS|NF")
	) %>%
    select(-ed, -ehm) %>% # Do not need anymore
    select(-long_nurse, -larg_nurse, -nurse_dead)

####################
#  Assembly total  #
####################

january_surv2 %>% nrow(.)
mar_surv2 %>% nrow(.)
june_surv2 %>% nrow(.)
november_surv2 %>% nrow(.)

which( january_surv2$label %in% mar_surv2$label)

var_joined_by <- c("ter", "plot", "label", "ms", "com", "watering", 
    "ind") 

# Full dataset
try1 <- full_join(january_surv2, mar_surv2) %>%
    full_join(., june_surv2) %>%
    full_join(., november_surv2)

####################
#  Assembly order  #
####################

jan <- january_surv2 %>% select(ter, plot, label, ms, com, species, watering)
mar <- mar_surv2 %>% select(ter, plot, label, ms, com, species, watering)
jun <- june_surv2 %>% select(ter, plot, label, ms, com, species, watering)
nov <- november_surv2 %>% select(ter, plot, label, ms, com, species, watering)
test <- setdiff(jan, mar)
test2 <- setdiff(mar, jun)
test3 <- setdiff(jun, nov)
test4 <- setdiff(jan, jun)
test5 <- setdiff(jan, nov)

## Mismatch between january and march
test

### 911 has been lost 
january_surv2 %>% filter(label == 911)
mar_surv2 %>% filter(label == 911) 
june_surv2 %>% filter(label == 911)
november_surv2 %>% filter(label == 911)
#### Had 991 to the other file
temp <- filter(january_surv2, label == 911) %>%
    mutate(h = "NA", d = "NA", survival = "NA", broken = "NA", missing = "NA") %>%
    mutate(notes = "NA")
if(filter(mar_surv2, label == 911) %>% nrow(.) == 0){
temp2 <- mutate(temp, date = "mar")
mar_surv2 <- rbind(mar_surv2, temp2)
}
if(filter(june_surv2, label == 911) %>% nrow(.) == 0){
temp2 <- mutate(temp, date = "jun")
june_surv2 <- rbind(june_surv2, temp2)
}

### 1081
filter(january_surv2, label == 1081) # 1081 watering column is badly reported as 0
filter(mar_surv2, label == 1081)
filter(june_surv2, label == 1081)
filter(november_surv2, label == 1081) 
january_surv2[which(january_surv2$label == 1081), "watering"] <- rep(1, 3)
 
### 804
january_surv2 %>% filter(label == 804) # In january, 804 ms is badly recorded as 0
mar_surv2 %>% filter(label == 804) 
june_surv2 %>% filter(label == 804)
november_surv2 %>% filter(label == 804)
january_surv2[which(january_surv2$label == 804), "ms"] <- rep(1, 3)

## Mismatch between march and june
test2

### 1013
#### There is 1013 in plot 11 and 1103 in plot 10
january_surv2 %>% filter(label == 1013) 
mar_surv2 %>% filter(label == 1013) 
june_surv2 %>% filter(label == 1013)
november_surv2 %>% filter(label == 1013)
#### In march and november, probably that 1013 is 1103 actually in plot 10 
# Let's check:
filter(mar_surv2, label == 1103) 
filter(november_surv2, label == 1103) # Nope 
#### May be that is the 1113 
filter(mar_surv2, label == 1113) 
filter(november_surv2, label == 1113) # Ok, not present in them
mar_surv2[which(mar_surv2$label == 1013 & mar_surv2$plot == 10), "label"] <- rep(1113, 3)
november_surv2[which(november_surv2$label == 1013 & november_surv2$plot == 10), "label"] <- rep(1113, 3)

### 857
january_surv2 %>% filter(label == 857) # Has been forgotten in January data
mar_surv2 %>% filter(label == 857) # Has been badly recorded as Anthyllis
june_surv2 %>% filter(label == 857)
november_surv2 %>% filter(label == 857)
#### Correcting in march
mar_surv2[which(mar_surv2$label == 857), "com"] <- rep("D", 3) 
mar_surv2[which(mar_surv2$label == 857), "species"] <- rep("dorycnium", 3) 
### Add January
temp <- mar_surv2 %>% filter(label == 857) %>%
    mutate(date = "jan", h = "NA", d = "NA", hm = "NA")
if(filter(january_surv2, label == 857) %>% nrow(.) == 0){
january_surv2 <- rbind(january_surv2, temp)  
}

## Mismatch between june and nov
test3

### 579
january_surv2 %>% filter(label == 579) 
mar_surv2 %>% filter(label == 579) 
june_surv2 %>% filter(label == 579)
november_surv2 %>% filter(label == 579) # it is missing in november
#### May be recorded as 578 in november
november_surv2 %>% filter(label == 578) # It is the case
june_surv2 %>% filter(label == 578)
mar_surv2 %>% filter(label == 578)
january_surv2 %>% filter(label == 578)
#### correction
november_surv2[which(november_surv2$label == 578), "label"] <- rep(579, 3)

### 1010
january_surv2 %>% filter(label == 1010) # Badly recorded in plot 10, instead of 11
mar_surv2 %>% filter(label == 1010) # Badly recorded in plot 10, instead of 11
june_surv2 %>% filter(label == 1010) # Double record, but empty in plot 10
november_surv2 %>% filter(label == 1010)
#### correction
january_surv2[which(january_surv2$label == 1010), "plot"] <- rep(11, 3)
mar_surv2[which(mar_surv2$label == 1010), "plot"] <- rep(11, 3)
if(filter(june_surv2, label == 1010, plot == 10) %>% nrow(.) != 0){
june_surv2 <- june_surv2[ - which(june_surv2$label == 1010 & june_surv2$plot == 10),]
}

### 803
january_surv2 %>% filter(label == 803) 
mar_surv2 %>% filter(label == 803) 
june_surv2 %>% filter(label == 803) # 803 only exists in june dataset
november_surv2 %>% filter(label == 803)
##### The ind 3 was dead and missing in june normally
june_surv2[which(june_surv2$label == 803 & june_surv2$ind == 3), c("survival", "missing")] <- c(0,1)
##### bug in ter number recorded 
june_surv2[which(june_surv2$label == 803), "ter"] <- rep(3, 3)
##### As been discovered in june but was not discovered again
temp <- filter(june_surv2, label == 803) %>%
    mutate(h = "NA", d = "NA", hm = "NA")
if(filter(january_surv2, label == 803) %>% nrow(.) == 0){
temp2 <- mutate(temp, date = "jan")
january_surv2 <- rbind(january_surv2, temp2)  
}
if(filter(mar_surv2, label == 803) %>% nrow(.) == 0){
temp2 <- mutate(temp, date = "mar")
mar_surv2 <- rbind(mar_surv2, temp2)  
}
if(filter(november_surv2, label == 803) %>% nrow(.) == 0){
temp2 <- mutate(temp, date = "nov")
november_surv2 <- rbind(november_surv2, temp2)  
}
### 857 should be D instead of A 
january_surv2 %>% filter(label == 857) 
mar_surv2 %>% filter(label == 857) 
june_surv2 %>% filter(label == 857) 
november_surv2 %>% filter(label == 857) # Seems good

## Mismatch between january and june
test4
### 1010 
january_surv2 %>% filter(label == 1010) # ms noted 1 instead of 0 
mar_surv2 %>% filter(label == 1010) # ms noted 1 instead of 0
june_surv2 %>% filter(label == 1010) 
november_surv2 %>% filter(label == 1010)
#### correction
january_surv2[which(january_surv2$label == 1010), "ms"] <- rep(0, 3)
mar_surv2[which(mar_surv2$label == 1010), "ms"] <- rep(0, 3)

## Mismatch between june and november
test5

jan <- select(january_surv2, ter, plot, label, ms, com, species, watering)
mar <- select(mar_surv2, ter, plot, label, ms, com, species, watering)
jun <- select(june_surv2, ter, plot, label, ms, com, species, watering)
nov <- select(november_surv2, ter, plot, label, ms, com, species, watering)
## test dataset
test <- setdiff(jan, mar)
setdiff(mar, jan)
test2 <- setdiff(mar, jun)
setdiff(jun, mar)
test3 <- setdiff(jun, nov)
setdiff(nov, jun)
test4 <- setdiff(jan, jun)
setdiff(jun, jan)
test5 <- setdiff(jan, nov) # All is okay!
setdiff(nov, jan) # All is okay!

## 913
filter(january_surv2, label == 913) ## 913 was not recorded in january
filter(mar_surv2, label == 913) 
filter(june_surv2, label == 913) 
filter(november_surv2, label == 913)
##### Has been discovered in march 
temp <- filter(june_surv2, label == 913) %>%
    mutate(h = "NA", d = "NA", hm = "NA")
if(filter(january_surv2, label == 913) %>% nrow(.) == 0){
temp2 <- mutate(temp, date = "jan")
january_surv2 <- rbind(january_surv2, temp2)  
}

## 1113
filter(january_surv2, label == 1113) ## 1113 was not recorded in january
filter(mar_surv2, label == 1113)
filter(june_surv2, label == 1113)
filter(november_surv2, label == 1113)
temp <- filter(june_surv2, label == 1113) %>%
    mutate(h = "NA", d = "NA", hm = "NA")
if(filter(january_surv2, label == 1113) %>% nrow(.) == 0){
temp2 <- mutate(temp, date = "jan")
january_surv2 <- rbind(january_surv2, temp2)  
}

## 517
filter(january_surv2, label == 517) ## 517 was not recorded in january,
filter(mar_surv2, label == 517) # neither in march
filter(june_surv2, label == 517)# neither in june 
filter(november_surv2, label == 517)
if(filter(november_surv2, label == 517) %>% nrow(.) != 0){
november_surv2 <- filter(november_surv2, label != 517)
}
## No more errors, it looks great

# Joining
try1 <- full_join(january_surv2, mar_surv2) %>%
    full_join(., mutate(june_surv2, hm = as.character(hm))) %>%
    full_join(., november_surv2)
## Transform date
try2 <- mutate(try1,
    date = str_replace_all(date, c("jan" = "2016-01", "mar" = "2016-03", "jun" = "2016-06", "nov" = "2016-11")),
    date = as.yearmon(date),
    month = month(date),
    survival = as.numeric(survival),
    h = as.numeric(h),
    hm = as.numeric(hm),
    d = as.numeric(d),
    ms = as.factor(ms),
    watering = as.factor(watering),
    h_d = h/d,
    hm_d = hm/d,
    hm_h = hm/h
)

#save(try2, file = "../data/try2.RData")

##Keep holes characteristics
label_characteristics <- filter(try2, ind == 1, month == 6) %>%
    select(ter, plot, label, ms, com, watering) %>%
    mutate(
	com = ifelse(com %in% c("A", "D", "P"), "Mono", "Poly"),
	ms = ifelse(ms == "0", "Open", "Patch"),
	watering = ifelse(watering == "0", "No Watered", "Watered"),
	watering = as.factor(watering)
	)

## Put bad nurses in open 
### Nurse status are in november dataset
temp <- filter(january, label == 911) %>%
    mutate(long_nurse = NA, larg_nurse  = NA, nurse_dead = NA) %>%
    rename(hm1 = f1, hm2 = f2, hm3 = f3)
if(filter(november, label == 911) %>% nrow(.) == 0){
temp2 <- mutate(temp, date = "nov")
november <- rbind(november, temp2)
}
#### November change columns
november <- rename(november, hm2 = `hm²`)
november[which(november$label == 1013 & november$plot == 10), "label"] <- rep(1113, 1)
november[which(november$label == 578), "label"] <- rep(579, 1)
temp <- filter(june, label == 803) %>%
    mutate(long_nurse = NA, larg_nurse  = NA, nurse_dead = NA)
if(filter(november, label == 803) %>% nrow(.) == 0){
temp2 <- mutate(temp, date = "nov")
november <- rbind(november, temp2)  
}
#### Merge nurse status and label characteristics 
nurse_status <- select(november, label, nurse_dead) 
if (! "nurse_dead" %in% label_characteristics) {
    label_characteristics <- left_join(label_characteristics, nurse_status)
}
#### Change patch for open in bad patches
##### Who's bad ?
filter(label_characteristics, nurse_dead %in% c("bad_nurse", "very_bad_nurse")) #12 in bad
# 6 very bad
##### Change ?
label_characteristics2 <- mutate(label_characteristics, 
    ms = replace(ms, nurse_dead %in% c("bad_nurse", "very_bad_nurse"), "Open")
    ) %>% select(-nurse_dead)

# Data by holes
try_survival <- try2 %>%
    group_by(label, species, date) %>%
    summarise(
	surv = mean(survival, na.rm = TRUE),
	hm = mean(hm, na.rm = TRUE),
	h = mean(h, na.rm = TRUE),
	d = mean(d, na.rm = TRUE)
	)
holes_data <- inner_join(try_survival, label_characteristics2, by = "label") %>%
    arrange(label)
## Reorder columns and make the naming cleaner
holes_data %<>% 
    ungroup(.) %>%
    select(ter, plot,label,ms, com, watering, species, date, surv, d, h, hm)
    




## save data
#save(holes_data, file = "../processed/holes_data.RData")
