###################

###################

# Load packages
library(zoo)
library(lubridate)
library(tidyverse)
library(stringr)
library(testthat)
library(survival)
library(lme4)

# TODO:
#- Put bad nurses as open sites 
#- Compute indices by pairs of holes  
#- Effect of nurse traits on facilitation  
#- Effect of functional traits growth and survival  
#- Effect of functional traits growth and survival  


##########
#  DATA  #
##########

# Load data# {{{


holes_summary <- holes_data %>%
    gather(key = variable, value = value, surv, hm, h, d) %>%
    group_by(ms, com, species, watering, date, variable) %>%
    summarise(
	mean = mean(value, na.rm = TRUE),
	ic_width = 1.96*sd(value, na.rm = TRUE)/sqrt(length(value)),
	n= length(value)
	)

# Data by plots
plot_data <- holes_data %>%
    group_by(date, plot, com, species, ms, watering) %>%
    summarise(
	surv = mean(surv, na.rm = TRUE),
	hm = mean(hm, na.rm = TRUE),
	h = mean(h, na.rm = TRUE),
	d = mean(d, na.rm = TRUE),
	n = n()
       	)
plot_summary <- plot_data %>%
    gather(key = variable, value = value, surv, hm, h, d) %>%
    group_by(ms, com, watering, species, date, variable) %>%
    summarise(
	mean = mean(value, na.rm = TRUE),
	ic_width = 1.96*sd(value, na.rm = TRUE)/sqrt(length(value)),
	n= n()
	)
ter_data <- holes_data %>%
    group_by(date, ter, com, species, watering, ms) %>%
    summarise(
	surv = mean(surv, na.rm = TRUE),
	hm = mean(hm, na.rm = TRUE),
	h = mean(h, na.rm = TRUE),
	d = mean(d, na.rm = TRUE),
	n = n()
       	)
ter_summary <- ter_data %>%
    gather(key = variable, value = value, surv, hm, h, d) %>%
    group_by(ms, com, species, watering, date, variable) %>%
    summarise(
	mean = mean(value, na.rm = TRUE),
	ic_width = 1.96*sd(value, na.rm = TRUE)/sqrt(length(value)),
	n= length(value)
	)# }}}

# Graph# {{{

## Survival
p <- ggplot(filter(holes_summary, variable == "surv"), 
    aes(x = date, y = mean, colour = ms, linetype = watering)) + 
#     scale_colour_grey() +
    geom_line() + geom_point() +
    geom_errorbar(aes(ymin = mean - ic_width, ymax = mean + ic_width), width = 0.025)
p + facet_grid(com ~ species)

## d
p <- ggplot(filter(holes_summary, variable == "d"), 
    aes(x = date, y = mean, colour = ms, linetype = watering)) + 
    geom_line() + geom_point() +
    geom_errorbar(aes(ymin = mean - ic_width, ymax = mean + ic_width), width = 0.025)
p + facet_grid(com ~ species)

## h
p <- ggplot(filter(holes_summary, variable == "h"), 
    aes(x = date, y = mean, colour = ms, linetype = watering)) + 
    geom_line() + geom_point() +
    geom_errorbar(aes(ymin = mean - ic_width, ymax = mean + ic_width), width = 0.025)
p + facet_grid(com ~ species)

## hm
p <- ggplot(filter(holes_summary, variable == "hm"),  
    aes(x = date, y = mean, colour = ms, linetype = watering)) + 
    geom_line() + geom_point() +
    geom_errorbar(aes(ymin = mean - ic_width, ymax = mean + ic_width), width = 0.025)
p + facet_grid(watering ~ com)
# }}}
# Test# {{{

## Probability of survival depending of your diameter
prob_surv11 <- filter(holes_data, date == "nov. 2016") %>%
    select(surv)
trait03 <- filter(holes_data, date == "mars 2016") %>%
    select(d, h, hm)
surv_d <- inner_join(prob_surv11, trait03) %>%
    inner_join(., label_characteristics)
p <- ggplot(surv_d, aes(x = h, y = surv)) + 
    geom_point()
p + facet_grid(species ~ com)


# Statistics 
#vignette("adjcurve")
test <- survfit(Surv(month(try2$date), survival) ~ com + ms + watering, try2)
plot(test)
str(test)# }}}

# Growth data# {{{
## Diameter growth
diam_growth <- spread(select(holes_data, -hm, -h, -surv), key = date, d) %>%
    mutate(
	mar = `mars 2016` - `janv. 2016`,
	jun = `juin 2016` - `mars 2016`,
	nov = `nov. 2016` - `juin 2016`
	) %>%
    select(-`mars 2016`, -`janv. 2016`, -`juin 2016`, -`nov. 2016`) %>%
    rename(`mars 2016` = mar, `juin 2016` = jun, `nov. 2016` = nov) %>%
    gather(key = date, value = growth, `mars 2016`, `juin 2016`, `nov. 2016`) %>%
    mutate(date = as.yearmon(date))

p <- ggplot(diam_growth, 
    aes(x = as.factor(date), y = growth)) + 
    geom_boxplot(aes( linetype = watering, colour = ms)) 
p + facet_grid(ter ~ com)

diam_growth_summary <- group_by(diam_growth, ms, com, watering , date) %>% 
    summarise(
	mean = mean(growth, na.rm = TRUE),
	ic_width = 1.96*sd(growth, na.rm = TRUE)/sqrt(length(growth))
	)

p <- ggplot(diam_growth_summary, 
    aes(x = date, y = mean, colour = ms, linetype = watering)) + 
    geom_line() + geom_point() +
    geom_errorbar(aes(ymin = mean - ic_width, ymax = mean + ic_width), width = 0.025)
p + facet_grid(. ~ com)

## Diameter growth
diam_growth2 <- spread(select(holes_data, -hm, -h, -surv), key = date, d) %>%
    mutate(
	mar = `mars 2016` - `janv. 2016`,
	jun = `juin 2016` - `janv. 2016`,
	nov = `nov. 2016` - `janv. 2016`
	) %>%
    select(-`mars 2016`, -`janv. 2016`, -`juin 2016`, -`nov. 2016`) %>%
    rename(`mars 2016` = mar, `juin 2016` = jun, `nov. 2016` = nov) %>%
    gather(key = date, value = growth, `mars 2016`, `juin 2016`, `nov. 2016`) %>%
    mutate(date = as.yearmon(date))

p <- ggplot(diam_growth, 
    aes(x = as.factor(date), y = growth)) + 
    geom_boxplot(aes( linetype = watering, colour = ms)) 
p + facet_grid(ter ~ com)# }}}

## ANOVA

summary(aov(growth ~ date*com + date*ms + date*watering + plot, diam_growth))

#########################
#  Interaction indices  #
#########################

n_int_c <- function(open, patch) {
    # Index works only with non-negative data  
    stopifnot(min(open, na.rm = TRUE) >= 0, min(patch, na.rm = TRUE) >= 0)
    p_sum <- open + patch
    delta_p <- patch - open
    res <- 2*delta_p/(p_sum + abs(delta_p))
    return(res)
}
n_int_a <- function(open, patch) {
    # Index works only with non-negative data  
    stopifnot(min(open, na.rm = TRUE) >= 0, min(patch, na.rm = TRUE) >= 0)
    p_sum <- open + patch
    delta_p <- patch - open
    res <- 2*delta_p/(open + abs(delta_p))
    return(res)
}

## by plot
plot_data_n_index <- plot_data %>%
    gather(key = variable, value = value, hm, h, d) %>%
    select(-surv, -n) %>%
    spread(ms, value) %>%
    mutate(
	n_int_c = map2_dbl(Open, Patch, n_int_c),
	n_int_a = map2_dbl(Open, Patch, n_int_a),
	n = n()) %>%
    select(-Open, -Patch) 

p <- ggplot(filter(plot_data_n_index), 
    aes(x = as.factor(date), y = n_int_a)) + 
    geom_boxplot(aes(linetype = watering, colour = species)) +
    geom_text(
	aes(y = -0.5, label = paste("n = ", n, sep = "")),
       	position = position_dodge(0.6)) +
    ylab("NintA") +
    geom_hline(yintercept = 0, colour = "red")
p + facet_grid(variable ~ com)

p <- ggplot(plot_data_n_index, 
    aes(x = as.factor(date), y = n_int_c)) + 
    ylab("NintC") +
    geom_boxplot(aes(linetype = watering, colour = species)) +
    geom_text(
	aes(y = -0.5, label = paste("n = ", n, sep = "")),
       	position = position_dodge(0.6)) +
    geom_hline(yintercept = 0, colour = "red")
p + facet_grid(variable ~ com)

## by terrace
ter_data_n_index <- ter_data %>%
    gather(key = variable, value = value, hm, h, d) %>%
    select(-surv, -n) %>%
    spread(ms, value) %>%
    ungroup() %>%
    mutate(
	n_int_c = map2_dbl(Open, Patch, n_int_c),
	n_int_a = map2_dbl(Open, Patch, n_int_a),
	n = n()) %>%
    select(-Open, -Patch) 

p <- ggplot(ter_data_n_index, 
    aes(x = as.factor(date), y = n_int_a)) + 
    #geom_point() +
    geom_jitter(aes(colour = species, shape = watering)) + #, size = 0.2
    ylab("NintA") +
    geom_hline(yintercept = 0, colour = "red")
p + facet_grid(variable ~ com)


##########################################
#  Proper analysis with normalized data  #
##########################################

################
#  Check data  #
################

filter(holes_data, com == "Poly", surv == 0, date != "nov. 2016")

# Mortality in polyculture
june_poly_mortality <- nrow(filter(holes_data, com == "Poly", surv == 0, date == "juin 2016"))/
    nrow(filter(holes_data, com == "Poly", date == "juin 2016"))
 
#TODO: plot the n!

n_data_date <- holes_data %>%
    group_by(date, species, ms, com, watering) %>%
    filter(surv != 0) %>%
    summarise(n_count = n())

g <- ggplot(n_data_date, 
    aes(y = n_count, x = date, fill = species, colour = ms, alpha = com)) +  
    geom_bar(stat="identity", position = "dodge")
g + facet_grid(. ~ watering)

####################
#  Normalize data  #
####################

#Baseline data
rp_pattern <- c(
    "Anthyllis" = "anthyllis",
    "Pistacia" = "pistachia",
    "Dorycnium" = "dorycnium") 
data_0 <- read_csv2("../data/biomass_t0.csv") %>%
    select(species, ind, height, dia1, dia2) %>%
    mutate(
	species = str_replace_all(species, rp_pattern),
	d = rowMeans(cbind(dia1, dia2), na.rm = TRUE)) %>%
    select(-dia1, -dia2)
##Average by species
summary_sp_0 <- data_0 %>%
    gather(key = variable, value = value, height, d) %>%
    mutate(variable = str_replace_all(variable, c("height" = "h0",  "d" = "d0"))) %>%
    group_by(species, variable) %>%
    summarise(value = mean(value, na.rm = TRUE)) %>%
    spread(variable, value)
summary_sp_0

## Calculate growth data# {{{
expe_duration <- function(date, starting = as.yearmon("nov. 2015")) {
    time_span <- interval(starting, date) / duration(1, units = "weeks")
    res  <- time_span
    as.integer(res)
}
holes_data2 <- inner_join(holes_data, summary_sp_0) %>%
    mutate(ln_d = log(d) - log(d0),
	ln_h = log(h) - log(h0), 
	ln_hm = log(hm) - log(h0),
	duration = expe_duration(date),
	rgr_d = ln_d/duration,
	rgr_h = ln_h/duration,
	rgr_hm = ln_hm/duration
	)

rgr_data <- select(holes_data2, -ln_d, -ln_h, -ln_hm, -duration, -d0, -h0, -hm , -h, -d) %>%
    ungroup() %>%
    mutate(date_m = month(date),
	duration_m = date_m + 2)
rgr_data2 <- filter(rgr_data, date != "nov. 2016", surv != 0) %>%
    ungroup() %>%
    mutate(date_m = month(date),
	duration_m = date_m + 2)
## save data
#save(rgr_data, file = "../data/rgr_data.RData")
#save(rgr_data2, file = "../data/rgr_data2.RData")


p <- ggplot(rgr_data2, 
    aes(x = as.factor(date), y = rgr_d)) + 
    geom_boxplot(aes(linetype = watering, colour = ms)) +
    ylab("RGR of basal diameter") 
p + facet_grid(com ~ species)

p <- ggplot(rgr_data2, 
aes(x = as.factor(date), y = rgr_h)) + 
    geom_boxplot(aes(linetype = watering, colour = ms)) +
    ylab("RGR of basal diameter")
p + facet_grid(com ~ species)
# p + geom_text(stat = "count", aes(label = ..count.., position = "dodge", y = 0))# }}}

## Summary growth (mean + ic)
rgr_summary <- gather(rgr_data2, key = "variable", value = "value", rgr_d,
    rgr_h, rgr_hm) %>%
    group_by(date, species, com, ms, watering, variable) %>%
    summarise(
	mean = mean(value, na.rm = TRUE),
	ic_width = 1.96*sd(value, na.rm = TRUE)/sqrt(n())
      	)

p <- ggplot(filter(rgr_summary, variable == "rgr_d"), 
    aes(x = date, y = mean, colour = ms, linetype = watering)) + 
    geom_line() + geom_point() +
    geom_errorbar(aes(ymin = mean - ic_width, ymax = mean + ic_width), width = 0.025)
p + facet_grid(com ~ species)

## Summary total# {{{
rgr_data2_summary <- gather(rgr_data2, key = "variable", value = "value", rgr_d,
    rgr_h, rgr_h) %>%
    group_by(date, species, com, ms, watering, variable) %>%
    ## Attention value + 1 pour ne plus avoir de valeur négatives
    summarise(mean = mean(value +1, na.rm = TRUE)) %>%
    spread(ms, mean) %>%
    ungroup() %>%
    mutate(
	n_int_c = map2_dbl(Open, Patch, n_int_c),
	n_int_a = map2_dbl(Open, Patch, n_int_a),
	n = n()) %>%
    select(-Open, -Patch)

g <- ggplot(filter(rgr_data2_summary, variable == "rgr_d"),
    aes(x = date, y = n_int_c, colour = watering)) + 
    geom_point() +
    ylim(-0.05, 0.05)
g + facet_grid(com ~ species)

g <- ggplot(filter(rgr_data2_summary, variable == "rgr_h"),
    aes(x = date, y = n_int_c, colour = watering)) + 
    geom_point() +
    ylim(-0.05, 0.05)
g + facet_grid(com ~ species)# }}}

## Summary plot# {{{
rgr_data2_summary_plot <- gather(rgr_data2, key = "variable", value = "value", rgr_d,
    rgr_h, rgr_h) %>%
    group_by(date, plot, species, com, ms, watering, variable) %>%
    ## Attention value + 1 pour ne plus avoir de valeur négatives
    summarise(mean = mean(value +1, na.rm = TRUE)) %>%
    spread(ms, mean) %>%
    ungroup() %>%
    mutate(
	n_int_c = map2_dbl(Open, Patch, n_int_c),
	n_int_a = map2_dbl(Open, Patch, n_int_a),
	n = n()) %>%
    select(-Open, -Patch)

g <- ggplot(filter(rgr_data2_summary_plot, variable == "rgr_d"),
    aes(x = date, y = n_int_c, colour = watering)) + 
    geom_point()
g + facet_grid(com ~ species)# }}}

## Statistical modeling

### non linear mixed effect models
ml0 <- nlmer(rgr_d ~ SSlogis(date_m,
	     Asym, xmid, scal) ~ Asym | species, rgr_data2, start = c(Asym = 0.1,
	     xmid = 0.02, scal = 2))

### Linear mixed effect models 
ml0 <- lmer(rgr_d ~ date_m + I(date_m^2) + (date_m|ter), rgr_data2)
summary(ml0)
plot(ml0)

ml1 <- lmer(rgr_d ~ I(date_m^2) + (date_m|ter), rgr_data2)
summary(ml1)
plot(ml1)

ml2 <- lmer(rgr_d ~ date_m + I(date_m^2) + species  + com + ms + watering + (date_m|ter), rgr_data2)
summary(ml2)
plot(ml2)
anova(ml1,ml2, ml0)


## Compute interaction index
