# PAYE tax receipt modelling
# 2025.07.05
# S Golightly

#### load libraries, set up & data import ####
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)

setwd("~/Documents/")
pct_PAYE<- readxl::read_excel("RTI_May_incomes_Ex.xlsx")
source_date <- "May 2025"
source <- paste("ONS RTI Statistics", source_date)
cnt_emp_PAYE <- 30174813

start.time <- Sys.time()

#### set rate/threshold starting values
s1 <- 12570 # tax-free personal allowance
s2 <- 0.2 # basic rate
s3 <- 50270 # higher rate threshold
s4 <- 0.4 # higher rate
s5 <- TRUE # tax-free allowance phases out?
s6 <- 100000 + s5*2*s1 # additional rate (dependent on TFPA & phase-out)
s7 <- 0.45 # additional rate

#### calculate midpoints for pct_PAYE ####
pct_PAYE <- pct_PAYE %>% 
  dplyr::mutate(midpoint = 0.5*12*(Salary + 
    lag(Salary, 1))) %>% # calculate midpoint of band upper/lower
  dplyr::select(Percentile,
                Salary,
                midpoint) %>% 
  na.omit()

# pct_PAYE <- pct_PAYE[1:20,]   # Use to omit 1% highest earner assumption
# pct_PAYE[,"scentxble"] = NA # TBC needed?

#### Function to calculate scenario revenue ####
calc_PAYE_rev_fnctn <- function(scenario, pct_PAYE, rate_code,
                                threshlow, threshhigh, rate) {
  pct_PAYE_scen <- pct_PAYE %>% 
    dplyr::mutate(scen_txble = pmax(0, # no tax if salary below LB
                                    pmin(midpoint,threshhigh)- #only calc up to UB
                                      threshlow)) 
  # last two bands aren't 5% buckets so need adjusting
  pct_PAYE_scen[20, "scen_txble"] <- pct_PAYE_scen[20,"scen_txble"]*0.8
  pct_PAYE_scen[21, "scen_txble"] <- pct_PAYE_scen[21,"scen_txble"]*0.2
  
  # sum the taxable salaries, multiply by 5%, by pop, by rate
  revenue_fn <- sum(pct_PAYE_scen$scen_txble)*0.05*cnt_emp_PAYE*rate
  
  # create table f outputs
  tbl_scen_output <- data.frame(scenario_fn = scenario,
                                rate_code_fn = rate_code,
                                rate_fn = rate,
                                threshlow_fn = threshlow,
                                threshhigh_fn = threshhigh,
                                revenue_fn = revenue_fn)
  return(tbl_scen_output)
  
}

#### run tax calc function for current rates / thresholds ####
scen_no <- 0
# create empty dataframes of predicted length +1 for 4 incomes
# can use single NA row then as check for method
scen_anlys_basi <- data.frame(matrix(NA, nrow = 1e3+2, ncol = 6)) %>% 
  dplyr::rename(scenario = X1,
                rate_code = X2,
                rate = X3,
                threshlow = X4,
                threshhigh = X5,
                revenue = X6)
scen_anlys_high <- data.frame(matrix(NA, nrow = 2e5+2, ncol = 6)) %>% 
  dplyr::rename(scenario = X1,
                rate_code = X2,
                rate = X3,
                threshlow = X4,
                threshhigh = X5,
                revenue = X6)
scen_anlys_phas <- data.frame(matrix(NA, nrow = 20+2, ncol = 6)) %>% 
  dplyr::rename(scenario = X1,
                rate_code = X2,
                rate = X3,
                threshlow = X4,
                threshhigh = X5,
                revenue = X6)
scen_anlys_addi <- data.frame(matrix(NA, nrow = 50+2, ncol = 6)) %>% 
  dplyr::rename(scenario = X1,
                rate_code = X2,
                rate = X3,
                threshlow = X4,
                threshhigh = X5,
                revenue = X6)

scen_anlys_basi[scen_no+1,] <- #enter scenario 0 in first row (0+1)
  calc_PAYE_rev_fnctn(scen_no, pct_PAYE,"b", s1, s3, s2) # basic rate calc
scen_anlys_high[scen_no+1,] <- #enter scenario 0 in first row (0+1)
  calc_PAYE_rev_fnctn(scen_no, pct_PAYE,"h", s3, s6-s5*2*s1, s4) # higher rate calc
scen_anlys_phas[scen_no+1,] <- #enter scenario 0 in first row (0+1)
  calc_PAYE_rev_fnctn(scen_no, pct_PAYE,"p", s6-s5*2*s1, s6, 0.6) # phase out calc
scen_anlys_addi[scen_no+1,] <- #enter scenario 0 in first row (0+1)
  calc_PAYE_rev_fnctn(scen_no, pct_PAYE,"a", s6, 1e7, s7) # additional rate calc

# sum up the revenues for current rates/thresholds across 4 dfs
cur_PAYE_rcpts <- 
  scen_anlys_basi[scen_no+1,6] +
  scen_anlys_high[scen_no+1,6] +
  scen_anlys_phas[scen_no+1,6] +
  scen_anlys_addi[scen_no+1,6] 
  
#### loops for modeling revenues ####
 # basic rates / thresholds
print("modelling basic rate revenues")
start.time <- Sys.time() # set timer running for each loop to monitor efficiency
for(i1 in 1:11) {
  s1 <- 12570 + 1000*(i1-6) # £1,000 increments x10 up/down from current
  for (i2 in 1:11) {
    s2 <- 0.14 + i2/100 # 1% increments from 15% to 24% 
    for (i3 in 1:16) {
      s3 <- 50270 + 1000*(i3-6) # £1,000 increments up/down from current
      scen_no <- scen_no+1
      scen_anlys_basi[scen_no+1,] <- 
        calc_PAYE_rev_fnctn(scen_no, pct_PAYE, "b",
                            s1, s3, s2)
    }
  }
}
Sys.time() - start.time

# higher rates / thresholds
print("modelling higher rate revenues - note runtime may be up to 10 mins")
start.time <- Sys.time() # set timer running for each loop to monitor efficiency
scen_no <- 0
for(i1 in 1:11) { 
  s1 <- 12570 + 1000*(i1-6) # £1,000 increments x10 up/down from current
  for (i3 in 1:11) {
    s3 <- 50270 + 1000*(i3-6) # £1,000 increments up/down from current
    for (i4 in 1:11) {
      s4<- 0.34+i4/100 # 1% increments from 35% to 44%
      for (i5 in 1:2) { 
        s5 <- case_when(i5 == 1 ~ TRUE,
                        i5 == 2 ~ FALSE) # to phase / not phase
        for (i6 in 1:11) {
          s6 <- 125140 + 10000*(i6-6) # £10,000 increments up/down from current
          scen_no <- scen_no + 1
          scen_anlys_high[scen_no+1,] <-
            calc_PAYE_rev_fnctn(scen_no, pct_PAYE, "h",
                                s3, s6-s5*2*s1, s4)
        }
      }
    }
  }
}
Sys.time() - start.time

# to phase out or not to phase out...
print("modelling reduced TFA revenues")
start.time <- Sys.time() # set timer running for each loop to monitor efficiency
scen_no <- 0
for(i1 in 1:11) { 
  s1 <- 12570 + 1000*(i1-6) # £1,000 increments x10 up/down from current
   for (i5 in 1:2) { 
    s5 <- case_when(i5 == 1 ~ TRUE,
                    i5 == 2 ~ FALSE) # to phase / not phase
    for (i6 in 1:11) {
      s6 <- 125140 + 10000*(i6-6) # £10,000 increments up/down from current
      scen_no <- scen_no + 1
      scen_anlys_phas[scen_no+1,] <- 
        calc_PAYE_rev_fnctn(scen_no, pct_PAYE, "p",
                            s6-s5*2*s1, s6, 0.6) # 60% rate combines 40% plus lost allowance
    }
   }
}
Sys.time() - start.time
# additional rates / thresholds
print("modelling additional rate revenues")
start.time <- Sys.time() # set timer running for each loop to monitor efficiency
scen_no <- 0
for (i6 in 1:11) {
  s6 <- 125140 + 10000*(i6-6) # £10,000 increments up/down from current
  for (i7 in 1:11) {
    s7 <- 0.25+0.05*i7 # 10% increments from 30% to 85%
    scen_no <- scen_no + 1
    scen_anlys_addi[scen_no+1,] <- 
      calc_PAYE_rev_fnctn(scen_no, pct_PAYE, "a",
                          s6, 1e7, s7)
  }
}
Sys.time() - start.time

#### interogating output ####
est_marg_rev_fn <- function(rate_b,
                                rate_h,
                                rate_p,
                                rate_a,
                                thresh_b,
                                thresh_h,
                                thresh_p,
                                thresh_a) {
  row_basic <- scen_anlys_basi %>% 
    dplyr::filter(near(threshlow, thresh_b),
                  near(threshhigh, thresh_h),
                  near(rate, rate_b)) %>% 
  slice(1) # account for duplicate of current
  rev_basic <- row_basic[,6]
  row_high <- scen_anlys_high %>% 
    dplyr::filter(near(threshlow,thresh_h),
                  near(threshhigh, thresh_p),
                  near(rate,rate_h)) %>% 
  slice(1) # account for duplicate of current
  rev_high <- row_high[,6]
  row_phas <- scen_anlys_phas %>% 
    dplyr::filter(near(threshlow,thresh_p),
                  near(threshhigh,thresh_a),
                  near(rate,rate_p)) %>% 
  slice(1) # account for duplicate of current
  rev_phas <- row_phas[,6]
  row_addi <- scen_anlys_addi %>% 
    dplyr::filter(near(threshlow, thresh_a),
                  near(threshhigh, 1e7),
                  near(rate, rate_a)) %>% 
  slice(1) # account for duplicate of current
  rev_addi <- row_addi[,6]
  rev_output <- 
    rev_basic + 
    rev_high +
    rev_phas +
    rev_addi -
    cur_PAYE_rcpts
  tbl_rev_output <- data.frame(rev_basic = rev_basic,
                               rev_high = rev_high,
                               rev_phas = rev_phas,
                               rev_addi = rev_addi,
                               rev_output = rev_output)
  return(tbl_rev_output)
}

#### stacked coloumn of current revenues ####
# rates / thresholds to chart
rate_b <- 0.2
rate_h <- 0.4
rate_p <- 0.6
rate_a <- 0.45
thresh_b <- 12570
thresh_h <- 50270
thresh_p <- 100000
thresh_a <- 125140

est_marg_rev_out <- est_marg_rev_fn(rate_b, rate_h, rate_p, rate_a, 
                thresh_b, thresh_h, thresh_p, thresh_a)
# estimate pop in each bucket
pop_b <- (1-pct_PAYE$Percentile[which.min(abs(thresh_b - pct_PAYE$midpoint))])*cnt_emp_PAYE
pop_h <- (1-pct_PAYE$Percentile[which.min(abs(thresh_h - pct_PAYE$midpoint))])*cnt_emp_PAYE
pop_p <- (1-pct_PAYE$Percentile[which.min(abs(thresh_p - pct_PAYE$midpoint))])*cnt_emp_PAYE
pop_a <- (1-pct_PAYE$Percentile[which.min(abs(thresh_a - pct_PAYE$midpoint))])*cnt_emp_PAYE
pop_tot <- pop_b + pop_h + pop_p + pop_a
rev_tot <- sum(est_marg_rev_out[,1:4])
# coordinates for bubbles
rate_desc <- c(rep(c("Basic rate", "Higher rate", "Phase-out of TFA", "Additional rate"),2))
rate_desc_order <- c("Additional rate","Phase-out of TFA", "Higher rate","Basic rate")
stat_desc <- c(rep("Population in band (000s)",4), rep("Tax revenue from band (£bn)",4))
values <- c(pop_b, pop_h, pop_p, pop_a,
            paste(est_marg_rev_out[,1:4]))
values_pct <- c(as.numeric(values[1:4])/pop_tot,
            as.numeric(values[5:8])/rev_tot)
label <- c(format(round(as.numeric(values[1:4])/1e3,0), nsmall = 0, big.mark = ","),
           format(round(as.numeric(values[5:8])/1e9,1), nsmall = 1, big.mark = ","))
df_bc <- data.frame(rate_desc, stat_desc, values_pct, label)

ggplot(df_bc,
       aes(fill = factor(rate_desc, rate_desc_order), 
           y = values_pct, 
           x = stat_desc,
           label = label)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = percent) +
  theme(legend.title = element_blank(),
        axis.title = element_blank())
  # theme(legend.position = "none")

cur_rev_flat <- c(112.2, 57.5, 22.78, 19.08)
cur_rev_200 <- c(112.2, 57.5, 22.78, 19.92)
cur_rev_600 <- c(112.2, 57.5, 22.78, 74.23)
df_cur_rev <- data.frame(rate_desc,cur_rev_flat,cur_rev_200,cur_rev_600) %>% 
  tidyr::pivot_longer(cols = starts_with("cur"),
                      values_to = "values_rev",
                      names_to = "scenario") %>% 
  dplyr::mutate(label = case_when(scenario == "cur_rev_flat"~ "Estimate 1 - flat 1%",
                                  scenario == "cur_rev_200"~ "Estimate 2 - £200k average 1%",
                                  scenario == "cur_rev_600"~ "Estimate 3 - 600k average 1%"))
scen_order <- c("Estimate 1 - flat 1%", "Estimate 2 - £200k average 1%", "Estimate 3 - 600k average 1%")

ggplot(df_cur_rev,
       aes(x = factor(label, scen_order), y = values_rev, 
           colour = factor(rate_desc, rate_desc_order),
           group = rate_desc)) +
  geom_line() +
  scale_x_discrete(labels = (c(
                      "cur_rev_flat" = "Flat 1% assumption", 
                      "cur_rev_200" = "£200k 1% assumption",
                      "cur_rev_600" = "£600k 1% assumption"))) +
  geom_point() +
  scale_y_continuous(limits = c(0, 120),
                     breaks = c(0, 20, 40, 60, 80, 100, 120),
                     labels = scales::label_currency(prefix = "£",suffix = "bn")) +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(y = "Estimated tax revenue")+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank())
  
#### set chart y-limits ####
constantscale <- TRUE
ylimits <- c(-30e9, 30e9)
ybreaks <- c(-30e9, -20e9, -10e9, 0, 10e9, 20e9, 30e9)
#### column chart to show TFA sensitivity ####
df_sens_basic <- est_marg_rev_out %>% 
  dplyr::mutate(thresh_b = 12570)

for (i in 1:11) {
  df_scen_sens <- est_marg_rev_fn(0.20, 0.4, 0.6, 0.45, # current assumptions
                  12570 + 1000*(i-6), # variable
                  50270, 100000, 125140) # current assumptions 
  df_scen_sens <- df_scen_sens %>% 
    dplyr::mutate(thresh_b = 12570 + 1000*(i-6))
  df_sens_basic <- rbind(df_sens_basic, df_scen_sens)
}
df_sens_basic <- df_sens_basic %>% 
  dplyr::arrange(rev_output) %>% 
  dplyr::distinct(.keep_all = TRUE) %>% 
  dplyr::mutate(x_label = thresh_b-12570)

TFA_col <- ggplot(df_sens_basic,
       aes(x = x_label, y = rev_output)) +
  geom_bar(stat = "identity", colour = "#C77CFF", fill = "#C77CFF") +
  scale_y_continuous(labels = unit_format(unit = "bn", scale = 1e-9),
                     limits = case_when(constantscale = TRUE ~ ylimits,
                                        constantscale = FALSE ~ NA),
                     breaks = case_when(constantscale = TRUE ~ ybreaks,
                                        constantscale = FALSE ~ NA)) +
  scale_x_continuous(labels = scales::label_currency(prefix = "£"),
                     limits = c(-5500,5500),
                     breaks = c(-4000, -2000,0,2000,4000)) +
  labs(x = "Modelled change to tax-free personal allowance",
       y = "") +
  theme(plot.background = element_blank(),
        legend.position = "none")

#### column chart to show basic rate sensitivity ####
df_sens_basic_rate <- est_marg_rev_out %>% 
  dplyr::mutate(rate_b = 0.2)

for (i in 1:11) {
  df_scen_sens <- est_marg_rev_fn(0.14 + i/100, 
                                  0.4, 0.6, 0.45, # current assumptions
                                  12570, 50270, 100000, 125140) # current assumptions 
  df_scen_sens <- df_scen_sens %>% 
    dplyr::mutate(rate_b = 0.14 + i/100)
  df_sens_basic_rate <- rbind(df_sens_basic_rate, df_scen_sens)
}
df_sens_basic_rate <- df_sens_basic_rate %>% 
  dplyr::arrange(rev_output) %>% 
  dplyr::distinct(.keep_all = TRUE) %>% 
  dplyr::mutate(x_label = rate_b - 0.2)

basic_rate_col <- ggplot(df_sens_basic_rate,
       aes(x = x_label, y = rev_output)) +
  geom_bar(stat = "identity", colour = "#C77CFF", fill = "#C77CFF") +
  scale_y_continuous(labels = unit_format(unit = "bn", scale = 1e-9),
                     limits = case_when(constantscale = TRUE ~ ylimits,
                                        constantscale = FALSE ~ NA),
                     breaks = case_when(constantscale = TRUE ~ ybreaks,
                                        constantscale = FALSE ~ NA)) +
  scale_x_continuous(labels = scales::label_percent(),
                     limits = c(-0.06,0.06),
                     breaks = c(-0.04, -0.02,0,0.02,0.04)) +
  labs(x = "Modelled change to basic rate of income tax",
       y = "") +
  theme(plot.background = element_blank(),
        legend.position = "none")

#### column chart to show higher rate threshold sensitivity ####
df_sens_high_t <- est_marg_rev_out %>% 
  dplyr::mutate(thresh_h = 50270)

for (i in 1:11) {
  df_scen_sens <- est_marg_rev_fn(0.20, 0.4, 0.6, 0.45, 
                                  12570, # current assumptions
                                  50270 + 1000*(i-6), # variable 
                                  100000, 125140) # current assumptions 
  df_scen_sens <- df_scen_sens %>% 
    dplyr::mutate(thresh_h = 50270 + 1000*(i-6),)
  df_sens_high_t <- rbind(df_sens_high_t, df_scen_sens)
}
df_sens_high_t <- df_sens_high_t %>% 
  dplyr::arrange(rev_output) %>% 
  dplyr::distinct(.keep_all = TRUE) %>% 
  dplyr::mutate(x_label = thresh_h-50270)

high_thresh_col <- ggplot(df_sens_high_t,
                  aes(x = x_label, y = rev_output)) +
  geom_bar(stat = "identity",
           colour = "#00BEC4", fill = "#00BEC4") +
  scale_y_continuous(labels = unit_format(unit = "bn", scale = 1e-9),
                     limits = case_when(constantscale = TRUE ~ ylimits,
                                        constantscale = FALSE ~ NA),
                     breaks = case_when(constantscale = TRUE ~ ybreaks,
                                        constantscale = FALSE ~ NA)) +
  scale_x_continuous(labels = scales::label_currency(prefix = "£"),
                     limits = c(-5500,5500),
                     breaks = c(-4000, -2000,0,2000,4000)) +
  labs(x = "Modelled change to higher rate threshold",
       y = "Change in modelled tax receipts from PAYE")+
  theme(legend.position = "none")

#### column chart to show higher rate sensitivity ####
df_sens_high_rate <- est_marg_rev_out %>% 
  dplyr::mutate(rate_h = 0.4)

for (i in 1:11) {
  df_scen_sens <- est_marg_rev_fn(0.2, 
                                  0.34+i/100, 
                                  0.6, 0.45, # current assumptions
                                  12570, 50270, 100000, 125140) # current assumptions 
  df_scen_sens <- df_scen_sens %>% 
    dplyr::mutate(rate_h = 0.34 + i/100)
  df_sens_high_rate <- rbind(df_sens_high_rate, df_scen_sens)
}
df_sens_high_rate <- df_sens_high_rate %>% 
  dplyr::arrange(rev_output) %>% 
  dplyr::distinct(.keep_all = TRUE) %>% 
  dplyr::mutate(x_label = rate_h - 0.4)

high_rate_col <- ggplot(df_sens_high_rate,
                         aes(x = x_label, y = rev_output)) +
  geom_bar(stat = "identity", 
           colour = "#00BEC4", fill = "#00BEC4") +
  scale_y_continuous(labels = unit_format(unit = "bn", scale = 1e-9),
                     limits = case_when(constantscale = TRUE ~ ylimits,
                                        constantscale = FALSE ~ NA),
                     breaks = case_when(constantscale = TRUE ~ ybreaks,
                                        constantscale = FALSE ~ NA)) +
  scale_x_continuous(labels = scales::label_percent(),
                     limits = c(-0.055,0.055),
                     breaks = c(-0.04, -0.02,0,0.02,0.04)) +
  labs(x = "Modelled change to higher rate of income tax",
       y = "Change in modelled tax receipts from PAYE") +
  theme(legend.position = "none")

#### column chart to show additional rate threshold sensitivity ####
df_sens_addi_t <- est_marg_rev_out %>% 
  dplyr::mutate(thresh_a = 12570)

for (i in 1:11) {
  df_scen_sens <- est_marg_rev_fn(0.20, 0.4, 0.6, 0.45, 
                                  12570, # current assumptions
                                  50270,  
                                  125140 + 10000*(i-6)-2*12570, # variable
                                  125140 + 10000*(i-6)) # current assumptions 
  df_scen_sens <- df_scen_sens %>% 
    dplyr::mutate(thresh_a = 125140 + 10000*(i-6))
  df_sens_addi_t <- rbind(df_sens_addi_t, df_scen_sens)
}
df_sens_addi_t <- df_sens_addi_t %>% 
  dplyr::arrange(rev_output) %>% 
  dplyr::distinct(.keep_all = TRUE) %>% 
  dplyr::mutate(x_label = thresh_a-125140)

addi_thresh_col <- ggplot(df_sens_addi_t,
                          aes(x = x_label, y = rev_output)) +
  geom_bar(stat = "identity", 
           colour = "#F8766D", fill = "#F8766D") +
  scale_y_continuous(labels = unit_format(unit = "bn", scale = 1e-9),
                     limits = case_when(constantscale = TRUE ~ ylimits,
                                        constantscale = FALSE ~ NA),
                     breaks = case_when(constantscale = TRUE ~ ybreaks,
                                        constantscale = FALSE ~ NA)) +
  scale_x_continuous(labels = scales::label_currency(prefix = "£"),
                     limits = c(-55000,55000),
                     breaks = c(-40000, -20000,0,20000,40000)) +
  labs(x = "Modelled change to additional rate threshold",
       # y = "Change in modelled tax receipts from PAYE"
       y = ""
       ) +
  theme(plot.background = element_blank(),
        legend.position = "none")

#### column chart to show additional rate sensitivity ####
df_sens_addi_rate <- est_marg_rev_out %>% 
  dplyr::mutate(rate_a = 0.45)

for (i in 1:11) {
  df_scen_sens <- est_marg_rev_fn(0.2, 0.4, 0.6, 
                                  0.25+0.05*i, # current assumptions
                                  12570, 50270, 100000, 125140) # current assumptions 
  df_scen_sens <- df_scen_sens %>% 
    dplyr::mutate(rate_a = 0.25+0.05*i)
  df_sens_addi_rate <- rbind(df_sens_addi_rate, df_scen_sens)
}
df_sens_addi_rate <- df_sens_addi_rate %>% 
  dplyr::arrange(rev_output) %>% 
  dplyr::distinct(.keep_all = TRUE) %>% 
  dplyr::mutate(x_label = rate_a - 0.45)

constantscale = FALSE

addi_rate_col <- ggplot(df_sens_addi_rate,
                        aes(x = x_label, y = rev_output)) +
  geom_bar(stat = "identity", 
           colour = "#F8766D", fill = "#F8766D") +
  scale_y_continuous(labels = unit_format(unit = "bn", scale = 1e-9),
                     limits = case_when(constantscale = TRUE ~ ylimits,
                                        constantscale = FALSE ~ NA),
                     breaks = case_when(constantscale = TRUE ~ ybreaks,
                                        constantscale = FALSE ~ NA)
                     ) +
  scale_x_continuous(labels = scales::label_percent(),
                     limits = c(-0.2, 0.4),
                     breaks = c( -0.1, 0, 0.1, 0.2, 0.3)) +
  labs(x = "Modelled change to additional rate of income tax",
       # y = "Change in modelled tax receipts from PAYE",
       y = "") +
  theme(plot.background = element_blank(),
        legend.position = "none")

#### Create combined panel ####
layout <- matrix(c(1:6), ncol = 2, byrow = TRUE)
plots <- c(TFA_col, basic_rate_col,
           high_thresh_col, high_rate_col,
           addi_thresh_col, addi_rate_col)



gridExtra::grid.arrange(TFA_col, basic_rate_col,
                        high_thresh_col, high_rate_col,
                        addi_thresh_col, addi_rate_col,
                        layout_matrix = layout,
                        top = "Figure X: Estimated marginal revenue for rate and threshold changes (Flat 1%)")
