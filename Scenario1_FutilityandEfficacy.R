require(flextable)
require(dplyr)
require(ggplot2)
require(ggpubr)
source("/Users/nataliarahman/Documents/Masters Thesis/functions.R") # Read in "function.R" file 

Design_Names = c("Fleming 2 Stage Minimax", 
                 "Sure Thing", "Simon-Sure Thing", 
                 "Bayes 0.1", "Bayes 0.1 Exclude 1st",
                 "Bayes 0.1 Exclude 1st Look 1 Later",
                 "Bayes 0.2","Bayes 0.2 Exclude 1st","Bayes 0.2 Exclude 1st Look 1 Later",
                 "Binomial-Bayes",
                 "Binomial-Bayes Exclude 1st","Binomial-Bayes Exclude 1st Look 1 Later"
)


################ simulate alternative data x10,000 ##########################

alt_data = data.simulator(num_sims = 10000, p = 0.3)

## Run alternative data through study monitoring functions
flemming_interim_alt = interim.monitor.fun(data = alt_data, 
                                           looks = c(16), fut.bound = c(1), 
                                           eff.bound=(5))
surething_interim_alt = interim.monitor.fun(data = alt_data, 
                                            looks = 5:24, fut.bound = c(rep(-1,16), 0:3), 
                                            eff.bound = c(rep(5, 20)))

simon_ST_interim_alt = interim.monitor.fun(data = alt_data, 
                                           looks = 5:24, fut.bound = c(rep(-1,11), 1, rep(-1,4), 0:3), 
                                           eff.bound = c(rep(5, 20)))

bayes0.1_interim_alt = interim.monitor.fun(data = alt_data, 
                                           looks = c(6, 13, 18, 22), fut.bound = c(0, 1, 2, 3), 
                                           eff.bound = c(2,4,5,5))
bayes0.1_minus1st_interim_alt = interim.monitor.fun(data = alt_data, 
                                                    looks = c(13, 18, 22), fut.bound = c(1, 2, 3), 
                                                    eff.bound = c(4,5,5))
bayes0.1_minus1st_plus1_interim_alt = interim.monitor.fun(data = alt_data, 
                                                          looks = c(14, 19, 23), fut.bound = c(1, 2, 3), 
                                                          eff.bound = c(4,5,5))


bayes0.2_interim_alt = interim.monitor.fun(data = alt_data, 
                                           looks = c(5, 10, 16, 21, 24), fut.bound = c(0,1,2,3,4), 
                                           eff.bound = c(2,3,5,5,5))
bayes0.2_minus1st_interim_alt = interim.monitor.fun(data = alt_data, 
                                                    looks = c(10, 16, 21, 24), fut.bound = c(1,2,3,4), 
                                                    eff.bound = c(3,5,5,5))
bayes0.2_minus1st_plus1_interim_alt = interim.monitor.fun(data = alt_data, 
                                                          looks = c(11, 17, 22, 24), fut.bound = c(1,2,3,4), 
                                                          eff.bound = c(4,5,5,5))

BB_interim_alt = interim.monitor.fun(data = alt_data, 
                                     looks = c(9,14,19,24), fut.bound = c(0,1,2,3), 
                                     eff.bound = c(3,4,5,5))
BB_minus1st_interim_alt = interim.monitor.fun(data = alt_data, 
                                              looks = c(14,19,24), fut.bound = c(1,2,3), 
                                              eff.bound = c(4,5,5))
BB_minus1st_plus1_interim_alt = interim.monitor.fun(data = alt_data, 
                                                    looks = c(15,20,24), fut.bound = c(1,2,3), 
                                                    eff.bound = c(4,5,5))

################ simulate null data x10,000 #########################

null_data = data.simulator(num_sims = 10000, p = 0.1)

## Run null data through study monitoring functions
flemming_interim_null = interim.monitor.fun(data = null_data, 
                                            looks = c(16), fut.bound = c(1), 
                                            eff.bound=(5))
surething_interim_null = interim.monitor.fun(data = null_data, 
                                             looks = 5:24, fut.bound = c(rep(-1,16), 0:3), 
                                             eff.bound = c(rep(5, 20)))

simon_ST_interim_null = interim.monitor.fun(data = null_data, 
                                            looks = 5:24, fut.bound = c(rep(-1,11), 1, rep(-1,4), 0:3), 
                                            eff.bound = c(rep(5, 20)))

bayes0.1_interim_null = interim.monitor.fun(data = null_data, 
                                            looks = c(6, 13, 18, 22), fut.bound = c(0, 1, 2, 3), 
                                            eff.bound = c(2,4,5,5))
bayes0.1_minus1st_interim_null= interim.monitor.fun(data = null_data, 
                                                    looks = c(13, 18, 22), fut.bound = c(1, 2, 3), 
                                                    eff.bound = c(4,5,5))
bayes0.1_minus1st_plus1_interim_null= interim.monitor.fun(data = null_data, 
                                                          looks = c(14, 19, 23), fut.bound = c(1, 2, 3), 
                                                          eff.bound = c(4,5,5))


bayes0.2_interim_null = interim.monitor.fun(data = null_data, 
                                            looks = c(5, 10, 16, 21, 24), fut.bound = c(0,1,2,3,4), 
                                            eff.bound = c(2,3,5,5,5))
bayes0.2_minus1st_interim_null = interim.monitor.fun(data = null_data, 
                                                     looks = c(10, 16, 21, 24), fut.bound = c(1,2,3,4), 
                                                     eff.bound = c(3,5,5,5))
bayes0.2_minus1st_plus1_interim_null = interim.monitor.fun(data = null_data, 
                                                           looks = c(11, 17, 22, 24), fut.bound = c(1,2,3,4), 
                                                           eff.bound = c(4,5,5,5))

BB_interim_null = interim.monitor.fun(data = null_data, 
                                      looks = c(9,14,19,24), fut.bound = c(0,1,2,3), 
                                      eff.bound = c(3,4,5,5))
BB_minus1st_interim_null = interim.monitor.fun(data = null_data, 
                                               looks = c(14,19,24), fut.bound = c(1,2,3), 
                                               eff.bound = c(4,5,5))
BB_minus1st_plus1_interim_null = interim.monitor.fun(data = null_data, 
                                                     looks = c(15,20,24), fut.bound = c(1,2,3), 
                                                     eff.bound = c(4,5,5))


############ OVERALL SUMMARY TABLE ###############

summary_table_alt = rbind(flemming_interim_alt$SummaryOfSims,
                          surething_interim_alt$SummaryOfSims, 
                          simon_ST_interim_alt$SummaryOfSims,
                          bayes0.1_interim_alt$SummaryOfSims,
                          bayes0.1_minus1st_interim_alt$SummaryOfSims,
                          bayes0.1_minus1st_plus1_interim_alt$SummaryOfSims,
                          bayes0.2_interim_alt$SummaryOfSims,
                          bayes0.2_minus1st_interim_alt$SummaryOfSims,
                          bayes0.2_minus1st_plus1_interim_alt$SummaryOfSims,
                          BB_interim_alt$SummaryOfSims,
                          BB_minus1st_interim_alt$SummaryOfSims,
                          BB_minus1st_plus1_interim_alt$SummaryOfSims)

summary_table_null = rbind(flemming_interim_null$SummaryOfSims,
                           surething_interim_null$SummaryOfSims, 
                           simon_ST_interim_null$SummaryOfSims,
                           bayes0.1_interim_null$SummaryOfSims,
                           bayes0.1_minus1st_interim_null$SummaryOfSims,
                           bayes0.1_minus1st_plus1_interim_null$SummaryOfSims,
                           bayes0.2_interim_null$SummaryOfSims,
                           bayes0.2_minus1st_interim_null$SummaryOfSims,
                           bayes0.2_minus1st_plus1_interim_null$SummaryOfSims,
                           BB_interim_null$SummaryOfSims,
                           BB_minus1st_interim_null$SummaryOfSims,
                           BB_minus1st_plus1_interim_null$SummaryOfSims)

colnames(summary_table_null) = c("AvgSampSize","Succ","ESF","ESE")

Design = Design_Names

summary_table_flex = cbind.data.frame(Design, summary_table_alt, summary_table_null)

summary_table_flex$Success = summary_table_flex$Success*100
summary_table_flex$EarlyStop_futility = summary_table_flex$EarlyStop_futility*100
summary_table_flex$EarlyStop_efficacy = summary_table_flex$EarlyStop_efficacy*100
summary_table_flex$Succ = summary_table_flex$Succ*100
summary_table_flex$ESF = summary_table_flex$ESF*100
summary_table_flex$ESE = summary_table_flex$ESE*100



summary_table_flex %>% flextable()%>% theme_box %>% set_header_labels(
  AvgSampleSize = "Avg Sample Size",
  Success = "Success (%)",
  EarlyStop_futility = "Early Stop Futility (%)",
  EarlyStop_efficacy = "Early Stop Efficacy (%)",
  AvgSampSize = "Avg Sample Size",
  Succ = "Success (%)",
  ESF = "Early Stop Futility (%)",
  ESE = "Early Stop Efficacy (%)") %>% 
  add_header_row( values = c("","Alternative","Null"), colwidths = c(1,4,4)) %>% 
  colformat_double(j = c(2,6), digits = 1) %>% colformat_double(j = c(3:5,7:9), digits = 1)

scenario1_futeff = summary_table_flex


##### For optimzed designs found in paper

scenario1_futeff$Design = factor(scenario1_futeff$Design, levels = Design_Names)

# Specify shapes to keep consistent across results (in paper)
s_simon = 0
s_st = 1
s_SST = 8
s_bayesE1L1 = 2
s_bb = 3
s_bbE1 = 4
s_bbE1L1 = 5
s_flem = 6

shape = c(s_flem, s_st, s_SST, s_bayesE1L1, s_bb, s_bbE1, s_bbE1L1)

# If you would like to reproduce plots seen in Supplemental material, do shape = 0:(length(Design_names)-1)
# And do not subset data below ... 

scenario1_futeff_for_plot = scenario1_futeff[c(1:3,6,10:12),]

########### POWER VS TYPE I ERROR ###########

scenario1_futeff_for_plot %>% ggplot(aes(x = Succ, y=Success, color = Design)) + 
  geom_point(shape = shape, size = 5) + labs(x = "Type 1 Error", y = "Power", title = "Scenario 1: Futility & Efficacy") +
  guides(color = guide_legend(
    override.aes=list(shape = shape))) + theme_classic() + 
  theme(plot.title =  element_text(face = "bold", size = 14, hjust = 0.5)) + xlim(9.5,12) +ylim(85,95)


########## ESS NULL VS ALT ###############

scenario1_futeff_for_plot %>% ggplot(aes(x = AvgSampSize, y=AvgSampleSize, color = Design)) + 
  geom_point(shape = shape, size = 5) + labs(x = "Expected Sample Size under Null", y = "Expected Sample Size under Alternative", title = "Scenario 2: Futility & Efficacy") +
  guides(color = guide_legend(
    override.aes=list(shape = shape))) + theme_classic() + 
  theme(plot.title =  element_text(face = "bold", size = 14, hjust = 0.5)) + 
  scale_y_continuous(breaks=seq(12,23,1))+ 
  scale_x_continuous(breaks=seq(13,25,1))





