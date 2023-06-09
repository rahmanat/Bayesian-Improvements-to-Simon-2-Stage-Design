require(flextable)
require(dplyr)
require(gt)

source("/Users/nataliarahman/Documents/Masters Thesis/functions.R") # Read in functions file

## get stopping criteria for binomial bayes (RB in this file)
# n =28
stopping_RB28 = c()

for (i in 0:28) {
  stopping_RB28 = c(stopping_RB28, which(pbinom(q=i,size = 1:28,prob=0.55) < 0.05)[1])
}

stopping_RB28 = stopping_RB28[!is.na(stopping_RB28)]


######### SIMULATE DATA ###########

#simulate 10,000 alternative (p= 55% Alternative)

alt_data28 = data.simulator(num_sims = 10000, p = 0.55, n = 28)

# Run alt data through each interim monitor


Simon_interim_alt = interim.monitor.fun(data = alt_data28, 
                                        looks = c(10), fut.bound = c(3), 
                                        eff.bound = rep(100), sig.num = 13)

Flemming_interim_alt= interim.monitor.fun(data = alt_data28, 
                                          looks = c(10), fut.bound = c(3), 
                                          eff.bound = c(7), sig.num = 13)

surething_interim_alt = interim.monitor.fun(data = alt_data28, 
                                            looks = 5:27, fut.bound = c(rep(-1, 11), 0:11), 
                                            eff.bound = rep(13, 23), sig.num = 13)
simon_ST_interim_alt = interim.monitor.fun(data = alt_data28, 
                                           looks = 5:27, fut.bound = c(rep(-1, 5), 3, rep(-1,5), 0:11), 
                                           eff.bound = rep(13, 23), sig.num = 13)


BB_interim_alt = interim.monitor.fun(data = alt_data28, 
                                     looks = stopping_RB28, fut.bound = c(0:10), 
                                     eff.bound = c(13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13), sig.num = 13)
BB_min1_interim_alt = interim.monitor.fun(data = alt_data28, 
                                          looks = stopping_RB28[-1], fut.bound = c(0:9), 
                                          eff.bound = c(13, 13, 13, 13, 13, 13, 13, 13, 13, 13), sig.num = 13)
BB_min1_plus1_interim_alt = interim.monitor.fun(data = alt_data28, 
                                                looks = c(8, 10, 13, 15, 17, 20, 22, 24, 26, 27), fut.bound = c(0:9), 
                                                eff.bound = c( 13, 13, 13, 13, 13, 13, 13, 13, 13, 13), sig.num = 13)


#Simualte 10,000 null (p = 30% for null)
null_data28 = data.simulator(num_sims = 10000, p = 0.3, n=28)


# Run alt data through each interim monitor



Simon_interim_null = interim.monitor.fun(data = null_data28, 
                                         looks = c(10), fut.bound = c(3), 
                                         eff.bound = rep(100), sig.num = 13)

Flemming_interim_null= interim.monitor.fun(data = null_data28, 
                                           looks = c(10), fut.bound = c(3), 
                                           eff.bound = c(7), sig.num = 13)

surething_interim_null= interim.monitor.fun(data = null_data28, 
                                            looks = 5:27, fut.bound = c(rep(-1, 11), 0:11), 
                                            eff.bound = rep(13, 23), sig.num = 13)
simon_ST_interim_null = interim.monitor.fun(data = null_data28, 
                                            looks = 5:27, fut.bound = c(rep(-1, 5), 3, rep(-1,5), 0:11), 
                                            eff.bound = rep(13, 23), sig.num = 13)

BB_interim_null = interim.monitor.fun(data = null_data28, 
                                      looks = stopping_RB28, fut.bound = c(0:10), 
                                      eff.bound = c(13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13), sig.num = 13)
BB_min1_interim_null = interim.monitor.fun(data = null_data28, 
                                           looks = stopping_RB28[-1], fut.bound = c(0:9), 
                                           eff.bound = c(13, 13, 13, 13, 13, 13, 13, 13, 13, 13), sig.num = 13)
BB_min1_plus1_interim_null = interim.monitor.fun(data = null_data28, 
                                                 looks = c(8, 10, 13, 15, 17, 20, 22, 24, 26, 27), fut.bound = c(0:9), 
                                                 eff.bound = c( 13, 13, 13, 13, 13, 13, 13, 13, 13, 13), sig.num = 13)





summary_table_alt = rbind(Simon_interim_alt$SummaryOfSims,
                          Flemming_interim_alt$SummaryOfSims, 
                          surething_interim_alt$SummaryOfSims,
                          simon_ST_interim_alt$SummaryOfSims,
                          BB_interim_alt$SummaryOfSims,
                          BB_min1_interim_alt$SummaryOfSims,
                          BB_min1_plus1_interim_alt$SummaryOfSims)

summary_table_null = rbind(Simon_interim_null$SummaryOfSims,
                           Flemming_interim_null$SummaryOfSims, 
                           surething_interim_null$SummaryOfSims,
                           simon_ST_interim_null$SummaryOfSims,
                           BB_interim_null$SummaryOfSims,
                           BB_min1_interim_null$SummaryOfSims,
                           BB_min1_plus1_interim_null$SummaryOfSims)

colnames(summary_table_null) = c("AvgSampSize","Succ","ESF","ESE")

Design = c("Simon 2-Stage","Flemming 2-Stage","Sure Thing","Simon-Sure Thing","Binomial-Bayes","Binomial-Bayes- Exclude 1st","Binomial-Bayes- Exclude 1st & Look 1 Subject Later")

summary_table_flex = cbind.data.frame(Design, summary_table_alt, summary_table_null)


summary_table_flex %>% flextable()%>% theme_box %>% set_header_labels(
  AvgSampleSize = "Avg Sample Size",
  Success = "Success (%)",
  EarlyStop_futility = "Early Stop Futility (%)",
  EarlyStop_efficacy = "Early Stop Efficacy (%)",
  AvgSampSize = "Avg Sample Size",
  Succ = "Success (%)",
  ESF = "Early Stop Futility (%)",
  ESE = "Early Stop Efficacy (%)") %>% 
  add_header_row( values = c("","Alternative","Null"), colwidths = c(1,4,4)) %>% colformat_double(j = c(2,6), digits = 1) %>% colformat_double(j = c(3:5,7:9), digits = 3)

canine_sum_table = summary_table_flex




###### POWER VS TYPE I ERROR #######

canine_sum_table$Design = factor(canine_sum_table$Design, levels = Design)

s_simon = 0
s_st = 1
s_SST = 8
s_bayesE1L1 = 2
s_bb = 3
s_bbE1 = 4
s_bbE1L1 = 5
s_flem = 6

shape = c(s_simon, s_flem,s_st,  s_SST, s_bb,s_bbE1, s_bbE1L1)

canine_sum_table %>% ggplot(aes(x = Succ, y=Success, color = Design)) + 
  geom_point(shape = shape, size = 5) + labs(x = "Type 1 error", y = "Power") +
  guides(color = guide_legend(
    override.aes=list(shape = shape))) + theme_classic()


#### ESS NULL VS ALT #########

canine_sum_table %>% ggplot(aes(x = AvgSampSize, y=AvgSampleSize, color = Design)) + 
  geom_point(shape = shape, size = 5) + labs(x = "Expected Sample Size under Null", y = "Expected Sample Size under Alternative") +
  guides(color = guide_legend(
    override.aes=list(shape = shape))) + theme_classic()





########## CONFUSION MATRICES ############## 

# confusion matrices alternative

PIsimon_conf_alt = confusion.fun(PIsimon_interim_alt[[1]])
CompSimon_conf_alt = confusion.fun(CompSimon_interim_alt[[1]])
surething28_conf_alt = confusion.fun(surething28_interim_alt[[1]])
surething30_conf_alt = confusion.fun(surething30_interim_alt[[1]])
RB28_conf_alt = confusion.fun(RB28_interim_alt[[1]])
RB30_conf_alt = confusion.fun(RB30_interim_alt[[1]])


# confusion matrices null
PIsimon_conf_null = confusion.fun(PIsimon_interim_null[[1]])
CompSimon_conf_null = confusion.fun(CompSimon_interim_null[[1]])
surething28_conf_null = confusion.fun(surething28_interim_null[[1]])
surething30_conf_null = confusion.fun(surething30_interim_null[[1]])
RB28_conf_null = confusion.fun(RB28_interim_null[[1]])
RB30_conf_null = confusion.fun(RB30_interim_null[[1]])




confusion_mat_table = data.frame(Design = c("PI Simon (n=30)","Computer Simon (n=28)", 
                                            "Sure Thing (n=28)","Sure Thing (n=30)",
                                            "Realistic Bayes (n=28)",
                                            "Realistic Bayes (n=30)"),
                                 Interim1Fixed1 = c(PIsimon_conf_alt$ConfusionMatrix[1,1],
                                                    CompSimon_conf_alt$ConfusionMatrix[1,1],
                                                    surething28_conf_alt$ConfusionMatrix[1,1],
                                                    surething30_conf_alt$ConfusionMatrix[1,1],
                                                    RB28_conf_alt$ConfusionMatrix[1,1],
                                                    RB30_conf_alt$ConfusionMatrix[1,1]),
                                 Interim0Fixed1 = c(PIsimon_conf_alt$ConfusionMatrix[2,1],
                                                    CompSimon_conf_alt$ConfusionMatrix[2,1],
                                                    surething28_conf_alt$ConfusionMatrix[2,1],
                                                    surething30_conf_alt$ConfusionMatrix[2,1],
                                                    RB28_conf_alt$ConfusionMatrix[2,1],
                                                    RB30_conf_alt$ConfusionMatrix[2,1]),
                                 Interim1Fixed0 = c(PIsimon_conf_alt$ConfusionMatrix[1,2],
                                                    CompSimon_conf_alt$ConfusionMatrix[1,2],
                                                    surething28_conf_alt$ConfusionMatrix[1,2],
                                                    surething30_conf_alt$ConfusionMatrix[1,2],
                                                    RB28_conf_alt$ConfusionMatrix[1,2],
                                                    RB30_conf_alt$ConfusionMatrix[1,2]),
                                 Interim0Fixed0 = c(PIsimon_conf_alt$ConfusionMatrix[2,2],
                                                    CompSimon_conf_alt$ConfusionMatrix[2,2],
                                                    surething28_conf_alt$ConfusionMatrix[2,2],
                                                    surething30_conf_alt$ConfusionMatrix[2,2],
                                                    RB28_conf_alt$ConfusionMatrix[2,2],
                                                    RB30_conf_alt$ConfusionMatrix[2,2]),
                                 Interim1Fix1n = c(PIsimon_conf_null$ConfusionMatrix[1,1],
                                                   CompSimon_conf_null$ConfusionMatrix[1,1],
                                                   surething28_conf_null$ConfusionMatrix[1,1],
                                                   surething30_conf_null$ConfusionMatrix[1,1],
                                                   RB28_conf_null$ConfusionMatrix[1,1],
                                                   RB30_conf_null$ConfusionMatrix[1,1]),
                                 Interim0Fix1n = c(PIsimon_conf_null$ConfusionMatrix[2,1],
                                                   CompSimon_conf_null$ConfusionMatrix[2,1],
                                                   surething28_conf_null$ConfusionMatrix[2,1],
                                                   surething30_conf_null$ConfusionMatrix[2,1],
                                                   RB28_conf_null$ConfusionMatrix[2,1],
                                                   RB30_conf_null$ConfusionMatrix[2,1]),
                                 Interim1Fix0n = c(PIsimon_conf_null$ConfusionMatrix[1,2],
                                                   CompSimon_conf_null$ConfusionMatrix[1,2],
                                                   surething28_conf_null$ConfusionMatrix[1,2],
                                                   surething30_conf_null$ConfusionMatrix[1,2],
                                                   RB28_conf_null$ConfusionMatrix[1,2],
                                                   RB30_conf_null$ConfusionMatrix[1,2]),
                                 Interim0Fix0n = c(PIsimon_conf_null$ConfusionMatrix[2,2],
                                                   CompSimon_conf_null$ConfusionMatrix[2,2],
                                                   surething28_conf_null$ConfusionMatrix[2,2],
                                                   surething30_conf_null$ConfusionMatrix[2,2],
                                                   RB28_conf_null$ConfusionMatrix[2,2],
                                                   RB30_conf_null$ConfusionMatrix[2,2])
)

confusion_mat_table %>% flextable() %>% theme_box() %>% 
  add_header_row( values = c("","Alternative","Null"), colwidths = c(1,4,4)) %>%
  set_header_labels(Interim1Fix1n = "Interim1Fixeed1",
                    Interim0Fix1n = "Interim0Fixed1",
                    Interim1Fix0n = "Interim1Fixed0",
                    Interim0Fix0n = "Interim0Fixed0") %>% colformat_double(digits = 3)















