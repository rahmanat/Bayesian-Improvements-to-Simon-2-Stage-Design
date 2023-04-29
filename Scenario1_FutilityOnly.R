require(flextable)
require(dplyr)
require(ggplot2)
require(ggpubr)

source("/Users/nataliarahman/Documents/Masters Thesis/functions.R") # Read in "function.R" file 

Design_Names = c("Simon 2 Stage Minimax", "Simon 2 Stage Optimal","Sure Thing", 
                 "Bayes 0.1", "Bayes 0.1 Exclude 1st",
                 "Bayes 0.1 Exclude 1st Look 1 Later",
                 "Bayes 0.2","Bayes 0.2 Minus 1st","Bayes 0.2 Exclude 1st Look 1 Later",
                 "Binomial-Bayes",
                 "Binomial-Bayes Exclude 1st","Binomial-Bayes Exclude 1st Look 1 Later"
)


############ Simulate alternative data x 10,000 ################

alt_data = data.simulator(num_sims = 10000, p = 0.3)
alt_data35 = data.simulator(num_sims = 10000, p = 0.3, n=35)

## Run alternative data through study monitoring functions

  # Simon Minimax
simon_interim_alt = interim.monitor.fun(data = alt_data, 
                                        looks = c(16), fut.bound = c(1), 
                                        eff.bound=(100))

  # Simon Optimal (requires n = 35 max sample)
simonOP_interim_alt = interim.monitor.fun(data = alt_data35, 
                                          looks = c(12), fut.bound = c(1), 
                                          eff.bound=(100), sig.num = 6)

  # Sure Thing
surething_interim_alt = interim.monitor.fun(data = alt_data, 
                                            looks = 21:24, fut.bound = c(0:3), 
                                            eff.bound = rep(100, 4))

  # Bayes 0.1 followed by fine tuning
bayes0.1_interim_alt = interim.monitor.fun(data = alt_data, 
                                           looks = c(6, 13, 18, 22), fut.bound = c(0, 1, 2, 3), 
                                           eff.bound = rep(100, 4))
bayes0.1_minus1st_interim_alt = interim.monitor.fun(data = alt_data, 
                                                    looks = c(13, 18, 22), fut.bound = c(1, 2, 3), 
                                                    eff.bound = rep(100, 3))
bayes0.1_minus1st_plus1_interim_alt = interim.monitor.fun(data = alt_data, 
                                                          looks = c(14, 19, 23), fut.bound = c(1, 2, 3), 
                                                          eff.bound = rep(100, 3))

  # Bayes 0.2 followed by fine-tuning 
bayes0.2_interim_alt = interim.monitor.fun(data = alt_data, 
                                           looks = c(5, 10, 16, 21, 24), fut.bound = c(0,1,2,3,4), 
                                           eff.bound = rep(100,5))
bayes0.2_minus1st_interim_alt = interim.monitor.fun(data = alt_data, 
                                                    looks = c(10, 16, 21, 24), fut.bound = c(1,2,3,4), 
                                                    eff.bound = rep(100,4))
bayes0.2_minus1st_plus1_interim_alt = interim.monitor.fun(data = alt_data, 
                                                          looks = c(11, 17, 22, 24), fut.bound = c(1,2,3,4), 
                                                          eff.bound = rep(100,4))

  # Binomial bayes followed by fine tuning
BB_interim_alt = interim.monitor.fun(data = alt_data, 
                                     looks = c(9,14,19,24), fut.bound = c(0,1,2,3), 
                                     eff.bound = rep(100,4))
BB_minus1st_interim_alt = interim.monitor.fun(data = alt_data, 
                                              looks = c(14,19,24), fut.bound = c(1,2,3), 
                                              eff.bound = rep(100,3))
BB_minus1st_plus1_interim_alt = interim.monitor.fun(data = alt_data, 
                                                    looks = c(15,20,24), fut.bound = c(1,2,3), 
                                                    eff.bound = rep(100,3))



############ Simulate null data x10,000 #################

null_data = data.simulator(num_sims = 10000, p = 0.1)
null_data35 = data.simulator(num_sims = 10000, p = 0.1, n=35)

## Run alternative data through study monitoring functions
  
  # Simon Minimax
simon_interim_null = interim.monitor.fun(data = null_data, 
                                         looks = c(16), fut.bound = c(1), 
                                         eff.bound=(25))
  # Simon Optimal
simonOP_interim_null = interim.monitor.fun(data = null_data35, 
                                           looks = c(12), fut.bound = c(1), 
                                           eff.bound=(100), sig.num = 6)
  # Sure Thing
surething_interim_null = interim.monitor.fun(data = null_data, 
                                             looks = 21:24, fut.bound = c(0:3), 
                                             eff.bound = rep(100, 4))
  # Bayes 0.1 followed by fine tuning
bayes0.1_interim_null = interim.monitor.fun(data = null_data, 
                                            looks = c(6, 13, 18, 22), fut.bound = c(0, 1, 2, 3), 
                                            eff.bound = rep(100, 4))
bayes0.1_minus1st_interim_null = interim.monitor.fun(data = null_data, 
                                                     looks = c(13, 18, 22), fut.bound = c(1, 2, 3), 
                                                     eff.bound = rep(100, 3))
bayes0.1_minus1st_plus1_interim_null = interim.monitor.fun(data = null_data, 
                                                           looks = c(14, 19, 23), fut.bound = c(1, 2, 3), 
                                                           eff.bound = rep(100, 3))

  # Bayes 0.2 followed by fine tuning
bayes0.2_interim_null = interim.monitor.fun(data = null_data, 
                                            looks = c(5, 10, 16, 21, 24), fut.bound = c(0,1,2,3,4), 
                                            eff.bound = rep(100,5))
bayes0.2_minus1st_interim_null = interim.monitor.fun(data = null_data, 
                                                     looks = c(10, 16, 21, 24), fut.bound = c(1,2,3,4), 
                                                     eff.bound = rep(100,4))
bayes0.2_minus1st_plus1_interim_null = interim.monitor.fun(data = null_data, 
                                                           looks = c(11, 17, 22, 24), fut.bound = c(1,2,3,4), 
                                                           eff.bound = rep(100,4))
  # Binomial Bayes followed by fine tuning
BB_interim_null = interim.monitor.fun(data = null_data, 
                                      looks = c(9,14,19,24), fut.bound = c(0,1,2,3), 
                                      eff.bound = rep(100,4))
BB_minus1st_interim_null = interim.monitor.fun(data = null_data, 
                                               looks = c(14,19,24), fut.bound = c(1,2,3), 
                                               eff.bound = rep(100,3))
BB_minus1st_plus1_interim_null = interim.monitor.fun(data = null_data, 
                                                     looks = c(15,20,24), fut.bound = c(1,2,3), 
                                                     eff.bound = rep(100,3))






################## Make overall summary table ####################


summary_table_alt = rbind(simon_interim_alt$SummaryOfSims,
                          simonOP_interim_alt$SummaryOfSims,
                          surething_interim_alt$SummaryOfSims, 
                          bayes0.1_interim_alt$SummaryOfSims,
                          bayes0.1_minus1st_interim_alt$SummaryOfSims,
                          bayes0.1_minus1st_plus1_interim_alt$SummaryOfSims,
                          bayes0.2_interim_alt$SummaryOfSims,
                          bayes0.2_minus1st_interim_alt$SummaryOfSims,
                          bayes0.2_minus1st_plus1_interim_alt$SummaryOfSims,
                          BB_interim_alt$SummaryOfSims,
                          BB_minus1st_interim_alt$SummaryOfSims,
                          BB_minus1st_plus1_interim_alt$SummaryOfSims)

summary_table_null = rbind(simon_interim_null$SummaryOfSims,
                           simonOP_interim_null$SummaryOfSims,
                           surething_interim_null$SummaryOfSims, 
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

scenario1_futonly = summary_table_flex




########### Plot the power vs type 1 error ################

scenario1_futonly$Design = factor(scenario1_futonly$Design, levels = Design_Names)

# Specify shapes to keep consistent across results (in paper)

s_simon = 0
s_st = 1
s_SST = 8
s_bayesE1L1 = 2
s_bb = 3
s_bbE1 = 4
s_bbE1L1 = 5
s_flem = 6

shape = c(s_simon, s_st, s_bayesE1L1, s_bb, s_bbE1, s_bbE1L1)
# If you would like to reproduce plots seen in Supplemental material, do shape = 0:(length(Design_names)-1)
# And do not subset data below ... 

scenario1_futonly_for_plot = scenario1_futonly[scenario1_futonly$Design == "Simon 2 Stage Minimax" | 
                                                 scenario1_futonly$Design == "Sure Thing" |
                                                 scenario1_futonly$Design == "Bayes 0.1 Exclude 1st Look 1 Later" | 
                                                 scenario1_futonly$Design == "Binomial-Bayes" | 
                                                 scenario1_futonly$Design == "Binomial-Bayes Exclude 1st" |  
                                                 scenario1_futonly$Design == "Binomial-Bayes Exclude 1st Look 1 Later",]

scenario1_futonly_for_plot %>% ggplot(aes(x = Succ, y=Success, color = Design)) + 
  geom_point(shape = shape, size = 5) + labs(x = "Type 1 error (%)", y = "Power (%)", title = "Scenario 1: Futility Only") +
  guides(color = guide_legend(
    override.aes=list(shape = shape))) + theme_classic() + xlim(8,11) + ylim(72,92) + 
  theme(plot.title =  element_text(face = "bold", size = 14, hjust = 0.5))



######## Plot expected sample under null vs expected sample under alt ######

scenario1_futonly_for_plot %>% ggplot(aes(x = AvgSampSize, y=AvgSampleSize, color = Design)) + 
  geom_point(shape = shape, size = 5) + labs(x = "Expected Sample Size under Null", y = "Expected Sample Size under Alternative",
                                             title = "Scenario 1: Futility Only") +
  guides(color = guide_legend(
    override.aes=list(shape = shape))) + theme_classic()+ 
  theme(plot.title =  element_text(face = "bold", size = 14, hjust = 0.5)) + ylim(23,25)+
  #scale_y_continuous(breaks=seq(23,25,1))+ 
  scale_x_continuous(breaks=seq(15,25,1))




############## CONFUSION MATRICES ##################

#  alternative

simon_conf_alt = confusion.fun(simon_interim_alt[[1]])
surething_conf_alt = confusion.fun(surething_interim_alt[[1]])

bayes0.1_conf_alt = confusion.fun(bayes0.1_interim_alt[[1]])
bayes0.1_minus1st_conf_alt = confusion.fun(bayes0.1_minus1st_interim_alt[[1]])
bayes0.1_minus1st_plus1_conf_alt = confusion.fun(bayes0.1_minus1st_plus1_interim_alt[[1]])

bayes0.2_conf_alt = confusion.fun(bayes0.2_interim_alt[[1]])
bayes0.2_minus1st_conf_alt = confusion.fun(bayes0.2_minus1st_interim_alt[[1]])
bayes0.2_minus1st_plus1_conf_alt = confusion.fun(bayes0.2_minus1st_plus1_interim_alt[[1]])


BB_conf_alt = confusion.fun(BB_interim_alt[[1]])
BB_minus1st_conf_alt = confusion.fun(BB_minus1st_interim_alt[[1]])
BB_minus1st_plus1_conf_alt = confusion.fun(BB_minus1st_plus1_interim_alt[[1]])


#  null
simon_conf_null = confusion.fun(simon_interim_null[[1]])
surething_conf_null = confusion.fun(surething_interim_null[[1]])

bayes0.1_conf_null = confusion.fun(bayes0.1_interim_null[[1]])
bayes0.1_minus1st_conf_null = confusion.fun(bayes0.1_minus1st_interim_null[[1]])
bayes0.1_minus1st_plus1_conf_null = confusion.fun(bayes0.1_minus1st_plus1_interim_null[[1]])

bayes0.2_conf_null = confusion.fun(bayes0.2_interim_null[[1]])
bayes0.2_minus1st_conf_null = confusion.fun(bayes0.2_minus1st_interim_null[[1]])
bayes0.2_minus1st_plus1_conf_null = confusion.fun(bayes0.2_minus1st_plus1_interim_null[[1]])


BB_conf_null = confusion.fun(BB_interim_null[[1]])
BB_minus1st_conf_null = confusion.fun(BB_minus1st_interim_null[[1]])
BB_minus1st_plus1_conf_null = confusion.fun(BB_minus1st_plus1_interim_null[[1]])





confusion_mat_table = data.frame(Design = Design_Names,
                                 Interim1Fixed1 = c(simon_conf_alt$ConfusionMatrix[1,1],
                                                    surething_conf_alt$ConfusionMatrix[1,1],
                                                    #bayes0.1_conf_alt$ConfusionMatrix[1,1],
                                                    #bayes0.1_minus1st_conf_alt$ConfusionMatrix[1,1],
                                                    bayes0.1_minus1st_plus1_conf_alt$ConfusionMatrix[1,1],
                                                    #bayes0.2_conf_alt$ConfusionMatrix[1,1],
                                                    #bayes0.2_minus1st_conf_alt$ConfusionMatrix[1,1],
                                                    #bayes0.2_minus1st_plus1_conf_alt$ConfusionMatrix[1,1],
                                                    BB_conf_alt$ConfusionMatrix[1,1],
                                                    BB_minus1st_conf_alt$ConfusionMatrix[1,1],
                                                    BB_minus1st_plus1_conf_alt$ConfusionMatrix[1,1]
                                 ),
                                 Interim0Fixed1 = c(simon_conf_alt$ConfusionMatrix[2,1],
                                                    surething_conf_alt$ConfusionMatrix[2,1],
                                                    #bayes0.1_conf_alt$ConfusionMatrix[2,1],
                                                    #bayes0.1_minus1st_conf_alt$ConfusionMatrix[2,1],
                                                    bayes0.1_minus1st_conf_alt$ConfusionMatrix[2,1],
                                                    #bayes0.2_conf_alt$ConfusionMatrix[2,1],
                                                    #bayes0.2_minus1st_conf_alt$ConfusionMatrix[2,1],
                                                    #bayes0.2_minus1st_plus1_conf_alt$ConfusionMatrix[2,1],
                                                    BB_conf_alt$ConfusionMatrix[2,1],
                                                    BB_minus1st_conf_alt$ConfusionMatrix[2,1],
                                                    BB_minus1st_plus1_conf_alt$ConfusionMatrix[2,1]
                                 ),
                                 Interim1Fixed0 = c(simon_conf_alt$ConfusionMatrix[1,2],
                                                    surething_conf_alt$ConfusionMatrix[1,2],
                                                    #bayes0.1_conf_alt$ConfusionMatrix[1,2],
                                                    #bayes0.1_minus1st_conf_alt$ConfusionMatrix[1,2],
                                                    bayes0.1_minus1st_conf_alt$ConfusionMatrix[1,2],
                                                    #bayes0.2_conf_alt$ConfusionMatrix[1,2],
                                                    #bayes0.2_minus1st_conf_alt$ConfusionMatrix[1,2],
                                                    #bayes0.2_minus1st_plus1_conf_alt$ConfusionMatrix[1,2],
                                                    BB_conf_alt$ConfusionMatrix[1,2],
                                                    BB_minus1st_conf_alt$ConfusionMatrix[1,2],
                                                    BB_minus1st_plus1_conf_alt$ConfusionMatrix[1,2]
                                 ),
                                 Interim0Fixed0 = c(simon_conf_alt$ConfusionMatrix[2,2],
                                                    surething_conf_alt$ConfusionMatrix[2,2],
                                                    #bayes0.1_conf_alt$ConfusionMatrix[2,2],
                                                    #bayes0.1_minus1st_conf_alt$ConfusionMatrix[2,2],
                                                    bayes0.1_minus1st_conf_alt$ConfusionMatrix[2,2],
                                                    #bayes0.2_conf_alt$ConfusionMatrix[2,2],
                                                    #bayes0.2_minus1st_conf_alt$ConfusionMatrix[2,2],
                                                    #bayes0.2_minus1st_plus1_conf_alt$ConfusionMatrix[2,2],
                                                    BB_conf_alt$ConfusionMatrix[2,2],
                                                    BB_minus1st_conf_alt$ConfusionMatrix[2,2],
                                                    BB_minus1st_plus1_conf_alt$ConfusionMatrix[2,2]),
                                 
                                 Interim1Fix1n = c(simon_conf_null$ConfusionMatrix[1,1],
                                                   surething_conf_null$ConfusionMatrix[1,1],
                                                   #bayes0.1_conf_null$ConfusionMatrix[1,1],
                                                   #bayes0.1_minus1st_conf_null$ConfusionMatrix[1,1],
                                                   bayes0.1_minus1st_plus1_conf_null$ConfusionMatrix[1,1],
                                                   #bayes0.2_conf_null$ConfusionMatrix[1,1],
                                                   #bayes0.2_minus1st_conf_null$ConfusionMatrix[1,1],
                                                   #bayes0.2_minus1st_plus1_conf_null$ConfusionMatrix[1,1],
                                                   BB_conf_null$ConfusionMatrix[1,1],
                                                   BB_minus1st_conf_null$ConfusionMatrix[1,1],
                                                   BB_minus1st_plus1_conf_null$ConfusionMatrix[1,1]
                                 ),
                                 Interim0Fix1n = c(simon_conf_null$ConfusionMatrix[2,1],
                                                   surething_conf_null$ConfusionMatrix[2,1],
                                                   #bayes0.1_conf_null$ConfusionMatrix[2,1],
                                                   #bayes0.1_minus1st_conf_null$ConfusionMatrix[2,1],
                                                   bayes0.1_minus1st_plus1_conf_null$ConfusionMatrix[2,1],
                                                   #bayes0.2_conf_null$ConfusionMatrix[2,1],
                                                   #bayes0.2_minus1st_conf_null$ConfusionMatrix[2,1],
                                                   #bayes0.2_minus1st_plus1_conf_null$ConfusionMatrix[2,1],
                                                   BB_conf_null$ConfusionMatrix[2,1],
                                                   BB_minus1st_conf_null$ConfusionMatrix[2,1],
                                                   BB_minus1st_plus1_conf_null$ConfusionMatrix[2,1]
                                 ),
                                 Interim1Fix0n = c(simon_conf_null$ConfusionMatrix[1,2],
                                                   surething_conf_null$ConfusionMatrix[1,2],
                                                   #bayes0.1_conf_null$ConfusionMatrix[1,2],
                                                   #bayes0.1_minus1st_conf_null$ConfusionMatrix[1,2],
                                                   bayes0.1_minus1st_plus1_conf_null$ConfusionMatrix[1,2],
                                                   #bayes0.2_conf_null$ConfusionMatrix[1,2],
                                                   #bayes0.2_minus1st_conf_null$ConfusionMatrix[1,2],
                                                   #bayes0.2_minus1st_plus1_conf_null$ConfusionMatrix[1,2],
                                                   BB_conf_null$ConfusionMatrix[1,2],
                                                   BB_minus1st_conf_null$ConfusionMatrix[1,2],
                                                   BB_minus1st_plus1_conf_null$ConfusionMatrix[1,2]),
                                 Interim0Fix0n = c(simon_conf_null$ConfusionMatrix[2,2],
                                                   surething_conf_null$ConfusionMatrix[2,2],
                                                   #bayes0.1_conf_null$ConfusionMatrix[2,2],
                                                   #bayes0.1_minus1st_conf_null$ConfusionMatrix[2,2],
                                                   bayes0.1_minus1st_plus1_conf_null$ConfusionMatrix[2,2],
                                                   #bayes0.2_conf_null$ConfusionMatrix[2,2],
                                                   #bayes0.2_minus1st_conf_null$ConfusionMatrix[2,2],
                                                   #bayes0.2_minus1st_plus1_conf_null$ConfusionMatrix[2,2],
                                                   BB_conf_null$ConfusionMatrix[2,2],
                                                   BB_minus1st_conf_null$ConfusionMatrix[2,2],
                                                   BB_minus1st_plus1_conf_null$ConfusionMatrix[2,2])
)

confusion_mat_table %>% flextable() %>% theme_box() %>% 
  add_header_row( values = c("","Alternative","Null"), colwidths = c(1,4,4)) %>%
  set_header_labels(Interim1Fix1n = "Interim1Fixeed1",
                    Interim0Fix1n = "Interim0Fixed1",
                    Interim1Fix0n = "Interim1Fixed0",
                    Interim0Fix0n = "Interim0Fixed0") %>% colformat_double(digits = 3)






# specify vector for shapes
shape_vec = c(0:5)

#factor design
confusion_mat_table$Design = factor(confusion_mat_table$Design, levels = Design_Names)

## CONFUSION MATRIX UNDER THE ALTERNATIVE

plot_alt11 = ggplot(data = confusion_mat_table, aes(Design, Interim1Fixed1*100, col = Design,
                                                    label = round(Interim1Fixed1*100,1)  )) +
  geom_point(shape = shape_vec, size = 3) + labs(x = "", y = " Effect", title = " Effect") +
  theme_classic() + 
  geom_text(vjust=-1, show.legend = FALSE, size=6)+
  scale_x_discrete(labels = NULL, breaks = NULL)+
  ylim(c(0,100)) + theme(plot.background = element_rect(color = "black"), 
                         plot.title = element_text(hjust = 0.5)
                         ,axis.title.y = element_text( size = 12)
  )+
  guides(color = guide_legend(
    override.aes=list(shape = shape_vec))) 

plot_alt10 = ggplot(data = confusion_mat_table, aes(Design, Interim1Fixed0*100, col = Design,
                                                    label = round(Interim1Fixed0*100,1))) +
  geom_point(shape = shape_vec, size = 3)+ labs(x = "", y ="%", title = " No Effect")+
  theme_classic()+ 
  scale_x_discrete(labels = NULL, breaks = NULL)+
  ylim(c(0,100))+ theme(plot.background = element_rect(color = "black"), plot.title = element_text(hjust = 0.5))+ 
  geom_text(vjust=-1, show.legend = FALSE, size=6)

plot_alt01 = ggplot(data = confusion_mat_table, aes(Design, Interim0Fixed1*100, col = Design,
                                                    label = round(Interim0Fixed1*100,1))) +
  geom_point(shape = shape_vec, size = 3)+ labs(x = "", y ="No Effect")+
  theme_classic()+ 
  scale_x_discrete(labels = NULL, breaks = NULL)+
  ylim(c(0,100))+ theme(plot.background = element_rect(color = "black")
                        , axis.title.y = element_text(  size = 12)
  )+ 
  geom_text(vjust=-1, show.legend = FALSE, size=6)

plot_alt00 = ggplot(data = confusion_mat_table, aes(Design, Interim0Fixed0*100, color = Design,
                                                    label = round(Interim0Fixed0*100,1))) +
  geom_point(shape = shape_vec, size = 3)+ labs(x = "", y ="%")+
  theme_classic()+ 
  scale_x_discrete(labels = NULL, breaks = NULL)+ ylim(c(0,100))+ theme(plot.background = element_rect(color = "black")) + 
  geom_text(vjust=-1, show.legend = FALSE, size=6)

confusion_plots_alt = ggarrange(plot_alt11, plot_alt10, plot_alt01,   plot_alt00, nrow = 2, ncol = 2, common.legend = TRUE, legend="right")

annotate_figure(confusion_plots_alt, top = text_grob("Fixed Design Conclusion", hjust = 0.8, size = 20, face = "bold"), 
                left = text_grob("Interim Design Conclusion", size = 20, rot = 90, face = "bold"))



## CONFUSION MATRIX UNDER THE NULL

plot_null11 = ggplot(data = confusion_mat_table, aes(Design, Interim1Fix1n*100, col = Design,
                                                     label = round(Interim1Fix1n*100,1) )) +
  geom_point(shape = shape_vec, size = 3) + labs(x = "", y ="Effect", title = "Effect") +
  theme_classic() + 
  scale_x_discrete(labels = NULL, breaks = NULL)+
  ylim(c(0,100)) + theme(plot.background = element_rect(color = "black"), 
                         plot.title = element_text(hjust = 0.5),
                         axis.title.y = element_text( size = 12))+
  guides(color = guide_legend(override.aes = list(shape = shape_vec) ) )+ 
  geom_text(vjust=-1, show.legend = FALSE, size=6)

plot_null10 = ggplot(data = confusion_mat_table, aes(Design, Interim1Fix0n*100, col = Design,
                                                     label = round(Interim1Fix0n*100,1) )) +
  geom_point(shape = shape_vec, size = 3)+ labs(x = "", y ="%", title = "No Effect")+
  theme_classic()+ 
  scale_x_discrete(labels = NULL, breaks = NULL)+
  ylim(c(0,100))+ theme(plot.background = element_rect(color = "black"), plot.title = element_text(hjust = 0.5))+ 
  geom_text(vjust=-1, show.legend = FALSE, size=6)

plot_null01 = ggplot(data = confusion_mat_table, aes(Design, Interim0Fix1n*100, col = Design,
                                                     label = round(Interim0Fix1n*100,1) )) +
  geom_point(shape = shape_vec, size = 3)+ labs(x = "", y ="No Effect")+
  theme_classic()+ 
  scale_x_discrete(labels = NULL, breaks = NULL)+
  ylim(c(0,100))+ theme(plot.background = element_rect(color = "black"),
                        axis.title.y = element_text( size = 14))+ 
  geom_text(vjust=-1, show.legend = FALSE, size=6)

plot_null00 = ggplot(data = confusion_mat_table, aes(Design, Interim0Fix0n*100, col = Design,
                                                     label = round(Interim0Fix0n*100,1) )) +
  geom_point(shape = shape_vec, size = 3)+ labs(x = "", y ="%")+
  theme_classic()+ 
  scale_x_discrete(labels = NULL, breaks = NULL)+ ylim(c(0,100))+ theme(plot.background = element_rect(color = "black"))+ 
  geom_text(vjust=-1, show.legend = FALSE, size=6)

confusion_plots_null = ggarrange(plot_null11, plot_null10, plot_null01,   plot_null00, nrow = 2, ncol = 2, common.legend = TRUE, legend="right")

annotate_figure(confusion_plots_null, top = text_grob("Fixed Design Conclusion", hjust = 0.8, face = "bold", size = 20), left = text_grob("Interim Design Conclusion", rot = 90, face = "bold", size = 20))


ESS_table = data.frame(Design = Design_Names,
                       Interim1Fixed1 = c(simon_conf_alt$ExpectedSampleSize[1,1],
                                          surething_conf_alt$ExpectedSampleSize[1,1],
                                          #bayes0.1_conf_alt$ExpectedSampleSize[1,1],
                                          #bayes0.1_minus1st_conf_alt$ExpectedSampleSize[1,1],
                                          bayes0.1_minus1st_plus1_conf_alt$ExpectedSampleSize[1,1],
                                          #bayes0.2_conf_alt$ExpectedSampleSize[1,1],
                                          #bayes0.2_minus1st_conf_alt$ExpectedSampleSize[1,1],
                                          #bayes0.2_minus1st_plus1_conf_alt$ExpectedSampleSize[1,1],
                                          BB_conf_alt$ExpectedSampleSize[1,1],
                                          BB_minus1st_conf_alt$ExpectedSampleSize[1,1],
                                          BB_minus1st_plus1_conf_alt$ExpectedSampleSize[1,1]
                       ),
                       Interim0Fixed1 = c(simon_conf_alt$ExpectedSampleSize[2,1],
                                          surething_conf_alt$ExpectedSampleSize[2,1],
                                          #bayes0.1_conf_alt$ExpectedSampleSize[2,1],
                                          #bayes0.1_minus1st_conf_alt$ExpectedSampleSize[2,1],
                                          bayes0.1_minus1st_conf_alt$ExpectedSampleSize[2,1],
                                          #bayes0.2_conf_alt$ExpectedSampleSize[2,1],
                                          #bayes0.2_minus1st_conf_alt$ExpectedSampleSize[2,1],
                                          #bayes0.2_minus1st_plus1_conf_alt$ExpectedSampleSize[2,1],
                                          BB_conf_alt$ExpectedSampleSize[2,1],
                                          BB_minus1st_conf_alt$ExpectedSampleSize[2,1],
                                          BB_minus1st_plus1_conf_alt$ExpectedSampleSize[2,1]
                       ),
                       Interim1Fixed0 = c(simon_conf_alt$ExpectedSampleSize[1,2],
                                          surething_conf_alt$ExpectedSampleSize[1,2],
                                          #bayes0.1_conf_alt$ExpectedSampleSize[1,2],
                                          #bayes0.1_minus1st_conf_alt$ExpectedSampleSize[1,2],
                                          bayes0.1_minus1st_conf_alt$ExpectedSampleSize[1,2],
                                          #bayes0.2_conf_alt$ExpectedSampleSize[1,2],
                                          #bayes0.2_minus1st_conf_alt$ExpectedSampleSize[1,2],
                                          #bayes0.2_minus1st_plus1_conf_alt$ExpectedSampleSize[1,2],
                                          BB_conf_alt$ExpectedSampleSize[1,2],
                                          BB_minus1st_conf_alt$ExpectedSampleSize[1,2],
                                          BB_minus1st_plus1_conf_alt$ExpectedSampleSize[1,2]
                       ),
                       Interim0Fixed0 = c(simon_conf_alt$ExpectedSampleSize[2,2],
                                          surething_conf_alt$ExpectedSampleSize[2,2],
                                          #bayes0.1_conf_alt$ExpectedSampleSize[2,2],
                                          #bayes0.1_minus1st_conf_alt$ExpectedSampleSize[2,2],
                                          bayes0.1_minus1st_conf_alt$ExpectedSampleSize[2,2],
                                          #bayes0.2_conf_alt$ExpectedSampleSize[2,2],
                                          #bayes0.2_minus1st_conf_alt$ExpectedSampleSize[2,2],
                                          #bayes0.2_minus1st_plus1_conf_alt$ExpectedSampleSize[2,2],
                                          BB_conf_alt$ExpectedSampleSize[2,2],
                                          BB_minus1st_conf_alt$ExpectedSampleSize[2,2],
                                          BB_minus1st_plus1_conf_alt$ExpectedSampleSize[2,2]),
                       
                       Interim1Fix1n = c(simon_conf_null$ExpectedSampleSize[1,1],
                                         surething_conf_null$ExpectedSampleSize[1,1],
                                         #bayes0.1_conf_null$ExpectedSampleSize[1,1],
                                         #bayes0.1_minus1st_conf_null$ExpectedSampleSize[1,1],
                                         bayes0.1_minus1st_plus1_conf_null$ExpectedSampleSize[1,1],
                                         #bayes0.2_conf_null$ExpectedSampleSize[1,1],
                                         #bayes0.2_minus1st_conf_null$ExpectedSampleSize[1,1],
                                         #bayes0.2_minus1st_plus1_conf_null$ExpectedSampleSize[1,1],
                                         BB_conf_null$ExpectedSampleSize[1,1],
                                         BB_minus1st_conf_null$ExpectedSampleSize[1,1],
                                         BB_minus1st_plus1_conf_null$ExpectedSampleSize[1,1]
                       ),
                       Interim0Fix1n = c(simon_conf_null$ExpectedSampleSize[2,1],
                                         surething_conf_null$ExpectedSampleSize[2,1],
                                         #bayes0.1_conf_null$ExpectedSampleSize[2,1],
                                         #bayes0.1_minus1st_conf_null$ExpectedSampleSize[2,1],
                                         bayes0.1_minus1st_plus1_conf_null$ExpectedSampleSize[2,1],
                                         #bayes0.2_conf_null$ExpectedSampleSize[2,1],
                                         #bayes0.2_minus1st_conf_null$ExpectedSampleSize[2,1],
                                         #bayes0.2_minus1st_plus1_conf_null$ExpectedSampleSize[2,1],
                                         BB_conf_null$ExpectedSampleSize[2,1],
                                         BB_minus1st_conf_null$ExpectedSampleSize[2,1],
                                         BB_minus1st_plus1_conf_null$ExpectedSampleSize[2,1]
                       ),
                       Interim1Fix0n = c(simon_conf_null$ExpectedSampleSize[1,2],
                                         surething_conf_null$ExpectedSampleSize[1,2],
                                         #bayes0.1_conf_null$ExpectedSampleSize[1,2],
                                         #bayes0.1_minus1st_conf_null$ExpectedSampleSize[1,2],
                                         bayes0.1_minus1st_plus1_conf_null$ExpectedSampleSize[1,2],
                                         #bayes0.2_conf_null$ExpectedSampleSize[1,2],
                                         #bayes0.2_minus1st_conf_null$ExpectedSampleSize[1,2],
                                         #bayes0.2_minus1st_plus1_conf_null$ExpectedSampleSize[1,2],
                                         BB_conf_null$ExpectedSampleSize[1,2],
                                         BB_minus1st_conf_null$ExpectedSampleSize[1,2],
                                         BB_minus1st_plus1_conf_null$ExpectedSampleSize[1,2]),
                       Interim0Fix0n = c(simon_conf_null$ExpectedSampleSize[2,2],
                                         surething_conf_null$ExpectedSampleSize[2,2],
                                         #bayes0.1_conf_null$ExpectedSampleSize[2,2],
                                         #bayes0.1_minus1st_conf_null$ExpectedSampleSize[2,2],
                                         bayes0.1_minus1st_plus1_conf_null$ExpectedSampleSize[2,2],
                                         #bayes0.2_conf_null$ExpectedSampleSize[2,2],
                                         #bayes0.2_minus1st_conf_null$ExpectedSampleSize[2,2],
                                         #bayes0.2_minus1st_plus1_conf_null$ExpectedSampleSize[2,2],
                                         BB_conf_null$ExpectedSampleSize[2,2],
                                         BB_minus1st_conf_null$ExpectedSampleSize[2,2],
                                         BB_minus1st_plus1_conf_null$ExpectedSampleSize[2,2])
)

ESS_table$Interim0Fixed1 = round(ESS_table$Interim0Fixed1,1)
ESS_table$Interim0Fixed0 = round(ESS_table$Interim0Fixed0,1)
ESS_table$Interim0Fix1n = round(ESS_table$Interim0Fix1n,1)
ESS_table$Interim0Fix0n = round(ESS_table$Interim0Fix0n,1)

#factor design
ESS_table$Design = factor(ESS_table$Design, levels = Design_Names)


ESS_table %>% flextable() %>% theme_box() %>% 
  add_header_row( values = c("","Alternative","Null"), colwidths = c(1,4,4)) %>%
  set_header_labels(Interim1Fix1n = "Interim1Fixed1",
                    Interim0Fix1n = "Interim0Fixed1",
                    Interim1Fix0n = "Interim1Fixed0",
                    Interim0Fix0n = "Interim0Fixed0") %>% colformat_double(digits = 1)


## Expected sample size ALTERNATIVE

ESSplot_alt11 = ggplot(data = ESS_table, aes(Design, Interim1Fixed1, col = Design,
                                             label = Interim1Fixed1)) +
  geom_point(shape = shape_vec, size = 3) + labs(x = "", y ="Effect", title = "Effect") +
  theme_classic() + 
  scale_x_discrete(labels = NULL, breaks = NULL)+
  ylim(c(0,25)) + theme(plot.background = element_rect(color = "black"), plot.title = element_text(hjust = 0.5),
                        axis.title.y = element_text( size = 12)) +
  guides(color = guide_legend(override.aes = list(shape = shape_vec) ) )+ 
  geom_text(vjust=2, show.legend = FALSE, size=6)

ESSplot_alt10 = ggplot(data = ESS_table, aes(Design, Interim1Fixed0, col = Design,
                                             label = Interim1Fixed0)) +
  geom_point(shape = shape_vec, size = 3)+ labs(x = "", y ="n", title = "No Effect")+
  theme_classic()+ 
  scale_x_discrete(labels = NULL, breaks = NULL)+
  ylim(c(0,25))+ theme(plot.background = element_rect(color = "black"), plot.title = element_text(hjust = 0.5))+ 
  geom_text(vjust=-1, show.legend = FALSE, size=6)

ESSplot_alt01 = ggplot(data = ESS_table, aes(Design, Interim0Fixed1, col = Design,
                                             label = Interim0Fixed1)) +
  geom_point(shape =shape_vec, size = 3)+ labs(x = "", y ="No Effect")+
  theme_classic()+ 
  scale_x_discrete(labels = NULL, breaks = NULL)+
  ylim(c(0,25))+ theme(plot.background = element_rect(color = "black"),
                       axis.title.y = element_text(size = 12))+ 
  geom_text(vjust=-1, show.legend = FALSE, size=6)

ESSplot_alt00 = ggplot(data = ESS_table, aes(Design, Interim0Fixed0, col = Design,
                                             label = Interim0Fixed0)) +
  geom_point(shape = shape_vec, size = 3)+ labs(x = "", y ="n")+
  theme_classic()+ 
  scale_x_discrete(labels = NULL, breaks = NULL)+ ylim(c(0,25))+ theme(plot.background = element_rect(color = "black"))+ 
  geom_text(vjust=2, show.legend = FALSE, size=6)

ESS_plots_alt = ggarrange(ESSplot_alt11, ESSplot_alt10, ESSplot_alt01, ESSplot_alt00, nrow = 2, ncol = 2, common.legend = TRUE, legend="right")

annotate_figure(ESS_plots_alt,top = text_grob("Fixed Design Conclusion", hjust = 0.8, face = "bold", size = 20), left = text_grob("Interim Design Conclusion", rot = 90, face = "bold", size = 20))



## Expected sample sizes under NULL

ESSplot_null11 = ggplot(data = ESS_table, aes(Design, Interim1Fix1n, col = Design,
                                              label = Interim1Fix1n)) +
  geom_point(shape = shape_vec, size = 3) + labs(x = "", y ="Effect", title = "Effect") +
  theme_classic() + ylim(c(0,25))+ 
  scale_x_discrete(labels = NULL, breaks = NULL)+
  theme(plot.background = element_rect(color = "black"), plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text( size = 12))+
  guides(color = guide_legend(override.aes = list(shape = shape_vec) ) )+ 
  geom_text(vjust=2, show.legend = FALSE, size=6)

ESSplot_null10 = ggplot(data = ESS_table, aes(Design, Interim1Fix0n, col = Design,
                                              label = Interim1Fix0n)) +
  geom_point(shape = shape_vec, size = 3)+ labs(x = "", y ="n", title = "No Effect")+
  theme_classic()+ ylim(c(0,25))+ 
  scale_x_discrete(labels = NULL, breaks = NULL)+
  theme(plot.background = element_rect(color = "black"), plot.title = element_text(hjust = 0.5))+ 
  geom_text(vjust=-1, show.legend = FALSE, size=6)

ESSplot_null01 = ggplot(data = ESS_table, aes(Design, Interim0Fix1n, col = Design,
                                              label = Interim0Fix1n)) +
  geom_point(shape = shape_vec, size = 3)+ labs(x = "", y ="No Effect")+
  theme_classic()+ ylim(c(0,25))+ 
  scale_x_discrete(labels = NULL, breaks = NULL)+
  theme(plot.background = element_rect(color = "black"),
        axis.title.y = element_text(size = 12))+ 
  geom_text(vjust=-1, show.legend = FALSE, size=5)

ESSplot_null00 = ggplot(data = ESS_table, aes(Design, Interim0Fix0n, col = Design,
                                              label = Interim0Fix0n)) +
  geom_point(shape = shape_vec, size = 3)+ labs(x = "", y ="n")+
  theme_classic()+ ylim(c(0,25))+ 
  scale_x_discrete(labels = NULL, breaks = NULL)+  theme(plot.background = element_rect(color = "black"))+ 
  geom_text(vjust=-1, show.legend = FALSE, size=6)

ESS_plots_null = ggarrange(ESSplot_null11, ESSplot_null10, ESSplot_null01,   ESSplot_null00, nrow = 2, ncol = 2, common.legend = TRUE, legend="right")

annotate_figure(ESS_plots_null, top = text_grob("Fixed Design Conclusion", hjust = 0.8, face = "bold", size = 20), left = text_grob("Interim Design Conclusion", rot = 90, face = "bold", size = 20))




















