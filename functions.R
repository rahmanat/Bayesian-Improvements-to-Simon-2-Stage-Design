
#Function that creates simulated data dataframe where rows are the simulation number and the columns are the number of particiapnts enrolled in the study.

data.simulator = function(num_sims, p, seed=1,n=25) {
### This function simulates study data with a binary outcome for 25 subjects (default)
  # num_sims: number of study simulations 
  # p: probability of success (specify either null or alternative response rate)
  # n: maximum number of subjects in trial
  df_sims = matrix(NA, ncol = n, nrow = num_sims)
    for (i in 1:num_sims) {
      set.seed(seed+i)
      subjects = rbinom(n = n, size = 1, prob = p)
      
      df_sims[i, ] <- subjects
    }
  df_sims= as.data.frame(df_sims)
  return(df_sims)}



## A function where you input number of sims and alt/null scenario and it produces this information for each study: 
  #(1) sample size stop, 
  #(2) if it was successful, 0/1 (>5), 
  #(3) If it stopped early (<5)
  #(4) If it stopped early (>=5)


interim.monitor.fun = function(data, looks, fut.bound, eff.bound, sig.num = 5){
  ### This function first outputs a summary of each simulated simulation based on specified interim monitor design.
  ### Next, it outputs a summary/averages over all sims. 
  ### Both summary tables include the following info:
  ###(1) sample size that the study ended, (2) if it was successful, 0/1,
  ###(3) If the trial stopped early for futility, (4) If trial stopped early for efficacy 
  
  # data: i x n matrix, simulated binary data for n subjects with i row iterations (use the output from data.simulator())
  # looks: a vector containing the the subject number after which we "look" 
  # fut.bound: futility bound, a vector (same length as looks) that if we observe that number of responses (or less) stop the study for futility
  # eff.bound: efficacy bound, a vector (same length as looks) that if we observe that number of responses (or more) stop the study for efficacy
      # If not monitoring for efficacy input a vector of 10000's (or a number larger than your trial's sample size) the same length as "looks"
  # sig.num: the number of responses at the end of the study that deems it successful, dflt = 5
  
  columns = c("Sample Size Stop", "Successful Study", "Early Stop Futility", "Early Stop Efficacy", "Fixed Responses","Fixed Success")
  df_sim_results = data.frame(matrix(NA, nrow = nrow(data), ncol = length(columns)))
  colnames(df_sim_results) = columns
  
  for (x in 1:nrow(data)) {
    
    #Add the total number of responses ("Fixed Responses") if the study finished to completion
    df_sim_results[x,5] = sum(data[x,])
    
    #Add if the study was successful in fixed scenario ("Fixed Success") 
    df_sim_results[x,6] = ifelse(df_sim_results[x,5] >= sig.num,1,0)
  
    for (i in 1:length(looks)) {
      success = sum(data[x, 1:looks[i]])
      
      if (success <= fut.bound[i] & sum(!is.na(df_sim_results[x,])) <= 2) {
        df_sim_results[x,1] = looks[i]
        df_sim_results[x,2] = 0
        df_sim_results[x,3] = 1
        df_sim_results[x,4] = 0
        break()
      }
      if (success >= eff.bound[i] & sum(!is.na(df_sim_results[x,])) <= 2) {
        df_sim_results[x,1] = looks[i]
        df_sim_results[x,2] = 1
        df_sim_results[x,3] = 0
        df_sim_results[x,4] = 1
        break()
      }
      if(i == length(looks) & sum(!is.na(df_sim_results[x,])) <= 2){
        df_sim_results[x,1] = ncol(data)
        df_sim_results[x,2] = if(sum(data[x,])>=sig.num){1}else{0}
        df_sim_results[x,3] = 0
        df_sim_results[x,4] = 0
        break()
      
      }
  }}
  
  
  # summary of all simulations
  df_summary = data.frame(AvgSampleSize = mean(df_sim_results$`Sample Size Stop`),
                                  Success = sum(df_sim_results$`Successful Study`)/nrow(data),
                                  EarlyStop_futility = sum(df_sim_results$`Early Stop Futility`)/nrow(data),
                                  EarlyStop_efficacy = sum(df_sim_results$`Early Stop Efficacy`)/nrow(data))
  
  
  
  result_list = list(SimResults = df_sim_results, 
                     SummaryOfSims = df_summary)
}





confusion.fun = function( interim_results){
  ### (1) This function creates a 2x2 confusion matrix comparing the interim results to no interim monitoring (success & no success)
  ### (2) The second part produces the expected sample size of confusion matrix for the interim monitor results
  ### (3) The third part looks at when interim monitoring stops early correctly
  
  # interim_results: the first output of the list from interim.monitor.fun()[1], the summary of each simulation
  
  #1
  confusion_mat = prop.table(table(factor(interim_results$`Successful Study`, levels = c(1,0)),
                        factor(interim_results$`Fixed Success`, levels = c(1,0)), dnn = c("Interim","Fixed")))
  
  # confusion_mat2 = data.frame(Fixed1 = c(nrow(interim_results[interim_results$`Successful Study`==1 & data$success==1,]),
  #                                       nrow(interim_results[interim_results$`Successful Study`==0 & data$success==1,])),
  #                            Fixed0 = c(nrow(interim_results[interim_results$`Successful Study`==1 & data$success==0,]),
  #                                       nrow(interim_results[interim_results$`Successful Study`==0 & data$success==0,])))
  # rownames(confusion_mat2) = c("Interim1","Interim0")
  
  #2 Expected Values at each for Interim
  
  interim1_fixed1 = interim_results[interim_results$`Successful Study`==1 & interim_results$`Fixed Success`==1,]
  
  interim1_fixed0 = interim_results[interim_results$`Successful Study`==1 & interim_results$`Fixed Success`==0,]
  
  interim0_fixed1 = interim_results[interim_results$`Successful Study`==0 & interim_results$`Fixed Success`==1,]
  
  interim0_fixed0 = interim_results[interim_results$`Successful Study`==0 & interim_results$`Fixed Success`==0,]
  
  ESS_mat = data.frame(Fixed1 = c(mean(interim1_fixed1$`Sample Size Stop`),mean(interim0_fixed1$`Sample Size Stop`)),
                      Fixed0 = c(mean(interim1_fixed0$`Sample Size Stop`),mean(interim0_fixed0$`Sample Size Stop`)))
  rownames(ESS_mat) = c("Interim1","Interim0")
  
  
  #3 Proportional table looking at fixed0 and interim0
  
  result_list = list(ConfusionMatrix = confusion_mat, 
                     
                     ExpectedSampleSize= ESS_mat,
                     Interim1andFixed1 = prop.table(table(interim1_fixed1$`Sample Size Stop`)),
                     Interim0andFixed1 = prop.table(table(interim0_fixed1$`Sample Size Stop`)),
                     Interim1andFixed0 = prop.table(table(interim1_fixed0$`Sample Size Stop`)),
                     Interim0andfixed0 = prop.table(table(interim0_fixed0$`Sample Size Stop`))
                     )
}




################################################################################

#### An example of how to use the functions for Simon 2-Stage design in a hypothetical 
### trial with 10% null repsonse, 30% alternative, 90% power, 10% Type I error, and 25 subjects

# Simon: 1 Interim monitor at 16th subject where if we observe 1 of fewer responses, end early for futility, and no efficacy montitoring

alt_data = data.simulator(num_sims = 1000, p = 0.3, n = 25)
simon_interim_alt = interim.monitor.fun(data = alt_data, looks = c(16), 
                                        fut.bound = c(1), eff.bound = c(100))
simon_conf_alt = confusion.fun(interim_results = simon_interim_alt[[1]])

null_data = data.simulator(num_sims = 1000, p = 0.10, n= 25)
simon_interim_null = interim.monitor.fun(data = null_data, looks = c(12), 
                                         fut.bound = c(4), eff.bound = c(30), sig.num = 13)
simon_conf_null = confusion.fun(interim_results = simon_interim_null[[1]])



