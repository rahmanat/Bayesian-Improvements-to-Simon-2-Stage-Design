##### Calculate the PPoS #####

num_sims = 10000 # number of simulations
num_subjects = 25 # Maximum number of subjects to be enrolled 
sig_num = 5 # Number of responses needed at end of trial to declare an effect 
null_response = 0.1 # Null response rate
PP_threshold = 0.90 # Posterior probability threshold in order to declare success at end of trial

# Specify the alpha and beta for the Beta(a,b) prior distribution
# Common non-informative priors are Beta(1,1) and Beta(0.5,0.5)
alpha = 1 
beta = 1


#### 0.1 null response, using Beta(1,1) prior
pred_p = matrix(data = 0, nrow = num_subjects, ncol = sig_num + 1)
a = alpha
b = beta

set.seed(515)
for (n in 1:num_subjects) {
  for (x in 0:sig_num) {
    p <- (a+x)/(a+n+b) #probability of success out those oberved so far
    future_x <- rbinom(num_sims, size=num_subjects-n, prob=p) #10,000 sims 
    future_pp <- 1 - pbeta(null_response, a+x+future_x, num_subjects+b-(x+future_x)) #probability that posterior prob >null response rate
    pred_p[n,x+1] <- sum(future_pp > PP_threshold)/num_sims # prob that posterior is > PP out of num_sims posteriors
  }
}

colnames(pred_p) = c(0:sig_num)
rownames(pred_p) = c(1:num_subjects)

# Print pred_p to get output and copy & paste into Excel to create visualizations
# Each row is number of subjects enrolled
# Each column is number of responses, starting from 0

pred_p


