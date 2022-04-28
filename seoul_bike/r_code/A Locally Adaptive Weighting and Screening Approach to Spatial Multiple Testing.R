library(dplyr)
library(data.table)
library(geosphere)

pvalues_spring = fread('D:\\bike\\모비율 pvalue\\계절별\\proportion_pvalue_spring.csv')

### change "dist_spring.csv" accordingly
dist_spring = fread('D:\\bike\\모비율 pvalue\\LAWS\\dist_proportion_spring.csv')
dist_spring = as.data.frame(dist_spring)

pvalues_spring2 = pvalues_spring [,c('place','latitude','longitude','two','greater','less')]
k = function(x){exp(-x^2/2)/sqrt(2*pi)}
kh = function(x){k(x/h)/h}
normalize <- function(x){return((x-min(x))/(max(x)-min(x)))}

########                       
tau = 0.5
h = 0.05
alpha = 0.05
####p_two####
pvalues_spring2$svh_two_0.05 = pvalues_spring2$tvh_two_0.05 = rep(0,nrow(pvalues_spring2))


above_tau_ind = which(pvalues_spring2$two > tau)

valvec = NULL
for (i in 1:nrow(pvalues_spring2)){
  tot_dist = dist_spring[,i]
  
  # norm_tot: denominator vector
  norm_tot = normalize(tot_dist)
  
  # get the index for the numerators
  first_ind = above_tau_ind[above_tau_ind < i]
  later_ind = above_tau_ind[above_tau_ind > i]
  altogether_ind = c(first_ind, (later_ind-1))
  # norm_udist: numerator vector  
  norm_udist = norm_tot[altogether_ind]
  
  tvh = kh(norm_tot)/kh(0)
  uvh = kh(norm_udist)/kh(0)
  
  pvalues_spring2[i,]$svh_two_0.05 = sum(uvh)
  pvalues_spring2[i,]$tvh_two_0.05 = sum(tvh)
}

phat_two_spring_0.05 = 1 - pvalues_spring2$svh_two_0.05/((1-tau)*pvalues_spring2$tvh_two_0.05)
pvalues_spring2$phat_two_0.05 = phat_two_spring_0.05
pvalues_spring2$w_two_0.05 = phat_two_spring_0.05/(1-phat_two_spring_0.05)
pvalues_spring2$pws_two_0.05 = pvalues_spring2$two/pvalues_spring2$w_two_0.05

for (j in 1:nrow(pvalues_spring2)){
  if(pvalues_spring2$pws_two_0.05[j]>1){
    pvalues_spring2$pws_two_0.05[j]=1
  }
}

pvalues_spring3 = pvalues_spring2[order(pvalues_spring2$pws_two_0.05),]

for (j in 1:nrow(pvalues_spring3)){
  s = sum(pvalues_spring3$phat_two_0.05*pvalues_spring3$pws_two_0.05[j])/j
  if(s>alpha){
    break
  }
}
k_two_spring_0.05 = j-1

pvalues_spring3$k_two_spring_0.05 = rep(1,nrow(pvalues_spring3))
for (j in 1:k_two_spring_0.05){
  pvalues_spring3$k_two_spring_0.05[j] = 0
}
pvalues_spring2$k_two_spring_0.05 = pvalues_spring3$k_two_spring_0.05
