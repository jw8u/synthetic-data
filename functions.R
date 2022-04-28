syndatafunction = function(m){
  syndatalist = list()
  for (j in 1:m){
    synfunction = syn(data, method = 'cart', visit.sequence = c(1,2))
    syndatalist[[j]] = synfunction$syn
  }
  return(syndatalist)
}

syndatafunction_p = function(m){
  syndatalist = list()
  for (j in 1:m){
    synfunction = syn(data, method = 'parametric', visit.sequence = c(1,2))
    syndatalist[[j]] = synfunction$syn
  }
  return(syndatalist)
}

syndatafunction_norm = function(m){
  syndatalist = list()
  for (j in 1:m){
    synfunction = syn(data, method = 'norm', visit.sequence = c(1,2))
    syndatalist[[j]] = synfunction$syn
  }
  return(syndatalist)
}

conf = function(data){
  fit = glm(data[,1] ~ ., data = data[,3:ncol(data)], family = binomial(probit))
  conf = data.frame(summary(fit)$coefficients[-1,1:2],confint(fit)[-1,1:2])
  colnames(conf) = c('est_syn','se_syn','lci_syn','uci_syn')
  return(conf)
}

conf2 = function(data){
  fit = lm(data[,2] ~ ., data = data[,3:ncol(data)])
  conf = data.frame(summary(fit)$coefficients[-1,1:2],confint(fit)[-1,1:2])
  colnames(conf) = c('est_syn','se_syn','lci_syn','uci_syn')
  return(conf)
}

overlap = function(orig, syn){
  overlaplist = list()
  overlaplist = 0.5*((pmin(orig[,4],syn[,4]) - pmax(orig[,3],syn[,3]))/(orig[,4] - orig[,3]) +
      (pmin(orig[,4],syn[,4]) - pmax(orig[,3],syn[,3]))/(syn[,4] - syn[,3]))
  overlapdf = t(data.frame(overlaplist))
  colnames(overlapdf) = rownames(orig)
  return(overlapdf)
}

standardized_difference = function(orig, syn){
  sd_list = (abs(orig[,1] - syn[,1]))/orig[,2]
  sd_df = t(data.frame(sd_list))
  colnames(sd_df) = rownames(orig)
  return(sd_df)
}

generate_disclosure_risk2 = function(){
  set.seed(001)
  m_list = rep(NA,30)
  disclosure_risk_list = rep(NA,30)
  for (i in 1:30){
    m_list[i] = paste0('m=',i)
    disclosure_risk_list[i] = disclosure_risk(n,i,syndatafunction(i))
  }
  disclosure_risk_table = t(as.data.frame(disclosure_risk_list))
  colnames(disclosure_risk_table) = m_list
  plot(1:30, disclosure_risk_list)
  return(t(disclosure_risk_table))
}

generate_disclosure_risk = function(){
  set.seed(001)
  disclosure_risk_list = rep(NA,10)
  for (i in 1:10){
    disclosure_risk_list[i] = disclosure_risk(n,i,syndatafunction(i))
  }
  plot(1:10, disclosure_risk_list, 
       pch = 19,
       main = 'How Disclosure Risk varies with m using CART method', 
       xlab = 'Number of synthetic datasets generated (m)',
       ylab = 'Disclosure Risk')
}

generate_overlap = function(){
  set.seed(001)
  overlap_list1 = overlap_list2 = overlap_list3 = overlap_list4 =
    overlap_list1_p = overlap_list2_p = overlap_list3_p = overlap_list4_p = 
    rep(NA,10)
  
  for (i in 2:10){
    overlap_list1[i] = mean(replicate(25,overlap(conf(data),combine_syn(i,syndatafunction(i)))[,1]))
    overlap_list2[i] = mean(replicate(25,overlap(conf(data),combine_syn(i,syndatafunction(i)))[,2]))
    overlap_list3[i] = mean(replicate(25,overlap(conf(data),combine_syn(i,syndatafunction(i)))[,3]))
    overlap_list4[i] = mean(replicate(25,overlap(conf(data),combine_syn(i,syndatafunction(i)))[,4]))
    overlap_list1_p[i] = mean(replicate(25,overlap(conf(data),combine_syn(i,syndatafunction_p(i)))[,1]))
    overlap_list2_p[i] = mean(replicate(25,overlap(conf(data),combine_syn(i,syndatafunction_p(i)))[,2]))
    overlap_list3_p[i] = mean(replicate(25,overlap(conf(data),combine_syn(i,syndatafunction_p(i)))[,3]))
    overlap_list4_p[i] = mean(replicate(25,overlap(conf(data),combine_syn(i,syndatafunction_p(i)))[,4]))
  }
  
  par(mfrow = c(2,2))
  plot(2:10, overlap_list1[2:10], type = 'l', col='red',
       main = 'Overlap measure for BMI',
       xlab = 'Number of synthetic datasets generated (m)',
       ylab = 'Overlap measure',
       ylim = c(0,1))
  lines(2:10, overlap_list1_p[2:10], col='green')
  plot(2:10, overlap_list2[2:10], type = 'l', col='red',
       main = 'Overlap measure for BFT',
       xlab = 'Number of synthetic datasets generated (m)',
       ylab = 'Overlap measure',
       ylim = c(0,1))
  lines(2:10, overlap_list2_p[2:10], col='green')
  plot(2:10, overlap_list3[2:10], type = 'l', col='red',
       main = 'Overlap measure for AC',
       xlab = 'Number of synthetic datasets generated (m)',
       ylab = 'Overlap measure',
       ylim = c(0,1))
  lines(2:10, overlap_list3_p[2:10], col='green')
  plot(2:10, overlap_list4[2:10], type = 'l', col='red',
       main = 'Overlap measure for Esophagitis',
       xlab = 'Number of synthetic datasets generated (m)',
       ylab = 'Overlap measure',
       ylim = c(0,1))
  lines(2:10, overlap_list4_p[2:10], col='green')
}




generate_standardized_difference = function(){
  set.seed(001)
  standardized_difference_list1 = standardized_difference_list2 =
    standardized_difference_list3 = standardized_difference_list4 =
    standardized_difference_list1_p = standardized_difference_list2_p =
    standardized_difference_list3_p = standardized_difference_list4_p = 
    rep(NA,10)
  
  for (i in 2:10){
    standardized_difference_list1[i] = 
      mean(replicate(25,standardized_difference(conf(data),combine_syn(i,syndatafunction(i)))[,1]))
    standardized_difference_list2[i] = 
      mean(replicate(25,standardized_difference(conf(data),combine_syn(i,syndatafunction(i)))[,2]))
    standardized_difference_list3[i] = 
      mean(replicate(25,standardized_difference(conf(data),combine_syn(i,syndatafunction(i)))[,3]))
    standardized_difference_list4[i] = 
      mean(replicate(25,standardized_difference(conf(data),combine_syn(i,syndatafunction(i)))[,4]))
    standardized_difference_list1_p[i] = 
      mean(replicate(25,standardized_difference(conf(data),combine_syn(i,syndatafunction_p(i)))[,1]))
    standardized_difference_list2_p[i] = 
      mean(replicate(25,standardized_difference(conf(data),combine_syn(i,syndatafunction_p(i)))[,2]))
    standardized_difference_list3_p[i] = 
      mean(replicate(25,standardized_difference(conf(data),combine_syn(i,syndatafunction_p(i)))[,3]))
    standardized_difference_list4_p[i] = 
      mean(replicate(25,standardized_difference(conf(data),combine_syn(i,syndatafunction_p(i)))[,4]))
  }
  
  par(mfrow = c(2,2))
  plot(2:10, standardized_difference_list1[2:10], type = 'l', col='red',
       main = 'Standardized Difference for BMI',
       xlab = 'Number of synthetic datasets generated (m)',
       ylab = 'Standardized Difference',
       ylim = c(0,8))
  lines(2:10, standardized_difference_list1_p[2:10], col='green')
  plot(2:10, standardized_difference_list2[2:10], type = 'l', col='red',
       main = 'Standardized Difference for BFT',
       xlab = 'Number of synthetic datasets generated (m)',
       ylab = 'Standardized Difference',
       ylim = c(0,8))
  lines(2:10, standardized_difference_list2_p[2:10], col='green')
  plot(2:10, standardized_difference_list3[2:10], type = 'l', col='red',
       main = 'Standardized Difference for AC',
       xlab = 'Number of synthetic datasets generated (m)',
       ylab = 'Standardized Difference',
       ylim = c(0,8))
  lines(2:10, standardized_difference_list3_p[2:10], col='green')
  plot(2:10, standardized_difference_list4[2:10], type = 'l', col='red',
       main = 'Standardized Difference for Esophagitis',
       xlab = 'Number of synthetic datasets generated (m)',
       ylab = 'Standardized Difference',
       ylim = c(0,8))
  lines(2:10, standardized_difference_list4_p[2:10], col='green')
}




