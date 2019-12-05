me_plot <- function(model, var1, var2, name1, name2, text_size=11){
  if(sum(class(model) == "lm") > 0){
    p <- length(coef(model))
  
    ## Indices
    v <- which(names(coef(model))==var1)
    i <- p
  
    ## Values of unemployment
    s <- seq(min(model$data[,var2]), max(model$data[,var2]), length.out = nrow(model$data))
  
    ## Calculate marg effect
    marg <- coef(model)[v] + coef(model)[i]*s
  
    ## Calculate the standard error
    se <- sqrt(vcov(model)[v,v] + (s^2)*vcov(model)[i,i] + 2*s*vcov(model)[v,i])
  
    ## Compute confidence intervals
    lo <- marg - qt((1-.95)/2,nrow(model$data)-p)*se
    hi <- marg + qt((1-.95)/2,nrow(model$data)-p)*se
  
    marg_plot <- ggplot() +
      labs(x=paste("Observed Values of ", name2, sep = ""), 
           y=paste("Marginal Effect of ", name1, sep = "")) +
      geom_line(aes(s, marg)) +
      geom_ribbon(aes(x=s, ymin=lo, ymax=hi), alpha=.5) +
      geom_rug(aes(model$data[,var2], marg), sides="b") +
      theme_bw(base_size = text_size) +
      theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(),legend.position="bottom")
  
  } else if(class(model)== "glmerMod"){
    p <-length(fixef(model))
    
    ## Indices
    v <- which(names(fixef(model))==var1)
    i <- p
    
    ## Data
    d <- model.matrix(model)[,-1]
    
    ## Values of unemployment
    s <- seq(min(d[,var2]), max(d[,var2]), length.out = nrow(d))
    
    ## Calculate marg effect
    marg <- fixef(model)[v] + fixef(model)[p]*s
    
    ## Calculate the standard error
    se <- sqrt(vcov(model)[v,v] + (s^2)*vcov(model)[p,p] + 2*s*vcov(model)[v,p])
    
    ## Compute confidence intervals
    lo <- marg - qt((1-.95)/2,nrow(d)-p)*se
    hi <- marg + qt((1-.95)/2,nrow(d)-p)*se
    
    marg_plot <- ggplot() +
      labs(x=paste("Observed Values of ", name2, sep = ""), 
           y=paste("Marginal Effect of ", name1, sep = "")) +
      geom_line(aes(s, marg)) +
      geom_ribbon(aes(x=s, ymin=lo, ymax=hi), alpha=.5) +
      geom_rug(aes(d[,var2], marg), sides="b") +
      theme_bw(base_size = text_size) +
      theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(),legend.position="bottom")
  } else(print("Model is not supported!"))
  
  return(marg_plot)
}