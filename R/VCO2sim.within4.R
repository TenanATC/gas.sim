#' VCO2_sim.within Determines the Probability that Two VCO2 Measures are the Same Within Days or Within an Exercise Trial
#'
#' This function models the reliability of VCO2 data from the Parvomedics 2400 TrueOne system as a truncated
#' univariate normal distribution based on the work by Macfarlane and Wu 2013
#' Additional credit goes to Stackexchange user Wolfgang for his example code of the overlapping coefficient
#'
#' @param a The first VCO2 value obtained
#' @param b The second VCO2 value obtained
#' @param plot False returns the probability they are same distribution. True returns a plot of overlapping distributions.
#' @export
#' @examples
#' put something here




VCO2_sim.within<- function(a, b, plot = F){
  
  library(ggplot2)
  library(msm)
  library(cubature)
  
  mu1 <- a
  mu2 <- b
  
  sd1 <- suppressWarnings(predict(vco2_geepred_ParvoWithin, data.frame(VCO2_mean = mu1)))
  sd2 <- suppressWarnings(predict(vco2_geepred_ParvoWithin, data.frame(VCO2_mean = mu2)))
  
  #throw warning that algorithm is extrapolating beyond original data
  if( any(mu1 > 2.10 | mu2 > 2.10) )  warning('Algorithm extrapolating beyond original data, results may be unreliable/unstable')
  
  
  #create funtion for determining integral
  min.f1f2 <- function(x, mu1, mu2, sd1, sd2, lowerb) {
    f1 <- dtnorm(x, mean=mu1, sd=sd1, lower=lowerb)
    f2 <- dtnorm(x, mean=mu2, sd=sd2, lower=lowerb)
    pmin(f1, f2)
  }
  
  
  
  
  
  if (plot == F ){
    #just return integral information
    areacurve <- pcubature(min.f1f2, 0, 1000, mu1=mu1, mu2=mu2, sd1=sd1, sd2=sd2, lowerb=0, fDim = 4)
    prob_diff <- areacurve[[1]][1]*100
    
    #integration occasionally returns tiny negative numbers, turn them to tiny positive numbers
    if( prob_diff < 0 ){prob_diff = 0.00000000000000000000000000000001}
    
    return(prob_diff)
    
    #This error is known, but can't determine root cause
    if( prob_diff == 0 ) stop('Known error in cubature integration, please re-run function to return non-zero answer.')
    
    
    
  }else if (plot ==T){
    #Return the simulation data for plotting
    #this needs to be fixed so all the data will plot correctly, just do it later
    xs <- seq(min(mu1 - 5*sd1, mu2 - 5*sd2), max(mu1 + 5*sd1, mu2 + 5*sd2), .0001)
    f1 <- dtnorm(xs, mean=mu1, sd=sd1, lower=0)
    f2 <- dtnorm(xs, mean=mu2, sd=sd2, lower=0)
    ys <- min.f1f2(xs, mu1=mu1, mu2=mu2, sd1=sd1, sd2=sd2, lowerb = 0)
    overall <- data.frame(f1,f2,xs, ys)
    
    dat_plot <-ggplot(overall, aes(x= xs, y=f1)) +
      geom_line() +
      geom_line(aes(x=xs, y=f2)) +
      geom_ribbon(aes(ymin =0, ymax=ys)) +
      labs(
        x= expression('VCO'[2]*' (L/min)'),
        y= 'Probability Density'
      ) +
      theme(panel.background = element_rect(fill = 'white'))
    
    #return(overall)
    return(dat_plot)
  }
  
  
}
