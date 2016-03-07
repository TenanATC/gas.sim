#' VO2sim Determines the Probability that Two VO2 Measures are the Same
#'
#' This function models the reliability of VO2 data from a particular system as univariate normal
#' based on the work by Crouter 2006
#' Additional credit goes to Stackexchange user Wolfgang for his example code of the overlapping coefficient
#'
#' @param a The first VO2 value obtained
#' @param b The second VO2 value obtained
#' @param system_a The system used, in quotes, to obtain measurement 'a'.
#' Accepts "parvo_2400" and "douglas_bag". Defaults to parvo_2400.
#' @param system_b The system used, in quotes, to obtain measurement 'b'.
#' Accepts "parvo_2400" and "douglas_bag". Defaults to parvo_2400.
#' @param plot False returns the probability they are same distribution. True returns a plot of overlapping distributions.
#' @export
#' @examples
#' put something here


#This may be a new VO2sim, we'll see
#Example comes from http://stats.stackexchange.com/questions/12209/percentage-of-overlapping-regions-of-two-normal-distributions


VO2_sim<- function(a, b, system_a='parvo_2400', system_b='parvo_2400', plot = F){

  mu1 <- a
  mu2 <- b

  if (system_a == 'parvo_2400'){sd1 <- predict(VO2_parvo_pred, data.frame(VO2_mean = mu1))}
  if (system_b == 'parvo_2400'){sd2 <- predict(VO2_parvo_pred, data.frame(VO2_mean = mu2))}
  if (system_a == 'douglas_bag'){sd1 <- predict(VO2_douglas_pred, data.frame(VO2_mean = mu1))}
  if (system_b == 'douglas_bag'){sd2 <- predict(VO2_douglas_pred, data.frame(VO2_mean = mu2))}


  #create funtion for determining integral
  min.f1f2 <- function(x, mu1, mu2, sd1, sd2) {
    f1 <- dnorm(x, mean=mu1, sd=sd1)
    f2 <- dnorm(x, mean=mu2, sd=sd2)
    pmin(f1, f2)
  }

  xs <- seq(min(mu1 - 5*sd1, mu2 - 5*sd2), max(mu1 + 5*sd1, mu2 + 5*sd2), .0001)
  f1 <- dnorm(xs, mean=mu1, sd=sd1)
  f2 <- dnorm(xs, mean=mu2, sd=sd2)
  ys <- min.f1f2(xs, mu1=mu1, mu2=mu2, sd1=sd1, sd2=sd2)

  if (plot == F ){
    #just return integral information
    areacurve <- sum(diff(xs) * (head(ys,-1)+tail(ys,-1)))/2
    prob_diff <- areacurve*100

    return(prob_diff)
  }else if (plot ==T){
    #Return the simulation data for plotting
    #this needs to be fixed so all the data will plot correctly, just do it later

    overall <- data.frame(f1,f2,xs, ys)

    dat_plot <-ggplot(overall, aes(x= xs, y=f1)) +
      geom_line() +
      geom_line(aes(x=xs, y=f2)) +
      geom_ribbon(aes(ymin =0, ymax=ys)) +
      labs(
        x= expression('VO'[2]*' (L/min)'),
        y= 'Probability Density'
      ) +
      theme(panel.background = element_rect(fill = 'white'))

    #return(overall)
    return(dat_plot)
  }


}
