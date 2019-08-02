compute_interception <- function(rainfall,
                                 S,
                                 E_m,
                                 R_m,
                                 p_t,
                                 cc){
  # Composite parameters
  n_days <- length(rainfall)
  ## S_c:
  S_c <- S/cc
  ## P'g: amount of rainfall needed to saturate canopy layer
  P_prime_g <- ((-R_m*S_c)/E_m)*log(1-(E_m/R_m))

  # Is the storm sufficient to saturate canopy ?

  saturated <- (rainfall >= P_prime_g)

  # Components of the interception balance

  ## For a storm insufficient to saturate the canopy
  # interception_1 = rep(0, ndays)
  # interception_1[!saturated] <- cc * rainfall[!saturated]

  #nb.: here, saturated is either 0 or 1 thus this equation will be !=0 only for storms > P'g
  interception_1 <- !saturated*(cc * rainfall)

  ## Wetting up the canopy for a storm superior to P'g which saturates the canopy

  interception_2 <- saturated * ((cc * P_prime_g) - (cc * S_c))



  # Evaporation from saturation until rainfall ceases
  interception_3 <- saturated*((cc * E_m/R_m) * (rainfall - P_prime_g))

  # Evaporation after rainfall ceases
  interception_4 <- saturated * cc * S_c

  # Evaporation rom trunks for a storm insuficient to saturate the trunk
  interception_5 <- saturated*p_t * rainfall

  #why was it in the original code although it is totally useless?
  interception_6 = rep(0, n_days)







  return(interception = interception_1 +
                               interception_2 +
                               interception_3 +
                               interception_4 +
                               interception_5)

  # return(data.frame(date = climate_data$date,
  #                   interception = interception_1 +
  #                     interception_2 +
  #                     interception_3 +
  #                     interception_4 +
  #                     interception_5))


}
