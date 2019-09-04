compute_interception <- function(rainfall,
                                 S,
                                 E_m,
                                 R_m,
                                 p_t,
                                 cc,
                                 S_t,
                                 model = "v1_package"){

  if(!model%in% c("v1_package","c_code","article","suggested")) stop("specify a model among: v1_package,c_code,article,suggested")
  # Composite parameters
  n_days <- length(rainfall)
  ## S_c:
  if(model == "v1_package" | model == "c_code") S_c = S*cc else S_c = S/cc

  ## P'g: amount of rainfall needed to saturate canopy layer
  P_prime_g <- ((-R_m*S_c)/E_m)*log(1-(E_m/R_m))

  # Is the storm sufficient to saturate canopy ?

  saturated_canopy <- (rainfall >= P_prime_g)
  saturated_trunks <- (rainfall >= S_t/p_t)
  # Components of the interception balance

  if(model == "v1_package"){
    S_c <- S*cc
    ## For a storm insufficient to saturate the canopy
    #nb.: here, saturated is either 0 or 1 thus this equation will be !=0 only for storms > P'g
    interception_1 <- !saturated_canopy*(cc * rainfall)
    ## Wetting up the canopy for a storm superior to P'g which saturates the canopy
    interception_2 <- saturated_canopy * ((cc * P_prime_g) - (cc * S_c))
    # Evaporation from saturation until rainfall ceases
    interception_3 <- saturated_canopy*((cc * E_m/R_m) * (rainfall - P_prime_g))
    # Evaporation after rainfall ceases
    interception_4 <- saturated_canopy * cc * S_c
    # Evaporation rom trunks for a storm insuficient to saturate the trunk
    interception_5 <- saturated_canopy*p_t * rainfall
    #why was it in the original code although it is totally useless?
    interception_6 = rep(0, n_days)
  }
  else if(model == "c_code"){
      S_c <- S*cc
      interception_1 <- !saturated_canopy*(cc * rainfall)
      interception_6 <- !saturated_canopy*(p_t) * rainfall
      interception_2 <- saturated_canopy * ((cc * P_prime_g) - (cc * S_c))
      interception_3 <- saturated_canopy*((cc * E_m/R_m) * (rainfall - P_prime_g))
      interception_4 <- saturated_canopy * cc * S_c
      interception_5 <- saturated_canopy*S_t

  }
  else if(model == "article"){
    S_c <- S/cc
    interception_1 <- !saturated_canopy*(cc * rainfall)
    interception_2 <- saturated_canopy * ((cc * P_prime_g) - (cc * S_c))
    interception_3 <- saturated_canopy*((cc * E_m/R_m) * (rainfall - P_prime_g))
    interception_4 <- saturated_canopy * cc * S_c
    interception_5 <- saturated_trunks*S_t
    interception_6 <- !saturated_trunks*(p_t) * rainfall
  }
  # else if(model == "gash_strict"){
  #   message("You selected 'gash_strict' model for interception computation. This is a litteral interpretation of Gash (1995)'s reformulation of his own 1979 model.")
  #   message("He said himself in the model table's caption : ''In the revised form no rainfall enters the trunk store when Pg < P'g.''")
  #
  #   S_c <- S/cc
  #
  #   interception_1 <- !saturated_canopy*(cc * rainfall)
  #   interception_2 <- saturated_canopy * ((cc * P_prime_g) - (cc * S_c))
  #   interception_3 <- saturated_canopy*((cc * E_m/R_m) * (rainfall - P_prime_g))
  #   interception_4 <- saturated_canopy * cc * S_c
  #   interception_5 <- saturated_canopy*saturated_trunks*S_t
  #   interception_6 <- saturated_canopy*!saturated_trunks*(p_t) * rainfall
  # }
  else if(model == "suggested"){
    message("You selected 'suggested' model for interception computation. This is a personal attempt of resolution of the inconsistencies (see appropriate document) of the interception submodel, by N. PAGE (2019, unpublished).")
    S_c <- S/cc

    interception_1 <- !saturated_canopy*(cc * rainfall)
    interception_2 <- saturated_canopy * ((cc * P_prime_g) - (cc * S_c))
    interception_3 <- saturated_canopy*((cc * E_m/R_m) * (rainfall - P_prime_g))
    interception_4 <- saturated_canopy * cc * S_c
    interception_5 <- saturated_canopy*saturated_trunks*S_t
    interception_6 <- saturated_canopy*!saturated_trunks*(p_t) * cc(rainfall - P_prime_g)
  }






  return(interception = interception_1 +
           interception_2 +
           interception_3 +
           interception_4 +
           interception_5 +
           interception_6
           )

  # return(data.frame(date = climate_data$date,
  #                   interception = interception_1 +
  #                     interception_2 +
  #                     interception_3 +
  #                     interception_4 +
  #                     interception_5))


}
