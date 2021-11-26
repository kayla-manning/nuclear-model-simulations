##################################
### *** NEXT STEPS ***
##################################

# - need to track how different the variances & estimates are between the scores
#   and/or distributions

# - should I hold the variance constant? to isolate the distributional effects?
# accomplish this by adding a and b columns from the uniform distribution,
# calculating the variance as (b-a)^2 / 12 & then using that variance to
# simulate from a normal distribution

##################################

# adding packages as needed

library(tidyverse)

# writing function to get the a & b parameters of a uniform after getting the
# radii... takes the vectors of mus and radii as input

# because cauchy support is 0 to infinity, I will take the absolute values of the
# parameters

get_params <- function(mus_vec, radii_vec) {
  params <- tibble(a = mus_vec - radii_vec,
                   b = mus_vec + radii_vec) %>% 
    mutate(mu = mus_vec,
           var = (b-a)^2 / 12)
  return(params)
}

##################################
### ILLUSTRATION
##################################

{
  # setting parameters
  
  {
    set.seed(100)
    max_score <- 100
    nsims <- 10^3
    nvar <- 4
    detailed_results <- tibble()
    i <- 1
  }
  for (i in 1:nsims) {
    
    # want to select the mean for each of my random variables
    
    mus_a <- runif(nvar, 0, max_score)
    mus_b <- -1 * mus_a
    
    # for now, I am assuming that we will draw from a uniform distribution... to get
    # the parameters, I will sample the width of each uniform from the distance
    # between mu and 100
    
    radii <- sapply(mus_a, function(x) runif(1, 0, 100-x))
    
    # using my function to get mean, variance, a, and b parameters in a table
    
    ctry_a_params <- get_params(mus_a, radii)
    ctry_b_params <- get_params(mus_b, radii)
    
    # getting the actual point estimates based off of these uniform distributions
    
    unif_scores_a <- sapply(1:length(mus_a), 
                            function(x) runif(1, ctry_a_params$a[x], 
                                              ctry_a_params$b[x]))
    unif_scores_b <- sapply(1:length(mus_b), 
                            function(x) runif(1, ctry_b_params$a[x], 
                                              ctry_b_params$b[x]))
    
    # getting normal scores with the appropriate parameters
    
    norm_scores_a <- sapply(1:length(mus_a), 
                            function(x) rnorm(1, ctry_a_params$mu[x], 
                                              sqrt(ctry_a_params$var[x])))
    norm_scores_b <- sapply(1:length(mus_b), 
                            function(x) rnorm(1, ctry_b_params$mu[x], 
                                              sqrt(ctry_b_params$var[x])))
    
    # now want cauchy scores... calculated shape and rate parameters by solving
    # system of equations with the variance and mean
    
    cauchy_scores_a <- sapply(1:length(mus_a), 
                             function(x) rcauchy(1, ctry_a_params$mu[x], 
                                                abs(ctry_a_params$mu[x])))
    cauchy_scores_b <- sapply(1:length(mus_b), 
                             function(x) rcauchy(1, ctry_b_params$mu[x], 
                                                abs(ctry_b_params$mu[x])))
    
    # want to put our individual variable data together in a table
    
    these_scores <- tibble(country = rep(c('country_a', 'country_b'), each = nvar),
                           var = rep(1:4, times = 2),
                           unif_scores = c(unif_scores_a, unif_scores_b),
                           norm_scores = c(norm_scores_a, norm_scores_b),
                           cauchy_scores = c(cauchy_scores_a, cauchy_scores_b),
                           mus = c(mus_a, mus_b),
                           vars = c((radii_a*2)^2 / 12,(radii_b*2)^2 / 12),
                           rep = i)
    
    detailed_results <- bind_rows(detailed_results, these_scores)
  }
  
  # calculating VRYAN output once I've run all of my simulations
  
  output_scores <- detailed_results %>% 
    group_by(country, rep) %>% 
    nest() %>% 
    ungroup() %>% 
    pivot_wider(names_from = country, values_from = data) %>% 
    mutate(unif_output_a = map_dbl(country_a, ~mean(.$unif_scores)),
           norm_output_a = map_dbl(country_a, ~mean(.$norm_scores)),
           cauchy_output_a = map_dbl(country_a, ~mean(.$cauchy_scores)),
           unif_output_b = map_dbl(country_b, ~mean(.$unif_scores)),
           norm_output_b = map_dbl(country_b, ~mean(.$norm_scores)),
           cauchy_output_b = map_dbl(country_b, ~mean(.$cauchy_scores)) * -1) %>% 
    select(rep, unif_output_a:cauchy_output_b)
  
  # now I want to determine which scenarios flagged danger with these output
  # scores... will first look at identical named distributions on each side
  
  output_scores %>% 
    summarize(a_less_than_b_unif = mean(unif_output_a < unif_output_b),
              a_less_than_b_norm = mean(norm_output_a < norm_output_b),
              a_less_than_b_cauchy = mean(cauchy_output_a < cauchy_output_b),
              b_danger_both_unif = mean(unif_output_a > 40 & unif_output_b < -40),
              b_danger_both_norm = mean(norm_output_a > 40 & norm_output_b < -40),
              b_danger_both_cauchy = mean(cauchy_output_a > 40 & cauchy_output_b < -40),
              contra_both_unif = mean(unif_output_a < -40 & unif_output_b < -40),
              contra_both_norm = mean(norm_output_a < -40 & norm_output_b < -40),
              contra_both_cauchy = mean(cauchy_output_a < -40 & cauchy_output_b < -40),
              contra_unif_norm = mean(unif_output_a < -40 & norm_output_b < -40),
              contra_unif_cauchy = mean(unif_output_a < -40 & cauchy_output_b < -40),
              contra_norm_unif = mean(norm_output_a < -40 & unif_output_b < -40),
              contra_norm_cauchy = mean(norm_output_a < -40 & cauchy_output_b < -40),
    ) %>% 
    pivot_longer(cols = everything(), names_to = 'scenario', values_to = 'prop')
}

##################################
### SAME MEANS
##################################

{
  # setting parameters
  
  {
    set.seed(100)
    max_score <- 100
    nsims <- 10^3
    nvar <- 4
    detailed_results <- tibble()
    i <- 1
  }
  
  for (i in 1:nsims) {
    
    # want to select the mean for each of my random variables
    
    mus_a <- runif(nvar, 0, max_score)
    mus_b <- -1 * mus_a
    
    # for now, I am assuming that we will draw from a uniform distribution... to get
    # the parameters, I will sample the width of each uniform from the distance
    # between mu and 100
    
    radii <- sapply(mus_a, function(x) runif(1, 0, 100-x))
    
    # using my function to get mean, variance, a, and b parameters in a table
    
    ctry_a_params <- get_params(mus_a, radii)
    ctry_b_params <- get_params(mus_b, radii)
    
    # getting the actual point estimates based off of these uniform distributions
    
    unif_scores_a <- sapply(1:length(mus_a), 
                            function(x) runif(1, ctry_a_params$a[x], 
                                              ctry_a_params$b[x]))
    unif_scores_b <- sapply(1:length(mus_b), 
                            function(x) runif(1, ctry_b_params$a[x], 
                                              ctry_b_params$b[x]))
    
    # getting normal scores with the appropriate parameters
    
    norm_scores_a <- sapply(1:length(mus_a), 
                            function(x) rnorm(1, ctry_a_params$mu[x], 
                                              sqrt(ctry_a_params$var[x])))
    norm_scores_b <- sapply(1:length(mus_b), 
                            function(x) rnorm(1, ctry_b_params$mu[x], 
                                              sqrt(ctry_b_params$var[x])))
    
    # now want cauchy scores... calculated shape and rate parameters by solving
    # system of equations with the variance and mean
    
    cauchy_scores_a <- sapply(1:length(mus_a), 
                            function(x) rcauchy(1, ctry_a_params$mu[x], 
                                              abs(ctry_a_params$mu[x])))
    cauchy_scores_b <- sapply(1:length(mus_b), 
                             function(x) rcauchy(1, ctry_b_params$mu[x], 
                                                abs(ctry_b_params$mu[x])))
    
    # want to put our individual variable data together in a table... will
    # truncate scores at the end since maybe we want certain respects to allow
    # negative/positive indicators to build up
    
    these_scores <- tibble(country = rep(c('country_a', 'country_b'), each = nvar),
                           var = rep(1:4, times = 2),
                           unif_scores = c(unif_scores_a, unif_scores_b),
                           norm_scores = c(norm_scores_a, norm_scores_b), 
                           cauchy_scores = c(cauchy_scores_a, cauchy_scores_b),
                           mus = c(mus_a, mus_b),
                           vars = c((radii*2)^2 / 12,(radii*2)^2 / 12),
                           rep = i)
    
    detailed_results <- bind_rows(detailed_results, these_scores)
  }
  
  # calculating VRYAN output once I've run all of my simulations
  
  output_scores <- detailed_results %>% 
    group_by(country, rep) %>% 
    nest() %>% 
    ungroup() %>% 
    pivot_wider(names_from = country, values_from = data) %>% 
    mutate(unif_output_a = map_dbl(country_a, ~mean(.$unif_scores)),
           norm_output_a = map_dbl(country_a, ~mean(.$norm_scores)),
           cauchy_output_a = map_dbl(country_a, ~mean(.$cauchy_scores)),
           unif_output_b = map_dbl(country_b, ~mean(.$unif_scores)),
           norm_output_b = map_dbl(country_b, ~mean(.$norm_scores)),
           cauchy_output_b = map_dbl(country_b, ~mean(.$cauchy_scores)) * -1) %>% 
    mutate(across(unif_output_a:cauchy_output_b, 
                  function(x) ifelse(abs(x)>100, sign(x)*100, x))) %>% 
    select(rep, unif_output_a:cauchy_output_b)
    
  # now I want to determine which scenarios flagged danger with these output
  # scores... will first look at identical named distributions on each side
  
  results_props <- output_scores %>% 
    summarize(a_less_than_b_unif = mean(unif_output_a < unif_output_b),
              a_less_than_b_norm = mean(norm_output_a < norm_output_b),
              a_less_than_b_cauchy = mean(cauchy_output_a < cauchy_output_b),
              b_danger_both_unif = mean(unif_output_a > 40 & unif_output_b < -40),
              b_danger_both_norm = mean(norm_output_a > 40 & norm_output_b < -40),
              b_danger_both_cauchy = mean(cauchy_output_a > 40 & cauchy_output_b < -40),
              contra_both_unif = mean(unif_output_a < -40 & unif_output_b < -40),
              contra_both_norm = mean(norm_output_a < -40 & norm_output_b < -40),
              contra_both_cauchy = mean(cauchy_output_a < -40 & cauchy_output_b < -40),
              contra_unif_norm = mean(unif_output_a < -40 & norm_output_b < -40),
              contra_unif_cauchy = mean(unif_output_a < -40 & cauchy_output_b < -40),
              contra_norm_unif = mean(norm_output_a < -40 & unif_output_b < -40),
              contra_norm_cauchy = mean(norm_output_a < -40 & cauchy_output_b < -40),
              ) %>% 
    pivot_longer(cols = everything(), names_to = 'scenario', values_to = 'prop')
}

results_props %>% 
  filter(str_detect(scenario, 'contra')) %>% 
  mutate(scenario = str_remove_all(scenario, '^contra_'),
         scenario = str_replace_all(scenario, '_', ' '),
         scenario = str_to_title(scenario),
         scenario = str_replace_all(scenario, 'Unif', 'Uniform'),
         scenario = str_replace_all(scenario, 'Norm', 'Normal')) %>% 
  rename(Scenario = scenario,
         Contradictions = prop)

# want to figure out how to graph the results

output_scores %>% 
  pivot_longer(unif_output_a:cauchy_output_b, names_to = 'scenario', values_to = 'output') %>% 
  mutate(country = str_sub(scenario, -1),
         scenario = str_extract(scenario, '[^_]+')) %>% 
  pivot_wider(names_from = country, values_from = output) %>% 
  mutate(diff = a-b) %>% 
  ggplot(aes(diff)) +
  geom_histogram() +
  facet_wrap(~scenario, scales = 'free') +
  geom_vline(xintercept = 80, color = 'red') +
  geom_vline(xintercept = -80, color = 'red') 
  # ggplot(aes(output, fill = country)) +
  # geom_histogram(position = 'identity',
  #                alpha = 0.5) +
  # facet_wrap(~scenario, scales = 'free')

output_scores %>% 
  pivot_longer(unif_output_a:cauchy_output_b, names_to = 'scenario', values_to = 'output') %>% 
  mutate(country = str_sub(scenario, -1),
         scenario = str_extract(scenario, '[^_]+')) %>% 
  filter(scenario == 'cauchy') %>% 
  ggplot(aes(output, fill = country)) +
  geom_histogram()

################################## 
# DIFFERENT MEANS & changing variance
##################################

{
  # setting parameters
  
  {
    set.seed(100)
    dist_btwn_means <- 0:100
    plus_minus <- c(-1, 1)
    var_match <- c(TRUE, FALSE)
    max_score <- 100
    nsims <- 10^2
    nvar <- 4
    detailed_results <- tibble()
    i <- 1
  }
  
  for (i in 1:nsims) {
    for (d in dist_btwn_means) {
      for (m in plus_minus) {
        
        # want to select the mean for each of my random variables
        
        mus_a <- runif(nvar, 0, max_score)
        mus_b <- -1 * (mus_a + m*d)
        
        # for now, I am assuming that we will draw from a uniform distribution... to get
        # the parameters, I will sample the width of each uniform from the distance
        # between mu and 100
        
        for (v in var_match) {
          
          if (v) {
            
          }
          
          else {
            radii_a <- sapply(mus_a, function(x) runif(1, 0, 100-x))
            radii_b <- sapply(mus_a, function(x) runif(1, 0, 100-x))
            
            # using my function to get mean, variance, a, and b parameters in a table
            
            ctry_a_params <- get_params(mus_a, radii_a)
            ctry_b_params <- get_params(mus_b, radii_b)
          }
        }
        
        # getting the actual point estimates based off of these uniform distributions
        
        unif_scores_a <- sapply(1:length(mus_a), 
                                function(x) runif(1, ctry_a_params$a[x], 
                                                  ctry_a_params$b[x]))
        unif_scores_b <- sapply(1:length(mus_b), 
                                function(x) runif(1, ctry_b_params$a[x], 
                                                  ctry_b_params$b[x]))
        
        # getting normal scores with the appropriate parameters
        
        norm_scores_a <- sapply(1:length(mus_a), 
                                function(x) rnorm(1, ctry_a_params$mu[x], 
                                                  sqrt(ctry_a_params$var[x])))
        norm_scores_b <- sapply(1:length(mus_b), 
                                function(x) rnorm(1, ctry_b_params$mu[x], 
                                                  sqrt(ctry_b_params$var[x])))
        
        # now want cauchy scores... calculated shape and rate parameters by solving
        # system of equations with the variance and mean
        
        cauchy_scores_a <- sapply(1:length(mus_a), 
                                 function(x) rcauchy(1, ctry_a_params$mu[x], 
                                                    abs(ctry_a_params$mu[x])))
        cauchy_scores_b <- sapply(1:length(mus_b), 
                                 function(x) rcauchy(1, ctry_b_params$mu[x], 
                                                    abs(ctry_b_params$mu[x])))
        
        # want to put our individual variable data together in a table
        
        these_scores <- tibble(country = rep(c('country_a', 'country_b'), each = nvar),
                               variable = rep(1:4, times = 2),
                               unif_scores = c(unif_scores_a, unif_scores_b),
                               norm_scores = c(norm_scores_a, norm_scores_b),
                               cauchy_scores = c(cauchy_scores_a, cauchy_scores_b),
                               dist_btwn_means = m*d,
                               mus = c(mus_a, mus_b),
                               vars = c((radii_a*2)^2 / 12,(radii_b*2)^2 / 12),
                               rep = i)
        
        detailed_results <- bind_rows(detailed_results, these_scores)
      }
      
      # calculating VRYAN output once I've run all of my simulations
      
      output_scores <- detailed_results %>% 
        mutate(across(unif_scores:cauchy_scores, 
                      function(x) ifelse(abs(x)>100, sign(x)*100, x))) %>% 
        group_by(country, rep) %>% 
        nest() %>% 
        ungroup() %>% 
        pivot_wider(names_from = country, values_from = data) %>% 
        mutate(unif_output_a = map_dbl(country_a, ~mean(.$unif_scores)),
               norm_output_a = map_dbl(country_a, ~mean(.$norm_scores)),
               cauchy_output_a = map_dbl(country_a, ~mean(.$cauchy_scores)),
               unif_output_b = map_dbl(country_b, ~mean(.$unif_scores)),
               norm_output_b = map_dbl(country_b, ~mean(.$norm_scores)),
               cauchy_output_b = map_dbl(country_b, ~mean(.$cauchy_scores)) * -1) %>% 
        select(rep, unif_output_a:cauchy_output_b)
      }
  }
  
  # now I want to determine which scenarios flagged danger with these output
  # scores... will first look at identical named distributions on each side
  
  output_scores %>% 
    summarize(a_less_than_b_unif = mean(unif_output_a < unif_output_b),
              a_less_than_b_norm = mean(norm_output_a < norm_output_b),
              a_less_than_b_cauchy = mean(cauchy_output_a < cauchy_output_b),
              b_danger_both_unif = mean(unif_output_a > 40 & unif_output_b < -40),
              b_danger_both_norm = mean(norm_output_a > 40 & norm_output_b < -40),
              b_danger_both_cauchy = mean(cauchy_output_a > 40 & cauchy_output_b < -40),
              contra_both_unif = mean(unif_output_a < -40 & unif_output_b < -40),
              contra_both_norm = mean(norm_output_a < -40 & norm_output_b < -40),
              contra_both_cauchy = mean(cauchy_output_a < -40 & cauchy_output_b < -40),
              contra_unif_norm = mean(unif_output_a < -40 & norm_output_b < -40),
              contra_unif_cauchy = mean(unif_output_a < -40 & cauchy_output_b < -40),
              contra_norm_unif = mean(norm_output_a < -40 & unif_output_b < -40),
              contra_norm_cauchy = mean(norm_output_a < -40 & cauchy_output_b < -40),
    ) %>% 
    pivot_longer(cols = everything(), names_to = 'scenario', values_to = 'prop')
  
  detailed_results %>% 
    drop_na(dist_btwn_means) %>% 
    group_by(country, rep, dist_btwn_means) %>% 
    mutate(var_match = v) %>% 
    nest() %>% 
    ungroup() %>% 
    pivot_wider(names_from = country, values_from = data) %>% 
    mutate(unif_output_a = map_dbl(country_a, ~mean(.$unif_scores)),
           norm_output_a = map_dbl(country_a, ~mean(.$norm_scores)),
           cauchy_output_a = map_dbl(country_a, ~mean(.$cauchy_scores)),
           unif_output_b = map_dbl(country_b, ~mean(.$unif_scores)),
           norm_output_b = map_dbl(country_b, ~mean(.$norm_scores)),
           cauchy_output_b = map_dbl(country_b, ~mean(.$cauchy_scores)) * -1) %>% 
    select(rep, dist_btwn_means, unif_output_a:cauchy_output_b) %>% 
    group_by(dist_btwn_means) %>% 
    summarize(a_less_than_b_unif = mean(unif_output_a < unif_output_b),
              a_less_than_b_norm = mean(norm_output_a < norm_output_b),
              a_less_than_b_cauchy = mean(cauchy_output_a < cauchy_output_b),
              b_danger_both_unif = mean(unif_output_a > 40 & unif_output_b < -40),
              b_danger_both_norm = mean(norm_output_a > 40 & norm_output_b < -40),
              b_danger_both_cauchy = mean(cauchy_output_a > 40 & cauchy_output_b < -40),
              contra_both_unif = mean(unif_output_a < -40 & unif_output_b < -40),
              contra_both_norm = mean(norm_output_a < -40 & norm_output_b < -40),
              contra_both_cauchy = mean(cauchy_output_a < -40 & cauchy_output_b < -40),
              contra_unif_norm = mean(unif_output_a < -40 & norm_output_b < -40),
              contra_unif_cauchy = mean(unif_output_a < -40 & cauchy_output_b < -40),
              contra_norm_unif = mean(norm_output_a < -40 & unif_output_b < -40),
              contra_norm_cauchy = mean(norm_output_a < -40 & cauchy_output_b < -40),
    ) %>% 
    pivot_longer(a_less_than_b_unif:contra_norm_cauchy, 
                 names_to = 'scenario', values_to = 'prop') %>% 
    filter(str_detect(scenario, 'contra_')) %>% 
    arrange(desc(prop))
    
}

