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
library(ggpubr)

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

# will use this function to determine the proportion of contradictions in each
# pairing of scenarios

prop_contradict <- function(output_a, output_b) {
  prop <- mean((output_a > 40 & output_b > 40) |
                 (output_b < -40 & output_a < -40))
  return(prop)
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
    nvar <- 5
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
                                              sqrt(ctry_a_params$var[x])))
    cauchy_scores_b <- sapply(1:length(mus_b), 
                             function(x) rcauchy(1, ctry_b_params$mu[x], 
                                                sqrt(ctry_b_params$var[x])))
    
    # want to put our individual variable data together in a table... will
    # truncate scores at the end since maybe we want certain respects to allow
    # negative/positive indicators to build up
    
    these_scores <- tibble(country = rep(c('country_a', 'country_b'), each = nvar),
                           var = rep(1:nvar, times = 2),
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
    summarise(cauchy_output = mean(cauchy_scores),
              unif_output = mean(unif_scores),
              norm_output = mean(norm_scores),
              .groups = 'drop') %>% 
    mutate(across(cauchy_output:norm_output, 
                  function(x) ifelse(abs(x)>100, sign(x)*100, x))) %>% 
    select(rep, country, cauchy_output:norm_output)
    
  # now I want to determine which scenarios flagged danger with these output
  # scores... will first look at identical named distributions on each side
  
  output_a <- output_scores %>% 
    filter(country == 'country_a') %>% 
    select(-country)
  output_b <- output_scores %>% 
    filter(country == 'country_b') %>% 
    select(-country)
  wide_output <- inner_join(output_a, output_b, by = 'rep', suffix = c('_a', '_b'))
  
  results_props <- wide_output %>% 
    summarize(contra_both_unif = prop_contradict(unif_output_a, unif_output_b),
              contra_both_norm = prop_contradict(norm_output_a, norm_output_b),
              contra_both_cauchy = prop_contradict(cauchy_output_a, cauchy_output_b),
              contra_unif_norm = mean(prop_contradict(unif_output_a, norm_output_b),
                                      prop_contradict(norm_output_a, unif_output_b)),
              contra_unif_cauchy = mean(prop_contradict(unif_output_a, cauchy_output_b),
                                        prop_contradict(cauchy_output_a, unif_output_b)),
              contra_norm_cauchy = mean(prop_contradict(norm_output_a, cauchy_output_b),
                                        prop_contradict(cauchy_output_a, norm_output_b))) %>% 
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
         Contradictions = prop) %>% 
  arrange(desc(Contradictions))

# graphing bell curves of the lineups

{
  p1 <- wide_output %>% 
    pivot_longer(c(cauchy_output_a, cauchy_output_b)) %>% 
    select(rep, name, value) %>% 
    ggplot(aes(value, fill = name)) +
    geom_histogram(position = 'identity', alpha = 0.5) +
    scale_x_continuous(limits = c(-101, 101)) +
    theme_minimal() +
    labs(title = 'Cauchy',
         x = '',
         y = '',
         fill = 'Country') +
    theme(text=element_text(family="Times New Roman", size=12)) +
    scale_fill_brewer(labels = c('Country A', 'Country B'),
                      palette = 'Dark2') +
    theme(legend.position = 'none')
  
  p2 <- wide_output %>% 
    pivot_longer(c(norm_output_a, norm_output_b)) %>% 
    select(rep, name, value) %>% 
    ggplot(aes(value, fill = name)) +
    geom_histogram(position = 'identity', alpha = 0.5) +
    scale_x_continuous(limits = c(-101, 101)) +
    theme_minimal() +
    labs(title = 'Normal',
         x = '',
         y = 'Count',
         fill = 'Country') +
    theme(text=element_text(family="Times New Roman", size=12)) +
    scale_fill_brewer(labels = c('Country A', 'Country B'),
                      palette = 'Dark2')

  p3 <- wide_output %>% 
    pivot_longer(c(unif_output_a, unif_output_b)) %>% 
    select(rep, name, value) %>% 
    ggplot(aes(value, fill = name)) +
    geom_histogram(position = 'identity', alpha = 0.5) +
    scale_x_continuous(limits = c(-101, 101)) +
    theme_minimal() +
    labs(title = 'Uniform',
         x = 'VRYAN output',
         y = '',
         fill = 'Country') +
    theme(text=element_text(family="Times New Roman", size=12)) +
    scale_fill_brewer(labels = c('Country A', 'Country B'),
                      palette = 'Dark2') +
    theme(legend.position = 'none')
  
  ggarrange(p1, p2, p3,
            nrow = 3,
            common.legend = TRUE,
            legend = 'bottom')
  ggsave('graphs/scenario_outputs.png',
         width = 6,
         height = 5)
}


# plotting random samples of lineups

{
  set.seed(100)
  
  # plotting Cauchy vs Cauchy lineups
  
  p1 <- output_scores %>% 
    select(rep:cauchy_output) %>% 
    group_by(country, rep) %>% 
    summarise(avg_score = mean(cauchy_output),
              .groups = 'drop') %>%
    pivot_wider(names_from = country, values_from = avg_score) %>% 
    mutate(contradiction = (country_a < -40 & country_b < -40) |
             (country_b > 40 & country_a > 40) |
             (country_a < -40 & country_b > 40)) %>% 
    sample_n(30) %>% 
    mutate(rep = 1:nrow(.)) %>% 
    pivot_longer(country_a:country_b, 
                 names_to = 'country',
                 values_to = 'output') %>% 
    ggplot(aes(output, rep, 
               group = rep,
               color = contradiction)) +
    geom_point(size = 0.7) +
    geom_line(size = 0.4) +
    theme_classic() +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    scale_color_manual(values = c('black', 'red3'),
                       labels = c('No Contradiction',
                                  'Contradiction')) +
    geom_vline(xintercept = -40, linetype = 'dashed', col = 'darkgray') +
    geom_vline(xintercept = 40, linetype = 'dashed', col = 'darkgray') +
    labs(title = 'Cauchy versus Cauchy',
         y = '',
         x = '',
         color = '') +
    theme(text=element_text(family="Times New Roman", size=12))
  
  # plotting Unif vs Unif lineups
  
  p2 <- output_scores %>% 
    select(rep:unif_output) %>% 
    group_by(country, rep) %>% 
    summarise(avg_score = mean(unif_output),
              .groups = 'drop') %>%
    pivot_wider(names_from = country, values_from = avg_score) %>% 
    mutate(contradiction = (country_a < -40 & country_b < -40) |
             (country_b > 40 & country_a > 40) |
             (country_a < -40 & country_b > 40)) %>% 
    sample_n(30) %>% 
    mutate(rep = 1:nrow(.)) %>% 
    pivot_longer(country_a:country_b, 
                 names_to = 'country',
                 values_to = 'output') %>% 
    ggplot(aes(output, rep, 
               group = rep,
               color = contradiction)) +
    geom_point(size = 0.7) +
    geom_line(size = 0.4) +
    theme_classic() +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    scale_color_manual(values = c('black', 'red3'),
                       labels = c('No Contradiction',
                                  'Contradiction')) +
    geom_vline(xintercept = -40, linetype = 'dashed', col = 'darkgray') +
    geom_vline(xintercept = 40, linetype = 'dashed', col = 'darkgray') +
    labs(title = 'Uniform versus Uniform',
         y = '',
         x = '',
         color = '') +
    theme(text=element_text(family="Times New Roman", size=12))
  
  # plotting Normal vs Normal lineups
  
  p3 <- output_scores %>% 
    select(rep:norm_output) %>% 
    group_by(country, rep) %>% 
    summarise(avg_score = mean(norm_output),
              .groups = 'drop') %>%
    pivot_wider(names_from = country, values_from = avg_score) %>% 
    mutate(contradiction = (country_a < -40 & country_b < -40)) %>% 
    sample_n(30) %>% 
    mutate(rep = 1:nrow(.)) %>% 
    pivot_longer(country_a:country_b, 
                 names_to = 'country',
                 values_to = 'output') %>% 
    ggplot(aes(output, rep, 
               group = rep,
               color = contradiction)) +
    geom_point(size = 0.7) +
    geom_line(size = 0.4) +
    theme_classic() +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    scale_color_manual(values = c('black', 'red3'),
                       labels = c('No Contradiction',
                                  'Contradiction')) +
    geom_vline(xintercept = -40, linetype = 'dashed', col = 'darkgray') +
    geom_vline(xintercept = 40, linetype = 'dashed', col = 'darkgray') +
    labs(title = 'Normal versus Normal',
         y = '',
         x = 'VRYAN Output',
         color = '') +
    theme(text=element_text(family="Times New Roman", size=12))
  
  ggarrange(p1, p2, p3,
            nrow = 3,
            common.legend = TRUE,
            legend = 'bottom')
}
ggsave('graphs/lineups.png',
       width = 4,
       height = 6)

  
