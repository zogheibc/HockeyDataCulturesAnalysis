##change point analysis
##USING BAYESCHANGE PACKAGE 
##https://cran.r-project.org/web/packages/BayesChange/BayesChange.pdf
##https://cran.r-project.org/web/packages/BayesChange/vignettes/tutorial.html
##https://arxiv.org/abs/2511.04785 


##TO DO: 
##- read up on and justify choices of parameters
##- multivariate analysis -> INCLUDE TYPE OF PUBLICATION AND SEE IF CHANGEPOINTS DIFFER BY TYPE
##- try running with another package maybe?? see if results converge??

##import packages
library(tidyverse)
library(dplyr)
library(readr)
library(readxl)

#install.packages("BayesChange")
library(BayesChange)

##load combined spreadsheet of documents
combineddocs <- read_csv("ProcessedData/combineddocs.csv", 
                         col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                          Year = col_number()))
##set seed
set.seed(613)

##make table of papers per year
yearlypapers <- combineddocs |>
  mutate(Year = as.numeric(Year)) |>
  count(Year, name = "NumberOfPapers") |>
  arrange(Year)
yearlypapers <- yearlypapers |>
  mutate(Year = as.numeric(Year),
         NumberOfPapers = as.numeric(NumberOfPapers)) 
##add missing years to the dataset with 0 papers
yearlypapers <- yearlypapers |>
  complete(Year = 1978:2026, fill = list(NumberOfPapers = 0))

##get paper counts
papercounts <- yearlypapers$NumberOfPapers

##(optional) export papers per year df
write.csv(yearlypapers, "ProcessedData/yearlypapers.csv", row.names = FALSE)


##TO DO: before running, compare different parameters

##defaults
prior_default <- list(
  a = 1,
  b = 1,
  c = 1,
  prior_var_phi = 0.1,
  prior_delta_c = 1,
  prior_delta_d = 1
)
##more diffuse prior
prior_diffuse <- list(
  a = 0.5,
  b = 0.5,
  c = 0.5,
  prior_var_phi = 1,
  prior_delta_c = 0.5,
  prior_delta_d = 0.5
)
##more concentrated prior
prior_concentrated <- list(
  a = 2,
  b = 2,
  c = 2,
  prior_var_phi = 0.05,
  prior_delta_c = 2,
  prior_delta_d = 2
)
##alt priors
prior_alt <- list(
  a = 1,
  b = 2,
  c = 1,
  prior_var_phi = 0.2,
  prior_delta_c = 1,
  prior_delta_d = 2
)

##now run the model for each set of parameters
priors <- list(
  Default = prior_default,
  Diffuse = prior_diffuse,
  Concentrated = prior_concentrated,
  Alternative = prior_alt
)

cp_models <- lapply(
  priors,
  function(p) {
    detect_cp(
      data = yearlypapers$NumberOfPapers,
      n_iterations = 10000,
      n_burnin = 5000,
      q = 0.5,
      params = p,
      kernel = "ts",
      print_progress = TRUE,
      user_seed = 613
    )
  }
)

names(cp_models) <- names(priors)

##now extract changepoints for each of the 4 models
extract_changepoints <- function(model, years) {
  cp_estimate <- posterior_estimate(
    model,
    loss = "VI"
  )
  change_points <- which(
    cp_estimate[-1] != cp_estimate[-length(cp_estimate)]
  ) + 1
  years[change_points]
}
sensitivity_results <- lapply(
  cp_models,
  extract_changepoints,
  years = yearlypapers$Year
)
sensitivity_results
##print results into a table
sensitivity_table <- bind_rows(
  lapply(names(sensitivity_results), function(prior_name) {
    years <- sensitivity_results[[prior_name]]
    data.frame(
      Prior = prior_name,
      ChangepointYear = years
    )
  })
)
sensitivity_table

##compare posterior probabilities ax models
extract_posterior_probabilities <- function(model, years) {
  
  orders_post <- model$orders[
    (model$n_burnin + 1):model$n_iterations,
    ,
    drop = FALSE
  ]
  
  boundary_probability <- numeric(ncol(orders_post) - 1)
  
  for (i in 1:(ncol(orders_post) - 1)) {
    boundary_probability[i] <- mean(
      orders_post[, i] != orders_post[, i + 1]
    )
  }
  
  data.frame(
    Year = years[-1],
    PosteriorProbability = boundary_probability
  )
}

posterior_sensitivity <- bind_rows(
  lapply(names(cp_models), function(prior_name) {
    result <- extract_posterior_probabilities(
      cp_models[[prior_name]],
      yearlypapers$Year
    )
    
    result$Prior <- prior_name
    
    result
  })
)
##plot sensitivities
ggplot(
  posterior_sensitivity,
  aes(
    x = Year,
    y = PosteriorProbability
  )
) +
  geom_col() +
  facet_wrap(~ Prior, ncol = 1) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent
  ) +
  labs(
    x = "Year",
    y = "Posterior Probability of a Changepoint",
    title = "Prior Sensitivity of Posterior Changepoint Probabilities"
  ) +
  geom_text(
    data = subset(
      posterior_sensitivity,
      PosteriorProbability >= 0.25
    ),
    aes(label = Year),
    vjust = 0.3,
    hjust = 1.2,
    size = 2,
    angle = 90,
    color = "white") +
  geom_hline(
    yintercept = 0.25,
    linetype = "dashed",
    colour = "grey",
    linewidth = 0.5) +
  theme_minimal(base_size = 14)





##run bayesian changepoint analysis

##set parameters
params_uni <- list(a = 1, 
                   b = 1, 
                   c = 1, 
                   prior_var_phi = 0.1, 
                   prior_delta_c = 1, 
                   prior_delta_d = 1) 
##run MCMC 
cp_model <- detect_cp(data = papercounts,
                      n_iterations = 10000, 
                      n_burnin = 5000, ##burn in discards super early samples to remove bias from starting points
                      q = 0.5, 
                      params = params_uni, 
                      kernel = "ts", 
                      print_progress = TRUE, 
                      user_seed = 613)

##explore results
print(cp_model) 
summary(cp_model)
plot(cp_model, 
      type = "partition")

##estimate posterior change point partition
cp_estimate <- posterior_estimate(cp_model, 
                                  loss = "VI")
##estimate years that were changepoints
change_points <- which(cp_estimate[-1] != cp_estimate[-length(cp_estimate)] ) + 1 
change_point_years <- yearlypapers$Year[change_points] 
change_point_years

##add changepoints to dataframe (AKA dividing it into segments)
change_point_results <- yearlypapers |> 
  mutate(Segment = cp_estimate) 
change_point_results

##explore # of pubs in each segment
segment_summary <- change_point_results |> 
  group_by(Segment) |> 
  summarise( StartYear = min(Year), 
             EndYear = max(Year), 
             MeanPapers = mean(NumberOfPapers), 
             MedianPapers = median(NumberOfPapers), 
             TotalPapers = sum(NumberOfPapers), 
             NumberOfYears = n(), 
             .groups = "drop")
segment_summary

##plot results
ggplot(change_point_results, 
       aes(x = Year, y = NumberOfPapers)) + 
  geom_line(linewidth = 0.8) + 
  geom_point(size = 2) + 
  geom_vline(xintercept = change_point_years, 
             linetype = "dashed", 
             linewidth = 0.8 ) + 
  labs( x = "Year", 
        y = "Number of Papers", 
        title = "Bayesian Change-Point Analysis of Publication Counts") + 
  theme_minimal(base_size = 14)


##NOW we want to explore probability of each changepoint
##remove burn in
orders_post <- cp_model$orders[(cp_model$n_burnin + 1):cp_model$n_iterations, , drop = FALSE]
##identify segment boundaries in each mcmc iteration
boundary_probability <- numeric(ncol(orders_post) - 1) 
for (i in 1:(ncol(orders_post) - 1)) {
  boundary_probability[i] <- mean( orders_post[, i] != orders_post[, i + 1] )
  }
##make those boundaries into a dataframe
posterior_cp <- data.frame(Year = yearlypapers$Year[-1], 
                           PosteriorProbability = boundary_probability)
##rank potential changepoints
posterior_cp <- posterior_cp |> 
  arrange(desc(PosteriorProbability)) 
##plot posterior probabilities of changepoints
ggplot(posterior_cp, 
       aes(x = Year, 
           y = PosteriorProbability)) + 
  geom_col() + 
  labs(x = "Year", 
       y = "Posterior Probability of a Changepoint", 
       title = "Posterior Probability of Structural Change by Year") + 
  scale_y_continuous(limits = c(0, 1), 
                     labels = scales::percent) + 
  geom_text(aes(label = Year),
            hjust = -0.05,
            vjust = 0.25,
            size = 3,
            angle = 90) +
  theme_minimal(base_size = 14)






##run another changepoint analysis but with a log transform BECAUSE DATA ARE HEAVILY RIGHT SKEWED)
yearlypaperslog <- yearlypapers |>
  mutate(LogPapers = log1p(NumberOfPapers))

##same default parameters
params_uni <- list(
  a = 1, 
  b = 1, 
  c = 1, 
  prior_var_phi = 0.1, 
  prior_delta_c = 1, 
  prior_delta_d = 1
)

##run mcmc
cp_model_log <- detect_cp(
  data = yearlypaperslog$LogPapers,
  n_iterations = 10000, 
  n_burnin = 5000,
  q = 0.5, 
  params = params_uni, 
  kernel = "ts", 
  print_progress = TRUE, 
  user_seed = 613
)

##view results
summary(cp_model_log)
plot(
  cp_model_log, 
  type = "partition"
)

##estimate partitions
cp_estimate_log <- posterior_estimate(
  cp_model_log, 
  loss = "VI"
)

##what years were changepoints
change_points <- which(
  cp_estimate_log[-1] != cp_estimate_log[-length(cp_estimate_log)]
) + 1
change_point_years <- yearlypapers$Year[change_points]
change_point_years

##add changepoints (/segments) to df
change_point_results <- yearlypapers |> 
  mutate(
    Segment = cp_estimate_log
  )

##view number of pubs per segment
segment_summary <- change_point_results |> 
  group_by(Segment) |> 
  summarise(
    StartYear = min(Year), 
    EndYear = max(Year), 
    MeanPapers = mean(NumberOfPapers), 
    MedianPapers = median(NumberOfPapers), 
    TotalPapers = sum(NumberOfPapers), 
    NumberOfYears = n(), 
    .groups = "drop"
  )
segment_summary

##plot results
##(MODEL WAS ESTIMATED WITH THE LOGS BUT THE PLOT SHOWS THE ACTUAL COUNTS; just the changepoints should be different)
ggplot(
  change_point_results, 
  aes(x = Year, y = NumberOfPapers)
) + 
  geom_line(linewidth = 0.8) + 
  geom_point(size = 2) + 
  geom_vline(
    xintercept = change_point_years, 
    linetype = "dashed", 
    linewidth = 0.8
  ) + 
  labs(
    x = "Year", 
    y = "Number of Papers", 
    title = "Bayesian Change-Point Analysis of Publication Counts \n (LOG TRANSFORMED VERSION)"
  ) + 
  theme_minimal(base_size = 14)


##now see posterior prob of each changepoint
##remove burn-in
orders_post <- cp_model_log$orders[
  (cp_model_log$n_burnin + 1):cp_model_log$n_iterations, 
  ,
  drop = FALSE
]

##ID segment segment boundaries in each MCMC iteration
boundary_probability <- numeric(
  ncol(orders_post) - 1
)

for (i in 1:(ncol(orders_post) - 1)) {
  
  boundary_probability[i] <- mean(
    orders_post[, i] != orders_post[, i + 1]
  )
}

##make bounds into a dataframe
posterior_cp <- data.frame(
  Year = yearlypaperslog$Year[-1], 
  PosteriorProbability = boundary_probability
)

##rank potential changepoints
posterior_cp <- posterior_cp |> 
  arrange(desc(PosteriorProbability))

##plot probabilities
ggplot(
  posterior_cp, 
  aes(
    x = Year, 
    y = PosteriorProbability
  )
) + 
  geom_col() + 
  labs(
    x = "Year", 
    y = "Posterior Probability of a Changepoint", 
    title = "Posterior Probability of Structural Change by Year \n (LOG TRANSFORMED VERSION)"
  ) + 
  scale_y_continuous(
    limits = c(0, 1), 
    labels = scales::percent
  ) + 
  geom_text(
    aes(label = Year),
    hjust = -0.05,
    vjust = 0.25,
    size = 3,
    angle = 90
  ) +
  theme_minimal(base_size = 14)


