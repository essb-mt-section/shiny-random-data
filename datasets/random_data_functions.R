library(tibble)
library(dplyr)
library(permute)
library(haven)

teaching_data <- function() {
  #
  # learning_data data set
  #

  # teaching_method: two teaching methods and control condition
  # city: city of the university
  # percent correct in a multiple choice test (with 4 choices) of statistics and cognitive psychology

  n = 30
  n_questions = 40

  city = rep(c("Leiden", "Rotterdam", "Utrecht", "Nijmegen", "Groningen"), n/5)

  # in percent
  teach = tibble(student=1:n,
                 teaching_method=rep(c("method_A", "method_B", "control"), n/3),
                 #study=c(rep("psychology", n/2), rep("edu_science", n/2)),
                 city = city[shuffle(city)],
                 time = "before",
                 statistics = round(rnorm(n, 25, sd=5),0),
                 cog_psy    = round(rnorm(n, 25, sd=5),0),
  )


  stat_effect_a = rnorm(n/3, 45, sd=10)
  stat_effect_b = rnorm(n/3, 35, sd=10)
  stat_effect_c = rnorm(n/3, 1, sd=5)
  cog_effect_a = rnorm(n/3, 60, sd=10)
  cog_effect_b = rnorm(n/3, 60, sd=10)
  cog_effect_c = rnorm(n/3, 1, sd=5)



  teach2 <- teach %>% mutate(
    time = "after",
    statistics = ifelse(teaching_method=="method_A", statistics+stat_effect_a,
                        ifelse(teaching_method=="method_B", statistics+stat_effect_b,
                               statistics+stat_effect_c )),
    cog_psy = ifelse(teaching_method=="method_A", cog_psy+cog_effect_a,
                     ifelse(teaching_method=="method_B", cog_psy+cog_effect_b,
                            cog_psy+cog_effect_c ))
  )

  teach <- rbind(teach, teach2) %>%
    mutate(
      statistics = ifelse(statistics>100, 100, statistics),
      cog_psy = ifelse(cog_psy>100, 100, cog_psy),
      # convert percent correct to n_questions and round
      statistics = round(n_questions/100*statistics),
      cog_psy = round(n_questions/100*cog_psy),
    )

  #teach$statistics[n+3] = NA
  #teach$cog_psy[3] = NA

  return(teach)
}


simulate_priming_data <- function() {

  # simulate priming data
  sub_mean =rnorm(1, mean=800, sd=100) # between variance
  primg_effect = rnorm(1, mean=10, sd=3)
  n_trials = 40
  n_subjects = 30
  s_id = 23

  priming_data = data.frame()
  for (s_id in 1:n_subjects) {
    unrelated = tibble(subject_id = s_id,
                       trial = 0,
                       stimulus = "word",
                       condition = "unrelated",
                       rt = rnorm(n_trials, mean=sub_mean, sd=100)) # with variance
    related = tibble(subject_id = s_id,
                     trial = 0,
                     stimulus = "word",
                     condition = "related",
                     rt = rnorm(n_trials, mean=sub_mean-primg_effect, sd=100)) # with variance

    nonword = tibble(subject_id = s_id,
                     trial = 0,
                     stimulus = "non_word",
                     condition = "none",
                     rt = rnorm(n_trials*2, mean=sub_mean-50, sd=50)) # with variance


    subject_data = rbind(related, unrelated, nonword) %>%
      mutate(trial = shuffle(1:(4*n_trials)),
             rt = round(rt)) %>%
      arrange(trial)
    priming_data = rbind(priming_data, subject_data)
  }
  return(priming_data)
}

sample_from_sav <- function(file, nsamples) {
  ## just an example, please look at original SPSS syntax and adapt sampling procedure 
  rtn = read_sav(file) %>%
    slice_sample(n=nsamples) %>%
    select(-rv, -rv2)
  return(rtn)
}
