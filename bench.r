### setup
library(tidyverse)
library(quanteda)
library(stm)
library(furrr)

corp <- quanteda.corpora::download("data_corpus_guardian")
time <- c(start = Sys.time())

### Matrix
N <- 10e3
M <- 2000
X <- matrix(rnorm(N * M), N)
time <- c(time, matrix = Sys.time())

### preprocessing
test_dfm <- corp %>% 
  tokens() %>% 
  tokens_remove(stopwords("en")) %>% 
  dfm() %>% 
  dfm_trim(min_termfreq = 10)
time <- c(time, nlp_prep = Sys.time())

### STM
test_mod <- stm(test_dfm, K = 100)
time <- c(time, stm_single = Sys.time())

### STM-multi
plan(multicore, workers = 4)
many_models <- data_frame(K = seq(40, 60, 5)) %>%
  mutate(topic_model = future_map(K, ~stm(test_dfm, K = .,
                                          verbose = FALSE)))
time <- c(time, stm_multi = Sys.time())

### report
tibble(
  stage = names(time),
  time_stamp = time
) %>% 
  mutate(time = time_stamp - lag(time_stamp),
         id = 1) %>% 
  drop_na(time) %>% 
  select(stage, time) %>% 
  pivot_wider(names_from = "stage", values_from = "time") %>% 
  mutate(cpu = gsub("model name\t: ", "", unique(system("awk '/model name/' /proc/cpuinfo", intern = TRUE))),
         session = list(sessionInfo())) %>% 
  {
    if (file.exists("bench_results.rds")) {
      bind_rows(readRDS("bench_results.rds"), .)
    } else {
      .
    }
  } %>% 
  saveRDS("bench_results.rds")
