# ASSIGNMENT DESCRIPTION #####################################
# File:         wrangleData.R
# Project:      ECI588 Final Project 
# Author:       Jennifer Houchins
#
# Purpose:      Wrangle the data for text mining course final project
#


# 1 PROJECT SETUP  ################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman,readr, dplyr, tidyverse, tidyr, stringr,  
               tidytext, rstudioapi, tm, ggplot2, vader)

# the following chunk deals with setting the working directory and creating
setwd(dirname(getActiveDocumentContext()$path)) # Set working directory to source file location
datapath <- file.path(getwd(), "data")
if (!file.exists(datapath)){
  dir.create(datapath)
  print("The data sub directory has been created in the working directory.")
}

# DATA PREPROCESSING #######################################3

eoc_data <- read_csv("data/eoc_survey_cleancolnames.csv") %>% 
  select(response,
         EffectivenessRating,
         IntroDiscussionRating,
         CourseContentRating,
         PuttingItTogetherRating,
         ActionPlanRating,
         ResourcesRating,
         MostValuableAspects,
         ImprovementEstablishingNorms,
         ImprovementBringingSEL,
         ImprovementMaintainingConnection,
         ImprovementSelectDigResources,
         ImprovementSupportSpecialPop,
         ImprovementProvidingFeedback,
         PositiveChangesEffectiveness,
         HasAttemptedChanges,
         AnticipatedApplicationPractice,
         ChangesToPractice,
         CourseRecommendations,
         DesiredActivityCompletion,
         CourseHoursEstimate) %>%
  mutate(HasAttemptedChanges = as.numeric(gsub(" .*$","", HasAttemptedChanges))) %>% 
  # mutate(CourseHoursEstimate = as.numeric(sub(" .*$", "", CourseHoursEstimate))) %>%
  mutate(MostValuableAspects = replace_na(MostValuableAspects, "N/A"),
         ChangesToPractice = replace_na(ChangesToPractice, "N/A"),
         CourseRecommendations = replace_na(CourseRecommendations, "N/A")) %>%
  # AnticipatedApplicationPractice = replace_na(AnticipatedApplicationPractice, ""),
  # CourseHoursEstimate = replace_na(CourseHoursEstimate, 0)
  mutate(class = ifelse(EffectivenessRating == 5, "very ineffective",
                        ifelse(EffectivenessRating == 4, "ineffective",
                               ifelse(EffectivenessRating == 3, "neither",
                                      ifelse(EffectivenessRating == 2, "effective", "very effective"))))) %>%
  
  mutate(text = paste0(MostValuableAspects, " ",ChangesToPractice, " ", CourseRecommendations)) %>% 
  mutate(text = gsub('[[:digit:]]+', ' ', text)) %>%   # removes numbers from the text
  mutate(text = gsub('\\_+',' ', text)) %>% 
  mutate(text = tolower(text)) %>% 
  mutate(CourseCompletion = as.factor(ifelse(DesiredActivityCompletion == 0, "incomplete", "complete"))) %>% 
  filter(response != 89417)

# save a data snapshot of preprocessed data with group info
write.csv(eoc_data, "data/eoc_surveydata_wrangled_grouped.csv")

# MOST VALUABLE ASPECTS #################################

eoc_aspects <- eoc_data %>%
  select(response, MostValuableAspects) %>%
  slice(-1, -2) %>%
  na.omit() %>% 
  filter(!grepl("N/A", MostValuableAspects)) %>% 
  filter(!grepl("NA", MostValuableAspects)) %>% 
  filter(!grepl("N/a", MostValuableAspects)) %>% 
  filter(!grepl("Na", MostValuableAspects))

eoc_aspects

custom_stop <- data.frame("word" = c("nbsp", "NA", "N/A", "n", "a", "na", "NA NA",
                                     "aspect", "valuable" ))
eoc_tidy_aspects <- eoc_aspects %>% 
  unnest_tokens(word, MostValuableAspects) %>%
  anti_join(stop_words) %>% 
  anti_join(custom_stop)

eoc_tidy_aspects %>% 
  dplyr::count(word, sort = TRUE) %>% 
  filter(n > 100) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) + 
  geom_col() + 
  labs(y = NULL)

eoc_aspects_bigrams <- eoc_aspects %>%
  unnest_ngrams(bigram, MostValuableAspects, n = 2, to_lower = TRUE) 

eoc_separated_aspects <- eoc_aspects_bigrams %>%  
  separate(bigram, into = c("word1", "word2"), sep = " ") %>% 
  na.omit()

eoc_united_aspects <- eoc_separated_aspects %>%
  filter(!(word1 %in% stop_words$word), 
         !(word2 %in% stop_words$word)) %>%  
  filter(!(word1 %in% custom_stop$word), 
         !(word2 %in% custom_stop$word)) %>%
  unite(bigram, c(word1, word2), sep = " ")

eoc_united_aspects %>% 
  dplyr::count(bigram, sort = TRUE) %>% 
  filter(n > 15) %>% 
  mutate(word = reorder(bigram,n)) %>%
  ggplot(aes(n, bigram)) + 
  geom_col() + 
  labs(y = NULL)

token_counts_aspects <-  eoc_tidy_aspects %>% 
  dplyr::count(word, sort = TRUE)

bigram_counts_aspects <- eoc_united_aspects %>% 
  dplyr::count(bigram, sort = TRUE)

bigram_counts_aspects

eoc_word_counts_aspects <- eoc_tidy_aspects %>%
  dplyr::count(response, word, sort = TRUE) %>%
  mutate(proportion = n / sum(n))

eoc_word_counts_aspects

total_words <- eoc_word_counts_aspects %>%
  group_by(response) %>% 
  summarise(total = sum(n))

# ANTICIPATED CHANGES #################
anticipated_changes <- eoc_data %>%
  select(response, AnticipatedApplicationPractice) %>%
  mutate(AnticipatedApplicationPractice = gsub('&nbsp;', ' ', AnticipatedApplicationPractice)) %>% 
  mutate(AnticipatedApplicationPractice = gsub('&amp;', ' ', AnticipatedApplicationPractice)) %>%
  slice(-1, -2) %>%
  na.omit() %>% 
  filter(AnticipatedApplicationPractice != 'N/A')

anticipated_changes

# anticipated_stop <- data.frame("word" = c("nbsp", "NA", "N/A", "amp"))
# 
# anticipated_changes_aspects <- anticipated_changes %>% 
#   unnest_tokens(word, AnticipatedApplicationPractice) %>%
#   anti_join(stop_words) %>% 
#   anti_join(anticipated_stop)
# 
# anticipated_changes_aspects %>% 
#   dplyr::count(word, sort = TRUE)
# 
# anticipated_changes_aspects %>% 
#   dplyr::count(word, sort = TRUE) %>% 
#   filter(n > 10) %>%
#   mutate(word = reorder(word, n)) %>% 
#   ggplot(aes(n, word)) + 
#   geom_col() + 
#   labs(y = NULL)

changes_quotes <- anticipated_changes %>%
  select(AnticipatedApplicationPractice) %>% 
  filter(grepl('plan', AnticipatedApplicationPractice))

view(changes_quotes)

sample_n(changes_quotes, 20)

# CHANGES TO PRACTICE #################
practice_changes <- eoc_data %>%
  select(response, ChangesToPractice) %>%
  mutate(ChangesToPractice = gsub('&nbsp;', ' ', ChangesToPractice)) %>% 
  mutate(ChangesToPractice = gsub('&amp;', ' ', ChangesToPractice)) %>%
  slice(-1, -2) %>%
  na.omit() %>% 
  filter(ChangesToPractice != 'N/A')

practice_changes

eoc_tidy_changes <- practice_changes %>% 
  unnest_tokens(word, ChangesToPractice) %>%
  anti_join(stop_words)

eoc_tidy_changes %>% 
  dplyr::count(word, sort = TRUE) %>% 
  filter(n > 150) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) + 
  geom_col() + 
  labs(y = NULL)

eoc_changes_bigrams <- practice_changes %>%
  unnest_ngrams(bigram, ChangesToPractice, n = 2, to_lower = TRUE) 

eoc_separated_changes <- eoc_changes_bigrams %>%  
  separate(bigram, into = c("word1", "word2"), sep = " ") %>% 
  na.omit()

eoc_united_changes <- eoc_separated_changes %>%
  filter(!(word1 %in% stop_words$word), 
         !(word2 %in% stop_words$word)) %>%
  unite(bigram, c(word1, word2), sep = " ")

eoc_united_changes %>% 
  dplyr::count(bigram, sort = TRUE) %>% 
  filter(n > 15) %>% 
  mutate(word = reorder(n, bigram)) %>%
  ggplot(aes(n, bigram)) + 
  geom_col() + 
  labs(y = NULL)

token_counts_changes <-  eoc_tidy_changes %>% 
  dplyr::count(word, sort = TRUE)

bigram_counts_changes <- eoc_united_changes %>% 
  dplyr::count(bigram, sort = TRUE)

bigram_counts_changes

eoc_word_counts_changes <- eoc_tidy_changes %>%
  dplyr::count(response, word, sort = TRUE) %>%
  mutate(proportion = n / sum(n))

eoc_word_counts_changes

total_words <- eoc_word_counts_changes %>%
  group_by(response) %>% 
  summarise(total = sum(n))


# RECOMMENDATIONS FOR IMPROVEMENT

anticipated_changes <- eoc_data %>%
  select(response, AnticipatedApplicationPractice) %>%
  mutate(AnticipatedApplicationPractice = gsub('&nbsp;', ' ', AnticipatedApplicationPractice)) %>% 
  mutate(AnticipatedApplicationPractice = gsub('&amp;', ' ', AnticipatedApplicationPractice)) %>%
  slice(-1, -2) %>%
  na.omit() %>% 
  filter(AnticipatedApplicationPractice != 'N/A')

anticipated_changes

# anticipated_stop <- data.frame("word" = c("nbsp", "NA", "N/A", "amp"))
# 
# anticipated_changes_aspects <- anticipated_changes %>% 
#   unnest_tokens(word, AnticipatedApplicationPractice) %>%
#   anti_join(stop_words) %>% 
#   anti_join(anticipated_stop)
# 
# anticipated_changes_aspects %>% 
#   dplyr::count(word, sort = TRUE)
# 
# anticipated_changes_aspects %>% 
#   dplyr::count(word, sort = TRUE) %>% 
#   filter(n > 10) %>%
#   mutate(word = reorder(word, n)) %>% 
#   ggplot(aes(n, word)) + 
#   geom_col() + 
#   labs(y = NULL)

changes_quotes <- anticipated_changes %>%
  select(AnticipatedApplicationPractice) %>% 
  filter(grepl('plan', AnticipatedApplicationPractice))

view(changes_quotes)

sample_n(changes_quotes, 20)

# CHANGES TO PRACTICE #################

participant_recommendations <- eoc_data %>%
  select(response, EffectivenessRating, CourseRecommendations) %>%
  mutate(CourseRecommendations = gsub('&nbsp;', ' ', CourseRecommendations)) %>% 
  mutate(CourseRecommendations = gsub('&amp;', ' ', CourseRecommendations)) %>%
  slice(-1, -2) %>%
  na.omit() %>% 
  filter(CourseRecommendations != 'N/A', 
         CourseRecommendations != 'n/a',
         EffectivenessRating >= 3)


recs_stop <- data.frame("word" = c("n/a", "N/A", "N/a"))

eoc_tidy_recs <- participant_recommendations %>%
  unnest_tokens(word, CourseRecommendations) %>%
  anti_join(stop_words) %>% 
  anti_join(recs_stop)

eoc_tidy_recs %>% 
  dplyr::count(word, sort = TRUE) %>% 
  filter(n > 2) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) + 
  geom_col() + 
  labs(y = NULL)

token_counts_recs <-  eoc_tidy_recs %>% 
  dplyr::count(word, sort = TRUE)

eoc_word_counts_recs <- eoc_tidy_recs %>%
  dplyr::count(EffectivenessRating, word, sort = TRUE) %>%
  mutate(proportion = n / sum(n))

eoc_word_counts_recs

total_words <- eoc_word_counts_recs %>%
  group_by(EffectivenessRating) %>% 
  summarise(total = sum(n))

recs_quotes <- participant_recommendations %>% 
  select(EffectivenessRating, CourseRecommendations) %>% 
  filter(EffectivenessRating == 3,
         CourseRecommendations != 'none',
         CourseRecommendations != 'N/a')

view(recs_quotes)

sample_n(recs_quotes, 20)

# SENTIMENT ANALYSIS #######################

#nrc

nrc <- get_sentiments("nrc")

survey_stop <- data.frame("word" = c("nbsp", "NA", "N/A", "n/a", "na", "amp"))

participant_tokens <- eoc_data %>% 
  select(response, text) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  anti_join(survey_stop)

sentiment_nrc <- inner_join(participant_tokens, nrc, by = "word")

summary_nrc <- sentiment_nrc %>% 
  count(sentiment, sort = TRUE) %>% 
  spread(sentiment, n) %>%
  mutate(lexicon = "nrc") %>%
  relocate(lexicon)

summary_nrc

# vader

summary_vader <- vader_df(eoc_data$text) 

vader_results <- summary_vader %>% 
  mutate(sentiment = ifelse(compound > 0, "positive", 
                            ifelse(compound < 0, "negative", "neutral"))) %>%
  count(sentiment, sort = TRUE) %>% 
  na.omit() %>% 
  mutate(proportion = n / sum(n)) %>% 
  mutate(lexicon = "Vader Sentiment")

courseRatings_count <- eoc_data %>%
  mutate(sentiment = ifelse(EffectivenessRating == 5, "negative",
                            ifelse(EffectivenessRating == 4, "negative",
                                   ifelse(EffectivenessRating == 3, "neutral",
                                          ifelse(EffectivenessRating == 2, "positive", "positive"))))) %>%
  dplyr::count(sentiment, sort = TRUE) %>%
  na.omit() %>% 
  mutate(proportion = n / sum(n)) %>% 
  mutate(lexicon = "User Rating Sentiment") 

comparison_df <- rbind(vader_results, courseRatings_count)

ggplot(comparison_df, aes(x = sentiment, y = proportion, fill = sentiment)) +
  geom_col() +
  facet_grid(. ~ lexicon) +
  theme_minimal() +
  theme(legend.position="none") +
  xlab("Sentiment") +
  ylab("Proportion")
