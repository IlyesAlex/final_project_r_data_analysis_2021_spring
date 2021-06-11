---
title: "Final Project for the course: 2020/21/2 DPSZ16-KVAN-106 1"
author: "Alex Ilyés"
date: '2021 04 09 - 2021 06 11'
output: html_document
editor_options: 
  chunk_output_type: console
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
library(tidyverse)
library(tidytuesdayR)
theme_set(theme_light())
library(data.table)
library(curl)
library(viridis)
library(scales)
library(magrittr)
library(tidytext)
library(stringr)
library(tokenizers)
library(tidylo)
```

# Loading in the data

For this final assingment project, I will use the Anime dataset from TidyTuesday's 2019-04-03 occasion:

This dataset contains every anime listed on the MyAnimeList.org website, which is the most popular site for rating and logging animes, visited not only by fans, but a more mainstream audience too. The most important data the set contains are:
  - The name of the anime,
  - The studio that produces it,
  - The year and season it released in,
  - The number of episodes in a series,
  - The average runtime of one episode (or the runtime of a movie),
  - The number of people who scored, favorited or watched that anime,
  - The average score it received from the viewer base,
  - The age rating it was categorized under,
  - And two rank measures: the Rank based on weighted score, and the Popularity based on how many people interacted with the anime.

```{r}
library(readr)
anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")
```

# Data Cleaning

First, I clean our data to have an adjusted dataset that is suitable for later analysis. To this end, I do some filtering and modification:
- I am only interested in anime TV series, so I will filter out movies
- I am only interested in which year and season it premiered - derived from "premiered"
- For one anime there are multiple producers, but the most important contribution is from the animating studio. Therefore I do not want to have the data duplicated for each producer and studios, so I aggregate those variable. For modelling analysis, I will only deal with animes that have one animating studio.
- Also, there is an entry for every genre under the same title, I will also collapse this and only use the number of different genres, and I will call that a "diversity" measure.
- I also remove "min per ep" from the runtime

```{r}
#We do the initial cleanings - but we keep multiplicand values separate in a long format for exploratory analysis and some plotting
anime_cleaned_long = anime %>%
  filter(type == "TV") %>%
  drop_na(duration) %>%
  drop_na(premiered) %>%
  separate(premiered, c("premiere_season", "premiere_year")) %>%
  separate(duration, c("duration_min"), sep = " ", convert = TRUE) %>%
  mutate_at(vars(duration_min, premiere_year), funs(as.numeric)) %>%
  group_by(animeID, producers, studio) %>% mutate(genre_list=paste(genre, collapse=", ")) %>% ungroup() %>%
  mutate(diversity = count.fields(textConnection(genre_list), sep = ","))
  
#for modeling we will use the aggregated measures
anime_cleaned_aggregated = anime_cleaned_long %>%
  group_by(animeID, genre, studio) %>% mutate(producers_list=paste(producers, collapse=", ")) %>% ungroup() %>%
  group_by(animeID, producers, studio) %>% mutate(genre_list=paste(genre, collapse=", ")) %>% ungroup() %>%
  group_by(animeID, producers, genre) %>% mutate(studio_list=paste(studio, collapse=", ")) %>% ungroup() %>%
  select(-c(producers, genre, studio)) %>%
  distinct(animeID, .keep_all = TRUE)
#There are 280 out of 4260 animes with multiple animating studios. We will not have these in further analysis
length(anime_cleaned_aggregated$studio_list[grep(",",anime_cleaned_aggregated$studio_list)])
anime_cleaned_model = anime_cleaned_aggregated %>%
  filter(!grepl(",", studio_list))
```

## Cleaning for modelling

After the initial cleaning, for exploratory analysis I will use the long or the aggregated version of the data.
For the modelling part, I further check the histograms of the data:

```{r}
#Check histograms of numerical data
anime_cleaned_model  %>%
  select(c(duration_min, score, scored_by, members, favorites, premiere_year, diversity)) %>%
  gather() %>%
  ggplot(aes(value)) +
    geom_histogram(bins = 10) + 
    facet_wrap(~key, scales = 'free_x')
```

Based on the histograms, I can see some anomalies
- Duration is mostly around the 20 min mark, which makes sense, as it is the standard anime runtime.
- More and more animes premiere each year, with a very low amount of anime produced before 2000s
- There are some animes that are scored and favoured by extrememly many people - these animes are probably the ones that "got mainstream" and are not only watched by anime "otakus" (experts of anime), but casual watchers. But also, there are huge amount of animes that do not get much traction, these are only  known by their own fanbase.

For modeling, I would like to use a less heterogenous and chaotic dataset, to be able to asnwer more specific questions.
To this end I further investigate varables, starting with runtime.

```{r}
anime_cleaned_model %>%
  mutate(duration_min = as.factor(duration_min)) %>%
  count(duration_min) %>%
  arrange(desc(n))
```

Based on the count, we can see that the standard anime run time is 24 min (+- 1 min). I could categorize the data into standard, short and long animes, but it would not be that informative, as categorization would be arbitrary, and still be uneven towards standrad duration animes. Therefore I chose to only work with animes above the 20 minute mark and under 27, and not use duration in modelling analysis.

I will investigate premiere_year now:

```{r}
anime_cleaned_model %>%
  mutate(premiere_year = as.factor(premiere_year)) %>%
  count(premiere_year) %>%
  arrange(desc(n)) %>%
  mutate(premiere_year = as.integer(as.character(premiere_year))) %>%
  mutate(premiere_category=cut(premiere_year, breaks=c(-Inf, 1994, Inf), labels=c("before-1994","after-1994"))) %>%
  group_by(premiere_category) %>%
  summarise(sum = sum(n))
```

I decided to work with only the animes after 1994, as this year marks the release of Neon Genesis Evangelion, which forever changed anime: https://www.youtube.com/watch?v=HcPIHPkXx6s
I will filter based on the relase date of that show.

```{r}
#neon genesis evangelion's date
nge_date = as.Date(anime_cleaned_long$start_date[anime_cleaned_long$name=="Neon Genesis Evangelion"][1])
anime_cleaned_model_long = anime_cleaned_long %>%
  filter(duration_min >= 20 & duration_min < 27) %>%
  filter(duration_min != "Unknown") %>%
  filter(start_date >= nge_date) 
  
anime_cleaned_model = anime_cleaned_model %>%
  filter(duration_min >= 20 & duration_min < 27) %>%
  filter(duration_min != "Unknown") %>%
  filter(start_date >= nge_date)
  
```

After filtering for these two variables, I will check the audience variables again:

For modeling I would like to use members (members, who have the anime in their list) as an independent variable as a proxy for how popular the anime is. However as members is skewed, I will filter it with the help of percent quntilies and some data modification.

```{r}
anime_cleaned_model = anime_cleaned_model %>%
  #filter_at(vars(scored_by, members, favorites), ~. < quantile(., probs = 0.90)) %>%
  filter_at(vars(scored_by, members, favorites), ~. > quantile(., probs = 0.20)) %>%
  mutate(members = log(members)) %>%
  filter(rating != "None")
```


Now I check how the filtering worked out and see the histograms

```{r}
anime_cleaned_model %>%
  mutate(duration_min = as.factor(duration_min)) %>%
  count(duration_min) %>%
  arrange(desc(n))
anime_cleaned_model %>%
  mutate(premiere_year = as.factor(premiere_year)) %>%
  count(premiere_year) %>%
  arrange(desc(n)) %>%
  mutate(premiere_year = as.integer(as.character(premiere_year))) %>%
  mutate(premiere_category=cut(premiere_year, breaks=c(-Inf, 1994, Inf), labels=c("before-1994","after-1994"))) %>%
  group_by(premiere_category) %>%
  summarise(sum = sum(n))
anime_cleaned_model  %>%
  select(c(duration_min, score, scored_by, popularity, members, favorites, premiere_year, diversity)) %>%
  gather() %>%
  ggplot(aes(value)) +
    geom_histogram(bins = 10) + 
    facet_wrap(~key, scales = 'free_x')
```

Favorites and members (number of members who favorited or added anime to their personal list) are still skewed, however I won't filter further, and I won't use these metrics in the modeling.


# Exploratory Analysis

First off, I do some exploratory analysis, to know more about the dataset.

I will investigate the following:

1. See the top 15 favored and scored by animes
2. See the ratio of rank-popularity: those animes that are more popular than scored high; and those that are scored high, but less popular (overrated animes vs. underrated gems)
3. Shounen, seinen frequency in years - are there more boyish or girlish years?
4. Check the genre "fingerprint" of the most popular studios (based on: https://www.youtube.com/watch?v=EHqFDXa-sH4)
5. Is anime for kids - age rating and scores?
6. Are there seasonal trends? I will do a tokenized vocabulary analysis for words that describe shows from different seasons (based on: https://www.youtube.com/watch?v=3PecUbnuYC4)

## 1. Most mainstream animes

We will check the most mainstream animes based on how many people put them amongst their favorites and how many users scored them at all.

The 15 most scored animes are shown below:

```{r}
#We can see that there are "mainstream" animes favorited and scored by exeptionally many members. Let's see which anime titles these are:
anime_cleaned_aggregated %>%
  slice_max(scored_by, n = 15) %>%
  mutate(name = as.factor(name)) %>%
  arrange(desc(scored_by)) %>%
  mutate(name = fct_reorder(name, scored_by)) %>%
  ggplot( aes(x = name, y = scored_by, fill=name)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(y = "Number of scored_by", title = "The 15 most scored animes on MyAnimeList") +
    theme_light() +
    theme(legend.position = "none") +
    scale_y_continuous(breaks= pretty_breaks(), labels = comma) +
    scale_fill_viridis(discrete = TRUE)
```

The 15 most favorited animes are shown below:

```{r}
anime_cleaned_aggregated %>%
  slice_max(favorites, n = 15) %>%
  mutate(name = as.factor(name)) %>%
  arrange(desc(favorites)) %>%
  mutate(name = fct_reorder(name, favorites)) %>%
  ggplot( aes(x = name, y = favorites, fill=name)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(y = "Number of favorited", title = "The 15 most favorited animes on MyAnimeList") +
    theme_light() +
    theme(legend.position = "none") +
    scale_y_continuous(breaks= pretty_breaks(), labels = comma) +
    scale_fill_viridis(discrete = TRUE)
```

Nice! We can see some overlap:
 - Death Note, Full metal Alchemist, Shingeki no Kyojin, etc. these are animes that were not only very popular in Japan but also in the West
 
## 2. Rank vs. popularity

Both of these measures are ordinal scales that range from 1 (the best) to the number of all animes.
 - Rank is given by the MyAnimeList algorithm (weighing raw ratings), while 
 - Popularity is based on how many members listed the anime as viewed.
 
By comparing these measures, we could try to find out, whether there exist any "overrated pulp" (worse ranked, more popular) and "underrated gems" (better ranked, less popular).
To visualize this we subtract rank from popularity, this way we get negative or positive numbers:
 - Positive number means that popularity is bigger than rank (overrated)
 - 0 means popularity and rank are the same
 - Negative means that rank is bigger than popularity (underrated)

First I show the relationship between the two ranking measurements:

```{r}
anime_cleaned_aggregated %>%
  select(name, rating, rank, popularity) %>%
  ggplot(aes(rank, popularity, colour = rating)) +
    geom_point() +
    labs(x = "Rank on MyAnimeList (weighted scores)", y = "Popularity (how many people has it on their list)", title = "All anime series plotted rank by popularity, coloured by age rating", colour="Age rating") +
    theme_light() +
    scale_color_viridis(discrete = TRUE)
  
```

Then I show the 10 most over- and underrated animes.

```{r}
anime_cleaned_aggregated %>%
  select(name, scored_by, rank, popularity) %>%
  mutate(name = as.factor(name)) %>%
  mutate(ratedness_index = popularity-rank) %>%
  mutate(index_meaning = ifelse(ratedness_index >= 0, "underrated", "overrated")) %>%
  arrange(desc(ratedness_index)) %>% {
  rbind(head(., 10), tail(., 10))
  } %>%
  mutate(name = fct_reorder(name, ratedness_index)) %>%
  ggplot(aes(x=name, y=ratedness_index, fill = index_meaning)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_light() +
    labs(x = "Anime titles", y = "Difference index", title = "Difference of rank and popularity of the\n10 most underrated and overrated animes", fill="Difference value") +
    scale_y_continuous(breaks=c(-5000,0,5000), labels= c("more popular", "center", "better ranked"))
  
```

## 3. Shounen vs. shoujo over the years

There are two anime-specific genres in Japan, the Shounen and the Shoujo genre, which stand for the words "boy" and "girl" respectively. Therefore, these signify male- or female aduience oriented stories:
  - Shounen typically follow a young (12-18 yo.) boy through action-packed, fantastic stories about battles and trials.
  - Shoujo tipycally follow a young (12-18 yo.) girl through school-related, slice of life stories about romance and friendship.
We could check whether there are any shifts over the year in how prevalent these genres are. Even though there are more shounen animes in general, we may find years, with predominantly shoujo material.

First let's check all the genres, and then select the ones we are interested in:

```{r}
#We can see that both shounen and shoujo have a subtype "ai", these are reverse, so shounen ai is about boys, but made by women and involve romance (and vice versa) - we won't care about these in analysis
anime_cleaned_long %>%
  mutate(genre = as.factor(genre)) %>%
  count(genre) %>%
  view()
  
#We can also see that the minimum year of each differs, shoujos came later than shounens
anime_cleaned_long %>%
  select(premiere_year, genre) %>%
  mutate(genre = as.factor(genre)) %>%
  filter(genre == "Shounen" | genre == "Shoujo") %>%
  group_by(genre) %>%
  summarize(min_year = min(premiere_year))
  
```

```{r}
anime_cleaned_long %>%
  mutate(genre = as.factor(genre)) %>%
  filter(genre == "Shounen" | genre == "Shoujo") %>%
  filter(premiere_year > 1967) %>%
  count(premiere_year, genre) %>%
  group_by(premiere_year) %>%
  mutate(percent = n/sum(n)) %>%
  view() %>%
  ggplot(aes(premiere_year, percent, fill=genre)) +
    geom_area() +
    theme_light() +
    labs(x = "Year of premieres", y = "Percentage of genres", title = "'Gender wars' - ratio of Shounen and Shoujo titles over the years", fill="The gendered genres") +
    scale_y_continuous(labels = percent)
```

There is an interesting pattern here. If we look closely there are spikes of shoujo material starting at the beginning of decades, then there are other "spikes" at the middle and the end of decades. Although, in the 1970's this mostly means a 1 or 2 difference, as there were small amount of animes releasing.

There is a year 1976, where no Shoujo were relaesed at all. Besides, Shounen is ever increaseing in popularity, and the ratio shifts towards it continously.

## 4. Genre "fingerprint" of studios

Based on the TidyTuesday screencast (https://www.youtube.com/watch?v=EHqFDXa-sH4), I will try to find out whether there are any distinguishable or meaningful profiles of different popular animating studios. 
For animes, usually a studio name is very telling for fans, much like in Hollywood, where for instance you know that Blumhouse equals horrors and A24 equals arthouse. Let's find out these typicalities.

```{r}
#we can visualize the genre fingerprints
anime_studio_fingerprint = anime_cleaned_long %>%
  filter(!is.na(genre), !is.na(studio)) %>%
  mutate(studio = as.factor(studio)) %>%
  mutate(studio = forcats::fct_lump(studio, n = 10)) %>%
  filter(studio != "Other") %>%
  ungroup()
anime_studio_fingerprint %>%
  group_by(studio, genre) %>%
  count() %>% 
  ungroup() %>%
  group_by(studio) %>%
  mutate(n = n/sum(n)) %>%
  slice_max(order_by = n, n = 6) %>%
  ungroup() %>%
  ggplot(aes(n,reorder_within(genre, n, studio), fill = factor(genre))) +
    geom_col(stat='identity') +
    scale_x_continuous(labels=percent) +
    scale_y_reordered() +
    facet_wrap(~studio, scales="free_y") +
    theme_light() +
    theme(legend.position = "none") +
    scale_color_gradientn(colours = rainbow(20)) +
    labs(x = "Percentage of genres in portfolio", y = "Different anime genres", title = "The 'genre-fingerprint' of the top 10 most succesful anime studios")
#We can also list out the most popular title of each studio, to check if the fingerprints indicate the title well.
anime_studio_fingerprint %>%
  select(studio, name, scored_by) %>%
  group_by(studio) %>%
  mutate(max_scored = max(scored_by)) %>%
  filter(scored_by == max_scored) %>%
  ungroup() %>%
  distinct() %>%
  arrange(studio)
  
```

Interestingly, most studios have "comedy" in a very high ratio. This is probably due to the fact that these studios need to produce low-effort, highly successful shows (which comedies are, i.e. sitcoms in the West) to gather  money for bigger projects.

However, if we look at the other genres, there are clear distinctions appearing, which are diagnostic for their most successful anime:
  - A-1 - with Sword Art as its most successful anime - has fantasy and action very high, and SwordArt is an anime about an action-packed fantasy videogame universe.
  - J.C. Staff mostly produces school and romance type animes, and Toradora! is one of the most successful japanese high school slice-of-life pieces.
  - Toei Animation produces shounen, action and adventure, and Tokyo Ghoul is one of the most appreciated shounen titles in decades.
  - Sunrise has sci-fi on top, closely followed by mecha which is an anime-original genre depicting people fighting in mechanical suits. Their most successful title, Code Geass is actually one of the most known mecha anime out there.

## 5 Is anime for children?

We have data about the age rating of the shows, so we could visualize whether the highest scored or most popular animes are rated for children or above. 

```{r}
anime_cleaned_model %>%
  filter(!is.na(rating)) %>%
  filter(rating != "None") %>%
  separate(rating, c("rating", "explanation"), sep = " - ") %>%
  mutate(explanation = factor(explanation, levels=c("All Ages", "Children", "Teens 13 or older", "17+ (violence & profanity)", "Mild Nudity"))) %>%
  mutate(rating = as.factor(rating)) %>%
  ggplot(aes(rating, score, fill=explanation)) +
    geom_boxplot() +
    theme_light() +
    labs(y = "Scores from MyAnimeList", x = "Age rating", title="Is anime for children? - the scores of different age-rated animes", fill="Age rating annotation")
  
```

It appears that animes are scored better the more mature they get, but when it involves nudity and gets the R+ rating, the score declines.
The best animes according to scores are rated R or PG-13, which include the teens and young adult oriented shows, who are the major part of the audience.
Based on the figures, animes are appreciated more when they are not for children and involve more mature conflicts.

## 6. Are there seasonal trends in terms of narratives?

For this analysis, I use the approach taken from the TidyTuesday screencast (https://www.youtube.com/watch?v=3PecUbnuYC4), whereby one can use TidyText to tokenize running text and investigate the vocabulary and word usage of certain groups.
I will check whether there exists any trend that could differentiate the word usage of anime description starting in different seasons. E.g. one could expect to have more grim animes for the winter, or more lighthearted ones for the summer.

```{r}
#I noticed that synopsis sometimes have some indication of source - we investigate and remove it
anime_cleaned_aggregated %>%
  select(name, synopsis) %>%
  unnest_tokens(sentence, synopsis, token="sentences") %>%
  group_by(name) %>% 
  summarise(across(everything(), last)) %>%
  ungroup() %>%
  count(sentence) %>%
  arrange(desc(n)) %>%
  view()
```

```{r}
#We can see that synopses often have a source at the end either starting with "( or [". I will remove these with regex
anime_text_cleaned = anime_cleaned_aggregated %>%
  select(name, premiere_season, synopsis) %>%
  filter(!is.na(synopsis)) %>%
  unnest_tokens(sentence, synopsis, token="sentences") %>%
  mutate(sentence = as.character(sentence)) %>%
  mutate(truth = ifelse(grepl("^\\(source|\\[source|\\[written", sentence), "true", "false")) %>%
  filter(truth != "true")
#I will test the removal
anime_text_cleaned %>%
  group_by(name) %>% 
  summarise(across(everything(), last)) %>%
  ungroup() %>%
  count(sentence) %>%
  arrange(desc(n)) %>%
  view()
```

After proper cleaning we can start the tokenization

```{r}
anime_words_unnested = anime_text_cleaned %>%
  unnest_tokens(word, sentence) %>%
  anti_join(stop_words, by="word") %>%
  select(name, premiere_season, word)
anime_words_unnested
  
anime_words_unnested %>%
  count(premiere_season, word, sort=TRUE)
```

After tokenizing the text and getting the words from each synopsis of each anime of each season, I can start visualizing the main question.
Based on the video above, I will calculate a log odds ratio, that reveals which words are the most frequent in each of the seasons relative to the others.
I will visualize the top 10 words of each season.

```{r}
library(qdapDictionaries)
#there are some japanese words that indicate characters, etc.. which will distort our aims, as e.g.  some animes always premiere in the same season so we would only get that info
#Therefore I decided to keep only English words - for this I use the en_Us dictionary word list of R
english_words <- readLines("en_US.dic") %>% 
# the vector contains extra information on the words, which is removed
  gsub("/.+", "", .)
  
anime_words_unnested %>%
  filter(word %in% english_words) %>%
  count(premiere_season, word, sort=TRUE) %>%
  bind_log_odds(premiere_season, word, n) %>%
  arrange(desc(log_odds_weighted)) %>%
  group_by(premiere_season) %>%
  top_n(10, log_odds_weighted) %>%
  ungroup() %>%
  mutate(word = fct_reorder(word, log_odds_weighted)) %>%
  ggplot(aes(log_odds_weighted, word, fill=word)) +
    geom_col() +
    theme_light() +
    theme(legend.position = "none") +
    scale_fill_viridis(discrete = TRUE) +
    facet_wrap(~premiere_season, scales="free_y") +
    labs(x = "Log-odds ratio of words appearing", y = "Words from anime synopses", title = "Words that appear in anime's synopses from one of the\npremiere seasons more frquently")
```

After proper cleaning, the data still does not show any real observable trends. Many of the words are English names that appear in some animes that premiered in one of the seasons. There are some interesting observation though:
 - Lain stands out for instance in summer, as it is a very rare name, and is the name of a protagonist of Serial Experiment Lain which released in the summer.
 - school is a very frequent word, yet it has a clear trend appearing in the summer, even though there is school's out in Japan too from July to September.
 - Summer also has badminton a very "summer-y" sport

Altogether, this data does not really show too many interesting observations, due to the premiere seasons not being too distinctive, only in terms of name usage.

# Modelling

For modelling, I ask question that is probably the most interesting for any studios or aspiring anime creators: "What makes anime succesful?"

Being successful in this data means, that many people added this anime to their list (members). Although there is a "popularity" metric in the dataset, it is an ordinal scale, with one value for each anime, which is not suitable for such analysis.

To answer this question, we construct a model based on some theoretical approaches:
  - We would like to see whether the number of members putting the anime on their list is dependant on these variables: Score (how well it is rated - better more popular?), Premiere_season (in which season it premiered), Diversity (how many genres it is listed under), Rating (age rating of anime - related to figure 5.). I use these variables and Score as independent ones to predict Popularity.
  
I build the model first

```{r}
anime_complex_model = lm(members ~ premiere_season + diversity + rating + score, anime_cleaned_model)
summary(anime_complex_model)
```

### Checking for influential outliers

Check for outlier values in the model.

```{r}
# Plot the Cook's Distance using the traditional 4/n criterion - based on: http://r-statistics.co/Outlier-Treatment-With-R.html
cook_distance_remove <- function(some_model, some_data) {
  cookdistance = cooks.distance(some_model)
  sample_size <- nrow(some_data)
  plot(cookdistance, pch="*", cex=2, main="Cook Distance influential outliers")  # plot cook's distance
  abline(h = 4/sample_size, col="red")  # add cutoff line
  text(x=1:length(cookdistance)+1, y=cookdistance, labels=ifelse(cookdistance>4/sample_size, names(cookdistance),""), col="red")  # add labels
  
  influential <- as.numeric(names(cookdistance)[(cookdistance > 4*mean(cookdistance, na.rm=T))])
  head(some_data[influential, ])
  
  some_data_outlierrm = some_data %>%
    slice(-influential)
  return(some_data_outlierrm)
}
#we run the cook's distance outlier removal several times, until we no longer find influential values.
anime_cleaned_model_outlierrm = cook_distance_remove(anime_complex_model, anime_cleaned_model)
anime_complex_model2 = lm(members ~ premiere_season + diversity + rating + score, anime_cleaned_model_outlierrm)
anime_cleaned_model_outlierrm2 = cook_distance_remove(anime_complex_model2, anime_cleaned_model_outlierrm)
anime_complex_model3 = lm(members ~ premiere_season + diversity + rating + score, anime_cleaned_model_outlierrm2)
anime_cleaned_model_outlierrm3 = cook_distance_remove(anime_complex_model3, anime_cleaned_model_outlierrm2)
anime_complex_model4 = lm(members ~ premiere_season + diversity + rating + score, anime_cleaned_model_outlierrm3)
anime_cleaned_model_outlierrm4 = cook_distance_remove(anime_complex_model4, anime_cleaned_model_outlierrm3)
anime_complex_model5 = lm(members ~ premiere_season + diversity + rating + score, anime_cleaned_model_outlierrm4)
anime_cleaned_model_outlierrm5 = cook_distance_remove(anime_complex_model5, anime_cleaned_model_outlierrm4)
anime_complex_model6 = lm(members ~ premiere_season + diversity + rating + score, anime_cleaned_model_outlierrm5)
```

### Checking assumptions

Check the normality assumption.

```{r}
#Q-Q plot
plot(anime_complex_model6, 2)
#Histogram
anime_complex_model6_residuals = enframe(residuals(anime_complex_model6))
anime_complex_model6_residuals %>% ggplot() + aes(x=value) + geom_histogram()
#Normality is not violated, so we can use the traditional p < 0.05 significance criterion
```

Check the linearity assumption.

```{r}
require(car)
plott = plot(anime_complex_model6, 1)
anime_complex_model4 %>% residualPlots()
#As the tests are significant for the "score" variable, we try to construct the model with a higher order term.
anime_complex_model_higher = lm(members ~ premiere_season + diversity + rating + poly(score), anime_cleaned_model)
summary(anime_complex_model_higher)
plott2 = plot(anime_complex_model_higher, 1)
anime_complex_model_higher %>% residualPlots()
#As this modification does not work, I decided to omit this variable
```

## Updating model

As I removed Score from the independent variables, I will reconstruct the model, and rerun the diagnostics before prceeding.


```{r}
#I construct the new model
anime_complex_model_noscore = lm(members ~ premiere_season + diversity + rating, anime_cleaned_model)
summary(anime_complex_model_noscore)
```

### Checking for influential outliers

Check for outlier values in the model.

```{r}
# Plot the Cook's Distance using the traditional 4/n criterion - based on: http://r-statistics.co/Outlier-Treatment-With-R.html
cook_distance_remove <- function(some_model, some_data) {
  cookdistance = cooks.distance(some_model)
  sample_size <- nrow(some_data)
  plot(cookdistance, pch="*", cex=2, main="Cook Distance influential outliers")  # plot cook's distance
  abline(h = 4/sample_size, col="red")  # add cutoff line
  text(x=1:length(cookdistance)+1, y=cookdistance, labels=ifelse(cookdistance>4/sample_size, names(cookdistance),""), col="red")  # add labels
  
  influential <- as.numeric(names(cookdistance)[(cookdistance > 4*mean(cookdistance, na.rm=T))])
  head(some_data[influential, ])
  
  some_data_outlierrm = some_data %>%
    slice(-influential)
  return(some_data_outlierrm)
}
#we run the cook's distance outlier removal several times, until we no longer find influential values.
anime_cleaned_model_outlierrm_noscore = cook_distance_remove(anime_complex_model_noscore, anime_cleaned_model)
anime_complex_model_noscore2 = lm(members ~ premiere_season + diversity + rating, anime_cleaned_model_outlierrm_noscore)
anime_cleaned_model_outlierrm_noscore2 = cook_distance_remove(anime_complex_model_noscore2, anime_cleaned_model_outlierrm_noscore)
anime_complex_model_noscore3 = lm(members ~ premiere_season + diversity + rating, anime_cleaned_model_outlierrm_noscore2)
anime_cleaned_model_outlierrm_noscore3 = cook_distance_remove(anime_complex_model_noscore3, anime_cleaned_model_outlierrm_noscore2)
anime_complex_model_noscore4 = lm(members ~ premiere_season + diversity + rating, anime_cleaned_model_outlierrm_noscore3)
```

### Checking assumptions

Check the normality assumption.

```{r}
#Q-Q plot
plot(anime_complex_model_noscore4, 2)
#Histogram
anime_complex_model_noscore4_residuals = enframe(residuals(anime_complex_model4))
anime_complex_model_noscore4_residuals %>% ggplot() + aes(x=value) + geom_histogram()
#Normality is not violated, so we can use the traditional p < 0.05 significance criterion
```

Check the linearity assumption.

```{r}
plott = plot(anime_complex_model_noscore4, 1)
anime_complex_model_noscore4 %>% residualPlots()
#As none of the tests are significant, even though we see a bit of a curvature, we assume linaerity holds.
```


Check the homoscedasticty assumption (homogeneity of variance).

```{r}
require(lmtest)
#We plot the stansardized residuals which show a curious pattern, which can be attributed to the ordinality of the dependent variable
plot(anime_complex_model_noscore4, 3)
#We test significant heteroscdacity with NCV and BT tests based on https://github.com/kekecsz/PSYP14-Advanced-Scientific-Methods/blob/main/Exercise_11_Model_diagnostics/Exercise_11_Model_diagnostics.pdf
anime_complex_model_noscore4 %>% ncvTest()
anime_complex_model_noscore4 %>% bptest()
#We see no significant heteroscedacity, so homoscedacity of the data can be assumed
```

Check the multicollinearity assumption.

(VIF above 5), or a VIF threshold of 3 is recommended in this paper: http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2009.00001.x/full

Some info about VIF: 
https://statisticalhorizons.com/multicollinearity
http://blog.minitab.com/blog/understanding-statistics/handling-multicollinearity-in-regression-analysis

```{r}
require(psych)
#Based on the suggested readings, we treat variables above VIF of 3.
car::vif(anime_complex_model_noscore4)
#There are no variables aboive VIF of 3
```

## Reporting and evaluating the model

I created the model based on the results of the model diagnostics. I now report the results for the model:

The model's test statistics are:
Adjusted R-squared = 0.1144
F-statistic = 19.35
df = 8 and 1128
p value < 2.2e-16

The model's equation:
Y = 2.32 - 0.004 * Spring - 0.0002 * Summer - 0.005 * Winter + 0.01 diversity + 0.088 Rated Teen + 0.03 Rated Children + 0.113 Rated 17+ + 0.1 Rated Mild Nudity

Based on the coefficients, we see that diversity (b = 0.106; p < 0.001), rating of PG-Teen (b = 0.088; p < 0.001), rating of 17+ (b = 0.106; p < 0.113) and rating of Mild Nudity (b = 0.101; p < 0.001) predicts the members putting an anime on their list significanlty.



```{r}
require(lsr)
#We use the final version of complex model without influential outliers and checked for all assumptions
summary(anime_complex_model_noscore4)
#Getting the relevant data of predictors in a table 
sum_comp = summary(anime_complex_model_noscore4)[["coefficients"]] #unstandardized coefficients, t and p
conf_comp = confint(anime_complex_model_noscore4) #95% confidence intervals
betas_comp = standardCoefs(anime_complex_model_noscore4) #standardized coefficients (B and beta)
anime_model_coef_table = cbind(sum_comp[-1,], conf_comp[-1,], betas_comp)
anime_model_coef_table
```

# Conclusions

## Exploratory analysis

Visualizing the anime datasets, we saw some interesting trends:

  1. The most scored and most favorited animes on MyAnimeList have some overlaps, although most of them have different ranks between these measures.
  2. There are clearly animes that are either more popular than ranked or vice versa.
  3. Shounen animes are produced at increasing rates, however there are years, when Shoujo animes get produced more than usually.
  4. Studios can be differentiated by the genres they produce mostly, and it is mirrored in their most succesful animes.
  5. Animes for more mature audiences are better scored, but too mature animes are not.
  6. There are no real observable trends of anime synopses using distinct vocabulary based on the premiere season.

## Modelling

For the modelling part, there were some limitations. The dependent varaible was log transformed, so we may have lost some precision in our prediction. There were also many categorical independent variables, which introduced a great amount of df, which made the model less precise and robust.

It is also a bit hard to explain the findings, but we can infer some conclusions:
  - The diversity of an anime significantly predicts receiveng more traction/popularity, meaning that the more genre an anime blends, the more popular it can get.
  - A higher age rating than Children significantly predicts an anime getting more popular - animes for Teen and Mature receive more viewings from people on MyAnimeList.org
