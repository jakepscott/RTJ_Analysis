# What’s in a word? Why tf-idf sometimes fails to accurately capture word importance, and what we can use instead

This repository contains the code and figures used in a Medium post published on [Towards Data Science](https://towardsdatascience.com/) called [What's in a Word](https://towardsdatascience.com/whats-in-a-word-da7373a8ccb). In this article I explore the drawbacks of term frequency-inverse document (tf-idf) in text analysis and propose a potential complementary statistic: proportional importance. 

Specifically, I demonstrate that tf-idf has two drawbacks: the zero value issue and the extensive margin issue. The former means that tf-idf suddenly plummets to zero if the word of interest appears in all of the outside documents, even if it only appears once in each of them. The latter means that tf-idf is excessively sensitive on the extensive margin and excessively resistant to changes on the intensive margin. I call my alternative statistic proportional importance. To calculate proportional importance I look at the proportion of total words in a given document made up by the word of interest and subtract the proportion of words outside that document made up by the word of interest.

To test the efficacy of proportional importance compared to tf-idf, I analyze the discography of the rap duo Run the Jewels. I find proportional importance handles the two above-described issues better than tf-idf. 

![tfidf](https://user-images.githubusercontent.com/56490913/97821433-6755cd00-1c80-11eb-854b-746d1bec348a.png)
![PI](https://user-images.githubusercontent.com/56490913/97821439-72106200-1c80-11eb-84dc-e025d5a93063.png)

## Getting Started

To see the results of this code, check out the [Medium article](https://towardsdatascience.com/whats-in-a-word-da7373a8ccb). 

If you want to run or edit the code on your own computer, download the code and data from this repo. If you download the data, you should be able to just run Making_Figures.R to get the figures, and Making_Tables.R to get the tables. If you want to get the data yourself, you should run Getting_Data.R, then Getting_Lyrical_Data.R, and then Calculating_Proportional_Importance.R. Do note that you will need to obtain a Spotify API key and use keys<-spotifyOAuth() to get Getting_Data.R to run. I of course keep my key secret. 

### Prerequisites

To run the code on your computer you need R and the following packages installed and loaded:

```
library(tidyverse)
library(readr)
library(tidytext)
library(gtable)
library(Rspotify)
library(geniusr)
library(genius)
```
## Author
* **Jake Scott** - [Twitter](https://twitter.com/jakepscott2020), [Medium](https://medium.com/@jakepscott16)
