rm(list = ls())
library(tidyverse)
library(rvest)
library(dummies)

#--------------------------------------------------------------------------------------
# Read all tables of interest from the working directory
#--------------------------------------------------------------------------------------
movie_summary <- read_csv("movie_summary.csv",
                          col_names = TRUE,
                          na = c("","NA","NULL"))

movie_production_companies <- read.csv("movie_production_companies.csv",
                                       header = TRUE,
                                       na = c("","NA","NULL"))

movie_ratings <- read.csv("movie_ratings.csv",
                          header = TRUE,
                          na = c("","NA","NULL"))

acting_credits <- read.csv("acting_credits.csv",
                           header = TRUE,
                           na = c("","NA","NULL"))


movie_keywords <- read.csv("movie_keywords.csv",
                           header = TRUE,
                           na = c("","NA","NULL"))

technical_credits <- read.csv("technical_credits.csv",
                           header = TRUE,
                           na = c("","NA","NULL"))

#--------------------------------------------------------------------------------------
# Encoding source in movie_summary
#--------------------------------------------------------------------------------------
source_encode <- select(movie_summary,
                        odid,
                        source)

# Original Screenplay
original_screenplay <- c("Original Screenplay")

real_factual <- c("Based on Real Life Events",
                  "Based on Factual Book/Article")

fiction_tale_novel <- c("Based on Fiction Book/Short Story",
                        "Based on Folk Tale/Legend/Fairytale",
                        "Based on Comic/Graphic Novel")

music_dance <- c("Based on Musical or Opera",
                 "Based on Ballet",
                 "Based on Musical Group",
                 "Based on Song")

play_TV_movie_film <- c("Based on TV",
                        "Based on Short Film",
                        "Based on Movie",
                        "Based on Play")

others <- c("Remake",
            "Compilation",
            "Based on Game",
            "Spin-Off",
            "Based on Religious Text",
            "Based on Toy",
            "Based on Theme Park Ride",
            "Based on Web Series")

source_encode$source_category <- NA
source_encode$source_category[source_encode$source %in% original_screenplay] <- "original_screenplay"
source_encode$source_category[source_encode$source %in% real_factual] <-"real_factual"
source_encode$source_category[source_encode$source %in% fiction_tale_novel] <- "fiction_tale_novel"
source_encode$source_category[source_encode$source %in% music_dance] <-"music_dance"
source_encode$source_category[source_encode$source %in% play_TV_movie_film] <- "play_TV_movie_film"
source_encode$source_category[source_encode$source %in% others] <- "others"

source_encoding <- select(source_encode,-source)


# source_encoding_final finishes encoding and is ready to join movie_summary
source_encoding_final <- dummy.data.frame(source_encoding,sep = ".")

source_encoding_final_before_dummy <- unique(select(source_encoding_final,
                                                    odid,
                                                    source_category))

source_encoding  <- dummy.data.frame(source_encoding_final_before_dummy ,
                                     sep = ".")

source_encoding  <- aggregate(source_encoding[2:6],
                              by = list(source_encoding$odid),
                              FUN = sum)

colnames(source_encoding)[1] <- "odid"





                                                     
#--------------------------------------------------------------------------------------
# Encoding movie_production_companies based on total domestic box office or total 
# worldwide box office --- web scraping from
# http://www.the-numbers.com/movies/production-companies/
#--------------------------------------------------------------------------------------
url <- "http://www.the-numbers.com/movies/production-companies/"
webpage <- read_html(url)
sb_table <- html_nodes(webpage, 'table')
production_companies_rank <- html_table(sb_table)[[1]]
write.csv(production_companies_rank,
          file = "production_companies_rank.csv",
          row.names = FALSE)

production_companies_rank <- read_csv("production_companies_rank.csv",
                                      col_names = TRUE,
                                      na = c("","NA","NULL"))

production_companies_rank$`Total Worldwide Box Office` <-
    gsub(",","",production_companies_rank$`Total Worldwide Box Office` )

production_companies_rank$`Total Worldwide Box Office` <-
    as.numeric(gsub("\\$","",production_companies_rank$`Total Worldwide Box Office`))

# Sort production_companies_rank --- Total Worldwide Box Office by descending order
production_companies_rank <-production_companies_rank[with(production_companies_rank,
                               order(-`Total Worldwide Box Office`)),]

# Categorize production companies by its rank in total worldwide box
Big_15 <- (filter(production_companies_rank,
                  `Total Worldwide Box Office`>= 10000000000))$`Production Companies`

Large <- (filter(production_companies_rank,
                 `Total Worldwide Box Office`<= 10000000000 &
                 `Total Worldwide Box Office`>= 1000000000))$`Production Companies`

Medium <- (filter(production_companies_rank,
                  `Total Worldwide Box Office`<= 1000000000 &
                  `Total Worldwide Box Office`>= 100000000))$`Production Companies`

Small <- (filter(production_companies_rank,
                 `Total Worldwide Box Office`<= 100000000 &
                 `Total Worldwide Box Office`>= 10000000))$`Production Companies`

Extra_small <- (filter(production_companies_rank,
                       `Total Worldwide Box Office`<= 10000000 &
                       `Total Worldwide Box Office`>= 1000000))$`Production Companies`

Others <- (filter(production_companies_rank,
                  `Total Worldwide Box Office`<= 1000000))$`Production Companies`

production_companies_size <- character(nrow(movie_production_companies))
Condition1 <- (movie_production_companies$production_company %in% Big_15)        
Condition2 <- (movie_production_companies$production_company %in% Large)
Condition3 <- (movie_production_companies$production_company %in% Medium)
Condition4 <- (movie_production_companies$production_company %in% Small)
Condition5 <- (movie_production_companies$production_company %in% Extra_small)
Condition6 <- (movie_production_companies$production_company %in% Others)

for(i in 1:nrow(movie_production_companies)){
    if(Condition1[i]){production_companies_size[i] <- "Big_15"}
    else if(Condition2[i]){production_companies_size[i] <- "Large"}
    else if(Condition3[i]){production_companies_size[i] <- "Medium"}
    else if(Condition4[i]){production_companies_size[i] <- "Small"}
    else if(Condition5[i]){production_companies_size[i] <- "Extra_small"}
    else if(Condition6[i]){production_companies_size[i] <- "Others"}
    else {production_companies_size[i] <- "NULL"}
}
movie_production_companies$production_companies_size <-
    production_companies_size

# Then we can delete production_company and transform production_companies_size to 
# dummy variables

movie_production_companies_new <- unique(select(movie_production_companies,
                                           -production_company,
                                           -display_name))

movie_production_companies_final <- dummy.data.frame(movie_production_companies_new,
                                                     sep = ".")

# In movie_production_companies_final, each row contains one unique odid and the according
# dummy variables --- encoding complete, ready to join movie_summary
movie_production_companies_final <- aggregate(movie_production_companies_final[,2:8],
                                              by = list(odid = movie_production_companies_final$odid),
                                              FUN=sum)

write.csv(movie_production_companies_final,"pro.csv")

#--------------------------------------------------------------------------------------
# Encoding actor/actress in acting_credits
#--------------------------------------------------------------------------------------
url_list <- c("http://www.imdb.com/list/ls058011111/?start=1&view=compact&sort=listorian:asc",
              "http://www.imdb.com/list/ls058011111/?start=251&view=compact&sort=listorian:asc",
              "http://www.imdb.com/list/ls058011111/?start=501&view=compact&sort=listorian:asc",
              "http://www.imdb.com/list/ls058011111/?start=751&view=compact&sort=listorian:asc")

datalist <- list()
for(i in 1:length(url_list)){
    url <- url_list[i]
    webpage <- read_html(url)
    sb_table <- html_nodes(webpage, 'table')
    actor_rank <- html_table(sb_table)[[1]]
    datalist[[i]] <- actor_rank
}
actor_full <- do.call(rbind,datalist)
colnames(actor_full)[1] <- "Actor Popularity"

actor_scraped <- select(actor_full,
                `Actor Popularity`,
                Name)

acting_credits_top5 <- select(filter(acting_credits,billing <= 5),odid,person)

act_250 <- filter(actor_scraped,
                  `Actor Popularity`<= 250)$`Name`

act_500 <- filter(actor_scraped,
                 `Actor Popularity`<= 500 &
                 `Actor Popularity`>= 251)$`Name`

act_750 <- filter(actor_scraped,
                  `Actor Popularity`<= 750 &
                  `Actor Popularity`>= 501)$`Name`

act_1000 <- filter(actor_scraped,
                  `Actor Popularity`<= 1000 &
                  `Actor Popularity`>= 751)$`Name`

acting_credits_top5$Rank <- NA

acting_credits_top5$Rank[acting_credits_top5$person %in% act_250] <- "act_250"
acting_credits_top5$Rank[acting_credits_top5$person %in% act_500] <- "act_500"
acting_credits_top5$Rank[acting_credits_top5$person %in% act_750] <- "act_750"
acting_credits_top5$Rank[acting_credits_top5$person %in% act_1000] <- "act_1000"

acting_credits_top5$Rank <- ifelse(is.na(acting_credits_top5$Rank), 
                             'others', acting_credits_top5$Rank)

acting_credits_top5_before_dummy <- unique(select(acting_credits_top5,
                                           odid,
                                           Rank))

acting_credits_top5_final <- dummy.data.frame(acting_credits_top5_before_dummy,
                                              sep = ".")

acting_credits_top5_final <- aggregate(acting_credits_top5_final[2:6],
                                       by = list(acting_credits_top5_final$odid),
                                       FUN = sum)

colnames(acting_credits_top5_final)[1] <- "odid"

#--------------------------------------------------------------------------------------
# Calculate the length of movie display name
# Assumption 1: length of movie display name may be related to domestic box office
#--------------------------------------------------------------------------------------
movie_summary$name_length <- sapply(movie_summary$display_name,
                                    nchar,
                                    USE.NAMES = FALSE)

#--------------------------------------------------------------------------------------
# Assumption 2: We're only interested in those movies which have box office in America
#--------------------------------------------------------------------------------------
movie_summary1 <- filter(movie_summary,inflation_adjusted_domestic_box_office != 0)

#--------------------------------------------------------------------------------------
# Assumption 3: We're not interested in dvd or bluray spending(since the number of NAs
# in these columns occupy 70% of the total records)
#--------------------------------------------------------------------------------------
movie_summary2 <- select(movie_summary1,
                         -source,
                         -domestic_dvd_units,
                         -domestic_dvd_spending,
                         -domestic_bluray_units,
                         -domestic_bluray_spending)

#--------------------------------------------------------------------------------------
# Assumption 4: In acting_credits and technical_credits, we only care about top 5 biling 
# people because they are the most important actors/actress, contributors 
# in the movie or in the backend
#--------------------------------------------------------------------------------------
# acting_credits <- filter(acting_credits,billing <= 5)
# technical_credits <- filter(technical_credits,billing <= 5)
#--------------------------------------------------------------------------------------
# Inner join 4 tables above to movie_summary
#--------------------------------------------------------------------------------------
df1 <- inner_join(movie_summary2,source_encoding,
                 by = c("odid"))

df2 <- left_join(df1,movie_production_companies_final,
                 by = c("odid"))

df3 <- left_join(df2,acting_credits_top5_final,
                 by = c("odid"))

df4 <- left_join(df3,genre_encoding,
                 by = c("odid"))

df5 <- inner_join(df4,rating_encode_dummy_final,
                 by = c("odid"))

#--------------------------------------------------------------------------------------
# Encoding genre in movie_summary(df)
#--------------------------------------------------------------------------------------
genre_encode <- select(movie_summary2,odid,genre)
genre_encode$genre <- ifelse(is.na(genre_encode$genre),
                             "Others",
                             genre_encode$genre)
genre_encode$genre[genre_encode$genre == "Educational" |
                       genre_encode$genre == "Reality" |
                       genre_encode$genre == "Multiple Genres"] <- "Others"

genre_encode$genre[genre_encode$genre == "Comedy" |
                       genre_encode$genre == "Romantic Comedy" |
                       genre_encode$genre == "Black Comedy"] <- "Comedy"

genre_encode$genre <- genre_encode$genre

genre_encode_dummy <- dummy.data.frame(genre_encode,sep = ".")

genre_encode_before_dummy <- unique(select(genre_encode_dummy,
                                           odid,
                                           genre))

genre_encoding  <- dummy.data.frame(genre_encode_before_dummy ,
                                    sep = ".")

#--------------------------------------------------------------------------------------
# Encoding rating in movie_rating
#--------------------------------------------------------------------------------------
rating_encode <- select(movie_ratings,odid,rating)
rating_encode_dummy_final <- dummy.data.frame(rating_encode,sep = ".")

#--------------------------------------------------------------------------------------
# Encoding reason in movie_rating
#--------------------------------------------------------------------------------------
rating_reason <- select(movie_ratings,
                        odid,
                        reason)
rating_reason <- filter(rating_reason,!is.na(rating_reason$reason))

rating_reason$reason_encoding <- strsplit(
    rating_reason$reason,"")
rating_reason$reason_encoding <- lapply(rating_reason$reason,
                                        strsplit)
for(i in 1:nrow(rating_reason)){
    rating_reason$reason_encoding[i] <- 
        strsplit(rating_reason$reason[i],"")
}





replacement <- c("for","some","and")
rating_reason$reason_encoding <- rating_reason$reason

for(i in 1:nrow(rating_reason)){
    for(j in 1:length(replacement)){
        rating_reason$reason_encoding[i] <-
            gsub(replacement[j],"",rating_reason$reason_encoding[i])}
}




for(i in 1:nrow(rating_reason)){
    rating_reason$reason_encoding[i] <-
        gsub("for","",rating_reason$reason[i]) 
}
for(i in 1:nrow(rating_reason)){
    rating_reason$reason_encoding[i] <-
        gsub(",","",rating_reason$reason_encoding[i]) 
}
for(i in 1:nrow(rating_reason)){
    rating_reason$reason_encoding[i] <-
        gsub("and","",rating_reason$reason_encoding[i]) 
}




for(i in 1:nrow(rating_reason)){
    for(j in 1:length(replacement)){
        rating_reason$reason_encoding[i] <-
            gsub(replacement[j],"",rating_reason$reason[i])}
}



for(i in 1:length(replacement)){
    rating_reason$reason_encoding <- gsub(replacement[i],"",rating_reason$reason)
}

#--------------------------------------------------------------------------------------
# Encoding person(director) in technical_credits in movie_rating
#--------------------------------------------------------------------------------------
Dir_rank <- read.csv("Dir_rank_fromweb.csv",
                     header = TRUE,
                     na = c("","NA","NULL"))

Dir_rank_scraped <- select(Dir_rank,
                           Row,
                           Person)

technical_credits_top1 <- select(filter(technical_credits,billing == 1 &
                                        role == "Director"),odid,person)

dir_20 <- filter(Dir_rank_scraped,
                 Row <= 20)$`Person`

dir_100<- filter(Dir_rank_scraped,
                 Row <= 100 &
                 Row >= 21)$`Person`

dir_200 <- filter(Dir_rank_scraped,
                  Row <= 200 &
                  Row >= 101)$`Person`

dir_500 <- filter(Dir_rank_scraped,
                  Row <= 500 &
                  Row >= 201)$`Person`

dir_900 <- filter(Dir_rank_scraped,
                  Row <= 871 &
                  Row >= 501)$`Person`

technical_credits_top1$Rank <- NA

technical_credits_top1$Rank[technical_credits_top1$person %in% dir_20] <- "dir_20"
technical_credits_top1$Rank[technical_credits_top1$person %in% dir_100] <- "dir_100"
technical_credits_top1$Rank[technical_credits_top1$person %in% dir_200] <- "dir_200"
technical_credits_top1$Rank[technical_credits_top1$person %in% dir_500] <- "dir_500"
technical_credits_top1$Rank[technical_credits_top1$person %in% dir_900] <- "dir_900"

technical_credits_top1$Rank <- ifelse(is.na(technical_credits_top1$Rank), 
                                      'others', technical_credits_top1$Rank)

technical_credits_top1_before_dummy <- unique(select(technical_credits_top1,
                                                     odid,
                                                     Rank))

technical_credits_top1_final <- dummy.data.frame(technical_credits_top1_before_dummy,
                                                 sep = ".")

colnames(technical_credits_top1_final)[1] <- "odid"

write.csv(technical_credits_top1_final,"technical_final.csv")





# df4 <- inner_join(df3,technical_credits,
#                  by = c("odid","display_name"))
# 
# df5 <- inner_join(df4,movie_production_companies,
#                   by = c("odid","display_name"))
# 
# colnames(df4)[c(24,25,28,29)] <- c("cast_billing","castperson",
#                                    "technical_billing","technical_person")
# 
# write.csv(df4,file = "mainTable.csv",
#           row.names = FALSE)
# 
# movie_maintable <- read.csv("mainTable.csv",
#                             header = TRUE,
#                             na = c("","NA","NULL"))
# 
# summary(movie_maintable)                          






























("Encoding_Version3.csv",
                      header = TRUE,
                      na = c("","NA","NULL"))

log.mainTable <- log(mainTable[,5:59])






























