# Simple analysis of data

rm(list = ls())

library(quanteda)
library(quanteda.textstats)
library(stringr)
library(dplyr)
library(haven)

sjmm <- read_dta("~/OneDrive - UvA/Job Advert Project/data/669_SMM_Data_SUF_stata_EN_v6.0.0.dta")
ads <- readRDS("~/OneDrive - UvA/Job Advert Project/data/joined_ads.rds")

# Readuce to ads which have a cler identificator  (some ads resulted in two observations sometimes because there wre two or more vacancies within the same ad)

sjmm <- distinct(sjmm, id_ad, .keep_all = TRUE) # 94649 - 82582 = 12067 observations lost
ads <- distinct(ads, id, .keep_all = TRUE)  # -5 observations 
sjmm$id <- as.character(sjmm$id_ad)
sjmm_ads <- inner_join(sjmm, ads, by = "id") # (82563 observations remaining)

# Choos only ads from the last decade and in german
attributes(sjmm$lang)
sjmm_ads_10 <- filter(sjmm_ads, year >= 2010, lang == 0) # 32578
sjmm_ads_10 <- select(sjmm_ads_10, id, year, documents, 
                      noga1995, noga1995_agg10, 
                      empl_type, 
                      scope_perc, scope_hours, 
                      exec, resp, maintask, 
                      occup_cop, occup, stem, vz90, ssco2000, isco08, isco19, 
                      isei08, trei08, 
                      edu1, edu2, edu1_type, edu1_years, edu1_field, edu2_type, edu2_years, edu2_field, edu_years_mean,
                      matches("exp*"), 
                      matches("kno*"),
                      matches("train*")
                      )
sjmm_ads_10$documents[[1]]


sjmm_corpus <- corpus(sjmm_ads_10, text_field = "documents")
sjmm_corpus[1:3]

sjmm_tokens <- tokens(sjmm_corpus, what = c("word"), 
                      remove_separators = TRUE, 
                      include_docvars = TRUE, 
                      ngrams = 1L, 
                      remove_numbers = TRUE, 
                      remove_symbols = TRUE, 
                      remove_url = TRUE, 
                      remove_hyphens = TRUE)

head(sjmm_tokens)

job_titles <- as.character(sjmm_ads_10$occup_cop)

sjmm_tokens <- sjmm_tokens %>%
  tokens_remove(pattern = "^[[:punct:]]+$", 
                valuetype = "regex",
                padding = TRUE)

sjmm_tokens <- sjmm_tokens %>%
  tokens_tolower()

sjmm_tokens <- sjmm_tokens %>%
  tokens_remove(stopwords("german"), padding = TRUE)

sjmm_tokens <- sjmm_tokens %>% tokens_remove("")

bad_tokens <- c("sowie", 
                "ag", 
                "suchen", "unsere", 
                "aufgaben", "bieten", "e", "tel", "bitte",
                "m", "weitere", "mail", "bewerbungsunterlagen", "unternehmen", 
                "n", "w", "unseren", "ab", "and", "de", "unseres", "#61623", "the", "ch", "of", "a", "http", 
                "to", "dabei", "b", "et", "t", "dass", "inkl", "d", "à", "r", "s", "o", "h", "f",
                "genève", "zürich", "c",  
                "gute", "per", "kunden", "team", "bewerbung", "erfahrung", "gerne", "freuen", "arbeiten", 
                "beriech", "ausbildung", "schweiz", "profil", "vereinbarung", "unserer", "vorteil", "verfügen", 
                "sofort", "senden", "telefon", "online", "abgeschlossene", "bewerben", "tätigkeit", "umfeld", 
                "kontakt", "interesse", "kenntnisse", "bereich", "frau", "herr", "erwartet", "erwarten", "mehr", 
                "berufserfahrung", "jahre", "fragen", "adresse", 
                "region", "bern", "winterthur", "basel", "luzern", "zusammenarbeit", "zudem", 
                "stelle", "kenntnis", "bereich", "auskünfte", "setzen", "mitarbeitenden", "persönlichkeit", "teams", "freude", "rund", "stellen", "arbeitsweise", "bringen",
                "teil", "umgang")

sjmm_tokens <- sjmm_tokens %>% tokens_remove(bad_tokens)
sjmm_tokens <- sjmm_tokens %>% tokens_remove(stopwords("english"))
sjmm_tokens <- sjmm_tokens %>% tokens_remove(stopwords("french"))
sjmm_tokens_15 <- tokens_subset(sjmm_tokens, year >=2015)
sjmm_tokens_20 <- tokens_subset(sjmm_tokens, year ==2020)


sjmm_dfm <- dfm(sjmm_tokens_20)
textstat_frequency(sjmm_dfm)

sjmm_dtm <-convert(sjmm_dfm, to = "topicmodels")
dtm <- 
  sjmm_dfm %>% 
  dfm_trim(., sparsity = 0.999) %>% 
  convert(., to = "topicmodels")

dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ] 


othertokks<- 
  sjmm_dfm %>% 
  dfm_trim(., sparsity = 0.999) 
topfeatures(othertokks)
library(topicmodels)
topfeatures(dtm)
lda.model_20 <- LDA(dtm, k = 20)
terms(lda.model_20, 10)
models <- posterior(lda.model)
top_text<- as.data.frame(post_dist_top_over_docs)
post_dist_top_over_docs <- models$topics
post_dist_top_over_docs[1,(max.col(post_dist_top_over_docs))] %>% as.data.frame()

sjmm_corpus[5810]
dtm[[5810]]

check <- filter(sjmm_ads_10, id == "22010111880006")
check$documents


weighted_dfm <- sjmm_dfm %>% 
  dfm_trim(., sparsity = 0.999) %>% dfm_tfidf()

similarity_docs <- textstat_simil(weighted_dfm, method = "cosine", margin = "documents")

as.matrix(similarity_docs) %>%.[1:10, 1:10]
install.packages("igraph")
library(igraph)


matt <- as.matrix(similarity_docs)
maattt <- graph_from_adjacency_matrix(matt, mode = "lower", weighted = TRUE)
plot(maattt)


library(stm)
dtm_stm <- 
  sjmm_dfm %>% 
  dfm_trim(., sparsity = 0.999) %>% 
  convert(., to = "stm")
stmmod<-stm(dtm_stm$documents,
            dtm_stm$vocab,
          prevalence = ~isco08, 
          K = 10, 
          data = select(dtm_stm$meta, isco08), 
          init.type = "LDA")

stmmod_magic_20<-stm(dtm_stm$documents,
            dtm_stm$vocab,
            prevalence = ~maintask + edu1 + edu2, 
            K = 20, 
            data = select(dtm_stm$meta, maintask, edu1, edu2), 
            init.type = "LDA")
?stm
labelTopics(stmmod_magic_20, n= 10)
