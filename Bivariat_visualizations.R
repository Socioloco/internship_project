# Basic bivariate visualizations


rm(list = ls())

library(quanteda)
library(quanteda.textstats)
library(stringr)
library(dplyr)
library(haven)
library(ggplot2)

sjmm <- read_dta("~/OneDrive - UvA/Job Advert Project/data/669_SMM_Data_SUF_stata_EN_v6.0.0.dta")
sjmm$noga1995
sjmm$maintask
sjmm$noga1995_agg10
sjmm_10 <- filter(sjmm, year >=2010)

ggplot(sjmm_10, aes(x= as_factor(noga1995_agg10), fill = as_factor(maintask)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 90))



ggplot(sjmm_10, aes(x= as_factor(maintask), fill = as_factor(noga1995)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 90))

ggplot(sjmm_10, aes(x= as_factor(noga1995), fill = as_factor(maintask)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 90))

ggplot(sjmm_10, aes(x= as_factor(edu1_type), fill = as_factor(maintask)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 90))


ggplot(sjmm_10, aes(x= isei08, fill = as_factor(maintask)))+
  geom_density()+
  theme(axis.text.x = element_text(angle = 90))

ggplot(sjmm_10, aes(x= isei08, color = as_factor(noga1995_agg10), fill = as_factor(noga1995_agg10)))+
  geom_density(alpha = 0.1)+
  theme(axis.text.x = element_text(angle = 90))

ggplot(sjmm_10, aes(x= isei08, color = as_factor(maintask), fill = as_factor(maintask)))+
  geom_density(alpha = 0.1)+
  theme(axis.text.x = element_text(angle = 90))

ggplot(sjmm_10, aes(x = as_factor(maintask), y= isei08, color = as_factor(maintask), fill = as_factor(maintask)))+
  geom_boxplot(alpha=0.2)+
  theme(axis.text.x = element_text(angle = 90))

ggplot(sjmm_10, aes(x = as_factor(maintask), y= trei08, color = as_factor(maintask), fill = as_factor(maintask)))+
  geom_boxplot(alpha=0.2)+
  theme(axis.text.x = element_text(angle = 90))

ggplot(sjmm_10, aes(x= as_factor(edu1_type), fill = as_factor(exec)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 90))

ggplot(sjmm_10, aes(x= as_factor(noga1995_agg10), fill = as_factor(exec)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 90))

ggplot(sjmm_10, aes(x= as_factor(industry), fill = as_factor(maintask)))+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 90))

ggplot(sjmm_10, aes(x = as_factor(maintask), y= scope_hours, color = as_factor(maintask), fill = as_factor(maintask)))+
  geom_boxplot(alpha=0.2)+
  theme(axis.text.x = element_text(angle = 90))










