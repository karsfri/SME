---
title: "International Comparison"
author: "Magnús Árni Skúlason og Kári S Friðriksson"
output: word_document
---

library(tidyverse)
library(magrittr)

# Source: http://www.gemconsortium.org/data/key-nes
# Source: http://www.gemconsortium.org/data/key-aps


# Flytja inn gögn ---------------------------------------------------------


survey <- read_csv("behavior_and_attitudes.csv", skip = 1)
framework <- read_csv("entrepreneurial framework conditions.csv", skip = 1)

framework %>% filter(economy == "Iceland")
framework %>% names
survey %>% filter(economy == "Iceland")


# Hreinsa -----------------------------------------------------------------

 
framework <- framework %>% 
    group_by(code, economy) %>% 
    arrange(year) %>% 
    filter(year == last(year)) %>% 
    ungroup %>% 
    mutate(Iceland = if_else(economy != "Iceland", "Nei", "Já"),
           economy = paste0(economy, " (", year,")"))

survey <- survey %>% 
    group_by(code, economy) %>% 
    arrange(year) %>% 
    filter(year == last(year)) %>% 
    ungroup %>% 
    mutate(Iceland = if_else(economy != "Iceland", "Nei", "Já"),
           economy = paste0(economy, " (", year,")"))


# Myndir ------------------------------------------------------------------




framework %>% 
    filter(!is.na(`Cultural and social norms`)) %>% 
    ggplot(aes(x = fct_reorder(economy, -`Cultural and social norms`), y = `Cultural and social norms`,
               fill = Iceland)) +
    geom_col() +
    labs(x = "Land (Ár)") +
    scale_fill_manual(values = intellecon_pal_discrete) +
    theme_intellecon() %+replace%
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
framework %>% 
    ggplot(aes(x = fct_reorder(economy, -`Financing for entrepreneurs`), y = `Financing for entrepreneurs`,
               fill = Iceland)) +
    geom_col() +
    labs(x = "Land (Ár)") +
    scale_fill_manual(values = intellecon_pal_discrete) +
    theme_intellecon() %+replace%
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


survey %>% 
    ggplot(aes(x = fct_reorder(economy, -`Entrepreneurship as a Good Career Choice`), y = `Entrepreneurship as a Good Career Choice`,
               fill = Iceland)) +
    geom_col() +
    labs(x = "Land (Ár)") +
    scale_fill_manual(values = intellecon_pal_discrete) +
    theme_intellecon() %+replace%
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


    
