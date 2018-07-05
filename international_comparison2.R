    framework %>% 
        left_join(survey) %>% 
        gather(variable, value, -code, -economy, -year, -Iceland) %>% 
        group_by(variable) %>%
        mutate(rank = min_rank(value),
               n = n(),
               label_text = paste(rank, " / ", n),
               y_label = if_else(rank < 60, rank + 5, rank - 55)) %>% 
        arrange(year) %>% 
        filter(economy %>% str_detect("Iceland"),
               !is.na(value)) %>% 
        ggplot(aes(x = fct_reorder(variable, rank), y = rank)) +
        geom_col() +
        geom_text(aes(label = label_text, y = y_label), color = intellecon_pal_discrete[1],
                  angle = 90, size = 3, hjust = 0) +
        theme_intellecon() %+replace%
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) +
        labs(x = "Breyta", y = "Raðtala / Sæti")
    
    df1 %>% left_join(df2, by)
    
    

# Einkunn -----------------------------------------------------------------

    # Source: http://www.gemconsortium.org/data/key-nes
    # Source: http://www.gemconsortium.org/data/key-aps

    library(tidyverse)
    library(magrittr)
    
    
    require(ggplot2)
    require(extrafont)
    require(scales)
    
    survey <- read_csv("behavior_and_attitudes.csv", skip = 1)
    framework <- read_csv("entrepreneurial framework conditions.csv", skip = 1)
    
    framework %>% filter(economy == "Iceland")
    framework %>% names
    survey %>% filter(economy == "Iceland")
    
    # loadfonts(device = "win")
    # font_import(pattern='GARA')
    
    
    intellecon_pal_discrete <- c(
        "#6E1704", # Vínrauður
        "#E69F00",  # Gull,
        "#999999",   # Grár)
        "#56B4E9",  # Ljósblár
        "#77AB43"
    ) 
    
    scale_fill_intellecon <- list(scale_fill_manual(values = intellecon_pal_discrete))
    
    
    vertical_x_label <- list(theme(axis.text.x = element_text(angle = 90, hjust = 0.8, vjust = 1)))
    y_format_percentage <- scale_y_continuous(labels = scales::percent)
    
    point <- scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)
    
    y_format_number <- scale_y_continuous(labels = point)
    
    # kr <-  scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)
    
    y_format_kr <- scale_y_continuous(labels = dollar_format(prefix = "",
                                                             suffix = " kr.",
                                                             largest_with_cents = 0, 
                                                             big.mark = ".",
                                                             decimal.mark = ",",
                                                             negative_parens = FALSE))
    
    # Have vertical lines
    coord_flip_lines <- theme(          panel.grid.major.y = element_blank(),
                                        panel.grid.major.x = element_line(colour = "gray70"),
                                        panel.grid.minor.x = element_blank())
    
    
    theme_intellecon <- function(){
        require(extrafont)
        col_title <- "#6E1704"
        theme_grey() %+replace% 
            theme(legend.direction = "horizontal",
                  legend.position = "bottom",
                  legend.background = element_rect(color = NA, fill = NA),
                  panel.background = element_rect(fill = "gray70", colour = NA), 
                  panel.border = element_rect(fill = NA, colour = NA), 
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_line(colour = "white"),
                  panel.grid.minor.y = element_blank(), 
                  panel.spacing = unit(0.2, "lines"),
                  axis.ticks = element_line(colour = NA, size = 0.25),
                  # axis.line = element_line(colour = "black", size = 0.3, inherit.blank = FALSE),
                  axis.line.x = element_blank(),
                  axis.title = element_text(hjust = 0.5),
                  axis.text = element_text(colour = col_title),
                  legend.key = element_rect(fill = "white", colour = NA), 
                  # strip.background = element_rect(fill = "grey60", colour = NA),
                  strip.background = element_rect(fill = col_title, colour = NA),
                  strip.text = element_text(colour = "white", size = rel(0.8)),
                  strip.placement = "outside",
                  plot.subtitle = element_text(colour = "gray70", family = "Garamond"),
                  plot.background = element_rect(fill = "gray80", color = NA),
                  # plot.text = element_text(size = 12),
                  title = element_text(colour = col_title, size = 16, hjust = 0, family = "Garamond"),
                  # aspect.ratio = 9/18,
                  complete = FALSE)
    }    
    
    
    # framework <- framework %>% 
    #     group_by(code, economy) %>% 
    #     arrange(year) %>% 
    #     filter(year == last(year)) %>% 
    #     ungroup %>% 
    #     mutate(Iceland = if_else(economy != "Iceland", "Nei", "Já"),
    #            economy = paste0(economy, " (", year,")"))
    # 
    # survey <- survey %>% 
    #     group_by(code, economy) %>% 
    #     arrange(year) %>% 
    #     filter(year == last(year)) %>% 
    #     ungroup %>% 
    #     mutate(Iceland = if_else(economy != "Iceland", "Nei", "Já"),
    #            economy = paste0(economy, " (", year,")"))
    
    
    framework %>% 
        mutate(Iceland = if_else(economy == "Iceland", "Já", "Nei")) %>% 
        group_by(economy) %>% 
        arrange(economy, year) %>% 
        filter(year == last(year)) %>% 
        # filter(!is.na(`Governmental support and policies`)) %>% 
        gather(var, value, -year, -code, -economy, -Iceland) %>% 
        mutate(value = as.numeric(value)) %>% 
        group_by(economy) %>% 
        mutate(rank = median(value, na.rm = TRUE),
               var = stringr::str_wrap(var, width = 18)) %>% 
        ungroup() %>% 
        mutate(
            value = if_else(is.na(value), 0, value),
            var_rank = as.numeric(as.factor(var))) %>% 
        filter(var_rank >= median(var_rank)) %>% 
        group_by(var) %>% 
        arrange(var, value) %>% 
        mutate(pos = -row_number(),
               lab = if_else(value == 0, "", economy)) %>% 
        ggplot(aes(x = pos, 
                   group = var, 
                   y = value,
                   fill = Iceland)) +
        geom_col() +
        geom_text(aes(label = lab, y = 0.2, colour = Iceland),
                  size = 1.8, hjust = "left", angle = 90) +
        facet_grid(var~., shrink = FALSE) +
        labs(x = "Land (Ár)") +
        scale_fill_manual(values = intellecon_pal_discrete) +
        theme_intellecon() %+replace%
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              strip.text.y = element_text(size = 8, angle = 0)) +
        xlim(c(0, 109)) +
        coord_cartesian(ylim = c(0, 5)) +
        scale_x_continuous(labels = NULL) +
        scale_color_manual(values = intellecon_pal_discrete[2:1], guide = "none")+
        labs(fill = "Ísland", y = "Einkunn") 
    
    ggsave("einkunnir.svg", height = 12, width = 10)
    
    
    framework %>% 
        mutate(Iceland = if_else(economy == "Iceland", "Já", "Nei")) %>% 
        group_by(economy) %>% 
        arrange(economy, year) %>% 
        filter(year == last(year)) %>% 
        # filter(!is.na(`Governmental support and policies`)) %>% 
        gather(var, value, -year, -code, -economy, -Iceland) %>% 
        mutate(value = as.numeric(value)) %>% 
        group_by(economy) %>% 
        mutate(rank = median(value, na.rm = TRUE),
               var = stringr::str_wrap(var, width = 18)) %>% 
        ungroup() %>% 
        mutate(
            value = if_else(is.na(value), 0, value),
            var_rank = as.numeric(as.factor(var))) %>% 
        filter(var_rank < median(var_rank)) %>% 
        group_by(var) %>% 
        arrange(var, value) %>% 
        mutate(pos = -row_number(),
               lab = if_else(value == 0, "", economy)) %>% 
        ggplot(aes(x = pos, 
                   group = var, 
                   y = value,
                   fill = Iceland)) +
        geom_col() +
        geom_text(aes(label = lab, y = 0.2, colour = Iceland),
                  size = 1.8, hjust = "left", angle = 90) +
        facet_grid(var~., shrink = FALSE) +
        labs(x = "Land (Ár)") +
        scale_fill_manual(values = intellecon_pal_discrete) +
        theme_intellecon() %+replace%
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              strip.text.y = element_text(size = 8, angle = 0)) +
        xlim(c(0, 109)) +
        coord_cartesian(ylim = c(0, 5)) +
        scale_x_continuous(labels = NULL) +
        scale_color_manual(values = intellecon_pal_discrete[2:1], guide = "none")+
        labs(fill = "Ísland", y = "Einkunn") 
    
    ggsave("einkunnir2.svg", height = 12, width = 10)
    
    
    survey %>% 
        mutate(Iceland = if_else(economy == "Iceland", "Já", "Nei")) %>% 
        group_by(economy) %>% 
        arrange(economy, year) %>% 
        filter(year == last(year)) %>% 
        # filter(!is.na(`Governmental support and policies`)) %>% 
        gather(var, value, -year, -code, -economy, -Iceland) %>% 
        mutate(value = as.numeric(value)) %>% 
        group_by(economy) %>% 
        mutate(rank = median(value, na.rm = TRUE),
               var = stringr::str_wrap(var, width = 18)) %>% 
        ungroup() %>% 
        mutate(
            value = if_else(is.na(value), 0, value),
            var_rank = as.numeric(as.factor(var))) %>% 
        filter(var_rank >= median(var_rank)) %>% 
        group_by(var) %>% 
        arrange(var, value) %>% 
        mutate(pos = -row_number(),
               lab = if_else(value == 0, "", economy)) %>% 
        ggplot(aes(x = pos, 
                   group = var, 
                   y = value,
                   fill = Iceland)) +
        geom_col() +
        geom_text(aes(label = lab, y = 0.2, colour = Iceland),
                  size = 1.8, hjust = "left", angle = 90) +
        facet_grid(var~., shrink = FALSE, scales = "free_y") +
        labs(x = "Land (Ár)") +
        scale_fill_manual(values = intellecon_pal_discrete) +
        theme_intellecon() %+replace%
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              strip.text.y = element_text(size = 8, angle = 0)) +
        xlim(c(0, 109)) +
        # coord_cartesian(ylim = c(0, 5)) +
        scale_x_continuous(labels = NULL) +
        scale_color_manual(values = intellecon_pal_discrete[2:1], guide = "none")+
        labs(fill = "Ísland", y = "Einkunn") 
    
    ggsave("einkunnir3.svg", height = 12, width = 10)
    
    
    survey %>% 
        mutate(Iceland = if_else(economy == "Iceland", "Já", "Nei")) %>% 
        group_by(economy) %>% 
        arrange(economy, year) %>% 
        filter(year == last(year)) %>% 
        # filter(!is.na(`Governmental support and policies`)) %>% 
        gather(var, value, -year, -code, -economy, -Iceland) %>% 
        mutate(value = as.numeric(value)) %>% 
        group_by(economy) %>% 
        mutate(rank = median(value, na.rm = TRUE),
               var = stringr::str_wrap(var, width = 18)) %>% 
        ungroup() %>% 
        mutate(
            value = if_else(is.na(value), 0, value),
            var_rank = as.numeric(as.factor(var))) %>% 
        filter(var_rank < median(var_rank)) %>% 
        group_by(var) %>% 
        arrange(var, value) %>% 
        mutate(pos = -row_number(),
               lab = if_else(value == 0, "", economy)) %>% 
        ggplot(aes(x = pos, 
                   group = var, 
                   y = value,
                   fill = Iceland)) +
        geom_col() +
        geom_text(aes(label = lab, y = 0.2, colour = Iceland),
                  size = 1.8, hjust = "left", angle = 90) +
        facet_grid(var~., shrink = FALSE, scales = "free_y") +
        labs(x = "Land (Ár)") +
        scale_fill_manual(values = intellecon_pal_discrete) +
        theme_intellecon() %+replace%
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              strip.text.y = element_text(size = 8, angle = 0)) +
        xlim(c(0, 109)) +
        # coord_cartesian(ylim = c(0, 5)) +
        scale_x_continuous(labels = NULL) +
        scale_color_manual(values = intellecon_pal_discrete[2:1], guide = "none")+
        labs(fill = "Ísland", y = "Einkunn") 
    
    ggsave("einkunnir4.svg", height = 12, width = 10)
    