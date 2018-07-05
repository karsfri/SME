require(ggplot2)
require(extrafont)
require(scales)

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



update_geom_defaults("boxplot", list(fill = intellecon_pal_discrete[1]))
update_geom_defaults("boxplot", list(outlier.colour = intellecon_pal_discrete[2]))
update_geom_defaults("col", list(fill = intellecon_pal_discrete[1]))
update_geom_defaults("bar", list(fill = intellecon_pal_discrete[1]))
update_geom_defaults("line", list(colour = intellecon_pal_discrete[2]))
update_geom_defaults("fill", list(colour = intellecon_pal_discrete[2]))

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
              legend.background = element_blank(),
              legend.key = element_blank(),
              panel.background = element_rect(fill = NA, colour = NA), 
              panel.border = element_rect(fill = NA, colour = NA), 
              panel.grid.major.y = element_line(colour = "white"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(), 
              panel.spacing = unit(0, "lines"),
              axis.ticks = element_line(colour = NA, size = 0.25),
              # axis.line = element_line(colour = "black", size = 0.3, inherit.blank = FALSE),
              # axis.line.y = ,
              axis.title = element_text(hjust = 0.5),
              # legend.key = element_rect(fill = NA, colour = NA), 
              legend.box.background = element_rect(fill = NA, colour = NA),
              strip.background = element_rect(fill = "grey70", colour = NA),
              strip.text = element_text(colour = "white", size = rel(0.8)),
              plot.subtitle = element_text(colour = "gray70", family = "Garamond"),
              plot.background = element_rect(fill = "gray80", color = NA),
              title = element_text(colour = col_title, size = 16, hjust = 0, family = "Garamond"),
              # aspect.ratio = 9/18,
              complete = FALSE)
}

theme_intellecon_line <- function(){
    require(ggplot2)
    
    
    list(
        theme_intellecon_basic(),
        coord_cartesian(expand = FALSE),
        scale_color_manual(values = intellecon_pal_discrete),
        scale_fill_manual(values = intellecon_pal_discrete)
    )
}



theme_intellecon_sleek <- function(){
    list(
        theme_intellecon_basic() %+replace%
            theme(axis.line.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.title.y = element_blank(),
                  panel.grid = element_blank()),
        coord_cartesian(expand = FALSE),
        scale_color_manual(values = intellecon_pal_discrete),
        scale_fill_manual(values = intellecon_pal_discrete)
    )
}

