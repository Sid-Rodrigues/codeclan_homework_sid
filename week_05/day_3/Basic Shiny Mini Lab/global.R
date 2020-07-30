# function to plot medals
plot_medal <- function(s, m){
 #olympics_overall_medals %>%
    olympics_overall_medals %>%
    filter(team %in% c("United States",
                       "Soviet Union",
                       "Germany",
                       "Italy",
                       "Great Britain")) %>%
    filter(season == s) %>%
    filter(medal == m)  %>%
    ggplot() +
    aes(x = team, y = count, fill = medal) +
    geom_col() +
        theme(
            panel.grid.major = element_line(colour = "grey", linetype = "dashed"), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA)
            
        ) +
    
    scale_fill_manual(values = case_when(
                                m == "Gold" ~ "#C98910",
                                m == "Silver" ~ "#A8A8A8",
                                m == "Bronze" ~ "#965A38"
                                )
                      ) 
        
                
    
}


