plot_stat_study_mann_whitney <- function(df){

    g <- ggplot(df, 
                aes(p.value, 
                    100*(value.1 - value.2)/value.1, 
                    label = ifelse(p.value < .05,cytokine,NA))) + 
      geom_point() + theme_bw() + geom_label_repel() + 
      geom_vline(xintercept = .05, linetype = "dashed") + scale_x_log10() +
      labs(x = "P value", 
           y = "Relative difference [%]", 
           caption = paste0("Groups ",df$group.1[1]," (ref) vs ",df$group.2[1], ", Mann-Whitney"))

  return(g)
}
