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

diffPlot <- function(df_plot, refLevel = "healthy"){
  tmp <- Reduce(rbind, lapply(unique(df_plot$condition), function(i){
    Reduce(rbind, lapply(unique(df_plot$cytokine[df_plot$condition == i]), function(x){
      data.frame(
        cytokine = x,
        diff = (subset(df_plot, condition == i & cytokine == x)$value-subset(df_plot, condition == refLevel & cytokine == x)$value)/subset(df_plot, condition == refLevel & cytokine == x)$value,
        condition = i
      )
    }))
  }))
  tmp <- subset(tmp, condition != refLevel)
  tmp$cytokine <- factor(tmp$cytokine, levels = unique(tmp$cytokine)[order(subset(tmp, condition == tmp$condition[1])$diff)])
  
  return(
    ggplotly(
      ggplot(tmp, aes(cytokine, 100*diff, color = condition)) + 
        geom_segment( aes(xend=cytokine, y=0, yend=100*diff)) + 
        geom_point() + theme_bw() + coord_flip() + 
        facet_wrap(~condition) + theme(legend.position = "none") +
        labs(y = paste0("Deviation from ", refLevel, " [%]"), x = "Cytokine")
    )
  )
}