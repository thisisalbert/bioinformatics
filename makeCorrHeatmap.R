makeCorrHeatmap <- function(counts, size_text) {
  
  corr_mat <- counts %>%
    cor(method = "pearson") %>%
    round(1)
  
  corr_pval <- counts %>%
    ggcorrplot::cor_pmat()
  
  corr_plot <- ggcorrplot::ggcorrplot(
    corr = corr_mat,
    p.mat = corr_pval,
    sig.level = 0.05,
    method = "square",
    type = "full",
    hc.order = TRUE,
    outline.color = "black",
    insig = "pch",
    tl.cex = size_text
  )
  
  if (any(corr_mat < 0)) {
    corr_plot <- corr_plot +
      scale_fill_gradient2(
        breaks = seq(-1, 1, 0.25),
        limit = c(-1, 1),
        low = "#4A6FE3",
        mid = "#DEDEE3",
        high = "#D33F6A",
        guide = guide_colorbar(
          frame.colour = "black",
          ticks.colour = "black",
          frame.linewidth = 2
        )
      )
  } else {
    corr_plot <- corr_plot +
      scale_fill_gradient2(
        breaks = seq(0, 1, 0.25),
        limit = c(0, 1),
        low = "white",
        high = "#D33F6A",
        guide = guide_colorbar(
          frame.colour = "black",
          ticks.colour = "black",
          frame.linewidth = 2
        )
      )
  }
  
  corr_plot +
    labs(
      fill = NULL, 
      title = "Correlation Heatmap with hierarchical clustering"
    ) +
    theme(
      plot.title = element_text(size = size_text, face = "bold", hjust = 0.5),
      legend.text = element_text(size = size_text),
      legend.key.height = unit(1.5, "cm"),
      panel.border = element_rect(size = 0.5, fill = NA),
      panel.background = element_rect(fill = "black"),
      panel.grid = element_blank()
    )
  
}
