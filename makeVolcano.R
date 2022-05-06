makeVolcano <- function(df, contrast) {
  
  suppressWarnings(
    deg_res %>%
      filter(Contrast == contrast) %>%
      mutate(Regulation = case_when(
        Padj < 0.05 & LogFC > 1 ~ "Upregulated",
        Padj < 0.05 & LogFC < -1 ~ "Downregulated",
        TRUE ~ "None"
      )) %>%
      mutate(Labels = ifelse(Gene_id %in% top_degs, Gene_id, NA_character_)) %>%
      ggplot(aes(x = LogFC, y = -log10(Padj), col = Regulation, label = Labels)) +
      geom_point(size = 2, alpha = 0.5) +
      ggrepel::geom_label_repel(
        size = 3, label.size = 0.5, fontface = 2, show.legend = FALSE,
        box.padding = 0.5, point.padding = 0.5, nudge_x = 0.5, nudge_y = 0.5,
        arrow = arrow(length = unit(x = 0.25, units = "cm")), max.overlaps = Inf
      ) +
      geom_vline(xintercept = c(-1, 1), col = "black", lty = "dashed") +
      geom_hline(yintercept = -log10(0.05), col = "black", lty = "dashed") +
      scale_color_manual(
        breaks = c("Upregulated", "Downregulated", "None"),
        values = c("firebrick1", "royalblue1", "gray50")
      ) +
      scale_x_continuous(
        breaks = seq.int(
          from = floor(min(deg_res$LogFC)),
          to = ceiling(max(deg_res$LogFC)),
          by = 1
        ),
        limits = c(
          floor(min(deg_res$LogFC)),
          ceiling(max(deg_res$LogFC))
        )
      ) +
      labs(
        x = expression(log[2]~fold~change),
        y = expression(-log[10]~adjusted~p.value),
        color = NULL,
        title = paste0("Comparison: ", str_to_title(contrast)),
        subtitle = "Top 10 up- and down-regulated DEGs"
      ) +
      guides(colour = guide_legend(override.aes = list(size = 5))) +
      theme(
        text = element_text(size = 14, color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "top",
        legend.direction = "horizontal",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, color = "black", size = 1),
        legend.key = element_rect(fill = "white")
      )
  )
  
}
