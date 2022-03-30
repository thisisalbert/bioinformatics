makeVolcano <- function(df, contrast) {
  
  suppressWarnings(
    df %>%
      filter(contrast == contrast) %>%
      mutate(regulation = case_when(
        padj < 0.05 & logFC > 1 ~ "Upregulated",
        padj < 0.05 & logFC < -1 ~ "Downregulated",
        TRUE ~ "None"
      )) %>%
      mutate(labels = ifelse(gene_id %in% top_degs, gene_id, NA_character_)) %>%
      ggplot(aes(x = logFC, y = -log10(padj), col = regulation, label = labels)) +
      geom_point(size = 2, alpha = 0.8) +
      ggrepel::geom_label_repel(
        size = 3, label.size = 0.5, fontface = 2, show.legend = FALSE,
        box.padding = 0.5, point.padding = 0.5, nudge_x = 0.5,
        arrow = arrow(length = unit(x = 0.25, units = "cm")), max.overlaps = Inf
      ) +
      geom_vline(xintercept = c(-1, 1), col = "black", lty = "dashed") +
      geom_hline(yintercept = -log10(0.05), col = "black", lty = "dashed") +
      scale_color_manual(
        breaks = c("Upregulated", "Downregulated", "None"),
        values = c("firebrick1", "royalblue1", "gray50")
      ) +
      scale_x_continuous(breaks = seq.int(
        from = ceiling(min(all_res$logFC)),
        to = ceiling(max(all_res$logFC)),
        by = 1
      )) +
      labs(
        x = expression(log[2]~fold~change),
        y = expression(-log[10]~adjusted~p.value),
        color = NULL,
        title = contrast %>% str_to_title(),
        subtitle = "Top DEGs"
      ) +
      theme(
        text = element_text(size = 14, color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "top",
        legend.direction = "horizontal",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray85"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, color = "black", size = 1),
        legend.background = element_blank()
      )
  )
  
}
