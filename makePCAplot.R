makePCAplot <- function(counts, metadata, color, fill, shape, size_text) {
  pca_obj <- counts %>%
    t() %>%
    prcomp(center = TRUE)
  
  pca_var <- summary(pca_obj) %>%
    chuck("importance") %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    filter(rowname == "Proportion of Variance") %>%
    select(PC1, PC2) %>%
    multiply_by(100) %>%
    round(2)
  
  pca_df <- pca_obj %>%
    chuck("x") %>%
    as.data.frame() %>%
    rownames_to_column("source_name") %>%
    left_join(x = ., y = metadata, by = "source_name") %>%
    select(source_name, PC1, PC2, all_of(color), all_of(fill), all_of(shape)) %>%
    as_tibble() %>% 
    mutate(across(-c(source_name, PC1, PC2), as.factor))
  
  pca_df %>%
    ggplot(
      aes_string(x = "PC1", y = "PC2", color = color, fill = fill, shape = shape)
    ) +
    geom_point(size = 8, alpha = 0.8) +
    stat_ellipse(
      aes_string(x = "PC1", y = "PC2", fill = fill, color = color),
      geom = "polygon",
      alpha = 0.1,
      inherit.aes = FALSE
    ) +
    scale_color_paletteer_d("ggthemes::Tableau_10") +
    scale_fill_paletteer_d("ggthemes::Tableau_10") +
    geom_hline(yintercept = 0, lty = 2, color = "gray75") +
    geom_vline(xintercept = 0, lty = 2, color = "gray75") +
    labs(
      title = "PCA plot",
      subtitle = paste0(sum(pca_var$PC1, pca_var$PC2), "% cumulative variance"),
      x = paste0("PC1 (", pca_var$PC1, "% explained variance)"),
      y = paste0("PC2 (", pca_var$PC2, "% explained variance)")
    ) +
    coord_fixed() +
    theme_bw() +
    theme(
      text = element_text(size = size_text),
      plot.title = element_text(face = "bold"),
      aspect.ratio = 1
    )
}
