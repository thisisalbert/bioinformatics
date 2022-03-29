makePCAplot <- function(counts, metadata, relevant_features, key_feature) {
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
      rownames_to_column("sample_id") %>%
      left_join(x = ., y = metadata, by = "sample_id") %>%
      select(sample_id, PC1, PC2, all_of(relevant_features)) %>%
      as_tibble()

    pca_df %>%
      ggplot(aes(x = PC1, y = PC2, color = key_feature, fill = key_feature,
                 shape = technical_replicate)) +
      geom_point(size = 8, alpha = 0.8) +
      stat_ellipse(
        aes(x = PC1, y = PC2, fill = key_feature, color = key_feature),
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
      theme_bw() +
      theme(
        text = element_text(size = 16),
        plot.title = element_text(face = "bold")
      )
  }
