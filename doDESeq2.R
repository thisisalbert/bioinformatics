doDESeq2 <- function(countData, colData, variable, name_contrast) {
  DESeqDataSetFromMatrix(
    countData = countData,
    colData = colData,
    design = formula(paste0("~ ", variable))
  ) %>%
    DESeq() %>%
    results() %>%
    as.data.frame() %>%
    rownames_to_column("gene_id") %>%
    as_tibble() %>%
    drop_na() %>%
    mutate(significant = case_when(
      padj < 0.05 ~ "Yes",
      TRUE ~ "No"
    )) %>%
    mutate(direction = case_when(
      log2FoldChange > 0 ~ "Upregulated",
      TRUE ~ "Downregulated"
    )) %>%
    mutate(contrast = name_contrast) %>%
    select(gene_id, contrast, log2FoldChange, padj, significant, direction) %>%
    dplyr::rename(logFC = log2FoldChange) %>%
    mutate(Tool = "DESeq2", .before = everything())
}
