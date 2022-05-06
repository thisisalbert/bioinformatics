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
    mutate(significant = ifelse(padj < 0.05, "Yes", "No")) %>%
    mutate(direction = ifelse(log2FoldChange > 0, "Upregulated", "Downregulated")) %>%
    mutate(contrast = name_contrast) %>%
    select(gene_id, contrast, logFC = log2FoldChange, padj, significant, direction) %>%
    mutate(Tool = "DESeq2", .before = everything())
}
