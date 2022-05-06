doEdgeR <- function(countData, colData, variable, name_contrast) {
  DGEList(
    counts = countData,
    group = colData[[variable]],
    lib.size = NULL,
    norm.factors = NULL
  ) %>%
    .[filterByExpr(.), , keep.lib.sizes = FALSE] %>%
    calcNormFactors() %>%
    estimateDisp(design = model.matrix(~ colData[[variable]])) %>%
    glmQLFit(design = model.matrix(~ colData[[variable]])) %>%
    glmQLFTest(coef = 2) %>%
    topTags() %>%
    as.data.frame() %>%
    rownames_to_column("gene_id") %>%
    mutate(contrast = name_contrast) %>%
    mutate(significant = ifelse(FDR < 0.05, "Yes", "No")) %>%
    mutate(direction = ifelse(logFC > 0, "Upregulated", "Downregulated")) %>%
    select(gene_id, contrast, logFC, padj = FDR, significant, direction) %>%
    as_tibble() %>%
    mutate(Tool = "EdgeR", .before = everything())
}
