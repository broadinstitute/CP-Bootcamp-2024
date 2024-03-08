
# ----
# CP BOOTCAMP 2024 : INTRODUCTION TO TIDYVERSE - LECTURE 3
# Author: Mustafa A. Kocak
# Date: March 8, 2024
# Description: Examples with DepMap data
# ----

# PLAN FOR TODAY ----
# Load the libraries we will use
# Useful taiga links for the project week
# Playing with Hotspot mutations - KRAS - EGFR, and more...
# Introduction to PRISM datasets - MDM2 inhibitors
# A quick projection of PRISM Repurposing Primary
# Examples of volcano and waterfall plots
# Listing MNNs between genetic and chemical dependencies
# Taking a look at ONCREF dataset

# Libraries ----

library(tidyverse)
library(magrittr)
library(useful)
library(scales)
library(ggthemes)
library(ggrepel)
library(taigr)
library(ggpubr)
library(uwot)
library(ggpubr)

# make sure to edit this line:
options(taigaclient.path=path.expand("/Users/mkocak/anaconda3/envs/taigapy/bin/taigaclient"))



# Loading data (and more taiga links) ----

# Cell line metadata 
Model <- load.from.taiga(data.name='internal-23q4-ac2b', data.version=68, data.file='Model')



# Genetic dependencies
CRISPRGeneDependency <- load.from.taiga(data.name='internal-23q4-ac2b', data.version=68, data.file='CRISPRGeneDependency')
CRISPRGeneEffect <- load.from.taiga(data.name='internal-23q4-ac2b', data.version=68, data.file='CRISPRGeneEffect')
OrganoidGeneEffect <- load.from.taiga(data.name='internal-23q4-ac2b', data.version=68, data.file='OrganoidGeneEffect')


# Omics datasets
OmicsAbsoluteCNGene <- load.from.taiga(data.name='internal-23q4-ac2b', data.version=68, data.file='OmicsAbsoluteCNGene')
OmicsArmLevelCNA <- load.from.taiga(data.name='internal-23q4-ac2b', data.version=68, data.file='OmicsArmLevelCNA')
OmicsCNGene <- load.from.taiga(data.name='internal-23q4-ac2b', data.version=68, data.file='OmicsCNGene')

OmicsExpressionGeneSetEnrichment <- load.from.taiga(data.name='internal-23q4-ac2b', data.version=68, data.file='OmicsExpressionGeneSetEnrichment')
OmicsExpressionProteinCodingGenesTPMLogp1 <- load.from.taiga(data.name='internal-23q4-ac2b', data.version=68, data.file='OmicsExpressionProteinCodingGenesTPMLogp1')

OmicsFusionFiltered <- load.from.taiga(data.name='internal-23q4-ac2b', data.version=68, data.file='OmicsFusionFiltered')


OmicsSomaticMutations <- load.from.taiga(data.name='internal-23q4-ac2b', data.version=68, data.file='OmicsSomaticMutations')
OmicsSomaticMutationsMatrixDamaging <- load.from.taiga(data.name='internal-23q4-ac2b', data.version=68, data.file='OmicsSomaticMutationsMatrixDamaging')
OmicsSomaticMutationsMatrixHotspot <- load.from.taiga(data.name='internal-23q4-ac2b', data.version=68, data.file='OmicsSomaticMutationsMatrixHotspot')


OmicsLoH <- load.from.taiga(data.name='internal-23q4-ac2b', data.version=68, data.file='OmicsLoH')
OmicsSignatures <- load.from.taiga(data.name='internal-23q4-ac2b', data.version=68, data.file='OmicsSignatures')
OmicsStructuralVariants <- load.from.taiga(data.name='internal-23q4-ac2b', data.version=68, data.file='OmicsStructuralVariants')


# Beta features
MethylationExpressionImpactBroad <- load.from.taiga(data.name='internal-beta-features-7130', data.version=31, data.file='MethylationExpressionImpactBroad')
MethylationExpressionImpactSanger <- load.from.taiga(data.name='internal-beta-features-7130', data.version=31, data.file='MethylationExpressionImpactSanger')
DriverGeneFunctionalAlterations <- load.from.taiga(data.name='internal-beta-features-7130', data.version=31, data.file='DriverGeneFunctionalAlterations')

# Harmonized Expression Dataset : GTEx, TCGA & CCLE
harmonized.TCGA.GTEx.CCLE <- load.from.taiga(data.name='harmonized-tcga-gtex-target-and-ccle-baseline-expression-280a', data.version=8, data.file='harmonized_TCGA_GTEx_CCLE')
metadata.TCGA.GTEx.CCLE <- load.from.taiga(data.name='harmonized-tcga-gtex-target-and-ccle-baseline-expression-280a', data.version=8, data.file='metadata_TCGA_GTEx_CCLE')
protein.coding.genes <- load.from.taiga(data.name='harmonized-tcga-gtex-target-and-ccle-baseline-expression-280a', data.version=8, data.file='protein_coding_genes') # little bit outdated


# PRISM Repurposing (Files from last year)
prism.bootcamp <- load.from.taiga(data.name='prism-repurposing-combined-matrix-0c75', data.version=2, data.file='prism_bootcamp')
prism.matrix.bootcamp <- load.from.taiga(data.name='prism-repurposing-combined-matrix-0c75', data.version=2, data.file='prism_matrix_bootcamp')
compound.metadata <- load.from.taiga(data.name='compound-metadata-de37', data.version=23, data.file='compound_metadata')


# PRISM OncRef
OncRef.Compound.List <- load.from.taiga(data.name='prism-oncology-reference-set-23q4-1a7c', data.version=14, data.file='PRISM_Oncology_Reference_23Q4-Compound_List')
OncRef.AUC.matrix <- load.from.taiga(data.name='prism-oncology-reference-set-23q4-1a7c', data.version=13, data.file='AUC_matrix') %>%
  t()
OncRef.LFC.Matrix <- load.from.taiga(data.name='prism-oncology-reference-set-23q4-1a7c', data.version=13, data.file='PRISM_Oncology_Reference_23Q4_LFC_Matrix')


# Please check out the portal for more, for example: proteomics datasets 
# and other drug datasets (CTD2 and GDSC)


# Vignette 0: Let's clean-up the compound meta-data -----

compound_metadata <- compound.metadata %>% 
  dplyr::distinct(Drug.Name,
                  IDs, MOA, 
                  repurposing_target, 
                  Synonyms) %>% 
  dplyr::rename(Target = repurposing_target) %>%
  tidyr::separate_rows(IDs, sep = ";") 


# Vignette 1: Which mutations are occuring or not occuring together ----

OmicsSomaticMutationsMatrixHotspot %>% corner

# simplify the column names for the hotspot mutation matrix
colnames(OmicsSomaticMutationsMatrixHotspot)  %<>% word()




# let's start with a simple example: KRAS and EGFR
EGFR.KRAS.mutations <- OmicsSomaticMutationsMatrixHotspot[, c("EGFR", "KRAS")] %>% 
  as.tibble() %>% 
  dplyr::mutate(ModelID = rownames(OmicsSomaticMutationsMatrixHotspot)) 


# What does 2 mean?
EGFR.KRAS.mutations %>% 
  dplyr::count(EGFR, KRAS)

# Let's keep it simple
EGFR.KRAS.mutations <- EGFR.KRAS.mutations %>%
  dplyr::mutate(EGFR  = EGFR > 0, KRAS = KRAS > 0)

# Is this an implicit NA? 
EGFR.KRAS.mutations %>% 
  dplyr::count(EGFR, KRAS)

# Do we have enough power to support our claim?
EGFR.KRAS.mutations %>% 
  dplyr::count(EGFR, KRAS) %>% 
  tidyr::complete(EGFR,KRAS, fill = list(n = 0)) %>% 
  .$n %>% matrix(2) %>%   
  fisher.test()

# Let's look for other pairs?

# First let's simplify our mutation matrix for relatively common mutations 
# and binarize it

HS.Mutations <- OmicsSomaticMutationsMatrixHotspot[, apply(OmicsSomaticMutationsMatrixHotspot, 2, sum) > 4] > 0

# let's get the pairwise co-occurance counts
co.occurences <- crossprod(HS.Mutations) 

corner(co.occurences)

# total number of samples
n = nrow(HS.Mutations)

gene1 = "EGFR"
gene2 = "KRAS"


# a  helper function to compute the contingency matrix
contingency_matrix <- function(gene1, gene2) {
  # note that we are keeping n and co.occurances matrix as global variables
  n4 = co.occurences[gene1, gene2]
  n3 = co.occurences[gene2, gene2] - n4
  n2 = co.occurences[gene1, gene1] - n4
  n1 = n - n2 - n3 - n4
  m <- matrix(c(n1,n2,n3,n4),2)
  colnames(m) <- paste0(gene2, c("_WT", "_MUT"))
  rownames(m) <- paste0(gene1, c("_WT", "_MUT"))
  m
}

contingency_matrix("EGFR", "KRAS")

# another helper to wrap the fisher's test 
is_powered_enough<- function(gene1, gene2) {
  m <- contingency_matrix(gene1, gene2)
  test <- fisher.test(m)
  data.frame(odds.ratio = test$estimate, p.val = test$p.value)
}

# let's summarize our tests
test.results <- co.occurences %>%
  reshape2::melt(varnames = c("gene1", "gene2")) %>%  # melt is a common function, note the column types
  dplyr::mutate(gene1 = as.character(gene1), 
                gene2 = as.character(gene2)) %>% 
  dplyr::select(-value) %>%
  dplyr::filter(gene1 < gene2) %>% 
  dplyr::rowwise() %>% # note this is the first time we are seeing this, please check the documentation
  dplyr::mutate(is_powered_enough(gene1, gene2)) %>% # this is a handy trick
  dplyr::ungroup() %>% 
  dplyr::mutate(q.val = p.adjust(p.val, method = "BH"))

test.results %>% head


p <- test.results %>% 
  dplyr::mutate(highlight = (q.val < 0.2) | (odds.ratio > 300), 
                control = (gene1 =="EGFR" ) & (gene2 == "KRAS"),
                label = paste0(gene1, "/", gene2)) %>% 
  ggplot(aes(x = log2(odds.ratio), 
             y = -log10(q.val),
             alpha = highlight|control,
             color = highlight,
             pair = label)) +
    geom_point(show.legend = F) +
    geom_hline(yintercept = -log10(0.2), lty = 2, lwd = 1) + 
    geom_text_repel(aes(label = ifelse(highlight | control,  label, NA)), 
                    size = 3,
                    show.legend = F) +
  theme_bw() + scale_color_wsj()

p

plotly::ggplotly(p)


contingency_matrix("POLE", "XPO1")
contingency_matrix("BRAF", "KRAS")
contingency_matrix("APC", "KRAS")
contingency_matrix("KRAS", "NRAS")

short.list <- c("KRAS", "NRAS", "BRAF", "EGFR", "TP53", "PIK3R1")
co.occurences[short.list, short.list]
is_powered_enough("TP53", "KRAS")

# This is admittedly primitive! 
# Think about how you can improve it? 


# Vignette 2: How many MDM2 inhibitors exist in Repurposing Primary and do they work? ----

# simplify the column names
colnames(OmicsExpressionProteinCodingGenesTPMLogp1)  %<>% word()
colnames(OmicsSomaticMutationsMatrixHotspot)  %<>% word()

# list of MDM2 inhibitors in Rep. Primary dataset
MDM2.inhibitors <- prism.bootcamp %>% 
  dplyr::filter(str_detect(Target, "MDM2"),
                screen == "REP.PRIMARY") 


MDM2.inhibitors

# tidying the PRISM data for the compounds, MDM2 expression and TP53 mutation status
MDM2i.data <- prism.matrix.bootcamp[, MDM2.inhibitors$column_name] %>%
  reshape2::melt(varnames = c("ModelID", "column_name"), value.name = "LFC") %>% 
  dplyr::filter(is.finite(LFC)) %>% 
  dplyr::left_join(MDM2.inhibitors) %>%
  dplyr::inner_join(tibble(ModelID = rownames(OmicsSomaticMutationsMatrixHotspot),
                          TP53.HS.Mutation = OmicsSomaticMutationsMatrixHotspot[,"TP53"])) %>%
  dplyr::inner_join(tibble(ModelID = rownames(OmicsExpressionProteinCodingGenesTPMLogp1),
                           MDM2.Expression = OmicsExpressionProteinCodingGenesTPMLogp1[,"MDM2"]))


# a faceted scatter plot
MDM2i.data %>%
  dplyr::mutate(TP53 = ifelse(TP53.HS.Mutation, "Mutant", "WT"),
                FC = pmin(1,2^LFC)) %>% 
   ggplot(aes(x = MDM2.Expression, y = FC,
             color = TP53, shape = TP53)) + 
  geom_point(size = 1, alpha = .5) +
  stat_cor(show.legend = F, size = 3 ,
           label.x = 5) + 
  geom_smooth(method = "lm", show.legend = FALSE, se = FALSE,
              alpha = .2, lwd = .5) + 
  facet_wrap(Name ~ .) + 
  labs(color = "TP53 Status", shape = "TP53 Status",
       x = "MDM2 Expression  - log2(1+TPM)", y = "Fold Change Viability") +
  theme_base(base_size = 12) + scale_color_wsj() +
  theme(legend.position = "bottom") + 
  coord_cartesian(ylim =c(0,1)) + 
  scale_shape_manual(values = c(1,4))


# Think about what would be a better way of visualizing this data?


# Vignette 3: A quick UMAP of PRISM data----


primary.profiles <- prism.bootcamp %>% 
  dplyr::filter(screen == "REP.PRIMARY") %>% 
  .$column_name

PRISM.primary <- prism.matrix.bootcamp[,primary.profiles]

# let's compute the correlation among compounds
C = WGCNA::cor(PRISM.primary, use = "p")

# create a umap with default parameters
umap.data <- uwot::umap(as.dist(1 - C), min_dist=0.0)

umap.data %>% 
  as.tibble() %>% 
  dplyr::mutate(column_name = rownames(umap.data)) %>% 
  dplyr::left_join(prism.bootcamp) %>% 
  dplyr::mutate(highlight = str_detect(Target, "MDM2")) %>% 
  ggplot(aes(x = V1, y = V2, 
             alpha = highlight, 
             color = highlight)) +
  geom_point(size = 1 , shape = 1, show.legend = F) +
  geom_text_repel(aes(label  = ifelse(highlight, Name, NA)),
                  size = 2, max.overlaps = 20, show.legend = F) +
  theme_few() + scale_color_wsj() +
  scale_alpha_manual(values = c(.3,1)) + 
  labs(x = "UMAP1", y = "UMAP2", 
       title = "MDM2 Inhibitors in PRISM Repurposing (Primary)")

?uwot::umap

p <- umap.data %>% 
  as.tibble() %>%
  dplyr::mutate(column_name = rownames(umap.data)) %>% 
  dplyr::left_join(prism.bootcamp) %>% 
  ggplot(aes(x = V1, y = V2, 
             name = Name,
             moa = MOA,
             target = Target)) +
  geom_point(size = .5 , alpha = .5, shape = 1, show.legend = F) +
  theme_few() + scale_color_fivethirtyeight()


plotly::ggplotly(p)

# Exercise: can you overlay the toxicity of each compoundas colors?
# What about the most common 8 MOA's? 
# Should we filter our pan-toxic or inert compounds before we start? If so 
# how would you do that?



# Vignette 4: Let's create a volcano plot based on correlations ----

MDM2.inhibitors %>% head

X = OmicsExpressionProteinCodingGenesTPMLogp1
y = prism.matrix.bootcamp[,"BRD-K62627508-001-01-5::2.5::HTS"]
y = y[is.finite(y)]

cl <- intersect(names(y), rownames(X))
result <- WGCNA::corAndPvalue(X[cl,], y[cl], use = "pairwise.complete")
result <- tibble(cor = result$cor[,1], p = result$p[1,], q = p.adjust(p, method = "BH"),
       gene = rownames(result$cor)) 


# I am leaving creating the actual volcano plot as an exercise.

# Let's move on with a waterfall plot:
result %>%
  dplyr::filter(!is.na(cor)) %>% 
  dplyr::arrange(desc(cor)) %>%
  dplyr::mutate(rank = 1:n(), n = n()) %>%
  dplyr::mutate(highlight = (rank <= 5) | (rank > (n-10)) )%>% 
  ggplot(aes(x = rank, y= cor, alpha = highlight)) +
  geom_point(aes(size = highlight), shape = 1, show.legend = F) +
  geom_text_repel(aes(label = ifelse(highlight, gene, NA)),
                  size = 2, show.legend = F, max.overlaps = 20) +
  scale_size_manual(values = c(.5,3)) + 
  theme_base(base_size = 12, base_family = "GillSans") +
  labs(x = "Rank", y = "Pearson Correlation", 
       title = "Expression Correlates of Idasanutlin")



# Vignette 5: List MNNs between genetic and chemical dependencies ----

colnames(CRISPRGeneEffect) %<>% word()

PRISM.primary %>% corner

CRISPRGeneEffect %>% corner

# Let's compute the correlations - this step is computationally heavy
cl = intersect(rownames(PRISM.primary), rownames(CRISPRGeneEffect))
C <- WGCNA::cor(PRISM.primary[cl,], CRISPRGeneEffect[cl, ], use = "p")
corner(C)

# Let's compute the ranks for both genes and compounds
rank.compounds <- apply(-C,1, rank)
rank.genes <- apply(-C,2, rank)

# keep only the top 3 most correlated neighbors
rank.compounds.shortlist <- rank.compounds %>% 
  reshape2::melt(varnames = c("gene", "column_name"), value.name = "rank.compound") %>%
  dplyr::filter(rank.compound <= 3)  # we are interested only in top 3 ranks

rank.genes.shortlist <- rank.genes %>% 
  reshape2::melt(varnames = c("column_name", "gene"), value.name = "rank.gene") %>%
  dplyr::filter(rank.gene <= 3) 

mnns <- rank.genes.shortlist %>%
  dplyr::inner_join(rank.compounds.shortlist) %>%  
  dplyr::left_join(prism.bootcamp) %>% 
  dplyr::select(Name, gene, Target, MOA, 
                rank.gene, rank.compound, brd) %>%
  dplyr::rename(compound = Name)
  
mnns %>% 
  head

# how is our MDM inhibitors doing? 
mnns %>%
  dplyr::filter(str_detect(Target, "MDM2")) 

mnns %>%
  dplyr::filter(gene == "MDM2")

# What else can we do with MNNs?




# Taking a high level look at ONCREF Dataset (WiP) ----- 

OncRef.AUC.matrix <- load.from.taiga(data.name='prism-oncology-reference-set-23q4-1a7c', data.version=13, data.file='AUC_matrix') %>%
  t()
OncRef.LFC.Matrix <- load.from.taiga(data.name='prism-oncology-reference-set-23q4-1a7c', data.version=13, data.file='PRISM_Oncology_Reference_23Q4_LFC_Matrix')

colnames(OncRef.AUC.matrix) <- tibble(colnames = colnames(OncRef.AUC.matrix)) %>% 
  dplyr::left_join(compound_metadata %>%
                     dplyr::distinct(Drug.Name, IDs),
                   by = c("colnames" = "IDs")) %>%
  .$Drug.Name

# Let's quickly put together a umap: 


OncRef.AUC.Cor <- cor(OncRef.AUC.matrix, use = "p")

OncRef.umap <- uwot::umap(as.dist(1-OncRef.AUC.Cor),
           min_dist = 0, n_neighbors = 2) 


C = 30


umap_clusters = OncRef.umap %>%
  dist() %>% 
  hclust() %>%
  cutree(k = C)

correlation_clusters <- as.dist(1-OncRef.AUC.Cor) %>% 
  hclust() %>%
  cutree(k = C)






OncRef.umap <- OncRef.umap %>%
  as_tibble() %>% 
  dplyr::mutate(Drug.Name = colnames(OncRef.AUC.matrix)) %>%
  dplyr::left_join(compound_metadata %>% 
                     dplyr::select(-IDs) %>% 
                     dplyr::distinct()) %>%
  dplyr::left_join(tibble(Drug.Name = names(umap_clusters),
                          umap_cluster_id = umap_clusters)) %>%
  dplyr::left_join(tibble(Drug.Name = names(correlation_clusters),
                          umap_cluster_id = correlation_clusters))

# A really quick heatmap - Add the cluster annotations

OncRef.AUC.Cor %>%
  pheatmap::pheatmap(show_rownames = FALSE,
                     treeheight_row = 0,
                     fontsize_col = 6,
                     color = colorRampPalette(c("navy", "white", "firebrick3"))(50))


heatmaply::heatmaply(OncRef.AUC.Cor,
                     show_dendrogram = c(TRUE, FALSE), 
                     xlab = "", ylab = "", 
                     symm = TRUE,
                     #scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                     #  high = "firebrick", mid="white",low = "navy", midpoint = 0, 
                     #  limits = c(-1, 1)),
                     showticklabels = c(FALSE,FALSE),
                     heatmap_layers = theme(axis.line=element_blank())
                     )

# We know how to make a umap, let's add cluster annotations



# Setting up a ready to use correlation volcano plot function -----

robust_linear_model <- function(X, y, W = NULL) {
  # W shouldn't have any NAs!!!
  Y = matrix(y, dim(X)[1], dim(X)[2])
  
  # Centering X 
  if(!is.null(W)){
    W = cbind(1, W) 
    H = diag(nrow(W)) - crossprod(t(W), tcrossprod(pracma::pinv(crossprod(W)), W))
    
    X.mask = is.na(X); y.mask = is.na(y); 
    mask <- X.mask | y.mask
    X[mask] = 0; Y[mask] = 0
    X <- crossprod(H, X); X[mask] = NA
    Y <- crossprod(H, Y); Y[mask] = NA
    d = dim(W)[2] + 1
  } else{
    d = 2
  }
  
  flag = apply(X, 2, function(x) var(x,na.rm = T)) 
  Y <- Y[, flag > 0.001, drop = FALSE]
  X <- X[, flag > 0.001, drop = FALSE] # this threshold is very arbitrary! 
  
  # this step is necessary due to the NA's in X !! 
  X <- scale(X, center = TRUE, scale = FALSE)
  Y <- scale(Y, center = TRUE, scale = FALSE)
  
  # Defining S and computing mean and variances 
  S = X * Y
  n = colSums(!is.na(S))
  muS = colMeans(S, na.rm = T)
  #sigmaS = apply(S, 2, function(x) sd(x, na.rm = T))
  sigmaS = sqrt(rowMeans((t(S) - muS)^2, na.rm = T))
  X[is.na(S)] = NA; Y[is.na(S)] = NA
  varX = colMeans(X^2, na.rm = T)
  varY = colMeans(Y^2, na.rm = T)
  
  # Estimation and inference
  beta = muS / varX
  rho = muS / sqrt(varX * varY)
  p.val <- 2*pt(abs(sqrt(n-d) * muS/sigmaS), 
                df = n-d, lower.tail = FALSE)
  
  
  # Homoskedastic p-values: Only for comparison! 
  p.val.homoskedastic <- 2*pt(sqrt( (n-d) /(1/rho^2-1)),
                              df = n-d, lower.tail = FALSE)
  # Experimental robustness score
  ns = apply(S,2, function(s) sum(cumsum(sort(s/sum(s, na.rm = T), decreasing = T)) < 1, na.rm = T)) + 1
  
  return(list(x = names(beta), rho = rho, beta = beta,
              p.val.rob = p.val, 
              p.val = p.val.homoskedastic, 
              n = n,
              ns = ns))
}



