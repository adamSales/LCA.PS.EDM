# LCA.PS.EDM
Replication code for:
"Problem-Solving Types and EdTech Effectiveness: A Model for Exploratory Causal Analysis"
By Adam C. Sales, Kirk P. Vanacore, Hyeon-Ah Kang, and Tiffany A. Whittaker
Forthcoming in the EDM2024 Conference Proceedings

The replication analyses require R version 4.2.2 (2022-10-31) and Stan version 2.21.0
The replicate code also depends on a number of packages in R--session info is supplied at the bottom.

To replicate the ASSISTments analysis, first obtain the data by following instructions at <https://osf.io/r3nf2/>.

[Kirk Add some stuff]

Then, in `R`, in the main working directory, run the following:
```
> source('code-AS/makeData.r') ## processes and formats the data for analysis
> source('code-AS/runPSas.r') ## this runs the analysis and saves the results IT TAKES A VERY VERY LONG TIME
> source('code-AS/stanTablesAS.r') ## this creates all of the tables and plots for the paper
```

Unfortunately, the CTA dataset is not publicly available.
However, we have included the analysis code in the `code-CTA` folder. Its structure is basically the same as ASSISTments.

In particular, the model code can be found in the two following files:

- ASSISTments: `code-AS/lca2classPSnoSch.stan`
- CTA: `code-CTA/lca2classPS.stan`






```
R version 4.2.2 (2022-10-31)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Debian GNU/Linux 11 (bullseye)

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] xtable_1.8-4          dplyr_1.1.3           rstan_2.21.8         
[4] ggplot2_3.4.1         StanHeaders_2.21.0-7  languageserver_0.3.15
[7] httpgd_1.3.1         

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.10        pillar_1.9.0       compiler_4.2.2     later_1.3.0       
 [5] tools_4.2.2        prettyunits_1.1.1  pkgbuild_1.3.1     jsonlite_1.8.0    
 [9] lifecycle_1.0.3    tibble_3.2.1       gtable_0.3.0       pkgconfig_2.0.3   
[13] rlang_1.1.1        DBI_1.1.3          cli_3.6.1          parallel_4.2.2    
[17] loo_2.5.1          gridExtra_2.3      withr_2.5.1        xml2_1.3.3        
[21] generics_0.1.3     vctrs_0.6.4        systemfonts_1.0.4  stats4_4.2.2      
[25] grid_4.2.2         tidyselect_1.2.0   glue_1.6.2         inline_0.3.19     
[29] R6_2.5.1           textshaping_0.3.6  processx_3.7.0     fansi_1.0.5       
[33] farver_2.1.1       callr_3.7.3        magrittr_2.0.3     splines_4.2.2     
[37] codetools_0.2-18   matrixStats_0.62.0 scales_1.2.1       ps_1.7.1          
[41] colorspace_2.0-3   ragg_1.2.5         labeling_0.4.2     utf8_1.2.3        
[45] RcppParallel_5.1.5 munsell_0.5.0      crayon_1.5.1   
```