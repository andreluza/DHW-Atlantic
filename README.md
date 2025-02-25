README
================
ALLuza
2025-02-25

Repository containing the data and scripts used in the article “The
thermal stress history of South Atlantic reefs reveals increasing
intensity, duration, frequency, and likely undocumented bleaching
episodes”, which you recently submitted to Global Change Biology.

<!-- badges: start -->
<!-- badges: end -->

# The project is organized as follows:

Root

\|— data  
\|———– DHW_raw_stats_table.csv: raw data set  
  
\|— output  
\|———– Figures and tables produced by the “Interpretation\*.R” scripts
shown below  
  
\|— RData: Model output in RData format:  
\|———– model_count_lat: latitude effect on DHW interval  
\|———– model_count2_lat: latitude effect on DHW duration  
\|———– model_count3_lat: latitude effect on intensity  
\|———– model_count: time and region effects on DHW interval  
\|———– model_count2: time and region effects on DHW duration  
\|———– model_count3: time and region effects on intensity  
  
\|— RScript  
\|———– packages.R: required packages  
\|———– Code_models.R: building and fitting models to data  
\|———– Interpretation_lat.R: Interpretation of models with latitude  
\|———– Interpretation_time_Reg.R: Interpretation of models with time and
region  

#### This paper was produced using the following software and associated packages:

    ## R version 4.4.1 (2024-06-14 ucrt)
    ## Platform: x86_64-w64-mingw32/x64
    ## Running under: Windows 10 x64 (build 19045)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=Portuguese_Brazil.utf8  LC_CTYPE=Portuguese_Brazil.utf8   
    ## [3] LC_MONETARY=Portuguese_Brazil.utf8 LC_NUMERIC=C                      
    ## [5] LC_TIME=Portuguese_Brazil.utf8    
    ## 
    ## time zone: Europe/Paris
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] ggbreak_0.1.2      brms_2.22.0        Rcpp_1.0.13        performance_0.12.4
    ## [5] reshape_0.8.9      dplyr_1.1.4        ggplot2_3.5.1      here_1.0.1        
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] gtable_0.3.5         tensorA_0.36.2.1     xfun_0.44           
    ##  [4] insight_0.20.5       lattice_0.22-6       vctrs_0.6.5         
    ##  [7] tools_4.4.1          generics_0.1.3       yulab.utils_0.1.8   
    ## [10] parallel_4.4.1       tibble_3.2.1         fansi_1.0.6         
    ## [13] pkgconfig_2.0.3      Matrix_1.7-0         checkmate_2.3.2     
    ## [16] ggplotify_0.1.2      distributional_0.5.0 RcppParallel_5.1.9  
    ## [19] lifecycle_1.0.4      compiler_4.4.1       farver_2.1.2        
    ## [22] stringr_1.5.1        Brobdingnag_1.2-9    munsell_0.5.1       
    ## [25] ggfun_0.1.6          htmltools_0.5.8.1    bayesplot_1.11.1    
    ## [28] yaml_2.3.8           pillar_1.9.0         bridgesampling_1.1-2
    ## [31] abind_1.4-8          nlme_3.1-164         posterior_1.6.0     
    ## [34] tidyselect_1.2.1     aplot_0.2.3          digest_0.6.35       
    ## [37] mvtnorm_1.3-1        stringi_1.8.4        rprojroot_2.0.4     
    ## [40] fastmap_1.2.0        grid_4.4.1           colorspace_2.1-1    
    ## [43] cli_3.6.2            magrittr_2.0.3       patchwork_1.3.0     
    ## [46] loo_2.8.0            utf8_1.2.4           withr_3.0.0         
    ## [49] scales_1.3.0         backports_1.5.0      estimability_1.5.1  
    ## [52] rmarkdown_2.28       matrixStats_1.4.1    emmeans_1.10.5      
    ## [55] coda_0.19-4.1        evaluate_1.0.1       knitr_1.48          
    ## [58] gridGraphics_0.5-1   rstantools_2.4.0     rlang_1.1.3         
    ## [61] xtable_1.8-4         glue_1.7.0           rstudioapi_0.16.0   
    ## [64] R6_2.5.1             plyr_1.8.9           fs_1.6.4
