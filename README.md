README
================
ALLuza
2025-02-25

Repository containing the data and scripts used in the article “The
thermal stress history of South Atlantic reefs reveals increasing
intensity, duration, frequency, and likely undocumented bleaching
episodes”, by Destri et al. (Mies Lab), recently submitted to Global
Change Biology.

<!-- badges: start -->
<!-- badges: end -->

# The project is organized as follows:

Root

\|— data  
\|———– DHW_raw_stats_table.csv: raw DHW data set  
  
\|— output  
\|———– Figures and tables produced by the “Interpretation\*.R” scripts
shown below  
  
\|— RData: Model output in RData format (folder created with
‘Code_models.R’):  
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
    ## [1] brms_2.22.0        Rcpp_1.0.13        performance_0.12.4 reshape_0.8.9     
    ## [5] dplyr_1.1.4        ggplot2_3.5.1      here_1.0.1        
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tensorA_0.36.2.1     bridgesampling_1.1-2 utf8_1.2.4          
    ##  [4] generics_0.1.3       stringi_1.8.4        lattice_0.22-6      
    ##  [7] digest_0.6.35        magrittr_2.0.3       estimability_1.5.1  
    ## [10] evaluate_1.0.1       grid_4.4.1           mvtnorm_1.3-1       
    ## [13] fastmap_1.2.0        rprojroot_2.0.4      plyr_1.8.9          
    ## [16] Matrix_1.7-0         backports_1.5.0      Brobdingnag_1.2-9   
    ## [19] fansi_1.0.6          scales_1.3.0         abind_1.4-8         
    ## [22] cli_3.6.2            rlang_1.1.3          munsell_0.5.1       
    ## [25] withr_3.0.0          yaml_2.3.8           tools_4.4.1         
    ## [28] parallel_4.4.1       rstantools_2.4.0     checkmate_2.3.2     
    ## [31] coda_0.19-4.1        colorspace_2.1-1     vctrs_0.6.5         
    ## [34] posterior_1.6.0      R6_2.5.1             emmeans_1.10.5      
    ## [37] matrixStats_1.4.1    lifecycle_1.0.4      stringr_1.5.1       
    ## [40] insight_0.20.5       pkgconfig_2.0.3      RcppParallel_5.1.9  
    ## [43] pillar_1.9.0         gtable_0.3.5         loo_2.8.0           
    ## [46] glue_1.7.0           xfun_0.44            tibble_3.2.1        
    ## [49] tidyselect_1.2.1     rstudioapi_0.16.0    knitr_1.48          
    ## [52] xtable_1.8-4         bayesplot_1.11.1     htmltools_0.5.8.1   
    ## [55] nlme_3.1-164         rmarkdown_2.28       compiler_4.4.1      
    ## [58] distributional_0.5.0
