# Variable Selection and Regression Analysis of Smoking Cessation Outcomes Using Regularization


## Background: 
Individuals with past or present major depressive disorder (MDD) experience unique barriers to smoking cessation. While varenicline has been shown to be more effective than traditional nicotine replacement therapy in achieving abstinence for individuals with and without MDD, its lower effectiveness for those with MDD suggests that MDD-responsive treatment strategies are necessary. A recent determined that varenicline improved abstinence, but BASC did not, either with or without varenicline. In light of this surprising result, we use data from this study to examine baseline variables as potential predictors of end-of-treatment abstinence and moderators of behavioral treatment's effect thereon.## Methods: 
We examine data from a randomized, placebo-controlled 2x2 factorial design study of 300 smokers with past or present MDD, who received either placebo or varenicline and standard treatment (ST) or behavioral activation for smoking cessation (BA). Missing data was imputed through multiple imputation. A cross-validated Lasso was applied at the optimally chosen value of $\lambda$ for coefficient estimates. Subsequently a bootstrap procedure is carried out to estimate confidence intervals and assess significance of the coefficients.
## Results: 
The participant's Fagerstrom Test for Cigarette Dependence score and the participant's Nicotine Metabolite Ratio were baseline predictors of abstinence controlling for treatment type. The models demonstrated fair to good discriminative ability. Our moderator analysis demonstrated that no variables could be identified as potential moderators of the therapy based treatment (BA or ST).
## Conclusion: 
Lasso regression with bootstrap confidence interval estimates identified meaningful predictors of abstinence noted above. We were unable to identify treatment effect moderators. Limitations include the relatively small sample size, which led to convergence issues and challenges handling class imbalance in the outcome which suggests these findings should be validated in larger studies. Future research might benefit from more sophisticated regularization approaches and larger, more diverse samples to better understand treatment effect heterogeneity in this population.


[The full report can be found here](https://github.com/tomrannosaurus/smoking_cessation_proj/blob/main/smoking_cessation_proj.pdf)


## Setup

- R Version: 4.3.1
- Package Versions:
   - tidyverse: 2.0.0
   - knitr: 1.45
   - kableExtra: 1.3.4
   - ggplot2: 3.4.3
   - naniar 1.0.0
   - visdat 0.6.0
   - car 3.1-2
   - lme4 1.1-34
   - ggpubr 0.6.0
   - outliers: 0.15
   - reshape2: 1.4.4
   - moments: 0.14.1
   - vcd: 1.4-12
   - glmnet 4.1-8   - mice 3.16.0   - caret 6.0-94   - pROC 1.18.4

## Files

- Report Quarto: smoking_cessation_proj.qmd
- Helper functions code: _helpers.R
- Tex files: column-commands.tex, float-setup.tex, geometry-settings.tex, table-packages.tex, title-settings.tex
- References file: references.bib
