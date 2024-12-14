# Variable Selection and Regression Analysis of Smoking Cessation Outcomes Using Regularization


## Background: 
Individuals with past or present major depressive disorder (MDD) experience unique barriers to smoking cessation. While varenicline has been shown to be more effective than traditional nicotine replacement therapy in achieving abstinence for individuals with and without MDD, its lower effectiveness for those with MDD suggests that MDD-responsive treatment strategies are necessary. A recent determined that varenicline improved abstinence, but BASC did not, either with or without varenicline. In light of this surprising result, we use data from this study to examine baseline variables as potential predictors of end-of-treatment abstinence and moderators of behavioral treatment's effect thereon.


## Methods: 
We examine data from a randomized, placebo-controlled 2x2 factorial design study of 300 smokers with past or present MDD, who received either placebo or varenicline and standard treatment (ST) or behavioral activation for smoking cessation (BA). Missing data was imputed through multiple imputation. A cross-validated Lasso was applied at the optimally chosen value of $\lambda$ for coefficient estimates. Subsequently a bootstrap procedure is carried out to estimate confidence intervals and assess significance of the coefficients.


## Results: 
The participant's Fagerstrom Test for Cigarette Dependence score and the participant's Nicotine Metabolite Ratio were baseline predictors of abstinence controlling for treatment type. The models demonstrated fair to good discriminative ability. Our moderator analysis demonstrated that no variables could be identified as potential moderators of the therapy based treatment (BA or ST).

**Baseline Model**

| Predictor                                | Estimate (Primary) | Estimate (Mean) | Estimate (Median) | Odds Ratio | Lower 2.5% | Upper 97.5% | Proportion Non-Zero | Sig. |
|------------------------------------------|---------------------|-----------------|-------------------|------------|------------|--------------|---------------------|------|
| VarVarenicline                           | 1.789              | 2.389           | 2.264             | 5.983      | 1.015      | 4.535        | 1.000               | Yes  |
| poly(ftcd_score, 3, raw = TRUE)1         | -2.576             | -3.424          | -3.235            | 0.076      | -7.220     | -0.655       | 0.999               | Yes  |
| poly(ftcd_score, 3, raw = TRUE)2         | 0.603              | 0.822           | 0.772             | 1.828      | 0.132      | 1.795        | 0.998               | Yes  |
| poly(ftcd_score, 3, raw = TRUE)3         | -0.043             | -0.060          | -0.056            | 0.957      | -0.134     | -0.010       | 0.999               | Yes  |
| poly(hedonsum_y_pq1, 3, raw = TRUE)1     | -0.157             | -0.205          | -0.201            | 0.855      | -0.431     | -0.008       | 0.999               | Yes  |
| logNMR                                   | 2.183              | 1.367           | 1.314             | 8.877      | 0.449      | 2.598        | 1.000               | Yes  |

**Moderator Model**
| Predictor       | Estimate (Primary) | Estimate (Mean) | Estimate (Median) | Odds Ratio | Lower 2.5% | Upper 97.5% | Proportion Non-Zero | Sig. |
|-----------------|---------------------|-----------------|-------------------|------------|------------|--------------|---------------------|------|
| VarVarenicline | 2.115              | 8.989           | 6.939            | 8.292      | 2.442      | 27.204       | 1                   | Yes  |


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
   - glmnet 4.1-8
   - mice 3.16.0
   - caret 6.0-94
   - pROC 1.18.4
	- gt 0.9.0
	- gtsummary 1.7.2

## Files

- Report Quarto: smoking_cessation_proj.qmd
- Helper functions code: _helpers.R
- Tex files: column-commands.tex, float-setup.tex, geometry-settings.tex, table-packages.tex, title-settings.tex
- References file: references.bib
