# Financial Impact of Gene Testing<br/>
Analyze the financial impact of various gene testing methods (Whole Exome Sequencing vs Panel Gene Testing) using data visualization and statistical testings.

## Experiment Setup: <br/>
1. contrast groups: Panel Testing vs EXOME SEQUENCE ANALYSIS
2. target variables: charge (A), payment (B), denial rate (C), remit code (D), claims duration (E)
3. covariates: financial class, DX, CCS categories
4. unit of analysis: patient level (A-C) and account level (D, E)

## Statistic Methods:
1. boxplots to show variability between and within case groups
2. test pairwise difference between two groups (Pairwise Wilcoxon Rank Sum Test)
3. test overall difference among multiple groups (Kruskal-Wallis Rank Sum Test)

## Example Results<br/>
### Case Distribution<br/>
![](img/Picture1.png)
### Percentage of Payment by Financial Class<br/>
![](img/Picture6.png)
Percentage of payment is overall significantly different among different financial classes based on Kruskal-Wallis Test.
