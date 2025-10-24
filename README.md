# Code and Data for EIG's Comment Letter on Revising Proposed H-1B Allocation Rules

******

## Identifying salaries and Wage Levels of H1-B lottery winners

We link H-1B lottery winners from FY 2021 to FY 2024, obtained from Bloomberg, with Labor Condition Application (LCA) disclosure data from FY 2015 to FY 2024 using Department of Labor case IDs, accounting for the multi-year processing time of LCAs. Entries censored under sections (b)(3), (b)(6), or (b)(7)(C) of the Freedom of Information Act (FOIA) are excluded. Entries listing applicants under 18 years of age are presumed to be erroneous, as the H-1B program requires at least a bachelor’s degree, and are therefore excluded from the analysis.

To correct for missing values and reporting errors in H-1B annual wages recorded on USCIS Form I-129, we proceed as follows:

- We first assume that annual earnings reported between $10 and $500 represent hourly wages and annualize them based on the worker’s full- or part-time status.
- For wage entries below $10 or between $500 and $30,000, we substitute the values with the corresponding LCA-reported annual wages whenever available.
- If an I-129 wage falls within the top 1 percent of entries and exceeds the LCA wage by more than a factor of ten, we infer a misplaced decimal point and divide the I-129 wage by the power of ten closest to the I-129-to-LCA wage ratio. Similarly, top 1 percent I-129 wages that are more than twice their corresponding LCA wages are replaced with the LCA values.
- When I-129 wages fall between one-tenth and 95 percent of their respective LCA wages, they are also replaced with the LCA values, provided that the LCA wages are below $1 million or below the LCA’s reported upper bound, whichever is smaller.
- Two records reporting annual earnings above $8 million are discarded due to apparent irregularities.

Since the same individual may file multiple petitions for different worksites, a loophole that was particularly exploited in FY 2024, we retain only one entry from each group of records in which multiple H-1B petitions were filed under the same LCA case and share identical demographic, educational, and immigration characteristics. Although this filter is not perfect, it provides the best available means of ensuring that each H-1B petition corresponds to a unique worker.

To assign H-1B petitions to metropolitan statistical areas (MSAs), we use the crosswalk provided by the Department of Housing and Urban Development (HUD) to link each I-129 worksite ZIP code to the county with which at least 90 percent of the ZIP code’s land area overlaps. Entries that cannot be matched through this method are instead assigned to counties using coordinates geocoded from the listed addresses. Records without valid geocoding are assigned to the LCA worksite county. Finally, counties (or towns, in the case of New England states) are merged into MSAs using the geographic crosswalk provided by the Office of Foreign Labor Certification (OFLC). Three entries lacking identifiable MSAs are excluded.

Most H-1B petitions already include Standard Occupational Classification (SOC) occupation codes in their corresponding LCA records. We crosswalk all existing SOC codes to the most recent vintage (2018). For petitions missing SOC codes, we first apply a string similarity algorithm to match their Dictionary of Occupational Titles (DOT) codes and applicant-entered job titles to SOC codes. We then use a machine learning model trained on entries with valid SOCs to predict missing occupational codes based on the petitioner’s firm name, degree field, and self-reported job title. Remaining entries are retained if they contain valid LCA Wage Levels, which drops around one thousand entries.

Because we only have access to 2024–2025 OFLC Wage Level data due to the government shutdown, we adjust 2021–2023 wages to 2024 levels for Wage Level identification purposes only, using the growth rates of mean wages for the corresponding MSA–SOC combinations from the Occupational Employment Statistics (OES). Records with missing or censored OES mean wages are instead adjusted to 2024 dollars using the PCE inflation rate. OFLC Level floors for each metropolitan area and occupation are applied wherever available. Wages below the Level I threshold are treated as Level I, while those lacking OFLC information are substituted with LCA-reported Wage Levels.

To account for cost-of-living differences across the visa applicants’ worksites, we further adjust wages using state-level Regional Price Parities (RPPs) from the Bureau of Economic Analysis (BEA). Wages from 2021 to 2023 are adjusted using each year’s corresponding RPP, while 2024 wages use the 2023 RPP values, the most recent available data.

In total, we identify around 373,000 H-1B petitions from lottery winners from FY2021 to FY2024 with valid wage, metropolitan area, occupation, and Wage Level information.

******

## Projecting Net Present Value (NPV) of H1-B applicants’ expected lifetime earnings 

To estimate the lifetime earnings of each year’s H-1B cohort, we begin with mean wages by age for all private-sector wage and salary workers holding at least a bachelor’s degree, as reported in the most recent five-year American Community Survey (ACS) from the U.S. Census Bureau. The wage universe is restricted to reflect the characteristics of H-1B applicants — skilled, college-educated workers employed in the private sector.

For each age between 22 and 65, we discount the ACS cross-sectional mean wages for subsequent ages by either 3 percent or 7 percent, as specified by Executive Order 14192, and sum the discounted values to calculate the expected NPV of earnings. Finally, we divide the NPV of each age’s projected earnings stream by the corresponding mean wage to derive the expected lifetime earnings multiplier.

For workers with age n, discount rate r, and ACS cross-sectional mean wage w, the equation for NPV multiplier is as follows:

$$
NPV multiplier_n = (w_n + {w_{n+1} \over (1+r)} + {w_{n+2} \over (1+r)^2} + ... + {w_{65} \over (1+r)^{65-n}}) / w_n
$$

We assign the NPV multiplier for 22-year-olds to all age groups younger than 22, and the multiplier for 59-year-olds to all ages above 59. Because H-1B visas require applicants to hold at least a bachelor’s degree, nearly all applicants are 22 or older. The few 20- and 21-year-olds in the sample are likely freshly graduated from college and are therefore treated the same as the 22-year-old recent graduates. Conversely, H-1B awardees are expected to work for two full visa terms (six years), so individuals between 60 and 65 can reasonably be assumed to remain employed for at least six additional years. Since most U.S. workers begin retiring around age 65, ACS cross-sectional mean wages beyond that age are not representative of actual late-career earnings. Given these limitations, the best practice is to apply the same multiplier used for 60- to 65-year-olds to those continuing to work past 65.

Our preferred proposal aims to maximize the total long-run earnings of H-1B awardees. To do so, we multiply each applicant’s wage by the NPV multiplier corresponding to their age and rank the resulting products in descending order to allocate visas under the regular (65,000) and graduate (20,000) caps.

******

## Simulating different H1-B selection schemes using synthetic samples

The strict randomness of the current H1-B lottery for the regular lottery cap makes the lottery winners a representative sample of the universe of all workers who entered the lottery.

Using the H-1B lottery winners from each fiscal year, after applying the data cleaning procedures outlined above, we first subset the sample to those who answered “B” in Section 3, Question 1 of Form I-129, indicating selection under the uncapped 65,000 quota. Among these uncapped winners, we then retain only approved petitions to calculate the approval rate. Since the lottery is random, we assume that all entrants faced the same approval probability as the uncapped sample and simulate only those whose applications would have been approved.

Among the uncapped lottery winners whose petitions were approved, we calculate the share of individuals holding master’s, Ph.D., or professional graduate degrees. This share is assumed to represent the proportion of graduate degree holders among all lottery entrants, although it may be underestimated due to missing education data in some I-129 records. With this estimate, we derive the number of H-1B lottery entrants eligible for the graduate cap and those who are not.

Combining the approved graduate degree holders among the uncapped lottery winners with the capped winners (those who answered “M” in Section 3, Question 1 of Form I-129), we obtain a complete sample of graduate degree holders. We then draw randomly with replacement from this group up to the estimated number of graduate-cap entrants. Similarly, we perform random draws with replacement from the non-graduate degree holders among the regular cap’s lottery winners up to the estimated number of bachelor’s-level entrants. Combining the regular cap-eligible and ineligible synthetic H-1B lottery entrants produces a synthetic dataset of all H-1B applicants for a given year who would have been approved had they been selected for a visa.

We generate the synthetic H-1B applicant dataset 500 times and apply the following selection procedures to obtain summary statistics for potential H-1B awardees:

1. The current, random lottery system
2. The proposed Wage-Level-weighted lottery
3. The 2021 Wage-Level-based ranking and lottery
4. Ranking by RPP-adjusted compensation alone
5. Ranking by lifetime earnings NPV using 3 percent and 7 percent discount rates

All wage summaries for FY 2021–FY 2023 are adjusted to 2024 dollars.
