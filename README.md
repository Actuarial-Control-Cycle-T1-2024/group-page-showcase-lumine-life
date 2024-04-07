[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/biNKOeX_)
# Actuarial Theory and Practice A @ UNSW

_"Tell me and I forget. Teach me and I remember. Involve me and I learn" - Benjamin Franklin_

---

### Congrats on completing the [2024 SOA Research Challenge](https://www.soa.org/research/opportunities/2024-student-research-case-study-challenge/)!

>Now it's time to build your own website to showcase your work.  
>To create a website on GitHub Pages to showcase your work is very easy.

This is written in markdown language. 
>
* Click [link](https://classroom.github.com/a/biNKOeX_) to accept your group assignment.

#### Follow the [guide doc](doc1.pdf) to submit your work. 

When you finish the task, please paste your link to the Excel [sheet](https://unsw-my.sharepoint.com/:x:/g/personal/z5096423_ad_unsw_edu_au/ETIxmQ6pESRHoHPt-PUleR4BuN0_ghByf7TsfSfgDaBhVg?rtime=GAd2OFNM3Eg) for Peer Feedback
---
>Be creative! Feel free to link to embed your [data](2024-srcsc-superlife-inforce-dataset-part1.csv), [code](sample-data-clean.ipynb), [image](unsw.png) here

More information on GitHub Pages can be found [here](https://pages.github.com/)
![](Actuarial.gif)



# MAIN README

<div>
<button onclick="location.href='https://www.soa.org/research/opportunities/2024-student-research-case-study-challenge/'">
  <img src="https://api.iconify.design/twemoji/briefcase.svg?height=24" aria-hidden="true">
  2024 SOA Case Study
</button>

<button onclick="location.href='https://github.com/Actuarial-Control-Cycle-T1-2024/group-page-showcase-lumine-life/tree/main/code'">
  <img src="https://api.iconify.design/icon-park/file-code.svg?height=24" aria-hidden="true">
  Code
</button>
</div>

# REPORT SUMMARY

### Assumptions
The health incentive program’s costs are composed of both fixed and variable parts, with the variable part being affected by assumptions. One key assumption is full participation from policyholders, which likely overestimates costs since participation is optional. Additionally, assumptions about smoking cessation’s impact on mortality was based on Gilpin’s (1997) research, suggesting that the effects start after 4 years of commencing the program, which potentially underestimates costs if participation exceeds 4 years. Furthermore, due to data limitations, the effect of cancer screening on mortality is assumed constant over age, simplifying profit modelling but possibly affecting accuracy. This assumption is significant as they influence the program’s cost estimation and overall profit projections.  

### Risk and Risk Mitigation Considerations
There are a multitude of risks that can potentially impact the proposed health incentive program. Some of the main ones are listed below with their risk mitigation strategy.

<table><tbody><tr><th><p><strong>Risk</strong></p></th><th><p><strong>Mitigation Strategy</strong></p></th></tr><tr><td><p><strong>Inflation Risk (Quantitative):</strong></p><p>Long-term policies are susceptible to rising costs.</p></td><td><ul><li>Build inflation protection into pricing models</li><li>Set aside reserves for future costs</li></ul></td></tr><tr><td><p><strong>Adverse Selection Risk (Quantitative):</strong></p><p>When more individuals with higher health risks participate, leading to an imbalanced risk pool.</p></td><td><ul><li>Transfer excess risk to reinsurers</li><li>Account for this risk in the model</li></ul></td></tr><tr><td><p><strong>Underwriting Risk (Quantitative):</strong></p><p>Inappropriate assessment of policyholder’s key risk features.</p></td><td><ul><li>Conduct comprehensive evaluation of policyholder attributes at entry</li></ul></td></tr><tr><td><p><strong>Participation Risk (Qualitative):</strong></p><p>Policyholders may not actively participate, leading to a suboptimal outcome</p></td><td><ul><li>Regular communication about benefits</li><li>Reach out to inactive participants</li><li>Include additional incentives</li></ul></td></tr><tr><td><p><strong>Communication / Education Risk (Qualitative):</strong></p><p>Policyholders may not understand or have a lack of awareness of the benefits that the program provides.</p></td><td><ul><li>Consistent and proper marketing</li><li>Content is well presented</li></ul></td></tr><tr><td><p><strong>Motivation Risk (Qualitative):</strong></p><p>Over time, participants may lose interest in the program</p></td><td><ul><li>Give annual rewards like subsidy on screening if program has been completed</li></ul></td></tr></tbody></table>

To test the limits of these risks, a sensitivity analysis was conducted to portray the effects of key financial impacts on the profit as depicted in Figure 3. The key financial impacts assessed were interest rate, cost of incentive programs and policy lapse rate. Under all cases, there is a steep increase in profits due to the lagged contribution of the incentive programs via mortality savings. This suggests that the company should maintain enough liquid capital in the early states to fund the incentive programs until they see a return on investment.

![]()

_Figure 3: Sensitivity Analysis for Profit_

The analysis also considered the certainty of projected values. The predicted decrease in mortality over the past 20 years has an average degree of certainty due to reliance on outdated data and research. Nonetheless, the health incentive program is expected to reduce mortality, supported by scientific evidence. The value of policies under the program is likely to be higher than those without, thanks to the program’s design that benefits both the company and policyholders. Even for non-participants, the program adds value by enhancing market competitiveness.company should maintain enough liquid capital in the early states to fund the incentive programs until they see a return on investment.

### Data and Data Limitations
Below is a summary of the data limitations and their impact on the analysis:

**Jama Network:**

- **Limitation:** Broad racial/ethnic categories, single-point smoking data, no disease status separation
- **Impact:** Affects the accuracy of smoking cessation’s impact on mortality rates and profit calculations.

**Science Direct:**

- **Limitation:** Homogeneity assumptions, single intervention model, short-term abstinence measure, findings from one trial.
- **Impact:** Influences the cost estimation of smoking programs and profit outcomes.

**International Agency for Research on Cancer - WHO:**

- **Limitation:** Variable quality and completeness of cancer registry data.
- **Impact:** Affects the relevance of cancer death rates data.

**National Library of Medicine:**

- **Limitation:** Small sample size, fixed attendance rate, exclusion of certain cancers, omission of treatment-related deaths.
- **Impact:** Impacts the accuracy of mortality rate of changes for cancer programs and profit.

**Oxford Academic - Journal of the National Cancer Institute:**

- **Limitation:** Cohort representativeness, self-reported smoking status, abstinence definition.
- **Impact:** Questions the relevance of data for determining the quitting time period.

**Society of Actuaries:**

- **Limitation:** AI-generated data with static variables, non representative sample, limited to one term insurance.
- **Impact:** Affects the overall analysis due to numerous assumptions based on limited data.
