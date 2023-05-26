# Corhort_analysis_R
This is a take-home assignment from Invitae. Completed on May 26th, 2023.

The cohort analysis aims to understand customers' ordering behavior based on their signup date. I grouped the customers into week long (7 days) cohorts and then calculated how many distinct customers ordered within X days from their signup date, where X is a multiple of 7. 

**Software requirements:**

R version 4.2.2 (2022-10-31 ucrt)
R packages required: dplyr, ascii, tidyr, lubridate, ggplot2, gridExtra (please see installation details in the R markdown html file, or the .rmd file)

**Input files:**

customers.csv: this file documents "id" for user id, and "creared" for user sign-up date . 
orders.csv: this file documents "id" for order id, "order_number" for the purchase amount, "user_id" for user id, and "created" for purchase date (in UTC). 

**Output files:**

cohort_analysis_output.csv: this file documents "CohortGroup" for the assession number of the cohort, "cohortname" for the range of sign-up date of customers, "TotalUsers" for the total number of unique customers within that corhort. The rest of the columns document the percentage of customers within the cohort and the number of customers that put down orders for every week (days 0-6, days 7-13, etc). 

retention.heatmaps.pdf: this is a plot of two heatmaps, showing the change in customers' purchase rates in weeks after signing up. A description of the figure is also shown on the figure. 
