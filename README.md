# Off Shelf Promotion Ranking Model
#### The goal of this model is to effectively quantify and optimize revenue from premium product placement in retail grocery stores. The premium placement refers to “end caps” or shelves/gondolas located at the end of the normal fixtures within the grocery store. These areas tend to attract more attention and the products placed in these areas tend to have a higher rate of sales then when they are placed in the normal position on the shelf. Because these areas within the stores are scarce, it is essential to be able to systematically select the products for these areas that will result in the most revenue. 
# Assumptions
#### The following assumptions were made when developing this model
1.	All historical data is accurate, offers were executed as indicated by historical records
2.	All store space/fixture data is accurate
3.	Ranking analysis is done on a regular cadence, 8 weeks prior to in-store sales dates
# Success Metrics
#### There are two metrics that predicate a successful Off Shelf promotion: Performance and Lift. Performance is defined as total dollar sales – returns – COGS + any type of supplier funding. Lift is defined as Off shelf promo placement performance –  median regular promo placement performance/median regular promo placement performance.  Data points with above average quantities in these two metrics were labeled “0” for success, all other data points were labeled “1” for failure. 
![alt text](https://github.com/golferjcd/OffShelf_Ranking/blob/master/data/OS_Metrics.png)
# Features
#### The following features proved the most meaningful to this analysis
1.	Stores selling (numeric) – how many stores were selling the product at a given time
2.	Category (categorical) – Category level of product hierarchy
3.	Seasonal Index (numeric) – Index based on historical sales that applies weights based on time of the year
4.	Comp Index (index) – Index based on pricing of rest of market for a given product
5.	Number of Days Previously on Off Shelf (numeric) – The number of times in the past the product has received Off Shelf placement
6.	Change in ARP (numeric) – the difference between first average retail price and average retail price at the time of analysis
All numeric features were normalized using min/max normalization on a [0,1] scale
# Model Performance
Accuracy: 89.08%
Sensitivity: 0.76
Specificity: 0.93
F1: 0.74
Kappa: 0.67
![alt text](https://github.com/golferjcd/OffShelf_Ranking/blob/master/data/ROC_Plot.png)