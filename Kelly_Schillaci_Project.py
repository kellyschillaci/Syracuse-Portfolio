# -*- coding: utf-8 -*-
"""
Created on Thu Apr 25 20:23:21 2019

@author: kdoyl
"""


import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns; sns.set(style="white", color_codes=True)

#Loads the data in and drops HDI for year and Country-year 
data = pd.read_csv("master.csv")
data.describe()
data.info()
data.isnull().sum()
data=data.drop(['HDI for year','country-year'],axis=1)

#Sorts the suicides per 100K population means in descending order, grouped by countries
data_suicide_mean = data['suicides/100k pop'].groupby(data.country).mean().sort_values(ascending=False)
f,ax = plt.subplots(1,1,figsize=(10,5))
ax = sns.barplot(data_suicide_mean.head(8).index,data_suicide_mean.head(8),palette='rainbow')

#Groups the years into buckets
def decade_mapping(data):
    if 1987<= data <= 1996:
        return "1987-1996"
    elif 1997<= data <= 2006:
        return "1997-2006"
    else:
        return "2007-2016"
data.year = data.year.apply(decade_mapping)

#Plots the suicides per 100K pop by year group and breaks out male vs female
plt.figure(figsize=(10,5))
sns.barplot(x = "year", y = "suicides/100k pop", hue = "sex",data = data.groupby(["year","sex"]).sum().reset_index()).set_title("Decades vs Suicides")

#Plots the total suicides by generation in a pie chart and column    
sns.countplot(data.generation)
plt.title('Generation Counter')
plt.xticks(rotation=45)
plt.show()
f,ax=plt.subplots(1,2,figsize=(18,8))
data['generation'].value_counts().plot.pie(explode=[0.1,0.1,0.1,0.1,0.1,0.1],autopct='%1.1f%%',ax=ax[0],shadow=True)
ax[0].set_title('Generations Count')
ax[0].set_ylabel('Count')
sns.countplot('generation',data=data,ax=ax[1])
ax[1].set_title('Generations Count')
plt.show()

#Creates a dataframe to sum the number of suicides by country then sorts in descending order and plots the reseults
suic_sum = pd.DataFrame(data['suicides_no'].groupby(data['country']).sum())
suic_sum = suic_sum.reset_index().sort_index(by='suicides_no',ascending=False)
most_cont = suic_sum.head(8)
fig = plt.figure(figsize=(18,8))
sns.set(font_scale=2)
sns.barplot(y='suicides_no',x='country',data=most_cont,palette="rainbow")
plt.xticks(rotation=45)
plt.tight_layout()

#Similar to above, adds a comparison of male vs female
suic_sum_m = data['suicides_no'].groupby([data['country'],data['sex']]).sum()
suic_sum_m = suic_sum_m.reset_index().sort_index(by='suicides_no',ascending=False)
most_cont_m = suic_sum_m.head(10)
most_cont_m.head(10)
fig = plt.figure(figsize=(20,5))
sns.set(font_scale=1.5)
sns.barplot(y='suicides_no',x='country',hue='sex',data=most_cont_m,palette='rainbow');
plt.ylabel('Count of suicides')
plt.tight_layout()

