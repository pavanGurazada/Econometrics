import pandas as pd

from pathlib import Path

data_dir = Path("data", "CORE")
income_file = data_dir / "income-by-country.csv"

income_df = pd.read_csv(income_file, skiprows=2)

income_df.head()

# Inequality ratio is defined as the ratio between the 10th Decile and the 1st 
# decile of income in a particular year

income_2014_df = (income_df.query("Year == 2014")
                           .assign(inequality_ratio=lambda x: (x["Decile 10 Income"]/
                                                               x["Decile 1 Income"])))

(income_2014_df.loc[:, ["Country", "Year", "inequality_ratio"]]
               .query("Country in ['Norway', 'Nigeria', 'India', \
                                   'United States', 'Botswana']"))

chosen_countries = ['China', 'India', 'Bangladesh', 'Pakistan', 'Srilanka']
chosen_years = [1980, 1990, 2014]

income_subset_df = (income_df.assign(inequality_ratio=lambda x: (x["Decile 10 Income"]/
                                                                 x["Decile 1 Income"]))
                             .query(f"Year in {chosen_years}")
                             .query(f"Country in {chosen_countries}"))

income_subset_df