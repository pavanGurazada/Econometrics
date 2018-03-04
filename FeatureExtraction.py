# -*- coding: utf-8 -*-
"""
Created on Sun Feb 25 13:44:46 2018

@author: Pavan
"""

import numpy as np
import pandas as pd

"""
One important difference to note coming from the R world, is that feature
processing is a bit more manual in sklearn. This is actually a good thing 
since the formula notation for R hides this as an abstraction from the user.
This is okay till the user needs to run some models that do not take formula
inputs.

A good way to encode categorical features is to use one-hot encoding. This can
be done either in pandas or within sklearn.

Here is a basic example:
"""

measurements = [{'city': 'Dubai','temperature': 33.0},
                {'city': 'London', 'temperature': 12.0},
                {'city': 'San Francisco', 'temperature': 18.0}]

from sklearn.feature_extraction import DictVectorizer
vec = DictVectorizer()
vec.fit_transform(measurements).toarray()
vec.get_feature_names()

"""
Personally, since the real data wrangling and initial exploration is done in 
Pandas, it might be a good idea to even do the encoding of categorical features
in one go and depend on scikit learn for the modelings
"""

titanic_train = pd.read_csv('data/general/titanic_train.csv')
titanic_test = pd.read_csv('data/general/titanic_test.csv')

titanic_train.info()

# We might choose to exclude some columns

titanic_train = titanic_train.drop(['Name', 'Ticket', 'Cabin'], axis = 1)\
                             .dropna()
titanic_train.info()

# There are two categorical variables now - Sex and Embarked. We need to use
# one-hot encoding to convert these
# The get_dummies function from pandas does this job, and autodetects the
# `object` columns before doing so!

titanic_train = pd.get_dummies(titanic_train)
titanic_train.info()

X_train = titanic_train.drop('Survived', axis = 1)
y_train = titanic_train['Survived']


