# -*- coding: utf-8 -*-
"""
Created on Sun Feb 25 12:28:14 2018

@author: Pavan
"""

import numpy as np


rng = np.random.RandomState(seed = 20130810)

# Begin with toy problems and work your way through

from sklearn import datasets

iris = datasets.load_iris()
digits = datasets.load_digits()

# Each of the built-in datasets have data and target attributes that capture
# the features and target respectively

iris.data
iris.target

# Each estimator in scikit-learn is an object that needs to be instantiated
# with the hyperparameters of the object. The `fit(X, y)` and `predict(T)` are
# always implemented for these objects



