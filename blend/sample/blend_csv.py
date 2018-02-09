# -*- coding: utf-8 -*-

import os, glob, re
import numpy as np
import pandas as pd


# to be modified according to your directory, 
# e.g. set it os.getcwd() if .csv files are in current directory
path = os.getcwd()  

# load data into a dict, <(str)filename, dataframe>
data = { f.split('/')[-1]: \
        pd.read_csv(f) for f in glob.glob(path + '/*.csv') }

# an alternative option to read data, using regular expression
#data = { re.search(r'([0-9A-Za-z._-]*?.csv)', f).group():
#        pd.read_csv(f) for f in glob.glob(path + '/*.csv') }
assert(len(data) > 0)
print('Loaded files:', data.keys())

def ensemble(data, w, method='arithmetic'):
    """
    @params: data: a dict of dataframes, <(str)filename: dataframe>
             w: a dict of weights, <(str)filename: (int or float)weight>
             method: either arithmetic mean or geometric mean.
    @return: a new dataframe for submission
    """
    submission = pd.DataFrame({'id': data[list(data.keys())[0]]['id']})
    assert(method in ['arithmetic', 'geometric'])
    
    if method == 'arithmetic':
        submission['visitors'] = 0.0
        for key in data.keys():
            submission['visitors'] += data[key]['visitors'] * w[key]
        submission['visitors'] /= sum(w.values())
    else:
        submission['visitors'] = 1.0
        for key in data.keys():
            submission['visitors'] *= data[key]['visitors'] ** w[key]
        submission['visitors'] **= 1. / sum(w.values())
    
    return submission


# Enter weights here
w = { key: 1 for key in data.keys() }
for key in w:
    w[key] = float(input("Enter the weight for {}: ".format(key)))

print('\nWeights for each file:', w)

filename = 'new_submission.csv'
new_submission = ensemble(data, w)
new_submission.to_csv(path + '\\' + filename, index=False)
print('New submission file {} is now created'.format(filename))
