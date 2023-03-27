import os

import numpy as np
import pandas as pd

# Save tests in csvs
src_dir = r'datasets/braatz_original'
dst_dir = r'datasets/braatz_anomaly_detection/test/'
cols = [f'XMEAS({i})' for i in range(1, 42)] + \
    [f'XMV({i})' for i in range(1, 12)]
fault_start = 160
length = 960
fault_array = np.zeros(length)
for fault in range(22):
    file = f'd{fault:02}_te'
    fault_array[fault_start:] = fault
    fault_df = pd.Series(fault_array, name='label')
    df = pd.read_csv(os.path.join(src_dir, f'{file}.dat'), delimiter='  ',
                     names=cols)
    df = pd.concat([df, fault_df], axis=1)
    df.to_csv(os.path.join(dst_dir, f'd{fault:02}.csv'), index=False)
