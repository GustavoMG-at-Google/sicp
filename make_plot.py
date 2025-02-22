#!/usr/bin/env python3

from argparse import ArgumentParser
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


parser = ArgumentParser()
parser.add_argument("input_file")
parser.add_argument("output_file")
args = parser.parse_args()

df = pd.read_csv(args.input_file, names=["prime", "runtime"])
lower_bound = df["runtime"].quantile(0.001)
upper_bound = df["runtime"].quantile(0.999)
filtered_df = df[
    (df["runtime"] >= lower_bound) & (df["runtime"] <= upper_bound)
]
means = filtered_df["runtime"].expanding().mean()
plt.plot(filtered_df["prime"], means)
plt.savefig(args.output_file)
