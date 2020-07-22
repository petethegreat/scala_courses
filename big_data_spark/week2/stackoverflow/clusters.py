#!/usr/bin/env python

import pandas as pd
import numpy as np
import seaborn as sns
from matplotlib import pyplot as plt 

sns.set()

def load_data(path):
    df = pd.read_csv(path,header=None,names=["cluster","language","score"])
    return df

def summarise(data):

    summary = data.groupby("cluster").agg(["mean","min",'max',"std","median","count"])
    slimmed = summary[summary["language","count"] > 20]
    print(slimmed)
def main():
    """ do stuff"""
    path = "./output/clusters_coalesced.csv"
    data = load_data(path)
    summarise(data)


    # print(data)
    fig,ax = plot_data(data)
    display(fig)


def plot_data(data):

    fig,ax = plt.subplots()
    ax.scatter(data["language"],data["score"],c=data["cluster"])


    return fig,ax
def display(fig):
    fig.show()
    instr = ""
    while not instr.lower().startswith("q"):
        instr = input("enter q to quit")


if __name__ == "__main__":
    main()
