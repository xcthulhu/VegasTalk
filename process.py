#!/usr/bin/env python3

from os import listdir
from os.path import isfile, join, dirname, realpath
import json
import pandas as pd

def list_files(dir_path):
    return [
        join(dir_path, f)
        for f in listdir(dir_path)
        if isfile(join(dir_path, f))
        if ".json" in f
    ]


def parse_json_files(files):
    for file_name in files:
        with open(file_name) as fp:
            try:
                json_data = json.load(fp)
                yield json_data
            except:
                print("Could not file: {}".format(file_name))


file_dir_path = dirname(realpath(__file__))


def load_sim_files():
    file_list = list_files(join(file_dir_path, "sims"))
    return [json_file for json_file in parse_json_files(file_list)]


def summarize_json_data(json_data, epsilon=1e-09):
    bet_count = len(json_data["regressedBets"])
    stake = sum([p for [_, p] in json_data["initialBets"]])
    regressed_stake = sum([p for [_, p] in json_data["regressedBets"]])
    loss = abs(stake - regressed_stake)
    loss_per_bet = loss / bet_count
    declined_bets = [
        p for [_, p] in json_data["regressedBets"] if p < epsilon or 1 - epsilon < p
    ]
    declined_bet_count = len(declined_bets)
    accepted_bet_count = bet_count - declined_bet_count
    likelihood_of_declined = declined_bet_count / bet_count
    likelihood_of_accepted = accepted_bet_count / bet_count
    return [
        bet_count,
        stake,
        regressed_stake,
        loss,
        loss_per_bet,
        declined_bet_count,
        accepted_bet_count,
        likelihood_of_declined,
        likelihood_of_accepted,
    ]

def get_pandas_data_frame():
    raw_data = list(map(summarize_json_data,load_sim_files()))
    return pd.DataFrame.from_records(raw_data, columns = ["bet_count", "stake", "regressed_stake", "loss", "loss_per_bet", "declined_bet_count", "accepted_bet_count", "likelihood_of_declined", "likelihood_of_accepted"])

if __name__ == "__main__":
    get_pandas_data_frame().sort_values(by=['bet_count']).to_csv("summary.csv")

# Local Variables:
# eval: (projectile-mode)
# projectile-project-compilation-cmd: "python process.py"
# End:
