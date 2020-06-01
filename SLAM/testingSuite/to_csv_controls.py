import sys
import json
import numpy as np
import pandas as pd
from pprint import pprint as pprint

def Hz_to_secs(Hz):
    """
    Converts Hz to seconds
    :param Hz: the Hz
    :return: Seconds
    """
    return 1/Hz

if __name__=="__main__":
    if len(sys.argv) != 3:
        print("Usage: <output.json> <controls.csv>")
    else:
        # open the output
        with open(sys.argv[1]) as f:
            data = json.load(f)

        #open the controls
        controls = np.genfromtxt(sys.argv[2], delimiter=',', skip_header=1)

        #open config file
        with open("config.json") as f:
            config = json.load(f)
            step = Hz_to_secs(float(config["FREQ"]))
            FREQ = int(config["FREQ"])  # frequency in Hz
            SIM_TIME = int(config["SIM_TIME"])  # time in seconds
            INITIAL_POSE = np.array([float(a) for a in config["INITIAL_POSE"]])
            RANGE = float(config["RANGE"])  # range of sensor

        # number of entry's to give the table
        n = len(data)

        # get the time steps
        time_steps = [step for _ in range(n)]

        # get controls
        controls_col = []
        controls[::,0] = np.array(list(controls[1:,0]) + [config["SIM_TIME"]])
        last_end_time = 0
        for row in controls:
            for _ in np.arange(last_end_time,row[0],step):
                controls_col.append("[{},{}]".format(row[1],row[2]))
            last_end_time = row[0]
        # for t in range(1, (FREQ * SIM_TIME) + 1):
        #     if controls.size != 3 and t / FREQ >= controls[1, 0]:
        #         controls = controls[1:, :]  # remove first row
        #     v = controls[0, 1]
        #     omega = controls[0, 2]
        #     controls_col.append('['+str(v)+','+str(omega)+']')

        # get the observations
        observations_col = []
        for k,v in data.items():
            observations_col.append(str(v["noisy_observations"]))

        # make table
        df = pd.DataFrame()

        print("move = ",len(controls_col))
        print("obs  = ",len(observations_col))
        print("step = ",len(time_steps))
        df["move"] = controls_col[:len(time_steps)]
        df["observations"] = observations_col
        df["time step"] = time_steps

        print(df)

        # save it to the correct directory
        save_loc = "run.csv"

        df.to_csv(save_loc,index=False)
        print("Saved in "+save_loc)