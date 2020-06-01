# viz.py
# useage: $ python viz.py *.json exampleInput.csv 
# info: The json file should be formated like "example.json"
#
import matplotlib.pyplot as plt
from matplotlib.patches import Ellipse
import pandas as pd
import numpy as np
import json
import sys
import ast


def random_color():
    colors = ["b","r",'c','g','m','y']
    n = np.random.randint(len(colors))
    return colors[n]

def draw_ovel(ov,ax):
    nstd=1
    pos,cov = ov
    vals, vecs = np.linalg.eig(cov)
    # apparently numpy doesn't sort eigenvalues/eigenvectors
    order = vals.argsort()[::-1]
    vals = vals[order]
    vecs = vecs[:, order]
    theta = np.degrees(np.arctan2(*vecs[:, 0][::-1]))
    w, h = nstd * np.sqrt(abs(vals))

    ell = Ellipse(xy=(pos[0], pos[1]),
                  width=w, height=h,
                  angle=theta, color=random_color())
    ell.set_alpha(0.1)
    ax.add_artist(ell)


def pi_to_pi(theta):
    return np.arctan2(np.sin(theta), np.cos(theta))


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("ERROR: Wrong useage\nuseage: $ python viz.py *.json exampleInput.csv\n")
    else:
        # get config setings
        with open("config.json") as f:
            dt = 1/(float(json.load(f)["FREQ"]))
        # get landmarks
        with open("landmarks.csv") as f:
            df = pd.read_csv(f)
            landmarks = []
            for index,row in df.iterrows():
                landmarks.append([row['X'],row['Y']])
            landmarks = np.array(landmarks)
        #unpack args
        controlInputFile = pd.read_csv(sys.argv[2])
        with open(sys.argv[1]) as f:
            filterOutputFile = json.load(f)
        # unpack the csv file
        for col in ["move","observations"]:
            controlInputFile[col]=controlInputFile[col].apply(eval)
        
        desiredPathPointsX = []
        desiredPathPointsY = []
        pointsSeenX = []
        pointsSeenY = []
        pos = np.array([0,0,0])
        for index, row in controlInputFile.iterrows():
            #move
            u = row["move"]
            x: np.float64 = u[0]*np.cos(pos[2]+u[1])*dt
            y: np.float64 = u[0]*np.sin(pos[2]+u[1])*dt
            th: np.float64 = u[1]*dt
            pos = pos + np.array([x,y,th])
            desiredPathPointsX.append(pos[0])
            desiredPathPointsY.append(pos[1])
            for thing in row["observations"]:
                x = pos[0]+thing[0]*np.cos(thing[1]+pos[2])
                y = pos[1]+thing[0]*np.sin(thing[1]+pos[2])
                pointsSeenX.append(x)
                pointsSeenY.append(y)

        # unpack the json file
        # filterOutputFile["positions"] = list(map(np.array,filterOutputFile["positions"]))
        # filterOutputFile["covariances"] = list(map(np.array,filterOutputFile["covariances"]))
        landMarksSeen = np.array(list(zip(filterOutputFile["positions"],filterOutputFile["covariances"])))
        robotsPath = np.array(filterOutputFile["robotPos"])
        #start graphing
        fig, ax = plt.subplots()

        # covariance ovels
        for ov in landMarksSeen:
            ov1,ov2 = ov
            ov = np.array(ov1),np.array(list(map(np.array,ov2)))
            print(ov)
            draw_ovel(ov,ax)

        # observations
        ax.scatter(pointsSeenX,pointsSeenY,s=0.1,c='gray', label="observations")
        ax.plot(desiredPathPointsX,desiredPathPointsY,c='black')
        #landmarks
        ax.scatter(landmarks[:,0],landmarks[:,1],c='black',label='landmarks')

        #draw robot path
        ax.plot(robotsPath[:,0],robotsPath[:,1],c='b')

        

        # finishing touches
        ax.set_aspect('equal')
        fig.tight_layout()

        plt.show()

        