#!/usr/bin/env python

import sys
import json
import numpy as np

def pi_to_pi(theta):
    return np.arctan2(np.sin(theta), np.cos(theta))

def observed_landmarks(landmarks, x, y, theta, dist):
    '''
    return sensed landmarks within the dist (assume 360 fov for now)
    '''
    # TODO add fov parameter

    # transform landmark locations to robot frame
    world_robot = np.array([
        [np.cos(theta), -np.sin(theta)],
        [np.sin(theta), np.cos(theta)]
        ])
    robot_world = np.linalg.inv(world_robot)

    # translate landmarks to robot frame
    landmarks_t = landmarks - np.array([x, y]).reshape(1,2)

    # rotate landmarks to robot frame
    robot_landmarks = np.dot(robot_world, landmarks_t.T).T

    # compute the range and bearing to each landmark
    ranges = np.linalg.norm(robot_landmarks, axis=1)
    bearings = np.arctan2(robot_landmarks[:,1], robot_landmarks[:,0])

    # get landmarks in range
    range_mask = ranges <= dist
    return list(zip(ranges[range_mask], bearings[range_mask]))


def move_robot(x, y, theta, v, omega, dt):
    new_x = x + v * np.cos(theta) * dt
    new_y = y + v * np.sin(theta) * dt
    new_theta = pi_to_pi(theta + omega * dt)
    return new_x, new_y, new_theta


if __name__ == '__main__':
    # initialize from a config file
    with open("config.json") as f:
        config = json.load(f)
    FREQ = int(config["FREQ"]) # frequency in Hz
    SIM_TIME = int(config["SIM_TIME"]) # time in seconds
    INITIAL_POSE = np.array([float(a) for a in config["INITIAL_POSE"]])
    RANGE = float(config["RANGE"]) # range of sensor
    RANGE_error = float(config["RANGE_ERROR"])
    BARING_ERROR = float(config["BARING_ERROR"])

    if len(sys.argv) != 3:
        print('Usage: <landmark file> <control file>')
        sys.exit()

    landmarks = np.genfromtxt(sys.argv[1], delimiter=',', skip_header=1)
    controls = np.genfromtxt(sys.argv[2], delimiter=',', skip_header=1)

    dt = 1/FREQ

    pose = INITIAL_POSE
    data = {}
    for t in range(1, (FREQ * SIM_TIME) + 1):
        x = pose[0]
        y = pose[1]
        theta = pose[2]
        if controls.size != 3 and t/FREQ >= controls[1,0]:
            controls = controls[1:,:] # remove first row
        v = controls[0,1]
        omega = controls[0,2]
        prev_pose = pose
        pose = np.array(move_robot(x, y, theta, v, omega, dt))
        # TODO use pose and prev_pose to create noisy odometry measurement

        # get observations
        observations = observed_landmarks(landmarks, x, y, theta, RANGE)
        # TODO use observations to create noisy observations
        # create noisy observations
        noise = np.array([RANGE_error,BARING_ERROR])
        noisy_observations = [list(np.array(o) + np.random.normal(0,noise)) for o in observations]
        #print(list(np.array(observations[0]) + np.random.normal(0,noise))," for ", observations[0])
        # TODO add missed detections and false alarms

        # add data to time indexed dict
        data[t/FREQ] = {}
        data[t/FREQ]['true_pose'] = list(pose)
        data[t/FREQ]['true_observations'] = observations
        data[t/FREQ]['noisy_odometry'] = []
        data[t/FREQ]['noisy_observations'] = noisy_observations

    with open('output.json', 'w+') as f:
        f.write(json.dumps(data,separators=(' ,',':\n')))
