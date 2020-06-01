#!/bin/bash
python3 gendata.py landmarks.csv controls.csv
python3 to_csv_controls.py output.json controls.csv
python3 viz.py example.json run.csv 2> errors.txt