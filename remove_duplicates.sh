#!/bin/bash

while [[ -n "$(python3 remove_duplicates.py $1)" ]]; do
    sleep 0.1
done
