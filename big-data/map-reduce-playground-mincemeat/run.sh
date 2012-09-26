#!/bin/sh
python2 map-reduce-mincemeat.py &

python2 mincemeat.py -p sample localhost
