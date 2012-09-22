#!/bin/sh

pkill python2 && killall python2

echo "Waiting for released sockets..."
sleep 30

./run-server.sh

echo "Server is running..."
sleep 2

./run-clients.sh

echo "Finished!"
