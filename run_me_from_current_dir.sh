#!/bin/bash

echo "Navigate your browser to http://localhost:8090/"
erl -noshell -pa ./ebin -s logic_server -s inets -config my_server

