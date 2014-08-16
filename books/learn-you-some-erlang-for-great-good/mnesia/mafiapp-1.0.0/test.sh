#!/bin/bash
erl -make
ct_run -pa ebin/ -spec mafiapp.spec
