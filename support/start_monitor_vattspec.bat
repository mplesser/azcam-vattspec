@echo off

start/min "azcammonitor" python -m azcam_server.monitor.azcammonitor.py -configfile parameters_vattspec_monitor.ini
