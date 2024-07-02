# start AzCamTool
import os
import subprocess

import azcam

exe = os.environ.get("AZCAMTOOL")
cmdserver = azcam.db.get("cmdserver")

if cmdserver is None:
    cmdport = 2402
else:
    cmdport = cmdserver.port

if exe is None:
    exe = "c:\\azcam\\azcam-tool\\azcam_tool\\builds\\azcamtool.exe"


s = f"start {exe} -s localhost -p {cmdport}"

if 0:
    os.system(s)
else:
    p1 = subprocess.Popen(s, shell=True)
    p1.wait()
