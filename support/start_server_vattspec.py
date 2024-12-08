"""
Python process start file
"""

import subprocess

OPTIONS = "-system vattspec"
CMD = f"ipython --ipython-dir=/data/ipython --profile azcamserver -i -m azcam_vattspec.server -- {OPTIONS}"

p = subprocess.Popen(
    CMD,
    creationflags=subprocess.CREATE_NEW_CONSOLE,
)
