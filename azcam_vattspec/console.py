"""
Setup method for vattspec console.
Usage example:
  ipython -i -m azcam_vattspec.console --profile azcamconsole
"""

import os
import sys
import threading

import azcam
import azcam.utils
import azcam_console.console
from azcam_console.tools.console_tools import create_console_tools
import azcam_console.shortcuts
from azcam.tools.ds9display import Ds9Display
from azcam_console.tools.focus import FocusConsole


def setup():
    try:
        i = sys.argv.index("-datafolder")
        datafolder = sys.argv[i + 1]
    except ValueError:
        datafolder = None

    # ****************************************************************
    # files and folders
    # ****************************************************************
    azcam.db.systemname = "vattspec"

    azcam.db.systemfolder = os.path.dirname(__file__)
    azcam.db.datafolder = azcam.utils.get_datafolder(datafolder)

    parfile = os.path.join(
        azcam.db.datafolder,
        "parameters",
        f"parameters_console_{azcam.db.systemname}.ini",
    )

    # ****************************************************************
    # start logging
    # ****************************************************************
    logfile = os.path.join(azcam.db.datafolder, "logs", "console.log")
    azcam.db.logger.start_logging(logfile=logfile)
    azcam.log(f"Configuring console for {azcam.db.systemname}")

    # ****************************************************************
    # display
    # ****************************************************************
    display = Ds9Display()
    dthread = threading.Thread(target=display.initialize, args=[])
    dthread.start()  # thread just for speed

    # ****************************************************************
    # console tools
    # ****************************************************************
    create_console_tools()

    # ****************************************************************
    # scripts
    # ****************************************************************
    focus = FocusConsole()
    focus.focus_component = "telescope"
    focus.focus_type = "absolute"

    # ****************************************************************
    # try to connect to azcamserver
    # ****************************************************************
    server = azcam.db.tools["server"]
    connected = server.connect(port=2412)
    if connected:
        azcam.log("Connected to azcamserver")
    else:
        azcam.log("Not connected to azcamserver")

    # ****************************************************************
    # read par file
    # ****************************************************************
    azcam.db.parameters.read_parfile(parfile)
    azcam.db.parameters.update_pars()

    # ****************************************************************
    # finish
    # ****************************************************************
    azcam.log("Configuration complete")


setup()
from azcam_console.cli import *

del setup
