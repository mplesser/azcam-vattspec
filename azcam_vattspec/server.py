"""
azcamserver config for vattspec
"""

import os
import sys

import azcam
import azcam.utils
import azcam.server.server
import azcam.server.shortcuts
from azcam.server.cmdserver import CommandServer
from azcam.header import System
from azcam.server.tools.instrument import Instrument
from azcam.server.tools.arc.controller_arc import ControllerArc
from azcam.server.tools.arc.exposure_arc import ExposureArc
from azcam.server.tools.arc.tempcon_arc import TempConArc
from azcam.server.tools.ds9display import Ds9Display
from azcam.server.webtools.webserver.fastapi_server import WebServer
from azcam.server.webtools.status.status import Status

from azcam.monitor.monitorinterface import AzCamMonitorInterface
from azcam_vattspec.telescope_vatt import VattTCS

# ****************************************************************
# parse command line arguments
# ****************************************************************
try:
    i = sys.argv.index("-system")
    option = sys.argv[i + 1]
except ValueError:
    option = "menu"
try:
    i = sys.argv.index("-datafolder")
    datafolder = sys.argv[i + 1]
except ValueError:
    datafolder = None
try:
    i = sys.argv.index("-lab")
    lab = 1
except ValueError:
    lab = 0


def setup():
    global option, datafolder, lab

    # ****************************************************************
    # define folders for system
    # ****************************************************************
    azcam.db.systemname = "vattspec"
    azcam.db.servermode = azcam.db.systemname

    azcam.db.systemfolder = os.path.dirname(__file__)
    azcam.db.systemfolder = azcam.utils.fix_path(azcam.db.systemfolder)
    azcam.db.datafolder = azcam.utils.get_datafolder(datafolder)

    parfile = os.path.join(
        azcam.db.datafolder,
        "parameters",
        f"parameters_server_{azcam.db.systemname}.ini",
    )

    # ****************************************************************
    # enable logging
    # ****************************************************************
    logfile = os.path.join(azcam.db.datafolder, "logs", "server.log")
    azcam.db.logger.start_logging(logfile=logfile)
    azcam.log(f"Configuring for vattspec")

    # ****************************************************************
    # controller
    # ****************************************************************
    controller = ControllerArc()
    controller.timing_board = "gen2"
    controller.clock_boards = ["gen2"]
    controller.video_boards = ["gen2"]
    controller.utility_board = "gen2"
    controller.set_boards()
    controller.pci_file = os.path.join(
        azcam.db.datafolder, "dspcode", "dsppci", "pci2.lod"
    )
    controller.timing_file = os.path.join(
        azcam.db.datafolder, "dspcode", "dsptiming", "tim2.lod"
    )
    controller.utility_file = os.path.join(
        azcam.db.datafolder, "dspcode", "dsputility", "util2.lod"
    )
    controller.video_gain = 10
    controller.video_speed = 1
    if lab:
        controller.camserver.set_server("conserver7", 2405)
    else:
        controller.camserver.set_server("vattccdc", 2405)

    # ****************************************************************
    # temperature controller
    # ****************************************************************
    tempcon = TempConArc()
    tempcon.set_calibrations([0, 0, 3])
    tempcon.set_corrections([2.0, 0.0, 0.0], [1.0, 1.0, 1.0])
    tempcon.temperature_correction = 0
    tempcon.control_temperature = -110.0

    # ****************************************************************
    # exposure
    # ****************************************************************
    exposure = ExposureArc()
    filetype = "FITS"
    exposure.filetype = exposure.filetypes[filetype]
    exposure.image.filetype = exposure.filetypes[filetype]
    exposure.display_image = 0
    if lab:
        exposure.send_image = 1
        exposure.folder = "/data/vattspec"
        exposure.sendimage.set_remote_imageserver()
    else:
        exposure.send_image = 1
        exposure.folder = "/mnt/TBArray/images"
        exposure.sendimage.set_remote_imageserver(
            "10.0.1.108", 6543, "dataserver"
        )  # vattcontrol.vatt

    # ****************************************************************
    # detector
    # ****************************************************************
    detector_vattspec = {
        "name": "vattspec",
        "description": "STA0520 2688x512 CCD",
        "ref_pixel": [1344, 256],
        "format": [2688, 16, 0, 20, 512, 0, 0, 0, 0],
        "focalplane": [1, 1, 1, 1, [0]],
        "roi": [1, 2688, 1, 512, 2, 2],
        "ext_position": [[1, 1]],
        "jpg_order": [1],
    }
    exposure.set_detpars(detector_vattspec)
    exposure.image.focalplane.wcs.ctype1 = "LINEAR"
    exposure.image.focalplane.wcs.ctype2 = "LINEAR"

    # ****************************************************************
    # instrument (not used)
    # ****************************************************************
    instrument = Instrument()

    # ****************************************************************
    # telescope
    # ****************************************************************
    telescope = VattTCS()

    # ****************************************************************
    # system header template
    # ****************************************************************
    template = os.path.join(
        azcam.db.datafolder, "templates", "fits_template_vattspec_master.txt"
    )
    system = System("vattspec", template)
    system.set_keyword("DEWAR", "vattspec_dewar", "Dewar name")

    # ****************************************************************
    # display
    # ****************************************************************
    display = Ds9Display()

    # ****************************************************************
    # read par file
    # ****************************************************************
    azcam.db.parameters.read_parfile(parfile)
    azcam.db.parameters.update_pars()

    # ****************************************************************
    # define and start command server
    # ****************************************************************
    cmdserver = CommandServer()
    cmdserver.port = 2412
    azcam.log(f"Starting cmdserver - listening on port {cmdserver.port}")
    # cmdserver.welcome_message = "Welcome - azcam-itl server"
    cmdserver.start()

    # ****************************************************************
    # web server
    # ****************************************************************
    webserver = WebServer()
    webserver.index = os.path.join(azcam.db.systemfolder, "index_vattspec.html")
    webserver.port = 2403  # common web port
    webserver.start()
    webstatus = Status(webserver)
    webstatus.initialize()

    # ****************************************************************
    # azcammonitor
    # ****************************************************************
    monitor = AzCamMonitorInterface()
    monitor.proc_path = "/azcam/azcam-vatt/bin/start_server_vattspec.bat"
    monitor.register()

    # ****************************************************************
    # GUIs
    # ****************************************************************
    if 1:
        import azcam_vattspec.start_azcamtool

    # ****************************************************************
    # finish
    # ****************************************************************
    azcam.log("Configuration complete")


setup()
from azcam.cli import *
