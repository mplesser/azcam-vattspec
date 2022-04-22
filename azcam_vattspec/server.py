"""
azcam server script for VattSpec systems.

Usage: python -m azcam_vatt.vattspec.server
"""

import azcam_vatt.vattspec.server_vattspec

# CLI commands - -m command line flags brings these into CLI namespace
from azcam.cli import *
