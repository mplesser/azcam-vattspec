"""
azcam server script for VattSpec systems.

Usage: python -m azcam_vattepec.server
"""

import azcam_vattspec.server_vattspec

# CLI commands - -m command line flags brings these into CLI namespace
from azcam.cli import *
