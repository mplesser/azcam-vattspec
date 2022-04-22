"""
azcam console app for VattSpec system

Usage: python -m azcam_vatt.console
"""

import azcam_vatt.vattspec.console_vattspec

# CLI commands - -m command line flags brings these into CLI namespace
from azcam.cli import *
