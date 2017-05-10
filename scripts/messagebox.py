# Code of simple message box in python

from Tkinter import *
import tkMessageBox as messageBox

import os

root=Tk()
root.withdraw()

messageBox.showinfo(sys.argv[1], sys.argv[2])