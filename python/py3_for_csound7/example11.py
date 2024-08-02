# Example 11 - Graphical User Interfaces
# Author: Steven Yi <stevenyi@gmail.com>
# 2013.10.28
#
# Adapted for Python 3 by François Pinot, July 2021
# Adapted for Csound 7.00 by François Pinot, August 2024
#
# This example demonstrates a minimal Graphical User Interface application.
# The setup of Csound and starting of the CsoundPerformanceThread is done in 
# the global scripting space.  Afterwards, a Tkinter GUI is created that has
# one button.  The button's callback (the command action) routes to a function
# that just sends an event to Csound.
#
# For this example, since there is no need to synchronize continous channel data
# changes with Csound, it is more efficient to use the CsoundPerformanceThread,
# as it is a native thread.  We use the CsoundPerformanceThread's InputMessage()
# function to ensure that the message is processed in a thread-safe manner. 

from tkinter import *
import ctcsound
from random import randint, random

###############################
# Our Orchestra for our project

orc = """
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1 
kenv linsegr 0, .05, 1, .05, .9, .8, 0
aout vco2 p4 * kenv, p5
aout moogladder aout, 2000, p6
outs aout, aout
endin"""

cs = ctcsound.Csound()    # create an instance of Csound
cs.set_option("-odac")    # Set option for Csound
cs.set_option("-m7")      # Set option for Csound
cs.compile_orc(orc)       # Compile Orchestra from String

cs.start()                # When compiling from strings, this call is
                          # necessary before doing any performing

perf_thread = ctcsound.CsoundPerformanceThread(cs.csound())
perf_thread.play()

class Application(Frame):

    def __init__(self,master=None):
        master.title("Csound API GUI Example")
        self.items = []
        self.notes = []
        Frame.__init__(self,master)
        self.pack()
        self.create_UI()
        self.master.protocol("WM_DELETE_WINDOW", self.quit)

    def create_UI(self):
        self.size = 600
        self.canvas = Canvas(self ,height=self.size, width=self.size, bg="darkgray")
        self.canvas.pack()
        # create button and setup the playNote() callback
        self.button = Button(self.canvas, text='Play Note', command=self.play_note)    
        self.button.pack()

    def play_note(self):
        perf_thread.input_message("i1 0 2 .5 400 .25")

    def quit(self):
        self.master.destroy()
        perf_thread.stop()
        perf_thread.join()


app = Application(Tk())
app.mainloop()
