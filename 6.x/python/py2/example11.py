# Example 11 - Graphical User Interfaces
# Author: Steven Yi <stevenyi@gmail.com>
# 2013.10.28
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

from Tkinter import *
import csnd6
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

c = csnd6.Csound()    # create an instance of Csound
c.SetOption("-odac")  # Set option for Csound
c.SetOption("-m7")  # Set option for Csound
c.CompileOrc(orc)     # Compile Orchestra from String

c.Start()             # When compiling from strings, this call is necessary before doing any performing

perfThread = csnd6.CsoundPerformanceThread(c)
perfThread.Play()

class Application(Frame):

    def __init__(self,master=None):
        master.title("Csound API GUI Example")
        self.items = []
        self.notes = []
        Frame.__init__(self,master)
        self.pack()
        self.createUI()
        self.master.protocol("WM_DELETE_WINDOW", self.quit)

    def createUI(self):
        self.size = 600
        self.canvas = Canvas(self,height=self.size,width=self.size,bg="darkgray")
        self.canvas.pack()
        # create button and setup the playNote() callback
        self.button = Button(self.canvas, text='Play Note', command=self.playNote)    
        self.button.pack()

    def playNote(self):
        perfThread.InputMessage("i1 0 2 .5 400 .25")

    def quit(self):
        self.master.destroy()
        perfThread.Stop()
        perfThread.Join()


app = Application(Tk())
app.mainloop()


