# Example 12 - Graphical User Interfaces
# Author: Steven Yi <stevenyi@gmail.com>
# 2013.10.28
#
# Adapted for Python 3 by François Pinot, July 2021
#
# This example demonstrates a slightly more advanced GUI example. 
# It uses a slider to allow setting the value of the frequency that
# the notes initiated by the button will play at.  
#
# Note: the actual use of update() here is not thread-safe.  In 
# real-world usage, we would need to drive Csound from a loop calling
# PerformKsmps to ensure thread-safety.  For this example, the updating
# generally works as there are few things demanding computation. 

from tkinter import *
import ctcsound
from random import randint, random
import sys

###############################

# Our Orchestra for our project
orc = """
sr=44100
ksmps=32
nchnls=2
0dbfs=1

gkpch chnexport "freq", 1

instr 1 
kpch port gkpch, 0.01, i(gkpch)
printk .5, gkpch
kenv linsegr 0, .05, 1, .05, .9, .8, 0
aout vco2 p4 * kenv, kpch
aout moogladder aout, 2000, .25 
outs aout, aout
endin"""

c = ctcsound.Csound()    # create an instance of Csound
c.setOption("-odac")  # Set option for Csound
c.setOption("-m7")  # Set option for Csound
c.compileOrc(orc)     # Compile Orchestra from String

c.start()             # When compiling from strings, this call is necessary before doing any performing

perfThread = ctcsound.CsoundPerformanceThread(c.csound())
perfThread.play()


def createChannel(csound, channelName):
    chn, err = csound.channelPtr(channelName, 
        ctcsound.CSOUND_CONTROL_CHANNEL | ctcsound.CSOUND_INPUT_CHANNEL)
    if err:
    	sys.exit(1)
    return chn

class SliderWrapper(object):
    def __init__(self, csound, channelName, slider):
        self.slider = slider
        self.channel = createChannel(csound, channelName)

    def update(self):
        self.channel[0] =  self.slider.get()

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
        self.canvas = Canvas(self,height=self.size,width=self.size)
        self.canvas.pack()
        self.button = Button(self.canvas, text='Play Note', command=self.playNote)    
        self.button.pack()
        self.freqSlider = Scale(self.canvas,from_=80.0, to=600.0,command=self.setFreq,label="Freq")
        self.freqSlider.pack()
        self.freqUpdater = SliderWrapper(c, "freq", self.freqSlider)

    def playNote(self):
        perfThread.inputMessage("i1 0 2 .5")

    def setFreq(self, val):
        print(val)
        self.freqUpdater.update()

    def quit(self):
        self.master.destroy()
        perfThread.stop()
        perfThread.join()


app = Application(Tk())
app.mainloop()
