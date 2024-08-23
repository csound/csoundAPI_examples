# Example 12 - Graphical User Interfaces
# Author: Steven Yi <stevenyi@gmail.com>
# 2013.10.28
#
# Adapted for Python 3 by François Pinot, July 2021
# Adapted for Csound 7.00 by François Pinot, August 2024
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

cs = ctcsound.Csound()    # create an instance of Csound
cs.set_option("-odac")    # Set option for Csound
cs.set_option("-m7")      # Set option for Csound
cs.compile_orc(orc)       # Compile Orchestra from String

cs.start()                # When compiling from strings, this call is
                          # necessary before doing any performing

perf_thread = ctcsound.CsoundPerformanceThread(cs.csound())
perf_thread.play()


def create_channel(csound, channel_name):
    chn, err = csound.channel_ptr(channel_name, 
        ctcsound.CSOUND_CONTROL_CHANNEL | ctcsound.CSOUND_INPUT_CHANNEL)
    if err:
    	sys.exit(1)
    return chn

class SliderWrapper(object):
    def __init__(self, csound, channel_name, slider):
        self.slider = slider
        self.channel = create_channel(csound, channel_name)

    def update(self):
        self.channel[0] =  self.slider.get()

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
        self.canvas = Canvas(self,height=self.size,width=self.size)
        self.canvas.pack()
        self.button = Button(self.canvas, text='Play Note', command=self.play_note)    
        self.button.pack()
        self.freq_slider = Scale(self.canvas, from_=80.0, to=600.0,
            command=self.set_freq, label="Freq")
        self.freq_slider.pack()
        self.freq_updater = SliderWrapper(cs, "freq", self.freq_slider)

    def play_note(self):
        perf_thread.input_message("i1 0 2 .5")

    def set_freq(self, val):
        print(val)
        self.freq_updater.update()

    def quit(self):
        self.master.destroy()
        perf_thread.stop()
        perf_thread.join()


app = Application(Tk())
app.mainloop()
