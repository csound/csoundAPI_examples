-- Example 11 - Graphical User Interfaces
-- Author: Steven Yi <stevenyi@gmail.com>
-- 2013.10.28
--
-- This example demonstrates a minimal Graphical User Interface application.
-- The setup of Csound and starting of the CsoundPerformanceThread is done in 
-- the global scripting space.  Afterwards, a Tkinter GUI is created that has
-- one button.  The button's callback (the command action) routes to a function
-- that just sends an event to Csound.
--
-- For this example, since there is no need to synchronize continous channel data
-- changes with Csound, it is more efficient to use the CsoundPerformanceThread,
-- as it is a native thread.  We use the CsoundPerformanceThread's InputMessage()
-- function to ensure that the message is processed in a thread-safe manner. 

require "luaCsnd6"
require "ltk"

--------------------------------------------------------------
-- Our Orchestra for our project
local orc = [[
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1 
kenv linsegr 0, .05, 1, .05, .9, .8, 0
aout vco2 p4 * kenv, p5
aout moogladder aout, 2000, p6
outs aout, aout
endin
]]

local c = luaCsnd6.Csound()    -- create an instance of Csound
c:SetOption("-odac")  -- Set option for Csound
c:SetOption("-m7")  -- Set option for Csound
c:CompileOrc(orc)     -- Compile Orchestra from String

c:Start()             -- When compiling from strings, this call is necessary before doing any performing

local perfThread = luaCsnd6.CsoundPerformanceThread(c)
perfThread:Play()


ltk.wm{'title', '.', "example 11 - Play a note"}

local b = ltk.button{text="Play Note",
        command=function()
            perfThread:InputMessage("i1 0 2 .5 400 .25")
        end
}

local frame = ltk.frame{}

function destroy()
    perfThread:Stop()
    perfThread:Join()
    ltk.exit()
end

ltk.pack {b}
ltk.bind{frame, '<Destroy>', {destroy}}

ltk.mainloop()


