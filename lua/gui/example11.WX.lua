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
require "wx"

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


local frame = wx.wxFrame(wx.NULL, wx.wxID_ANY, "example 11 - wxLua", wx.wxDefaultPosition,
                wx.wxSize(300, 100), wx.wxDEFAULT_FRAME_STYLE )
local button = wx.wxButton(frame, wx.wxID_ANY, "Play Note", wx.wxDefaultPosition, wx.wxSize(20, 50))

function onClick (event)
    perfThread:InputMessage("i1 0 2 .5 400 .25")
end

function onQuit(event)
    event:Skip()
    perfThread:Stop()
    perfThread:Join()
    frame:Show(false)
    frame:Destroy()
end

button:Connect(wx.wxEVT_COMMAND_BUTTON_CLICKED, onClick)
frame:Connect(wx.wxEVT_CLOSE_WINDOW, onQuit)

sizerTop = wx.wxBoxSizer(wx.wxVERTICAL)
sizerTop:Add(button, 3, wx.wxGROW + wx.wxALL, 6)

frame:SetAutoLayout(true)
frame:SetSizer(sizerTop)

frame:Show(true)

wx.wxGetApp():MainLoop()



