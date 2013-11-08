-- Example 11 - Graphical User Interfaces
-- Author: Steven Yi <stevenyi@gmail.com>
-- 2013.10.28
--
-- This example demonstrates a slightly more advanced GUI example. 
-- It uses a slider to allow setting the value of the frequency that
-- the notes initiated by the button will play at.  
--
-- Note: the actual use of update() here is not thread-safe.  In 
-- real-world usage, we would need to drive Csound from a loop calling
-- PerformKsmps to ensure thread-safety.  For this example, the updating
-- generally works as there are few things demanding computation. 

require "luaCsnd6"
require "wx"

--------------------------------------------------------------
-- Our Orchestra for our project
local orc = [[
sr=44100
ksmps=32
nchnls=2
0dbfs=1

gkpch chnexport "freq", 1

instr 1 
kpch port gkpch, 0.5
printk .5, gkpch
kenv linsegr 0, .05, 1, .05, .9, .8, 0
aout vco2 p4 * kenv, kpch
aout moogladder aout, 2000, .25 
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

function createChannel(csound, channelName)
    -- Creates a Csound Channel and returns a CsoundMYFLTArray wrapper object
    local chn = luaCsnd6.CsoundMYFLTArray(1)
    csound:GetChannelPtr(chn:GetPtr(), channelName, 
        luaCsnd6.CSOUND_CONTROL_CHANNEL + luaCsnd6.CSOUND_INPUT_CHANNEL) 
    return chn
end

local SliderWrapper = {}

function SliderWrapper:new(csound, channelName)
    local o = {}
    setmetatable(o, self)
    self.__index = self
    o.slider = self
    o.channel = createChannel(csound, channelName)
    return o
end


local frame = wx.wxFrame(wx.NULL, wx.wxID_ANY, "example 12 - wxLua", wx.wxDefaultPosition,
                wx.wxSize(450, 450), wx.wxDEFAULT_FRAME_STYLE )

local button = wx.wxButton(frame, wx.wxID_ANY, "Play Note", wx.wxDefaultPosition, wx.wxSize(20, 50))

local slider = wx.wxSlider(frame, wx.wxID_ANY, 100, 80, 600, wx.wxDefaultPosition, wx.wxSize(200, 40),
    wx.wxSL_AUTOTICKS + wx.wxSL_LABELS)


function onClick (event)
    perfThread:InputMessage("i1 0 2 .5 400 .25")
end

function onScroll ()
    freqUpdater = SliderWrapper:new(c, "freq", freqSlider)
    freqUpdater.channel:SetValue(0, slider:GetValue())
end


function onQuit(event)
    event:Skip()
    perfThread:Stop()
    perfThread:Join()
    frame:Show(false)
    frame:Destroy()
end

button:Connect(wx.wxEVT_COMMAND_BUTTON_CLICKED, onClick)
slider:Connect(wx.wxEVT_COMMAND_SLIDER_UPDATED, onScroll)
frame:Connect(wx.wxEVT_CLOSE_WINDOW, onQuit)

sizerTop = wx.wxBoxSizer(wx.wxVERTICAL)
sizerTop:Add(button, 3, wx.wxGROW + wx.wxALL, 6)
sizerTop:Add(slider, 5, wx.wxGROW + wx.wxALL, 6)

frame:SetAutoLayout(true)
frame:SetSizer(sizerTop)
sizerTop:SetSizeHints(frame)
sizerTop:Fit(frame)

frame:Show(true)

wx.wxGetApp():MainLoop()



