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
local lgi = require 'lgi'
local Gtk = lgi.require('Gtk')

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

local window = Gtk.Window {
   title = 'example 12 - Lua and GTK lgi',
   default_width = 200,
   default_height = 200
}

local button = Gtk.Button {label = 'Play Note'}
local scale = Gtk.VScale{}
scale:set_vexpand(true)
scale:set_value(80)
scale:set_range(80, 600)

if tonumber(Gtk._version) >= 3 then
   window.has_resize_grip = true
end

function window:on_destroy()
    perfThread:Stop()
    perfThread:Join()
    Gtk.main_quit()
end

function button:on_clicked()
    perfThread:InputMessage("i1 0 2 .5")
end

function scale:on_value_changed(v)
    print(self:get_value())
    freqUpdater = SliderWrapper:new(c, "freq", freqSlider)
    freqUpdater.channel:SetValue(0, self:get_value())
end

local vbox = Gtk.VBox()
vbox:pack_start(button, false, false, 10)
vbox:pack_start(scale, false, true, 10)
window:add(vbox)

window:show_all()
Gtk.main()

