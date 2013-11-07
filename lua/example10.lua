-- Example 10 - More efficient Channel Communications
-- Author: Steven Yi <stevenyi@gmail.com>
-- 2013.10.28
--
-- This example continues on from Example 10 and introduces a 
-- ChannelUpdater object. The ChannelUpdater will create and 
-- store a CsoundMYFLTArray that is wrapping a Csound Channel.
-- Additionally, it will store and call an object that has a 
-- getValue() method to update values in the channel when 
-- update() is called. 
-- 
-- This example continues the illustration of a progression of 
-- a project.  Note that the process has changed a little bit
-- where we now create a number of ChannelUpdater objects and
-- store them in a list.  The list is then iterated through for
-- updating the channel with the latest values.  In a real-world
-- project, this kind of scenario occurs when there are n-number of 
-- items to update channels and one wants to have a flexible number
-- that may even change dynamically at runtime.


require "luaCsnd6"


local RandomLine = {}

function RandomLine:new(base, range)
    local o = {}
    setmetatable(o, self)
    self.__index = self
    o.curVal = 0.0
    o:reset()
    o.base = base
    o.range = range
    return o
end

function RandomLine:reset()
    self.dur = math.random(256,512)
    self.End = math.random()
    self.increment = (self.End - self.curVal) / self.dur
end

function RandomLine:getValue()
    self.dur = self.dur - 1
    if(self.dur < 0) then
        self:reset()
    end
    retVal = self.curVal
    self.curVal = self.curVal + self.increment
    return self.base + (self.range * retVal)
end


function createChannel(csound, channelName)
    -- Creates a Csound Channel and returns a CsoundMYFLTArray wrapper object
    local chn = luaCsnd6.CsoundMYFLTArray(1)
    csound:GetChannelPtr(chn:GetPtr(), channelName, 
        luaCsnd6.CSOUND_CONTROL_CHANNEL + luaCsnd6.CSOUND_INPUT_CHANNEL) 
    return chn
end

local ChannelUpdater = {}

function ChannelUpdater:new(csound, channelName, updater)
    local o = {}
    setmetatable(o, self)
    self.__index = self
    o.updater = updater
    o.channel = createChannel(csound, channelName)
    return o
end

function ChannelUpdater:update()
    self.channel:SetValue(0, self.updater:getValue())
end

--------------------------------------------------------------

-- Our Orchestra for our project
orc = [[
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1 
kamp chnget "amp"
kfreq chnget "freq"
kres chnget "resonance"
printk 0.5, kamp
printk 0.5, kfreq
printk 0.5, kres
aout vco2 kamp, kfreq
aout moogladder aout, 2000, kres
outs aout, aout
endin
]]

local c = luaCsnd6.Csound()    -- create an instance of Csound
c:SetOption("-odac")  -- Set option for Csound
c:SetOption("-m7")  -- Set option for Csound
c:CompileOrc(orc)     -- Compile Orchestra from String

local sco = "i1 0 60\n"

c:ReadScore(sco)     -- Read in Score generated from notes 

c:Start()             -- When compiling from strings, this call is necessary before doing any performing

-- Create a set of ChannelUpdaters
channels = {ChannelUpdater:new(c, "amp", RandomLine:new(.4, .2)),
            ChannelUpdater:new(c, "freq", RandomLine:new(400, 80)),
            ChannelUpdater:new(c, "resonance", RandomLine:new(0.4, .3))}

-- Initialize all Channel Values
for i, chn in ipairs(channels) do
    chn:update()
end

while (c:PerformKsmps() == 0) do
    for i, chn in ipairs(channels) do   -- update all channel values
        chn:update()
    end
end

c:Stop()



