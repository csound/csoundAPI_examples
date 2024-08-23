-- Example 9 - More efficient Channel Communications
-- Author: Steven Yi <stevenyi@gmail.com>
-- 2013.10.28
--
-- This example continues on from Example 9 and just refactors the
-- creation and setup of CsoundMYFLTArray's into a createChannel() 
-- function.  This example illustrates some natural progression that
-- might occur in your own API-based projects, and how you might 
-- simplify your own code.


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
printk 0.5, kamp
printk 0.5, kfreq
aout vco2 kamp, kfreq
aout moogladder aout, 2000, 0.25
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

ampChannel = createChannel(c, "amp")   -- uses utility method to create a channel and get a CsoundMYFLTArray
freqChannel = createChannel(c, "freq")

amp = RandomLine:new(.4, .2)
freq = RandomLine:new(400, 80)

ampChannel:SetValue(0, amp:getValue())
freqChannel:SetValue(0, freq:getValue())

while (c:PerformKsmps() == 0) do
    ampChannel:SetValue(0, amp:getValue())
    freqChannel:SetValue(0, freq:getValue())
end

c:Stop()



