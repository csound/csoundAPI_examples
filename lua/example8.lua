-- Example 8 - More efficient Channel Communications
-- Author: Steven Yi <stevenyi@gmail.com>
-- 2013.10.28
--
-- This example builds on Example 7 by replacing the calls to SetChannel
-- with using GetChannelPtr. In the Csound API, using SetChannel and GetChannel
-- is great for quick work, but ultimately it is slower than pre-fetching the
-- actual channel pointer.  This is because Set/GetChannel operates by doing a 
-- lookup of the Channel Pointer, then setting or getting the value.  This 
-- happens on each call. The alternative is to use GetChannelPtr, which fetches
-- the Channel Pointer and lets you directly set and get the value on the pointer.
--
-- In C/C++/Objective-C, one can directly use MYFLT* to get/set values.  However,
-- for wrapped languages such as Python, Java, and Lua, it is generally not possible
-- to get/set the value on the pointer itself.  The Csound API for host languages 
-- uses a special wrapper object called CsoundMYFLTArray, which will hold a reference
-- to a MYFLT*.  The CsoundMYFLTArray in turn has convenience methods for setting
-- and getting values. 
--
-- The code below shows how to use the CsoundMYFLTArray in conjunction with GetChannelPtr
-- to have a more optimized channel setting system.


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

ampChannel = luaCsnd6.CsoundMYFLTArray(1)    -- create a CsoundMYFLTArray of size 1 
freqChannel = luaCsnd6.CsoundMYFLTArray(1)   -- create a CsoundMYFLTArray of size 1 

c:GetChannelPtr(ampChannel:GetPtr(), "amp", luaCsnd6.CSOUND_CONTROL_CHANNEL + luaCsnd6.CSOUND_INPUT_CHANNEL)
c:GetChannelPtr(freqChannel:GetPtr(), "freq", luaCsnd6.CSOUND_CONTROL_CHANNEL + luaCsnd6.CSOUND_INPUT_CHANNEL)

amp = RandomLine:new(.4, .2)
freq = RandomLine:new(400, 80)

ampChannel:SetValue(0, amp:getValue())    -- note we are now setting values on the CsoundMYFLTArray
freqChannel:SetValue(0, freq:getValue())

--print(amp:getValue())
--print(freq:getValue())

while (c:PerformKsmps() == 0) do
    ampChannel:SetValue(0, amp:getValue())
    freqChannel:SetValue(0, freq:getValue())
end

c:Stop()


