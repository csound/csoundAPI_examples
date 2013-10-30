\ library csound64.dll
library libcsound64.dylib

function: csoundCreate ( options -- csound )
function: csoundCompile ( csound argc argv -- result )
function: csoundPerformKsmps ( csound -- result )
function: csoundStart ( csound -- int )
function: csoundDestroy ( csound -- )
function: csoundCreateThread ( routine[ptr--ptr] userdata -- )
function: csoundScoreEvent ( csound type pfields count -- )
function: csoundGetChannelPtr ( CSOUND >pointervar *name type -- n )  \ maybe not safe
function: csoundGetAudioChannel ( csound *name *samples -- )
function: csoundSetAudioChannel ( csound *name *samples -- )
function: csoundSetControlChannel ( csound *name floatval -- )
function: csoundGetKsmps ( csound -- n )  \ # of samples per control frame
function: csoundSetScorePending ( csound flag -- )  \ used to stop further score events (may not be needed)
function: csoundScoreEvent ( csound type *float-pfields long-numfields -- )
\ used to update function tables (waveforms)
function: csoundTableCopyIn ( csound table# *floats -- )  \ direct alternative way (non-generative, non-normalizing)
function: csoundPerform ( csound -- int )
function: csoundReadScore ( csound *code -- int )
function: csoundSetOption ( csound *option -- int )

\ both can be called during performance...
function: csoundCompileOrc ( csound *code -- int )
function: csoundEvalCode ( csound *code -- flt )  \ may be useful - allows return values


0     csoundCreate value csound 

create myargs z" csound", z" test1.csd", 

csound z" -B256" csoundSetOption .
csound z" -odac" csoundSetOption .
csound 2 myargs csoundCompile .

csound csoundStart .
csound csoundPerform .
csound csoundStop .
