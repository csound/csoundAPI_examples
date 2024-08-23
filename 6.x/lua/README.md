# Csound 6 - Lua API Examples
Author: Francesco Armando Porta <ilterzouomo@fastwebnet.it>
2013.11.8

This folder contains examples for using the Csound API in Lua. They start with a minimal usage of Csound and each example afterwards builds upon the previous one.  There are numerous comments in each file to explain what is new in the example as well as explain how the new things may be used. 

## Useful Notes
* Preferably use LuaJIT instead of Lua for better performance.
* To run an example, in a terminal, type "luajit" or "lua" followed by the example file. For example, to run example1.lua, use "luajit example1.lua" or "lua example1.lua".
* You can query the Csound API from a lua prompt by doing "for k,v in pairs(luaCsnd6) do print(k,v) end", assuming you have imported csnd6 with "require 'luaCsnd6'". 

