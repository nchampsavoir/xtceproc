XTCEproc
========

XTCE-Based Spacecraft Telemetry Processor

Overview
--------

This is an attempt at writing a set of tools for telemetry and telecommand processing in LuaJIT. Currently, only a (partial) telemetry decommutation engine is provided. 

XTCEproc works as follows : 

 1. It takes an XTCE file as input and generates a docummuation model in LuaJIT. 
 2. It makes use of this decommutation to extract parameter values from a stream of telemtry packets. 

The fact the decommutation model is JIT-compiled with LuaJIT makes it very fast: the decommutation library can process 50Mb of raw telemetry in less than 15 seconds on my MacBook Air. 

Content
-------

This repo consists of :

  for LuaJIT in `decom.lua` (with various included dependencies) and a set of scripts:

 - **ssm**: a Space System Model represnetation in Lua. Currently, it can parse an XTCE File, load it as a set of objects in memoty, and use that memory model to generate the lua source code that corresponds to the Telemtry-related part of the XTCE-File.
 - **decom**: a decommutation library with a pull-stye API. 
 - **xtceproc**: A command line interface to the decom library and the SSM Model. 

How stable is it?
-----------------

It's all very much a work in progress. i will break code and change APIs without warning. Do not use this software for anything serious (yet). I mean it. You've been warned.

Requirements
------------

The decommutation library requires LuaJIT 2.1+ as it relies on 64 bits bitop operations.

Documentation
-------------

There isn't any. But you can call the **xtceproc** script with '--help' to get the basic options. For the rest, just look at the code.

License
-------

MIT

Contributing
------------

All contributions are welcome, especially pull requests (as long as the code is compatible with the MIT license).


