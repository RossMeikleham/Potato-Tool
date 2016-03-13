Potato Tool
===========

[![Build Status](https://travis-ci.org/RossMeikleham/Potato-Tool.svg?branch=master)](https://travis-ci.org/RossMeikleham/Potato-Tool)

A Portable command line toolset written in Haskell for interacting with the Dreamcast Virtual Memory Unit filesystem. Named after the 8-bit Sanyo LC8670 "Potato" CPU contained in the VMU

The main reason I created this was to be able to rip/burn individual saves from/to a VMU flash image on my Linux machine to use in Dreamcast emulators such as [Reicast](http://reicast.com/). I wasn't able to find any other tools available that weren't Windows only.


Current Features
================
  - View files
  - Remove Files
  - Unlock 41 unused blocks on the VMU for a total of 241 blocks of storage (An increase from 100KB to 120.5KB of available user storage) 
  - Extract files in the nexus DCI file format
  - Inject nexus DCI Data files into the file system
  - Remove files

TODO
====
  - Support for injecting Game files, at the moment only supports Data files
  - VMI/VMU file support
  - Extraction/Injection options for graphics and other file header information 


Required
========
  - [GHC >= 7](http://www.haskell.org/haskellwiki/Haskell)
  - [Cabal](http://www.haskell.org/cabal/)
  - [Stack](http://docs.haskellstack.org/en/stable/README/)

Installing
==========
 ```
 git clone https://github.com/RossMeikleham/Potato_Tool
 cd Potato_Tool
 stack build
 stack install
 ```
