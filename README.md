Potato_Tool
===========

Portable command line toolset written in Haskell for interacting with the Dreamcast Virtual Memory Unit filesystem. Named after the 8-bit Sanyo LC8670 "Potato" CPU contained in the VMU

Current Features
================
  - View files
  - Remove Files
  - Unlock 41 unused blocks on the VMU for a total of 241 blocks of storage
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
  - [GHC](http://www.haskell.org/haskellwiki/Haskell)
  - [Cabal](http://www.haskell.org/cabal/)

Installing
==========
 ```
 git clone https://github.com/RossMeikleham/Potato_Tool
 cd Potato_Tool
 cabal install
 ```
