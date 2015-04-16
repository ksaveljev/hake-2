#!/bin/bash

mv src/Sys/Socket.hs src/Sys/Socket.hs.tmp
mv src/Sys/Socket.hs.mock src/Sys/Socket.hs
mv src/Sys/Socket.hs.tmp src/Sys/Socket.hs.mock
