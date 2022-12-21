#!/bin/bash
ca65 -o tmp.o $1 && (ld65 -C nes.cfg -o main.nes tmp.o; rm tmp.o)
