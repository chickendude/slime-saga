#!/bin/bash
title=slimesaga
cd src
rgbasm -Werror -Weverything -Hl -o $title.o $title.asm
cd ..

# Link everything
mkdir -p build
rgblink --dmg --tiny --map build/$title.map --sym build/$title.sym -o build/$title.gb src/$title.o

# Fix headers
rgbfix --title game --pad-value 0 --validate build/$title.gb
rm src/*.o
