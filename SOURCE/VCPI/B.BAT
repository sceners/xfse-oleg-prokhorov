@echo off
nasm -fobj xfse.asm
alink -m xfse -oEXE
del xfse.obj
del xfse.map
