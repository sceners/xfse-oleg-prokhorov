@echo off
nasm -fobj xfse.asm
alink -m xfse -oEXE
nasm -fobj xfse95.ASM
wlink file xfse95.OBJ format os2 le op stub = xfse.exe export XFSE_DDB
patch
del xfse.exe
del xfse.obj
del xfse95.obj
del xfse.map
