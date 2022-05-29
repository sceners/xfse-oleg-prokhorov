# xFSE by Oleg Prokhorov.

#### Written in 2000.

For MS-DOS and Windows 95.

[Original package](https://defacto2.net/f/ae2b6b4)

```
---------------------------------
           xFSE by Oleg Prokhorov
'Final Fantasy Security Envelope'
                      0.55 - 0.76
          remover for Win'95/VCPI
      (full source code included)
---------------------------------
```

---

## xFSE by Oleg Prokhorov
#### 'Final Fantasy Security Envelope'
#### remover for Win'95/VCPI
#### 10-03-2000

PROLOGUE
--------------
The dark days have come...
The last fortress called 'Final Fantasy Security Envelope' is destroyed. 
Just ruins are left. Lamers are raping your progies by changing copyrights 
& names. Nothing can stop them. Nowhere to hide and escape. Nothing 
can save your code. PRAY FOR SALVATION!

The author is glad to introduce you 'xFSE', which is able to remove  
annoying FSE from your files. I.e. xFSEd files will run properly in RM on 
486, will not occasionally hang under Windows, you'll be able to upgrade 
such them easily by changing version number with HIEW and so on. 
BTW, have you noticed that most modern  unprotectors are written by 
Russians (DTG members, VAG)? Where do you think I'm from? 
FSE was the last (only) serious stuff of its kind, which didn't have an 
automatic remover. But still this wasn't the reason for releasing 'xFSE'. 
I just want to show you the OTHER WAY of removing protectors. 

I call it BPF (Breakpoint at fault). 

The idea is simple: in V86 CPU mode several instruction cause (General 
protection) faults, giving ring-0 code decide what to do. Among them are 
'cli','pushf(d)','popf(d)', 'iret(d)', 'mov drx, reg32' and others. Most 
protectors do have such opcodes in its code. Often their authors put such 
opcodes after all their cool anti-debugging tricks and don't care about 
morphing them a bit. 
I.e. FSE always puts
'xor eax, eax'
'mov dr7,eax'
'mov dr6,eax'
'mov dr0,eax'
'mov dr1,eax'...
just before the entry point.

The problem is how to dig in to ring-0 and catch GPFs. Here's the way.

Win'9x
--------------
Windows provides (leaves) the official way of getting into ring-0 via VXD.

VCPI
--------------
Yes, old DOS + EMM386. The idea was inspired by 386swat debugger, 
but still mostly original. You know EMM386 (and other VCPI-servers 
may be too) has a piece of its PM code in 1st MB. It's 16-bit 0ffffh bytes 
sized selector. Since it's real size is quite less, you could see that some 
memory after (EMM386 1st MB code part) is accessible from PM via this 
selector. It means if you put your code there it can be executed as a ring-0 
code. We just need to change EMM386 GPF handler. I do it through 
looking for EMM386 IDT (enter PM via VCPI) and installing own GPF 
handler, since EMM386 is too lazy to care about its IDT (go to VM).
The only difficulty is that the desired region of memory can be occupied by 
other programs. Nothing to do with it. REMOVE TSRs!

xFSE 
--------------
Removes FSE 0.55 - 0.76.
Requires Win'9x or VCPI via EMM386 (other VCPI servers may suit).
Checked with Win'95 (4.00.950 B) and EMM386 (4.49 & 4.95)

Usage:
xfse <filename>

It is recommended to use VCPI version for all files except for those which 
were processed with FSE 0.73. (This version hangs in VCPI).
Windows version may be useful for those ones when FSE >=0.73. Note 
that previous FSE versions don't grant stable working under Windows. 
Don't complain about bugs they probably are (FSE + Windows) ones. 
Mind that despite the fact that Windows may hang the output file will be 
written.

Advantages:
Removes FSE 0.55 - 0.76.
Full source code included. 

Disadvantages:
It still lacks FSE detection. There's no detection of ANY KIND! To 
check if your EXE was processed with FSE find TYP by Veit 
Kannegieser (http://www-user.TU-Cottbus.DE/~kannegv). A nice 
program which gives a lot of info about files and is not limited to the 
executables. It has CPU emulation to bypass protectors polymorphism.   
Output file will be 256 bytes aligned. I don't see an easy way to determine 
original file size for all FSE versions. The more so I'm not sure whether it 
is possible.
If  xFSE for VCPI warns you about VCPI errors than remove your TSRs 
and try again!

FSE MESSAGE
--------------
After digging in FSE I found that Zenix is rather a sentimental person. Why 
don't you marry your girlfriend? 31 is about the good age for it.
FSE anti-lamer code is extremely good. Rather soon I had given up the 
idea of tracing it all through, though I didn't even need  that (new DG 0.05 
is able to run FSEd files!).
But after all I found that FSE stores unprotected EXE in its original form 
(with header and  relocations)! Quite a disappointing fact after its nice 
anti-debugging code!

CREDITS:
--------------
Main idea and code:
Oleg Prokhorov - olegpro@mail.ru
Final Fantasy Security Envelope:
Zenix Yang  - zenix@ms10.hinet.net

Win'95 version:
Elicz - elicz@email.cz. EDK source 
(ftp://ftp.elf.stuba.sk/pub/pc/util/prog/9xedk.zip).
Tran - Guess which part of code belongs to Tran...

VCPI version:
Daniel Horchner - dbjh@gmx.net. RAW32 extender source 
(ftp://ftp.elf.stuba.sk/pub/pc/util/prog/raw32r3.zip)
ALI & MASTER - Deglucker original source.

GREETS:
--------------
Snoopy - Hi, fella! 
Jibz - Impatiently waiting for new APACK & APLIB.
Vladimir Gneushev - No news from you. Have you got my 3 last e-mails?
Michael Hering - Thanks for the FileInfo!
Eddy Hawk - Do you accept comments in your Protectors Info?
