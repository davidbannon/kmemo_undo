KMemo-Undo
===========



This is a small FPC/lazarus project that demonstrates a simple undo/redo for KMemo.  Its made specificially for my tomboy-ng application but just might be useful for other applications that use KMemo.



This not a patch or addition to KMemo, its a stand alone unit that sits in the same unit that contains a KMemo. I do not know if it would be better done as something intergrated into KMemo, what's here works for me.



To use the functionality, copy just the *tbundo.pas* to your project source, add it to your project, declare and create a TUndo_Redo. See the public methods as to where they need be hooked into you code that surrounds KMemo.



This code is released under BSD 3-Clause Clear License with the exception that the KControls Author, TK is free to use any part of it in any way he sees fit.



Suggestions and feedback welcome.



**See also** 



* KControls - https://github.com/kryslt/KControls

* https://spdx.org/licenses/BSD-3-Clause-Clear.html

* tomboy-ng - https://github.com/tomboy-notes/tomboy-ng




