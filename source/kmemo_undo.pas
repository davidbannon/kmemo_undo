unit Kmemo_undo;

{   Copyright (C) 2021 David Bannon
    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html
    ------------------

    A Lazarus project to demonstrate use of the TBUndo unit, providing a Text Only
    undo/redo facility to KMemo.

    Intended for use in Tomboy, it may well be useful in other Lazarus applications
    that use KMemo, a component of KControls.

    See also
    tomboy-ng - https://github.com/tomboy-notes/tomboy-ng
    KControls - https://github.com/kryslt/KControls

}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, kmemo, TBundo;

type TChangeMarkup=(                // Changes made to the appearence of Text
        cmNone,                     // That is, its a text content change, not appearance
        cmBOLD,                     // These changes are a toggle, reverses what
        cmItalic,                   // ever the first character is and applies to
        cmHighlight,                // all the selection.
        cmUnderline,
        cmStrikout,
        cmSizeSmall,               // Size changes set a specific size.
        cmSizeNormal,
        cmSizeLarge,
        cmSizeHuge);

type

    { TFormKMemoUndo }

    TFormKMemoUndo = class(TForm)
        ButtonReport: TButton;
        ButtonBold: TButton;
        ButtonItalics: TButton;
        ButtonUndo: TButton;
        ButtonRedo: TButton;
        KMemo1: TKMemo;
        procedure ButtonBoldClick(Sender: TObject);
        procedure ButtonItalicsClick(Sender: TObject);
        procedure ButtonRedoClick(Sender: TObject);
        procedure ButtonReportClick(Sender: TObject);
        procedure ButtonUndoClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure KMemo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure KMemo1KeyPress(Sender: TObject; var Key: char);
        procedure KMemo1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

    private
        procedure AlterBlockFont(const FirstBlockNo, BlockNo: longint;
            const Command: TChangeMarkUp; const NewFontSize: integer=0);
        procedure AlterFont(const Command: TChangeMarkUp);

    public

    end;

var
    FormKMemoUndo: TFormKMemoUndo;

implementation

{$R *.lfm}


uses lcltype;


var
    Undoer : TUndo_Redo;





(*             We now use TChangeMarkup rather than this.
const
 ChangeSize   = 1;     // Used by AlterFont(..) and its friends.
 ChangeBold   = 2;
 ChangeItalic = 3;
*)

{ The next few methods copied (and simplified) from tomboy-ng so we can play with MarkUp }

procedure TFormKMemoUndo.AlterFont(const Command : TChangeMarkUp);
var
	FirstBlockNo, LastBlockNo, IntIndex, LastChar, FirstChar : longint;
	SplitStart : boolean = false;
begin
    if KMemo1.ReadOnly then exit();
    Undoer.RecordInitial(0);
	LastChar := Kmemo1.RealSelEnd;			// SelEnd points to first non-selected char
    FirstChar := KMemo1.RealSelStart;
	FirstBlockNo := Kmemo1.Blocks.IndexToBlockIndex(FirstChar, IntIndex);
    if IntIndex <> 0 then			// Not Starting on block boundary.
		SplitStart := True;
    LastBlockNo := Kmemo1.Blocks.IndexToBlockIndex(LastChar, IntIndex);
    if IntIndex <> (length(Kmemo1.Blocks.Items[LastBlockNo].Text) -1) then 	// Not Last char in block
        LastBlockNo := KMemo1.SplitAt(LastChar) -1;       // we want whats before the split.
    while LastBlockNo > FirstBlockNo do begin
        AlterBlockFont(FirstBlockNo, LastBlockNo, Command, 0);           // !!!!!!!!!!!!! '0' ?
        dec(LastBlockNo);
    end;
    // Now, only First Block to deal with
    if SplitStart then
		FirstBlockNo := KMemo1.SplitAt(FirstChar);
    AlterBlockFont(FirstBlockNo, FirstBlockNo, Command, 0);           // !!!!!!!!!!!!! '0' ?
    KMemo1.SelEnd := LastChar;	// Any splitting above seems to subtly alter SelEnd, reset.
    KMemo1.SelStart := FirstChar;
    Undoer.AddMarkup({Command});
end;


	{  Takes a Block number and applies changes to that block }
procedure TFormKMemoUndo.AlterBlockFont(const FirstBlockNo, BlockNo : longint; const Command : TChangeMarkUp; const NewFontSize : integer = 0);
var
	Block, FirstBlock : TKMemoTextBlock;
begin
    FirstBlock := TKMemoTextBlock(KMemo1.Blocks.Items[FirstBlockNo]);
	Block := TKMemoTextBlock(KMemo1.Blocks.Items[BlockNo]);
    case Command of
		cmBold :
                    if fsBold in FirstBlock.TextStyle.Font.style then
					     Block.TextStyle.Font.Style := Block.TextStyle.Font.Style - [fsBold]
					else Block.TextStyle.Font.Style := Block.TextStyle.Font.Style + [fsBold];
		cmItalic :
					if fsItalic in FirstBlock.TextStyle.Font.style then
						 Block.TextStyle.Font.Style := Block.TextStyle.Font.Style - [fsItalic]
					else Block.TextStyle.Font.Style := Block.TextStyle.Font.Style + [fsItalic];
	end;
end;

procedure TFormKMemoUndo.ButtonBoldClick(Sender: TObject);
begin
    AlterFont(cmBold);
end;

procedure TFormKMemoUndo.ButtonItalicsClick(Sender: TObject);
begin
    AlterFont(cmItalic);
end;


// --------------  Methods relating to Undo / Redo -----------------

procedure TFormKMemoUndo.FormCreate(Sender: TObject);
begin
    Undoer := TUndo_Redo.create(Kmemo1);
    //Undoer.AlterFontProcedure := @AlterFont;         // Required to allow Undoer to use local AlterFont()
    ButtonRedo.Enabled := Undoer.CanReDo;
    ButtonUndo.enabled := Undoer.CanUnDo;
end;

procedure TFormKMemoUndo.FormDestroy(Sender: TObject);
begin
    undoer.free;
end;

procedure TFormKMemoUndo.KMemo1KeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    Undoer.RecordInitial(Key);
    if (ssCtrl in Shift) then
        case key of
            VK_V : Undoer.AddPasteOrCut();         // Paste
            VK_X : Undoer.AddPasteOrCut(True);     // Cut
        end;
     // Now let them go through to the keeper
end;

procedure TFormKMemoUndo.KMemo1KeyPress(Sender: TObject; var Key: char);
begin
    Undoer.AddKeyPress(Kmemo1.blocks.RealSelStart, Key);
end;

procedure TFormKMemoUndo.KMemo1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    Undoer.AddKeyUp(Key, Shift);                        // deals with Delete and Backspace
    ButtonUndo.enabled := Undoer.CanUnDo();
    ButtonRedo.Enabled := Undoer.CanRedo();
end;

procedure TFormKMemoUndo.ButtonRedoClick(Sender: TObject);
begin
    ButtonRedo.Enabled := Undoer.ReDo;
    ButtonUndo.enabled := Undoer.CanUnDo();
end;

procedure TFormKMemoUndo.ButtonReportClick(Sender: TObject);
begin
    Undoer.report;
end;

procedure TFormKMemoUndo.ButtonUndoClick(Sender: TObject);
begin
    ButtonUndo.enabled := Undoer.UnDo;
    ButtonRedo.Enabled := Undoer.CanRedo();
end;



end.

