unit Kmemo_undo;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, kmemo;

type

    { TFormKMemoUndo }

    TFormKMemoUndo = class(TForm)
        ButtonBold: TButton;
        ButtonItalics: TButton;
        ButtonUndo: TButton;
        ButtonRedo: TButton;
        KMemo1: TKMemo;
        procedure ButtonBoldClick(Sender: TObject);
        procedure ButtonItalicsClick(Sender: TObject);
        procedure ButtonRedoClick(Sender: TObject);
        procedure ButtonUndoClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure KMemo1KeyDown(Sender: TObject; var Key: Word;
            Shift: TShiftState);
        procedure KMemo1KeyPress(Sender: TObject; var Key: char);
        procedure KMemo1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

    private
        procedure AlterBlockFont(const FirstBlockNo, BlockNo: longint;
            const Command: integer; const NewFontSize: integer=0);
        procedure AlterFont(const Command: integer; const NewFontSize: integer=0);

    public

    end;

var
    FormKMemoUndo: TFormKMemoUndo;

implementation

{$R *.lfm}


uses lcltype, tbundo;


var
    Undoer : TUndo_Redo;



{ ----------- Some basic stuff to make a KMemo editor work ----------------}

const
 ChangeSize   = 1;     // Used by AlterFont(..) and its friends.
 ChangeBold   = 2;
 ChangeItalic = 3;

{ The next 2 methods copied (and simplified) from tomboy-ng. }

procedure TFormKMemoUndo.AlterFont(const Command : integer; const NewFontSize : integer = 0);
var
	FirstBlockNo, LastBlockNo, IntIndex, LastChar, FirstChar : longint;
	SplitStart : boolean = false;
begin
    if KMemo1.ReadOnly then exit();
	LastChar := Kmemo1.RealSelEnd;			// SelEnd points to first non-selected char
    FirstChar := KMemo1.RealSelStart;
	FirstBlockNo := Kmemo1.Blocks.IndexToBlockIndex(FirstChar, IntIndex);
    if IntIndex <> 0 then			// Not Starting on block boundary.
		SplitStart := True;
    LastBlockNo := Kmemo1.Blocks.IndexToBlockIndex(LastChar, IntIndex);
    if IntIndex <> (length(Kmemo1.Blocks.Items[LastBlockNo].Text) -1) then 	// Not Last char in block
        LastBlockNo := KMemo1.SplitAt(LastChar) -1;       // we want whats before the split.
    while LastBlockNo > FirstBlockNo do begin
        AlterBlockFont(FirstBlockNo, LastBlockNo, Command, NewFontSize);
        dec(LastBlockNo);
    end;
    // Now, only First Block to deal with
    if SplitStart then
		FirstBlockNo := KMemo1.SplitAt(FirstChar);
    AlterBlockFont(FirstBlockNo, FirstBlockNo, Command, NewFontSize);
    KMemo1.SelEnd := LastChar;	// Any splitting above seems to subtly alter SelEnd, reset.
    KMemo1.SelStart := FirstChar;
end;


	{  Takes a Block number and applies changes to that block }
procedure TFormKMemoUndo.AlterBlockFont(const FirstBlockNo, BlockNo : longint; const Command : integer; const NewFontSize : integer = 0);
var
	Block, FirstBlock : TKMemoTextBlock;
begin
    FirstBlock := TKMemoTextBlock(KMemo1.Blocks.Items[FirstBlockNo]);
	Block := TKMemoTextBlock(KMemo1.Blocks.Items[BlockNo]);
    case Command of
		ChangeBold :
                    if fsBold in FirstBlock.TextStyle.Font.style then
					     Block.TextStyle.Font.Style := Block.TextStyle.Font.Style - [fsBold]
					else Block.TextStyle.Font.Style := Block.TextStyle.Font.Style + [fsBold];
		ChangeItalic :
					if fsItalic in FirstBlock.TextStyle.Font.style then
						 Block.TextStyle.Font.Style := Block.TextStyle.Font.Style - [fsItalic]
					else Block.TextStyle.Font.Style := Block.TextStyle.Font.Style + [fsItalic];
	end;
end;

procedure TFormKMemoUndo.ButtonBoldClick(Sender: TObject);
begin
    AlterFont(ChangeBold);
end;

procedure TFormKMemoUndo.ButtonItalicsClick(Sender: TObject);
begin
    AlterFont(ChangeItalic);
end;


// --------------  Methods relating to Undo / Redo -----------------

procedure TFormKMemoUndo.FormCreate(Sender: TObject);
begin
    undoer := TUndo_Redo.create(Kmemo1);
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
    Undoer.ProcessKeyDown(Key);
    if (ssCtrl in Shift) then
        case key of
            VK_V : Undoer.CatchUndoFromPaste();         // Paste
            VK_X : Undoer.CatchUndoFromPaste(True);     // Cut
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

procedure TFormKMemoUndo.ButtonUndoClick(Sender: TObject);
begin
    ButtonUndo.enabled := Undoer.UnDo;
    ButtonRedo.Enabled := Undoer.CanRedo();
end;



end.

