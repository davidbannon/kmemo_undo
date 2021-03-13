unit tbundo;
{    Copyright (C) 2021 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------

}

{ A unit to provide undo / redo of the text kmemo, a component of KControls.

  It may be of some use in other projects that use KMemo.

  For information about tomboy-ng please see https://github.com/tomboy-notes/tomboy-ng

  For information about KControls please see https://github.com/kryslt/KControls

  How it works -

  We have a data structure (an array of records) that stores information about
  each change that is made, as its made. Stored is what was changed and where.
  The array has a fixed number of entries, as more changes, the earlist made
  will be overwritten.

  An Undo involves reversing the most recent change, a redo reverses that reversal.

  AvailChanges represents the number of changes we have in the data structure.
               It starts at zero and goes up to MaxChanges, once MaxChanges is
               reached, it stays there, overwriting oldest entries. It goes down
               if we undo, undo ... and then add a new change.

  AvailRedos   is usually zero, is only incremented during an Undo session. As
                soon as that session is terminated (by a new change) we zonk
                this var.

  NextChange    is an index that points to the next data location to store a
                change. It starts at zero and is incremented up to MaxChanges -1
                after that, rolls around to zero.
                Undo and Redo play with this var too.



  Actions

    AddChange   Stores data at location pointed to by NextChange, inc NextChange
                and AvailChanges (observing respective Max).
                If AvailRedos is not zero, zero it, end of a undo session.

    UnDo        We can only do this if AvailChanges is greater than zero.
                Sets Current to data in previous location. inc AvailRedos and
                dec AvailChanges and NextChange.

    ReDo        We can only do this if AvailRedos is greater than zero.
                Sets Current to data in NextChange location (we must have
                backed over it already). dec AvailRedos and inc AvailChanges and
                NextChange.

    Refinement ?
                If, in AddChange, we are just adding a single char and its prev
                StartSelIndex is only one less than this one, can we add that
                char to previous NewData ?  No, cannot tell after the first one.
                Maybe add a flag saying this is a single char at a time entry
                and count the char in there already ????
                ToDo : read above.

    In the Unit using KMemo :
        We must intercept every key press, cut and paste, delete, backspace key.

        Because by time we get to OnKeyPress, selected text has already been removed, we
        must record any selection in the OnKeyDown event. But the OnKeyDown event
        does not give us the plain text char typed by that key. So, we hook into all
        three, OnKeyDown, OnKeyPress, OnKeyUp (the latter just for delete and backspace).

        We capture the initial state in OnKeyDown, the actual key and final state in
        OnKeyPress (or onKeyUp for Delete and Backspace).

        Paste and Cut are more strait forward, we watch for a Ctrl-V or Ctrl-X and
        record any currently selected content and in the case of Ctrl-V the clipboard
        content. Easy.

        Capturing addition or removal of markup is more of an issue.

}


{$mode ObjFPC}{$H+}


{$DEFINE DEBUG_UNDO}                  // Warning, debug uses writeln, don't allow on Windows !

interface

uses
    Classes, SysUtils, KMemo, KControls;

type TChangeRec = record
    StartSelIndex : integer;        // Zero based index where activity starts
    ExistLen  : integer;
    NewLen    : integer;
    ExistData : string;             // The content that was initially there and deleted.
    NewData   : string;             // The content that was initially added.
end;

const  MaxChange = 5;               // ToDo : set this to, eg, 100, when tested OK

type
    TChangeStructure = array[0..MaxChange-1] of TChangeRec;


{ Undo_Redo }

{ TUndo_Redo }

TUndo_Redo = class
    private
        CurrentCR : TChangeRec;         // Only valid after a call to Undo or Redo
        AvailChanges : integer;         // Number of usable changes we have in the structure
        AvailReDos   : integer;         // Number of changes we have just undone, we can redo this many
        NextChange  : integer;          // An index to the next place to put a change, may, or may not be empty
        ChangeStructure : TChangeStructure;

        Overwritten : string;           // For split actions, eg, a keypress, stores (RTF) selected content
        OverwrittenLen : integer;       // Length of plain, displayed text matching Overwritten
//        procedure ApplyUndoRedo(Redo: boolean);
        procedure CopyChange(const SelStart, ELen, NLen : integer; const ExistData, NewData: string; var CRec: TChangeRec);
        procedure CopyChange(const FromCRec: TChangeRec; var ToCRec : TChangeRec);
                                        // Returns an RTF version of current selection, '' if nothing selected.
        function GetSelectedRTF(): string;
                                        // Adds a Change to man data structure, either data may be empty/0.
        procedure AddChange(const SelStart, ELen, NLen: integer; const ExistData, NewData: string);
        procedure AddChange(CR : TChangeRec);
                                        // Pushes the indicated content into KMemo1, may be an Undo or Redo, accepts
                                        // both plain text or RTF.
        procedure InsertIntoKMemo(loc: integer; St: string);


    Public
       TheKMemo : TKMemo;
                                        // Public : Hooked into the KMemo1KeyDown. It pre loads
                                        // Overwritten with selected content (or content near cursor
                                        // for Delete or Backspace.
        procedure ProcessKeyDown(const Key: word);
                                        // Public : Called before a paste happens, captures incoming
                                        // content and the existing selected content, all as RFT.
        procedure CatchUndoFromPaste(CutOnly: boolean=false);
                                        // Public : Called from KMemo1 onKeyPress event, assumes privare var,
                                        // Overwritten has been initialised with anything being overwritten.
        procedure AddKeyPress(SelStart : integer; Key : char);
                                        // Public : Called from KMemo1 onKeyUp event, handles delete and backspace
        procedure AddKeyUp(Key: Word; Shift: TShiftState);
        function CanUnDo() : boolean;
        function CanRedo() : boolean;
                                        // Public : Does Undo, rets True if another Undo is possible
        function UnDo : boolean;
                                        // Public : Does Redo, rets True if another Redo is possible
        function ReDo : boolean;
                                        // Public : For debug purposes only, don't leave for release.
        procedure Report();
        constructor Create(KM: TKMemo);
end;


implementation

uses LCLType;


// First, a pivate helper function.

                        // Returns Clipboard contents as either RTF or Text, '' if unavailable
function ClipboardContents(var Content : string; var TSize : integer) : boolean;
var
  AStream: TMemoryStream;
begin
    Result := true;
    AStream := TMemoryStream.Create;
    try
        // We use kcontrols tool, ClipBoardLoadStreamAs() here to ensure we get exactly the same result.
        if ClipBoardLoadStreamAs(cRichText, AStream, Content) and (AStream.Size > 0) then begin
                TSize := Content.Length;                     // Grab it before overwriting, thats bytes, not char, UTF8 issue ??
                AStream.Seek(0, soFromBeginning);
                setlength(Content, AStream.Size);
                AStream.ReadBuffer(Pointer(Content)^, AStream.Size);
        end else TSize := Content.Length;                         // even if above fails, we probably have text.
    finally
        AStream.Free;
    end;
end;

{ Undo_Redo }



procedure TUndo_Redo.ProcessKeyDown(const Key : word);
begin
    // We may arrive here under a number of conditions -
    // 1. A simple key press, 'normal' key, nothing selected
    // 2. A simple Delete, nothing selected, char UNDER cursor goes away.
    // 3. A simple Backspace, nothing selected, char to left of cursor goes away.
    // 4. Any one of the above, but with something selected.  What ever is selected
    //    goes away and is replaced with nothing or the key if its 1. above.
    Overwritten := GetSelectedRTF();
    OverwrittenLen := TheKMemo.RealSelLength;
    if OverwrittenLen = 0 then begin                 // OK, nothing selected then.
        // VK_Back nor VK_Delete will go on to trigger a KeyPress event, we call that from KeyUp event.
        if (Key = VK_Delete) and (TheKmemo.text.Length > TheKmemo.Blocks.RealSelStart) then begin    // Must be delete char under cursor
                TheKmemo.SelLength := 1;
                Overwritten := TheKmemo.Blocks.SelText;       // Note this is plain text, not RTF
                TheKmemo.SelLength := 0;
                OverwrittenLen := 1;
        end;
        if (Key = VK_Back) and (TheKMemo.RealSelStart > 0) then begin
                TheKmemo.SelStart := TheKmemo.RealSelStart - 1;
                TheKmemo.SelLength := 1;
                Overwritten := TheKmemo.Blocks.SelText;         // Note this is plain text, not RTF
                TheKmemo.SelStart := TheKmemo.RealSelStart + 1;
                TheKmemo.SelLength := 0;
                OverwrittenLen := 1;
        end;
    end;
end;

function TUndo_Redo.GetSelectedRTF() : string;
var
    AStream : TMemoryStream;
begin
    if TheKMemo.Blocks.RealSelLength > 0 then begin
        AStream := TMemoryStream.Create;
        try
            TheKMemo.SaveToRTFStream(AStream, True);
            if AStream.Size > 0 then begin
                AStream.Seek(0, soBeginning);
                SetLength(Result, AStream.Size);
                AStream.ReadBuffer(Pointer(Result)^, AStream.Size);
            end;
        finally
            AStream.Free;
        end;
    end;
end;

procedure TUndo_Redo.CatchUndoFromPaste(CutOnly : boolean = false);
var
  CR : TChangeRec;
begin
    CR.StartSelIndex := TheKmemo.blocks.RealSelStart;
    if CutOnly then begin
        CR.NewData := '';
        CR.NewLen := 0;
    end else
        ClipboardContents(CR.NewData, CR.NewLen); // wot, not checking return value ?
    CR.ExistLen := TheKMemo.SelLength;
    CR.ExistData := GetSelectedRTF();
    AddChange(CR);
end;

procedure TUndo_Redo.AddKeyPress(SelStart: integer; Key: char);
begin
    AddChange(SelStart-1, OverwrittenLen, 1, Overwritten, Key);         // -1 `cos its already happened
end;

procedure TUndo_Redo.AddKeyUp(Key: Word; Shift: TShiftState);
begin
    if Key = VK_Delete then begin               // Maybe delete char under cursor or a selected block
        if TheKmemo.text.Length > TheKmemo.Blocks.RealSelStart  then begin
            AddChange(TheKmemo.blocks.RealSelStart, OverwrittenLen, 0, Overwritten, '');
        end;
    end;
    if Key = VK_Back then begin
        if TheKMemo.RealSelStart >= 0 then
            AddChange(TheKmemo.blocks.RealSelStart, OverwrittenLen, 0, Overwritten, '');
    end;
end;


function TUndo_Redo.CanUnDo(): boolean;
begin
    Result := (AvailChanges > 0);
end;

function TUndo_Redo.CanRedo(): boolean;
begin
    Result :=  (AvailReDos > 0);
end;

procedure TUndo_Redo.AddChange(const SelStart, ELen, NLen : integer; const ExistData, NewData: string);
begin
    {$IFDEF DEBUG_UNDO}
    writeln('AddChange at ' + inttostr(SelStart)
        + ' replace [' + ExistData
        + '] (' + inttostr(ELen) + ') with [' + NewData + '] (' + inttostr(NLen) + ')');
    {$ENDIF}
    CopyChange(SelStart, ELen, NLen, ExistData, NewData, ChangeStructure[NextChange]);
    inc(NextChange);
    if NextChange = MaxChange then
        NextChange := 0;
    if AvailChanges < MaxChange then
        inc(AvailChanges);
    AvailReDos := 0;                        // Once we make a non undo/redo change, no more redos available
end;

procedure TUndo_Redo.AddChange(CR: TChangeRec);
begin
    AddChange(CR.StartSelIndex, CR.ExistLen, CR.NewLen, CR.ExistData, CR.NewData);
end;

// ---------  Do and Undo methods ------------



procedure TUndo_Redo.InsertIntoKMemo(loc : integer; St : string);
var
    AStream: TMemoryStream;
begin
    if copy(St, 1, 11) = '{\rtf1\ansi' then begin
        AStream := TMemoryStream.Create;
        try
            AStream.Write(St[1], St.length);
            AStream.Seek(0, soFromBeginning);
            TheKMemo.LoadFromRTFStream(AStream, Loc);
        finally
            AStream.Free;
        end;
    end else
        TheKMemo.ActiveBlocks.InsertPlainText(Loc, St);
end;

function TUndo_Redo.UnDo: boolean;
var
  Target : integer;
begin
    if CanUnDo() then begin
        Target := NextChange;
        if Target > 0 then dec(Target)
        else Target :=  MaxChange -1;
        CopyChange(ChangeStructure[Target], CurrentCR);
        inc(AvailReDos);
        dec(AvailChanges);
        if NextChange > 0 then dec(NextChange)
        else NextChange := MaxChange-1;
    end;
    result := (AvailChanges > 0);                    // can we call UnDo again ?
    with CurrentCR do begin
        {$IFDEF DEBUG_UNDO}
        writeln('Undo at ' + inttostr(StartSelIndex) + ' replace ['
                    + NewData + '] with [' + ExistData + ']');
        {$ENDIF}
        Thekmemo.Blocks.LockUpdate;
        if NewData <> '' then begin
            Thekmemo.SelStart := StartSelIndex{ - NewLen};
            Thekmemo.SelLength := NewLen;
            TheKmemo.Blocks.ClearSelection;
        end;
        // Insert Replace at Loc
        if ExistData <> '' then
            InsertIntoKMemo(StartSelIndex, ExistData);
    end;
    Thekmemo.Blocks.UnLockUpdate;
    Report();                               // ToDo : debug line
end;

function TUndo_Redo.ReDo: boolean;    // A redo uses the data currently pointed to by NextChange
begin
    if CanReDo() then begin
        CopyChange(ChangeStructure[NextChange], CurrentCR);
        dec(AvailReDos);                    // one less ReDos available
        inc(AvailChanges);                  // cos we can go back there is we choose.
        inc(NextChange);                    // Point to next one
        if NextChange = MaxChange then
            NextChange := 0;
    end;
    result := (AvailReDos > 0);             // we can call ReDo again if we wish.
    //ApplyUndoRedo(True);
    with CurrentCR do begin
        {$IFDEF DEBUG_UNDO}
        writeln('Redo at ' + inttostr(StartSelIndex) + ' replace ['
                    + ExistData + '] with [' + NewData + '] redo=');
        {$ENDIF}
        Thekmemo.Blocks.LockUpdate;
        if ExistData <> '' then begin
            Thekmemo.SelStart := StartSelIndex;
            Thekmemo.SelLength := ExistLen;
            TheKmemo.Blocks.ClearSelection;
        end;
        if NewData <> '' then
            InsertIntoKMemo(StartSelIndex, NewData);
        Thekmemo.Blocks.UnLockUpdate;
    end;
    {$IFDEF DEBUG_UNDO}
    Report();  {$ENDIF}
end;


// ------------- House Keeping ------------------

procedure TUndo_Redo.CopyChange(const SelStart, ELen, NLen: integer; const ExistData, NewData: string; var CRec: TChangeRec);
begin
    CRec.ExistData:= ExistData;
    CRec.NewData:= NewData;
    CRec.StartSelIndex:= SelStart;
    CRec.ExistLen := ELen;
    CRec.NewLen := NLen;
end;

procedure TUndo_Redo.CopyChange(const FromCRec: TChangeRec; var ToCRec: TChangeRec);
begin
    CopyChange(FromCRec.StartSelIndex, FromCRec.ExistLen, FromCRec.NewLen, FromCRec.ExistData, FromCRec.NewData, ToCRec);
end;

procedure TUndo_Redo.Report();
var
  I : integer = 0;
begin
    writeln('---------- Undo Report ---------');
    writeln('NextChange=' + inttostr(NextChange)
        + '  AvailChanges=' + inttostr(AvailChanges)
        + '  AvailReDos=' + inttostr(AvailReDos));
    for I := 0 to MaxChange -1 do                          // ToDo : this is unnecessary, remove after testing
        if ChangeStructure[i].StartSelIndex >= 0 then
            writeln('Slot:' + inttostr(I) + ' Index:'
                + inttostr(ChangeStructure[i].StartSelIndex)
                + ' [' + ChangeStructure[i].ExistData + '] - ['
                + ChangeStructure[i].NewData + ']');
    writeln('Current : ' + inttostr(CurrentCR.StartSelIndex) + ' [' + CurrentCR.ExistData + '] - [' + CurrentCR.NewData + ']');
    writeln('Content [' + TheKMemo.Text + ']');
    writeln('--------------------------------');
end;

constructor TUndo_Redo.Create(KM : TKMemo);
var
  I : integer;
begin
    TheKMemo := KM;
    for I := 0 to MaxChange-1 do                          // ToDo : maybe this is unnecessary, remove after testing
        ChangeStructure[i].StartSelIndex:= -1;
end;

end.

