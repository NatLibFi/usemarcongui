{
 /***************************************************************************
                            ruleeditorunit.pas
                            ------------------

 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}

unit RuleEditorUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, SynEdit, Buttons, SynEditKeyCmds, LCLType, StdCtrls,
  SynHighlighterRule, LMessages;

type

  { TFormRuleEditor }

  TFormRuleEditor = class(TForm)
    ARuleMenu: TMainMenu;
    Bevel1: TBevel;
    BitBtnSaveRun: TBitBtn;
    LabelSyntax: TLabel;
    MenuItem1: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItemConvertTemp: TMenuItem;
    MenuItemFindPrevious: TMenuItem;
    MenuItemFindNext: TMenuItem;
    MenuItemReplace: TMenuItem;
    MenuItemFind: TMenuItem;
    MenuItemAddPreviousSub: TMenuItem;
    MenuItemAddNextSub: TMenuItem;
    MenuItemAddRegReplace: TMenuItem;
    MenuItemAddRegMatch: TMenuItem;
    MenuItemAddRegFind: TMenuItem;
    MenuItemAddFrom: TMenuItem;
    MenuItemAddTo: TMenuItem;
    MenuItemAddIfThenElse: TMenuItem;
    MenuItemAddIfThen: TMenuItem;
    MenuItemAddBetween: TMenuItem;
    MenuItemAddTable: TMenuItem;
    MenuItemAddMem: TMenuItem;
    MenuItemAddSto: TMenuItem;
    MenuItemAddDelete: TMenuItem;
    MenuItemAddReplaceOcc: TMenuItem;
    MenuItemAddReplace: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemRedo: TMenuItem;
    MenuItemUndo: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemAdd: TMenuItem;
    MenuItemClose: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemFile: TMenuItem;
    BitBtnClose: TBitBtn;
    BitBtnSave: TBitBtn;
    BitBtnSaveAndClose: TBitBtn;
    PanelEditor: TPanel;
    PanelTop: TPanel;
    StatusBar1: TStatusBar;
    Editor: TSynEdit;
    procedure BitBtnCloseClick(Sender: TObject);
    procedure BitBtnSaveAndCloseClick(Sender: TObject);
    procedure BitBtnSaveClick(Sender: TObject);
    procedure BitBtnSaveRunClick(Sender: TObject);
    procedure EditorClick(Sender: TObject);
    procedure EditorCommandProcessed(Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure EditorDblClick(Sender: TObject);
    procedure EditorEnter(Sender: TObject);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorReplaceText(Sender: TObject; const ASearch,
      AReplace: string; Line, Column: integer;
      var ReplaceAction: TSynReplaceAction);
    procedure FormRuleEditorCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormRuleEditorCreate(Sender: TObject);
    procedure FormRuleEditorDestroy(Sender: TObject);
    procedure FormRuleEditorKeyPress(Sender: TObject; var Key: char);
    procedure FormRuleEditorShortCut(var Msg: TLMKey; var Handled: Boolean);
    procedure FormRuleEditorShow(Sender: TObject);
    procedure MenuItemCopyClick(Sender: TObject);
    procedure MenuItemPasteClick(Sender: TObject);
    procedure MenuItemAddBetweenClick(Sender: TObject);
    procedure MenuItemAddDeleteClick(Sender: TObject);
    procedure MenuItemAddFromClick(Sender: TObject);
    procedure MenuItemAddIfThenClick(Sender: TObject);
    procedure MenuItemAddIfThenElseClick(Sender: TObject);
    procedure MenuItemAddMemClick(Sender: TObject);
    procedure MenuItemAddNextSubClick(Sender: TObject);
    procedure MenuItemAddPreviousSubClick(Sender: TObject);
    procedure MenuItemAddRegFindClick(Sender: TObject);
    procedure MenuItemAddRegMatchClick(Sender: TObject);
    procedure MenuItemAddRegReplaceClick(Sender: TObject);
    procedure MenuItemAddReplaceClick(Sender: TObject);
    procedure MenuItemAddReplaceOccClick(Sender: TObject);
    procedure MenuItemAddStoClick(Sender: TObject);
    procedure MenuItemAddTableClick(Sender: TObject);
    procedure MenuItemAddToClick(Sender: TObject);
    procedure MenuItemCloseClick(Sender: TObject);
    procedure MenuItemConvertTempClick(Sender: TObject);
    procedure MenuItemCutClick(Sender: TObject);
    procedure MenuItemEditClick(Sender: TObject);
    procedure MenuItemFindClick(Sender: TObject);
    procedure MenuItemFindNextClick(Sender: TObject);
    procedure MenuItemFindPreviousClick(Sender: TObject);
    procedure MenuItemRedoClick(Sender: TObject);
    procedure MenuItemReplaceClick(Sender: TObject);
    procedure MenuItemSaveClick(Sender: TObject);
    procedure EditorChange(Sender: TObject);
    procedure MenuItemUndoClick(Sender: TObject);
  private
    FName: string;
    FFileName: string;
    FInitialLine: Integer;
    FChanges: Boolean;
    FHighlighter: TSynRuleSyn;
    FShiftState: TShiftState;

    FLoadedFileName: string;
    FLoadedFileTimestamp: TDateTime;

    procedure UpdateStatus;
    function SaveFile: Boolean;
    function LoadFile: Boolean;
    procedure SetChanges(const AValue: Boolean);
    procedure AddCommand(Cmd: string; CaretPos: Integer);
    procedure GetDialogPosition(AWidth, AHeight: integer; out ALeft, ATop: integer);
    procedure LoadFileAsync(Data: PtrInt);

    property Changes: Boolean read FChanges write SetChanges;
  public
    function Execute(FileName: string; DialogName: string = ''; InitialLine: Integer = 0): Integer;
  end;

var
  FormRuleEditor: TFormRuleEditor;

implementation

uses
  SynEditTypes,
  MainUnit, SearchReplaceUnit, ConvertSelectionToTempUnit;

{ TFormRuleEditor }

procedure TFormRuleEditor.FormRuleEditorCreate(Sender: TObject);
var
  WinState: TWindowState;
begin
  FHighlighter := TSynRuleSyn.Create(Self);
  Editor.Highlighter := FHighlighter;

  WinState := TWindowState(MainForm.Config.GetValue('RuleEditorState', LongInt(Self.WindowState)));
  if WinState = wsMinimized then WinState := wsNormal;
  Self.WindowState := WinState;
  if WinState = wsNormal then begin
    Self.Left := MainForm.Config.GetValue('RuleEditorLeft', Self.Left);
    Self.Top := MainForm.Config.GetValue('RuleEditorTop', Self.Top);
    Self.Width := MainForm.Config.GetValue('RuleEditorWidth', Self.Width);
    Self.Height := MainForm.Config.GetValue('RuleEditorHeight', Self.Height);
  end;

  FLoadedFileName := '';
  FLoadedFileTimeStamp := 0;
end;

procedure TFormRuleEditor.FormRuleEditorDestroy(Sender: TObject);
begin
  Editor.Highlighter := nil;
  FreeAndNil(FHighlighter);

  if Self.WindowState = wsNormal then begin
    MainForm.Config.SetValue('RuleEditorLeft', Self.Left);
    MainForm.Config.SetValue('RuleEditorTop', Self.Top);
    MainForm.Config.SetValue('RuleEditorWidth', Self.Width);
    MainForm.Config.SetValue('RuleEditorHeight', Self.Height);
  end;
  MainForm.Config.SetValue('RuleEditorState', LongInt(Self.WindowState));
end;

procedure TFormRuleEditor.FormRuleEditorKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #9) or ((FShiftState = [ssAlt]) and (Key in ['a'..'z','A'..'Z'])) then
    Key := #0;
end;

procedure TFormRuleEditor.FormRuleEditorShortCut(var Msg: TLMKey;
  var Handled: Boolean);
var
  ShiftState: TShiftState;
begin
  ShiftState := KeyDataToShiftState(Msg.KeyData);

  Handled := (ShiftState = [ssAlt]) and (Char(Msg.CharCode) in ['a'..'z', 'A'..'Z']);
end;

procedure TFormRuleEditor.FormRuleEditorShow(Sender: TObject);
begin
  if Editor.CanFocus then
    Editor.SetFocus;
end;

procedure TFormRuleEditor.LoadFileAsync(Data: PtrInt);
begin
  if not LoadFile then
    ModalResult := mrCancel;
end;

procedure TFormRuleEditor.MenuItemCopyClick(Sender: TObject);
begin
  Editor.CopyToClipboard;
end;

procedure TFormRuleEditor.MenuItemPasteClick(Sender: TObject);
begin
  Editor.PasteFromClipboard;
end;

procedure TFormRuleEditor.MenuItemAddBetweenClick(Sender: TObject);
begin
  AddCommand('Between(,)', 9);
end;

procedure TFormRuleEditor.MenuItemAddDeleteClick(Sender: TObject);
begin
  AddCommand('Delete('''')', 9);
end;

procedure TFormRuleEditor.MenuItemAddFromClick(Sender: TObject);
begin
  AddCommand('From()', 6);
end;

procedure TFormRuleEditor.MenuItemAddIfThenClick(Sender: TObject);
begin
  AddCommand('If () Then ', 5);
end;

procedure TFormRuleEditor.MenuItemAddIfThenElseClick(Sender: TObject);
begin
  AddCommand('If () Then Else', 5);
end;

procedure TFormRuleEditor.MenuItemAddMemClick(Sender: TObject);
begin
  AddCommand('Mem()', 5);
end;

procedure TFormRuleEditor.MenuItemAddNextSubClick(Sender: TObject);
begin
  AddCommand('NextSub(, ''>=1'')', 9);
end;

procedure TFormRuleEditor.MenuItemAddPreviousSubClick(Sender: TObject);
begin
  AddCommand('PreviousSub(, ''>=1'')', 13);
end;

procedure TFormRuleEditor.MenuItemAddRegFindClick(Sender: TObject);
begin
  AddCommand('RegFind('''')', 10);
end;

procedure TFormRuleEditor.MenuItemAddRegMatchClick(Sender: TObject);
begin
  AddCommand('RegMatch()', 10);
end;

procedure TFormRuleEditor.MenuItemAddRegReplaceClick(Sender: TObject);
begin
  AddCommand('RegReplace('''', '''')', 13);
end;

procedure TFormRuleEditor.MenuItemAddReplaceClick(Sender: TObject);
begin
  AddCommand('Replace('''' By '''')', 10);
end;

procedure TFormRuleEditor.MenuItemAddReplaceOccClick(Sender: TObject);
begin
  AddCommand('ReplaceOcc('''' By '''',''>1'')', 13);
end;

procedure TFormRuleEditor.MenuItemAddStoClick(Sender: TObject);
begin
  AddCommand('Sto()', 5);
end;

procedure TFormRuleEditor.MenuItemAddTableClick(Sender: TObject);
begin
  AddCommand('Table('''')', 8);
end;

procedure TFormRuleEditor.MenuItemAddToClick(Sender: TObject);
begin
  AddCommand('To()', 4);
end;

procedure TFormRuleEditor.BitBtnSaveAndCloseClick(Sender: TObject);
begin
  SaveFile;
  ModalResult := mrOk;
end;

procedure TFormRuleEditor.BitBtnCloseClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormRuleEditor.BitBtnSaveClick(Sender: TObject);
begin
  SaveFile;
end;

procedure TFormRuleEditor.BitBtnSaveRunClick(Sender: TObject);
begin
  SaveFile;
  ModalResult := mrRetry;
end;

procedure TFormRuleEditor.EditorClick(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TFormRuleEditor.EditorCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
begin
  UpdateStatus;
end;

procedure TFormRuleEditor.EditorDblClick(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TFormRuleEditor.EditorEnter(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TFormRuleEditor.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = Ord('G')) and (Shift = [ssCtrl]) then
    MenuItemFindNext.Click
  else if (Key = Ord('G')) and (Shift = [ssCtrl, ssShift]) then
    MenuItemFindPrevious.Click;
end;

procedure TFormRuleEditor.EditorReplaceText(Sender: TObject; const ASearch,
  AReplace: string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
var
  Res, X, Y: integer;
  Prompt: string;
begin
  Prompt := 'Replace"' + ASearch + '"' + #13 + 'with "' + AReplace + '"?';

  GetDialogPosition(300,150,X,Y);
  Res := MessageDlgPos(Prompt, mtConfirmation,
    [mbYes, mbYesToAll, mbNo, mbCancel], 0, X, Y);

  case Res of
    mrYes: ReplaceAction := raReplace;
    mrNo : ReplaceAction := raSkip;
    mrAll, mrYesToAll: ReplaceAction := raReplaceAll;
  else
    ReplaceAction := raCancel;
  end;
end;

procedure TFormRuleEditor.GetDialogPosition(AWidth, AHeight: Integer;
  out ALeft, ATop: Integer);
var
  P: TPoint;
begin
  with Editor do
    P := ClientToScreen(Point(CaretXPix, CaretYPix));
  ALeft := Editor.ClientOrigin.X + (Editor.Width - AWidth) div 2;
  ATop := P.Y - AHeight - 3 * Editor.LineHeight;
  if ATop < 10 then
    ATop := P.y + 2 * Editor.LineHeight;
  if ATop + AHeight > Screen.Height then
    ATop := (Screen.Height - AHeight) div 2;
  if ATop < 0 then ATop := 0;
end;

procedure TFormRuleEditor.FormRuleEditorCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  CanClose := False;
  if Changes then begin
    case MessageDlg(FName, 'Save changes?', mtConfirmation, mbYesNoCancel, 0) of
      mrYes: SaveFile;
      mrNo: ;
      else Exit;
    end;
  end;
  CanClose := True;
end;

procedure TFormRuleEditor.MenuItemCloseClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormRuleEditor.MenuItemConvertTempClick(Sender: TObject);
var
  RuleStr: string;
begin
  RuleStr := Editor.SelText;
  if FormConvertSelectionToTemp.Execute(RuleStr) = mrOk then
    Editor.SelText := RuleStr;
end;

procedure TFormRuleEditor.MenuItemCutClick(Sender: TObject);
begin
  Editor.CutToClipboard;
end;

procedure TFormRuleEditor.MenuItemEditClick(Sender: TObject);
begin
  MenuItemUndo.Enabled := Editor.CanUndo;
  MenuItemRedo.Enabled := Editor.CanRedo;
end;

procedure TFormRuleEditor.MenuItemFindClick(Sender: TObject);
var
  OldCaretXY: TPoint;
begin
  if FormSearchReplace.Execute(False) <> mrOk then
    Exit;

  OldCaretXY := Editor.CaretXY;
  Editor.SearchReplace(FormSearchReplace.FindText, FormSearchReplace.ReplaceText,
    FormSearchReplace.Options);
  if (OldCaretXY.X = Editor.CaretXY.X) and (OldCaretXY.Y = Editor.CaretXY.Y) then
    ShowMessage('Search string ''' + FormSearchReplace.FindText + ''' not found');
end;

procedure TFormRuleEditor.MenuItemFindNextClick(Sender: TObject);
var
  Opts: TSynSearchOptions;
  OldCaretXY: TPoint;
begin
  Opts := FormSearchReplace.Options - [ssoEntireScope, ssoReplaceAll];
  Editor.SelStart := Editor.SelEnd;
  OldCaretXY := Editor.CaretXY;
  Editor.SearchReplace(FormSearchReplace.FindText, FormSearchReplace.ReplaceText,
    Opts);
  if (OldCaretXY.X = Editor.CaretXY.X) and (OldCaretXY.Y = Editor.CaretXY.Y) then
    ShowMessage('Search string ''' + FormSearchReplace.FindText + ''' not found');
end;

procedure TFormRuleEditor.MenuItemFindPreviousClick(Sender: TObject);
var
  Opts: TSynSearchOptions;
  OldCaretXY: TPoint;
begin
  Opts := FormSearchReplace.Options - [ssoEntireScope, ssoReplaceAll];
  if ssoBackwards in Opts then begin
    Exclude(Opts, ssoBackwards);
    Editor.SelStart := Editor.SelEnd;
  end
  else begin
    Include(Opts, ssoBackwards);
    Editor.SelStart := Editor.SelStart;
    Editor.SelEnd := Editor.SelStart;
  end;
  OldCaretXY := Editor.CaretXY;
  
  if Editor.SelAvail then
    Editor.LogicalCaretXY := Editor.BlockBegin;

  Editor.SearchReplace(FormSearchReplace.FindText, FormSearchReplace.ReplaceText,
    Opts);
  if (OldCaretXY.X = Editor.CaretXY.X) and (OldCaretXY.Y = Editor.CaretXY.Y) then
    ShowMessage('Search string ''' + FormSearchReplace.FindText + ''' not found');
end;

procedure TFormRuleEditor.MenuItemRedoClick(Sender: TObject);
begin
  Editor.Redo;
end;

procedure TFormRuleEditor.MenuItemReplaceClick(Sender: TObject);
var
  Opts: TSynSearchOptions;
begin
  case FormSearchReplace.Execute(True) of
    mrOk: begin
      Opts := FormSearchReplace.Options;
    end;
    mrAll: begin
      Opts := FormSearchReplace.Options;
      Include(Opts, ssoReplaceAll);
    end;
    else
      Exit;
  end;

  Editor.SearchReplace(FormSearchReplace.FindText, FormSearchReplace.ReplaceText,
    Opts);
end;

procedure TFormRuleEditor.MenuItemSaveClick(Sender: TObject);
begin
  SaveFile;
end;

procedure TFormRuleEditor.EditorChange(Sender: TObject);
begin
  Changes := True;
end;

procedure TFormRuleEditor.MenuItemUndoClick(Sender: TObject);
begin
  Editor.Undo;
end;

procedure TFormRuleEditor.SetChanges(const AValue: Boolean);
begin
  FChanges := AValue;
  if FChanges then begin
    StatusBar1.Panels[1].Text := 'Modified';
    FLoadedFileName := '';
    FLoadedFileTimeStamp := 0;
  end
  else
    StatusBar1.Panels[1].Text := '';
end;

procedure TFormRuleEditor.AddCommand(Cmd: string; CaretPos: Integer);
var
  Comment: string;
  Line: string;
  i: Integer;
begin
  Line := Editor.LineText;
  i := Pos('//', Line);
  if i > 0 then begin
    Comment := Copy(Line, i, Length(Line));
    Line := Copy(Line, 1, i - 1);
  end
  else begin
    Comment := '';
  end;
  if Pos('|', Line) = 0 then begin
    StatusBar1.Panels[2].Text := 'Current line doesn''t look like an instruction line';
    Exit;
  end;
  
  while Copy(Line, Length(Line), 1) = ' ' do
    Line := Copy(Line, 1, Length(Line) - 1);

  if (Copy(Line, Length(Line), 1) <> ';') and
   (lowercase(Copy(Line, Length(Line) - 3, 4)) <> 'then') and
   (lowercase(Copy(Line, Length(Line) - 3, 4)) <> 'else') then
    Line := Line + ';';
  Line := Line + ' ' + Cmd;
  i := Length(Line) - Length(Cmd) + CaretPos;
  
  if Comment <> '' then
    Line := Line + ' ' + Comment;
    
  Editor.LineText := Line;
  Editor.CaretX := i;
  
  FChanges := True;
end;

function TFormRuleEditor.Execute(FileName: string; DialogName: string; InitialLine: Integer): Integer;
begin
  if DialogName <> '' then begin
    FName := DialogName;
    MenuItemAdd.Visible := False;
    LabelSyntax.Visible := False;
  end
  else begin
    FName := 'Rule Editor';
    MenuItemAdd.Visible := True;
    LabelSyntax.Visible := True;
  end;
  Self.Caption := FName;
  if (FFileName <> FileName) then begin
    Editor.CaretX := 1;
    Editor.CaretY := 1;
  end;
  FFileName := FileName;
  FInitialLine := InitialLine;
  // Delay loading file, otherwise the window creation for editor might fail
  Application.QueueAsyncCall(@LoadFileAsync, 0);
  Result := ShowModal;
end;

procedure TFormRuleEditor.UpdateStatus;
begin
  StatusBar1.Panels[0].Text := IntToStr(Editor.CaretY) + ': ' + IntTostr(Editor.CaretX);
  StatusBar1.Panels[2].Text := FFileName;
end;

function TFormRuleEditor.LoadFile: Boolean;
var
  SaveXY: TPoint;
  SaveTopLine: Longint;
begin
  Result := True;
  Changes := False;
  try
    SaveXY := Editor.CaretXY;
    SaveTopLine := Editor.TopLine;
    if not FileExists(FFileName) then begin
      case MessageDlg(FName, FFileName + ' does not exist. Create new?',
       mtConfirmation, mbYesNo, 0) of
        mrYes: ;
        else begin
          Result := False;
          Exit;
        end;
      end;
    end
    else begin
      if (FLoadedFileName <> FFileName) or (FLoadedFileTimeStamp <> FileAge(FFileName)) then begin
        Editor.Lines.Clear;
        Editor.Lines.LoadFromFile(FFileName);
        FLoadedFileName := FFileName;
        FLoadedFileTimeStamp := FileAge(FFileName);
      end;
    end;
    try
      Editor.CaretXY := SaveXY;
      Editor.TopLine := SaveTopLine;
      if (FInitialLine > 0) then begin
        Editor.CaretX := 1;
        Editor.CaretY := FInitialLine;
      end;
    except
    end;
    UpdateStatus;
  except
    on E: Exception do begin
      Result := False;
      MainForm.ShowError('Failed to load file', E.Message);
    end;
  end;
end;

function TFormRuleEditor.SaveFile: Boolean;
var
  BackupName: string;
begin
  Result := True;
  try
    // Create backup first
    BackupName := FFileName + '.bak';
    try
      if FileExists(BackupName) then
        DeleteFile(BackupName);
      RenameFile(FFileName, BackupName);
    except
      on E: Exception do begin
        Result := False;
        MainForm.ShowError('Failed to create backup file', E.Message);
        Exit;
      end;
    end;
    Editor.Lines.SaveToFile(FFileName);
    Changes := False;
    FLoadedFileName := FFileName;
    FLoadedFileTimeStamp := FileAge(FFileName);
  except
    on E: Exception do begin
      Result := False;
      MainForm.ShowError('Failed to save file', E.Message);
    end;
  end;
end;

initialization
  {$I ruleeditorunit.lrs}

end.

