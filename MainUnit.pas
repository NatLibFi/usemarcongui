{
 /***************************************************************************
                            MainUnit.pas
                            ------------

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

unit MainUnit;

{$mode objfpc}{$H+}

// To compile widget in:
// Other unit files:
// $(LazarusDir)\lcl\;$(LazarusDir)\lcl\interfaces\$(LCLWidgetType)\;$(LazarusDir)\components\synedit\;$(LazarusDir)\fpc\2.0.4\source\fcl\inc\
// Include files:
// $(LazarusDir)\components\synedit\;$(LazarusDir)\lcl\units\i386\win32\;$(LazarusDir)\lcl\include\;$(LazarusDir)\lcl\interfaces\win32\


interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, Buttons, EditBtn, SynEdit, XMLCfg, ComCtrls,
  SynHighlighterMARC, SynEditKeyCmds, LCLType, RegExpr, LR_Class, LR_DSet,
  LR_E_TXT, LR_E_HTM, LR_E_CSV, LR_Desgn, LMessages,
  RecordConverterUnit, Int64List, BufferedFile;

const
  VERSION = '3.16';

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    BitBtnEditRuleFile: TBitBtn;
    BitBtnFind: TBitBtn;
    BitBtnNext: TBitBtn;
    BitBtnLast: TBitBtn;
    BitBtnPrevious: TBitBtn;
    BitBtnFirst: TBitBtn;
    CheckBoxSplitSubfields: TCheckBox;
    CheckBoxUTF8Autodetect: TCheckBox;
    CheckBoxSynchronousScroll: TCheckBox;
    EditDecodedUTF8: TEdit;
    EditFind: TEdit;
    FontDialog1: TFontDialog;
    frCSVExport1: TfrCSVExport;
    frDesigner1: TfrDesigner;
    frHTMExport1: TfrHTMExport;
    frReport1: TfrReport;
    frTextExport1: TfrTextExport;
    frUserDatasetRecordLine: TfrUserDataset;
    frUserDatasetRecord: TfrUserDataset;
    Label2UTF8: TLabel;
    Label3: TLabel;
    LabelMatch: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItemFindPrevious: TMenuItem;
    MenuItemCustomFont: TMenuItem;
    MenuItemDefaultFont: TMenuItem;
    MenuItemPrintInput: TMenuItem;
    MenuItemPrintOutput: TMenuItem;
    MenuItemPrint: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItemNextError: TMenuItem;
    MenuItemOpenRecent: TMenuItem;
    MenuItemClearRecent: TMenuItem;
    MenuItemSep5: TMenuItem;
    MenuItemSep4: TMenuItem;
    MenuItemSep1: TMenuItem;
    MenuItemSep2: TMenuItem;
    MenuItemFindNext: TMenuItem;
    MenuItemOpenProject: TMenuItem;
    MenuItemSaveProject: TMenuItem;
    MenuItemExport: TMenuItem;
    MenuItemEditOutputFixedFieldListFile: TMenuItem;
    MenuItemEditOutputFormatCheckingFile: TMenuItem;
    MenuItemEditInputFixedFieldListFile: TMenuItem;
    MenuItemEditInputFormatCheckingFile: TMenuItem;
    MenuItemEditCharConv: TMenuItem;
    MenuItemEditINIFile: TMenuItem;
    MenuItemEditRuleFile: TMenuItem;
    MenuItemInputFile: TMenuItem;
    MenuItemConversionINIFile: TMenuItem;
    MenuItemOutputFile: TMenuItem;
    MenuItemUSEMARCONProgram: TMenuItem;
    MenuItemStopConversion: TMenuItem;
    MenuItemRunConversion: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemExit: TMenuItem;
    BitBtnStop: TBitBtn;
    BitBtnConvert: TBitBtn;
    EditRecNr: TEdit;
    FileNameEditUSEMARCON: TFileNameEdit;
    FileNameEditInput: TFileNameEdit;
    FileNameEditConversion: TFileNameEdit;
    FileNameEditOutput: TFileNameEdit;
    Label1: TLabel;
    LabelRecNr: TLabel;
    LabelConversion: TLabel;
    LabelOutputFile: TLabel;
    LabelInputFile: TLabel;
    OpenDialog1: TOpenDialog;
    OpenDialogProject: TOpenDialog;
    PanelResultsTitle: TPanel;
    PanelTop: TPanel;
    PanelInput: TPanel;
    PanelOutput: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    PanelFiles: TPanel;
    PanelBottomTop: TPanel;
    PanelBottom: TPanel;
    SaveDialog1: TSaveDialog;
    SaveDialogProject: TSaveDialog;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    SynEditResults: TSynEdit;
    SynEditInput: TSynEdit;
    SynEditOutput: TSynEdit;
    TimerPrev: TTimer;
    TimerNext: TTimer;
    Config: TXMLConfig;
    procedure BitBtnEditRuleFileClick(Sender: TObject);
    procedure BitBtnExitClick(Sender: TObject);
    procedure BitBtnConvertClick(Sender: TObject);
    procedure BitBtnFindClick(Sender: TObject);
    procedure BitBtnFirstClick(Sender: TObject);
    procedure BitBtnLastClick(Sender: TObject);
    procedure BitBtnNextKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BitBtnNextKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure BitBtnNextMouseDown(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BitBtnNextMouseUp(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BitBtnPreviousKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BitBtnPreviousKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BitBtnPreviousMouseDown(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BitBtnPreviousMouseUp(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BitBtnStopClick(Sender: TObject);
    procedure CheckBoxSplitSubfieldsChange(Sender: TObject);
    procedure CheckBoxUTF8AutodetectChange(Sender: TObject);
    procedure EditFindChange(Sender: TObject);
    procedure EditFindKeyPress(Sender: TObject; var Key: char);
    procedure EditRecNrEditingDone(Sender: TObject);
    procedure EditRecNrKeyPress(Sender: TObject; var Key: char);
    procedure FileNameEditConversionChange(Sender: TObject);
    procedure FileNameEditInputAcceptFileName(Sender: TObject; var Value: String);
    procedure FileNameEditInputKeyPress(Sender: TObject; var Key: char);
    procedure FileNameEditOutputAcceptFileName(Sender: TObject; var Value: String);
    procedure FileNameEditOutputKeyPress(Sender: TObject; var Key: char);
    procedure FormActivate(Sender: TObject);
    procedure frReport1GetValue(const ParName: String; var ParValue: Variant);
    procedure frUserDatasetRecordCheckEOF(Sender: TObject; var Eof: Boolean);
    procedure frUserDatasetRecordFirst(Sender: TObject);
    procedure frUserDatasetRecordLineCheckEOF(Sender: TObject; var Eof: Boolean
      );
    procedure frUserDatasetRecordLineFirst(Sender: TObject);
    procedure frUserDatasetRecordLineNext(Sender: TObject);
    procedure frUserDatasetRecordNext(Sender: TObject);
    procedure MainFormCreate(Sender: TObject);
    procedure MainFormDestroy(Sender: TObject);
    procedure MainFormKeyPress(Sender: TObject; var Key: char);
    procedure MainFormResize(Sender: TObject);
    procedure MainFormShortCut(var Msg: TLMKey; var Handled: Boolean);
    procedure MenuItemCustomFontClick(Sender: TObject);
    procedure MenuItemDefaultFontClick(Sender: TObject);
    procedure MenuItemFindPreviousClick(Sender: TObject);
    procedure MenuItemPrintInputClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemClearRecentClick(Sender: TObject);
    procedure MenuItemConversionINIFileClick(Sender: TObject);
    procedure MenuItemEditCharConvClick(Sender: TObject);
    procedure MenuItemEditINIFileClick(Sender: TObject);
    procedure MenuItemEditInputFixedFieldListFileClick(Sender: TObject);
    procedure MenuItemEditInputFormatCheckingFileClick(Sender: TObject);
    procedure MenuItemEditOutputFixedFieldListFileClick(Sender: TObject);
    procedure MenuItemEditOutputFormatCheckingFileClick(Sender: TObject);
    procedure MenuItemEditRuleFileClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemExportClick(Sender: TObject);
    procedure MenuItemFindNextClick(Sender: TObject);
    procedure MenuItemInputFileClick(Sender: TObject);
    procedure MenuItemNextErrorClick(Sender: TObject);
    procedure MenuItemOpenProjectClick(Sender: TObject);
    procedure MenuItemOutputFileClick(Sender: TObject);
    procedure MenuItemPrintOutputClick(Sender: TObject);
    procedure MenuItemSaveProjectClick(Sender: TObject);
    procedure MenuItemUSEMARCONProgramClick(Sender: TObject);
    procedure SynEditInputClick(Sender: TObject);
    procedure SynEditInputCommandProcessed(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure SynEditInputStatusChange(Sender: TObject;
      Changes: TSynStatusChanges);
    procedure SynEditOutputClick(Sender: TObject);
    procedure SynEditOutputCommandProcessed(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure SynEditOutputStatusChange(Sender: TObject;
      Changes: TSynStatusChanges);
    procedure SynEditResultsDblClick(Sender: TObject);
    procedure TimerPrevTimer(Sender: TObject);
    procedure TimerNextTimer(Sender: TObject);
  private
    FCurrentRecNr: LongInt;
    FCancelled: Boolean;
    FInputIndex: TInt64List;
    FOutputIndex: TInt64List;
    FInputHighlighter: TSynMARCSyn;
    FOutputHighlighter: TSynMARCSyn;
    FShiftState: TShiftState;
    FRecentProjects: TStringList;
    FProjectName: string;
    FIndexedInputFileTimestamp: Integer;
    FIndexedInputFileName: string;
    FIndexedOutputFileTimestamp: Integer;
    FIndexedOutputFileName: string;
    FPrintInput: Boolean;
    FCurrentPrintRecord: Integer;
    FCurrentPrintLine: Integer;
    FInputRecordUTF8: Boolean;
    FOutputRecordUTF8: Boolean;
    FConverter: TRecordConverter;
    FContinueSearch: Boolean;

    procedure InitAll;
    procedure ExceptionHandler(Sender: TObject; E: Exception);

    function GetInputCount: LongInt;
    function GetOutputCount: LongInt;
    function LeftPadCh(S: string; Len: LongInt; PadCh: Char): string;
    function SafeStrToInt(S: string): LongInt;
    function ReplaceChar(S: string; Old: Char; New: Char): string;
    function ReplaceStr(S: string; Old: string; New: string; StartOccurrence: LongInt = 1): string;
    function GetMaxRecNr: LongInt;
    procedure SetRunning(const AValue: Boolean);

    procedure ReadMARCFile(const Filename: string; var List: TInt64List;
      IgnoreErrors: Boolean = False);
    function SetCurrentRecNr(const AValue: LongInt): Boolean;
    function ShowRecord(RecNr: Integer): Boolean;
    function ISO2709ToString(Data: string; out UTF8: Boolean): string;
    procedure ReadFiles(Input: Boolean; Output: Boolean);
    procedure UpdateInputStatus(Data: PtrInt);
    procedure UpdateOutputStatus(Data: PtrInt);
    procedure UpdateStatus(var Editor: TSynEdit; UTF8: Boolean);
    function MatchRecord(RecNr: LongInt; var RegExp: TRegExpr; var InputF, OutputF: TBufferedFile): Boolean;
    procedure ShowNextError(FirstTime: Boolean);
    procedure EditRuleFile(InitialLine: Integer);
    procedure FindString(NextHit: Boolean = True);

    function GetINIPath(const INIFile: string; const Section: string;
      const Setting: string): string;
    procedure ShowFileDialog(var Edit: TFileNameEdit);
    procedure LoadProject(const FileName: string);
    procedure SaveProject(const FileName: string);
    procedure AddToRecentList(const FileName: string);
    function JustFileName(FileName: string): string;
    procedure UpdateRecentProjectList;
    procedure MenuItemRecentClick(Sender: TObject);
    function MaxLineLength(S: string): LongInt;

    property Running: Boolean write SetRunning;
  public
    procedure ShowError(Msg: string; Error: string);
    function GetMarcData(Input: Boolean; Index: LongInt; out Success: Boolean): string;
    function GetMarcData(var F: TBufferedFile; RecPos: Int64; out Success: Boolean): string;

    property CurrentRecNr: LongInt read FCurrentRecNr;
    property MaxRecNr: LongInt read GetMaxRecNr;
    property InputCount: LongInt read GetInputCount;
    property OutputCount: LongInt read GetOutputCount;
  end;

var
  MainForm: TMainForm;

implementation

uses
  Process, Messages, sysconst, LCLProc, Math, LazUTF8,
  RuleEditorUnit, INIEditorUnit,
  ExportUnit, SynEditTypes;

{ TMainForm }

procedure TMainForm.MainFormCreate(Sender: TObject);
begin
  Application.OnException := @ExceptionHandler;
  Application.ShowButtonGlyphs := sbgSystem;

  FRecentProjects := TStringList.Create;

  FInputIndex := TInt64List.Create;
  FOutputIndex := TInt64List.Create;
  SetCurrentRecNr(1);
  
  FInputHighlighter := TSynMARCSyn.Create(Self);
  SynEditInput.Highlighter := FInputHighlighter;
  
  FOutputHighlighter := TSynMARCSyn.Create(Self);
  SynEditOutput.Highlighter := FOutputHighlighter;
  
  FConverter := TRecordConverter.Create;

  FContinueSearch := False;

  FIndexedInputFileTimestamp := 0;
  FIndexedInputFileName := '';
  FIndexedOutputFileTimestamp := 0;
  FIndexedOutputFileName := '';

{$IFNDEF WINDOWS}
  FileNameEditConversion.Filter := 'INI Files (*.ini)|*.ini|All Files|*';
  FileNameEditUSEMARCON.Filter := 'All Files|*';
{$ENDIF}
end;

procedure TMainForm.MainFormDestroy(Sender: TObject);
var
  i: Integer;
begin
  Config.SetValue('InputFile', FileNameEditInput.FileName);
  Config.SetValue('ConversionIni', FileNameEditConversion.FileName);
  Config.SetValue('USEMARCONExe', FileNameEditUSEMARCON.FileName);
  Config.SetValue('OutputFile', FileNameEditOutput.FileName);

  Config.SetValue('RecordFontName', SynEditInput.Font.Name);
  Config.SetValue('RecordFontSize', SynEditInput.Font.Size);

  if Self.WindowState = wsNormal then begin
    Config.SetValue('WindowLeft', Self.Left);
    Config.SetValue('WindowTop', Self.Top);
    Config.SetValue('WindowWidth', Self.Width);
    Config.SetValue('WindowHeight', Self.Height);
  end;
  Config.SetValue('WindowState', LongInt(Self.WindowState));

  Config.SetValue('InputPanelWidth', PanelInput.Width);
  Config.SetValue('FilePanelHeight', PanelFiles.Height);
  Config.SetValue('BottomPanelHeight', PanelBottom.Height);

  Config.SetValue('SynchronousScroll', CheckBoxSynchronousScroll.Checked);
  Config.SetValue('SplitSubfields', CheckBoxSplitSubfields.Checked);
  Config.SetValue('UTF8Autodetect', CheckBoxUTF8Autodetect.Checked);

  for i := 0 to FRecentProjects.Count - 1 do begin
    Config.SetValue('RecentProject' + IntToStr(i + 1), FRecentProjects[i]);
  end;
  for i := FRecentProjects.Count + 1 to 20 do begin
    Config.SetValue('RecentProject' + IntToStr(i), '');
  end;

  FreeAndNil(FConverter);

  FreeAndNil(FInputIndex);
  FreeAndNil(FOutputIndex);
  
  SynEditInput.Highlighter := nil;
  FreeAndNil(FInputHighlighter);

  SynEditOutput.Highlighter := nil;
  FreeAndNil(FOutputHighlighter);
  
  FRecentProjects.Clear;
  UpdateRecentProjectList;
  FreeAndNil(FRecentProjects);

  Application.OnException := nil;
end;

procedure TMainForm.MainFormKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #9) or ((FShiftState = [ssAlt]) and (Key in ['a'..'z','A'..'Z'])) then
    Key := #0;
end;

procedure TMainForm.MainFormResize(Sender: TObject);
begin
  if PanelFiles.Height > Self.ClientHeight - PanelTop.Height - 50 then
    PanelFiles.Height := Self.ClientHeight - PanelTop.Height - 50;
  if PanelFiles.Height < 50 then
    PanelFiles.Height := 50;
  if PanelInput.Width > Self.ClientWidth - 50 then
    PanelInput.Width := Self.ClientWidth - 50;
  if PanelInput.Width < 50 then
    PanelInput.Width := 50;
end;

procedure TMainForm.MainFormShortCut(var Msg: TLMKey; var Handled: Boolean);
var
  ShiftState: TShiftState;
begin
  ShiftState := KeyDataToShiftState(Msg.KeyData);

  Handled := (ShiftState = [ssAlt]) and (Char(Msg.CharCode) in ['a'..'z', 'A'..'Z']);
end;

procedure TMainForm.MenuItemCustomFontClick(Sender: TObject);
begin
  FontDialog1.Font := SynEditInput.Font;
  if FontDialog1.Execute then begin
    SynEditInput.Font.Name := FontDialog1.Font.Name;
    SynEditInput.Font.Size := FontDialog1.Font.Size;
    SynEditInput.Font.Style := FontDialog1.Font.Style;

    SynEditOutput.Font := SynEditInput.Font;
  end;
end;

procedure TMainForm.MenuItemDefaultFontClick(Sender: TObject);
begin
  SynEditInput.Font.Name := 'Lucida Console';
  SynEditInput.Font.Size := 10;
  SynEditInput.Font.Style := [];

  SynEditOutput.Font := SynEditInput.Font;
end;

procedure TMainForm.MenuItemFindPreviousClick(Sender: TObject);
begin
  if BitBtnFind.Enabled then
    FindString(False);
end;

procedure TMainForm.MenuItemPrintInputClick(Sender: TObject);
begin
  FPrintInput := True;
  frReport1.ShowReport;
end;

procedure TMainForm.MenuItemAboutClick(Sender: TObject);
begin
  ShowMessage('USEMARCON GUI v' + VERSION + #13#10 +
    'Copyright 2005-2020 Ere Maijala' + #13#10 +
    'The National Library of Finland');
end;

procedure TMainForm.MenuItemClearRecentClick(Sender: TObject);
begin
  FRecentProjects.Clear;
  UpdateRecentProjectList;
end;

procedure TMainForm.MenuItemConversionINIFileClick(Sender: TObject);
begin
  ShowFileDialog(FileNameEditConversion);
end;

procedure TMainForm.MenuItemEditCharConvClick(Sender: TObject);
var
  FileName: string;
begin
  FileName := GetINIPath(FileNameEditConversion.FileName, 'DEFAULT_FILES',
    'TranscodingCharacterTable');
  if FileName = '' then begin
    ShowMessage('Character conversion file not specified in INI file');
    Exit;
  end;
  FormRuleEditor.Execute(FileName, 'Character'); // Conversion File Editor');
end;

procedure TMainForm.MenuItemEditINIFileClick(Sender: TObject);
begin
  FormINIEditor.Execute(FileNameEditConversion.FileName);
end;

procedure TMainForm.MenuItemEditInputFixedFieldListFileClick(Sender: TObject);
var
  FileName: string;
begin
  FileName := GetINIPath(FileNameEditConversion.FileName, 'DEFAULT_FILES',
    'InputMarcEditConfigurationFile');
  if FileName = '' then begin
    ShowMessage('Input fixed field list file not specified in INI file');
    Exit;
  end;
  FormRuleEditor.Execute(FileName, 'Input Fixed Field List File Editor');
end;

procedure TMainForm.MenuItemEditInputFormatCheckingFileClick(Sender: TObject);
var
  FileName: string;
begin
  FileName := GetINIPath(FileNameEditConversion.FileName, 'DEFAULT_FILES',
    'InputFormatCheckingTable');
  if FileName = '' then begin
    ShowMessage('Input format checking file not specified in INI file');
    Exit;
  end;
  FormRuleEditor.Execute(FileName, 'Input Format Checking File Editor');
end;

procedure TMainForm.MenuItemEditOutputFixedFieldListFileClick(Sender: TObject);
var
  FileName: string;
begin
  FileName := GetINIPath(FileNameEditConversion.FileName, 'DEFAULT_FILES',
    'OutputMarcEditConfigurationFile');
  if FileName = '' then begin
    ShowMessage('Output fixed field list file not specified in INI file');
    Exit;
  end;
  FormRuleEditor.Execute(FileName, 'Output Fixed Field List File Editor');
end;

procedure TMainForm.MenuItemEditOutputFormatCheckingFileClick(Sender: TObject);
var
  FileName: string;
begin
  FileName := GetINIPath(FileNameEditConversion.FileName, 'DEFAULT_FILES',
    'OutputFormatCheckingTable');
  if FileName = '' then begin
    ShowMessage('Output format checking file not specified in INI file');
    Exit;
  end;
  FormRuleEditor.Execute(FileName, 'Output Format Checking File Editor');
end;

procedure TMainForm.MenuItemEditRuleFileClick(Sender: TObject);
begin
  BitBtnEditRuleFile.Click;
end;

procedure TMainForm.MenuItemExitClick(Sender: TObject);
begin
  FCancelled := True;
  Application.Terminate;
end;

procedure TMainForm.MenuItemExportClick(Sender: TObject);
begin
  FormExport.ShowModal;
end;

procedure TMainForm.MenuItemFindNextClick(Sender: TObject);
begin
  if BitBtnFind.Enabled then
    FindString(True);
end;

procedure TMainForm.MenuItemInputFileClick(Sender: TObject);
begin
  ShowFileDialog(FileNameEditInput);
end;

procedure TMainForm.MenuItemNextErrorClick(Sender: TObject);
begin
  ShowNextError(True);
end;

procedure TMainForm.ShowNextError(FirstTime: Boolean);
var
  Opts: TSynSearchOptions;
  OldCaretXY: TPoint;
begin
  Opts := [ssoMatchCase, ssoWholeWord];
  if not FirstTime then
    Include(Opts, ssoEntireScope);
  SynEditResults.SelEnd := SynEditResults.SelStart;
  // Laz bug?
  SynEditResults.SelStart := SynEditResults.SelEnd;
  OldCaretXY := SynEditResults.CaretXY;
  SynEditResults.SearchReplace('ERROR', '', Opts);
  if (OldCaretXY.X = SynEditResults.CaretXY.X) and (OldCaretXY.Y = SynEditResults.CaretXY.Y) then begin
    if FirstTime then
      ShowNextError(False)
    else
      ShowMessage('No errors found in output');
  end;
end;

procedure TMainForm.MenuItemOpenProjectClick(Sender: TObject);
begin
  if OpenDialogProject.Execute then begin
    LoadProject(OpenDialogProject.FileName);
  end;
end;

procedure TMainForm.MenuItemOutputFileClick(Sender: TObject);
begin
  ShowFileDialog(FileNameEditOutput);
end;

procedure TMainForm.MenuItemPrintOutputClick(Sender: TObject);
begin
  FPrintInput := False;
  frReport1.ShowReport;
end;

procedure TMainForm.MenuItemSaveProjectClick(Sender: TObject);
begin
  SaveDialogProject.FileName := FProjectName;
  if SaveDialogProject.Execute then begin
    SaveProject(SaveDialogProject.FileName);
  end;
end;

procedure TMainForm.MenuItemUSEMARCONProgramClick(Sender: TObject);
begin
  ShowFileDialog(FileNameEditUSEMARCON);
end;

procedure TMainForm.SynEditInputClick(Sender: TObject);
begin
  Application.QueueAsyncCall(@UpdateInputStatus, 0);
end;

procedure TMainForm.SynEditInputCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
begin
  Application.QueueAsyncCall(@UpdateInputStatus, 0);
end;

procedure TMainForm.SynEditInputStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if CheckBoxSynchronousScroll.Checked then begin
    if scLeftChar in Changes then begin
      SynEditOutput.LeftChar := SynEditInput.LeftChar;
    end;
    if scTopLine in Changes then begin
      SynEditOutput.TopLine := SynEditInput.TopLine;
    end;
  end;
end;

procedure TMainForm.SynEditOutputClick(Sender: TObject);
begin
  Application.QueueAsyncCall(@UpdateOutputStatus, 0);
end;

procedure TMainForm.SynEditOutputCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
begin
  Application.QueueAsyncCall(@UpdateOutputStatus, 0);
end;

procedure TMainForm.SynEditOutputStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if CheckBoxSynchronousScroll.Checked then begin
    if scLeftChar in Changes then begin
      SynEditInput.LeftChar := SynEditOutput.LeftChar;
    end;
    if scTopLine in Changes then begin
      SynEditInput.TopLine := SynEditOutput.TopLine;
    end;
  end;
end;

procedure TMainForm.SynEditResultsDblClick(Sender: TObject);
var
  RegExp: TRegExpr;
begin
  RegExp := TRegExpr.Create();
  RegExp.Expression := ' at line (\d*)$';
  if (RegExp.Exec(SynEditResults.LineText) or RegExp.Exec(SynEditResults.Lines[SynEditResults.CaretY]))
   and BitBtnEditRuleFile.Enabled then begin
    EditRuleFile(StrToInt(RegExp.Match[1]));
  end;
end;

procedure TMainForm.TimerPrevTimer(Sender: TObject);
begin
  if TimerPrev.Interval > 150 then
    TimerPrev.Interval := 150;
  if TimerPrev.Interval > 5 then
    TimerPrev.Interval := TimerPrev.Interval - 5;

  if not SetCurrentRecNr(CurrentRecNr - 1) then
    TimerPrev.Enabled := False;
end;

procedure TMainForm.TimerNextTimer(Sender: TObject);
begin
  if TimerNext.Interval > 150 then
    TimerNext.Interval := 150;
  if TimerNext.Interval > 5 then
    TimerNext.Interval := TimerNext.Interval - 5;
    
  if not SetCurrentRecNr(CurrentRecNr + 1) then
    TimerNext.Enabled := False;
end;

function TMainForm.SetCurrentRecNr(const AValue: LongInt): Boolean;
begin
  if AValue < 1 then
    FCurrentRecNr := 1
  else if AValue > MaxRecNr then
    FCurrentRecnr := MaxRecNr
  else
    FCurrentRecNr := AValue;
  EditRecNr.Text := IntToStr(FCurrentRecNr);

  Result := ShowRecord(FCurrentRecNr);
end;

procedure TMainForm.SetRunning(const AValue: Boolean);
begin
  if AValue then begin
    BitBtnConvert.Enabled := False;
    BitBtnStop.Enabled := True;
    Cursor := crHourGlass;
    MenuItemRunConversion.Enabled := False;
    MenuItemStopConversion.Enabled := True;
  end
  else begin
    BitBtnConvert.Enabled := True;
    BitBtnStop.Enabled := False;
    Cursor := crDefault;
    MenuItemRunConversion.Enabled := True;
    MenuItemStopConversion.Enabled := False;
  end;
end;

function TMainForm.GetMaxRecNr: LongInt;
begin
  if FInputIndex.Count > FOutputIndex.Count then
    Result := FInputIndex.Count
  else
    Result := FOutputIndex.Count;
end;

procedure TMainForm.ReadMARCFile(const Filename: string;
  var List: TInt64List; IgnoreErrors: Boolean);
const
  Numbers = ['0'..'9'];
  Lock: Boolean = False;
var
  F: TBufferedFile;
  LengthStr: array[0..5] of Char;
  RecLength: LongInt;
  RecData: Ansistring;
  Count: Int64;
  RecordCount: LongInt;
  XML: Boolean;
  InRecord: Boolean;
  Line: string;
  FPos: Int64;
  RecPos: Int64;
  StartPos: LongInt;
begin
  if Lock then
    Exit;
  Lock := True;
  List.Clear;
  F := TBufferedFile.Create;
  try
    F.Open(Filename);
  except
    on E: Exception do begin
      if not IgnoreErrors then
        ShowError('Could not open file ' + Filename + ' for reading', E.Message);
      Lock := False;
      Exit;
    end;
  end;
  RecordCount := 0;
  XML := False;
  try
    while not F.Eof do begin
      // Find the beginning of the leader
      F.Read(LengthStr[0], 1, Count);
      while not F.Eof and not (LengthStr[0] in Numbers) and (Count > 0) do begin
        if (RecordCount = 0) And (LengthStr[0] = '<') then begin
          XML := True;
          break;
        end;
        F.Read(LengthStr[0], 1, Count);
      end;
      RecPos := F.Position - 1;
      if F.Eof or (Count = 0) or XML then Break;
      F.Read(LengthStr[1], 4, Count);
      LengthStr[5] := #0;
      RecLength := StrToInt(LengthStr);
      SetLength(RecData, RecLength);
      RecData[1] := LengthStr[0];
      RecData[2] := LengthStr[1];
      RecData[3] := LengthStr[2];
      RecData[4] := LengthStr[3];
      RecData[5] := LengthStr[4];
      F.Read(RecData[6], RecLength - 5, Count);
      List.Add(RecPos);
      Inc(RecordCount);
      if RecordCount mod 117 = 0 then begin
        StatusBar1.Panels[3].Text := 'Indexing records (' + IntToStr(RecordCount) + ')';
        Application.ProcessMessages;
        if Application.Terminated then
          break;
      end;
    end;
    if XML then begin
      F.Rewind;
      InRecord := False;
      while not F.Eof do begin
        FPos := F.Position;
        Line := F.ReadTextLine();
        if not InRecord then begin
          StartPos := Pos('<record', Line);
          if StartPos > 0 then begin
            RecPos := FPos + StartPos - 1;
            InRecord := True;
          end;
        end
        else begin
          if Pos('</record>', Line) > 0 then begin
            List.Add(RecPos);
            Inc(RecordCount);
            if RecordCount mod 117 = 0 then begin
              StatusBar1.Panels[3].Text := 'Indexing records (' + IntToStr(RecordCount) + ')';
              Application.ProcessMessages;
              if Application.Terminated then
                break;
            end;
              
            StartPos := Pos('<record', Line);
            if StartPos > 0 then
              RecPos := FPos + StartPos - 1
            else
              InRecord := False;
          end
        end;
      end;
    end;
  except
    on E: EInOutError do begin
      if E.Message <> SEndOfFile then
        ShowError('Error reading record ' + IntToStr(RecordCount) + ' from file ' + FileName, E.Message);
    end;
    on E: Exception do begin
      ShowError('Error reading record ' + IntToStr(RecordCount) + ' from file ' + FileName, E.Message);
    end;
  end;
  StatusBar1.Panels[3].Text := '';
  F.Close;
  FreeAndNil(F);
  Lock := False;
end;

function TMainForm.ISO2709ToString(Data: string; out UTF8: Boolean): string;
var
  BaseAddr: LongInt;

  function ReadStr(Offset: LongInt; Length: LongInt): string;
  begin
    Result := Copy(Data, Offset + 1, Length);
  end;

  function ReadInt(Offset: LongInt; Length: LongInt): LongInt;
  begin
    Result := StrToInt(ReadStr(Offset, Length));
  end;

const
  FieldStart = #$1f;
  FieldEnd = #$1e;
var
  DirPos, FieldCodeInt, FieldLen, FieldOffset: LongInt;
  FieldCode: string;
  FieldData: string;
  Field: string;
  Split: Boolean;
  FieldCount: LongInt;
begin
  Result := '000   ' + ReplaceChar(ReadStr(0, 24), ' ', '.') + #13;
  UTF8 := CheckBoxUTF8Autodetect.Checked and (Copy(Result, 16, 1) = 'a');
  try
    BaseAddr := ReadInt(12, 5);
  except
    Result := '        <invalid record>';
    Exit;
  end;
  Split := CheckBoxSplitSubfields.Checked;
  DirPos := 24;
  FieldCount := 0;
  try
    while ReadStr(DirPos, 1) <> FieldEnd do begin
      FieldCode := ReadStr(DirPos, 3);
      FieldCodeInt := SafeStrToInt(FieldCode);
      FieldLen := ReadInt(DirPos + 3, 4);
      FieldOffset := ReadInt(DirPos + 7, 5);

      if FieldCodeInt < 10 then begin
        FieldData := ReadStr(BaseAddr + FieldOffset, FieldLen);
        if not UTF8 then
          FieldData := AnsiToUtf8(FieldData);
        FieldData := '  ' + ReplaceStr(FieldData, ' ', '·');
      end
      else begin
        FieldData := ReadStr(BaseAddr + FieldOffset, FieldLen);
        if not UTF8 then
          FieldData := AnsiToUtf8(FieldData);
        if Split then
          FieldData := ReplaceStr(FieldData, FieldStart, #13 + '      ' + FieldStart, 2);
        FieldData := ReplaceChar(FieldData, FieldStart, '|');
      end;
      Field := FieldCode + ' ' + FieldData;
      Result := Result + Field + #13;
      Inc(DirPos, 12);
      
      Inc(FieldCount);
      if (FieldCount > 1000) then begin
        Result := Result + '        <record too long to be displayed completely>';
        break;
      end;
    end;
  except
    if Result = '' then
      Result := '        <invalid record>';
    Exit;
  end;
end;

function TMainForm.ShowRecord(RecNr: Integer): Boolean;
var
  SaveLeft,
  SaveTop: Integer;
  SaveSync: Boolean;
  Data: string;
begin
  SaveSync := CheckBoxSynchronousScroll.Checked;
  Result := True;
  try
    CheckBoxSynchronousScroll.Checked := False;
    if (RecNr > 0) and (RecNr <= InputCount) then begin
      SaveLeft := SynEditInput.LeftChar;
      SaveTop := SynEditInput.TopLine;
      Data := ISO2709ToString(GetMarcData(True, RecNr - 1, Result), FInputRecordUTF8);
      SynEditInput.Text := Data;
      SynEditInput.MaxLeftChar := MaxLineLength(Data);
      SynEditInput.LeftChar := SaveLeft;
      SynEditInput.TopLine := SaveTop;
    end
    else begin
      SynEditInput.MaxLeftChar := 30;
      SynEditInput.Text := '        <no record>';
    end;

    if (RecNr > 0) and (RecNr <= OutputCount) then begin
      SaveLeft := SynEditOutput.LeftChar;
      SaveTop := SynEditOutput.TopLine;
      Data := ISO2709ToString(GetMarcData(False, RecNr - 1, Result), FOutputRecordUTF8);
      SynEditOutput.Text := Data;
      SynEditOutput.MaxLeftChar := MaxLineLength(Data);
      SynEditOutput.LeftChar := SaveLeft;
      SynEditOutput.TopLine := SaveTop;
    end
    else begin
      SynEditOutput.MaxLeftChar := 30;
      SynEditOutput.Text := '        <no record>';
    end;
  finally
    CheckBoxSynchronousScroll.Checked := SaveSync;
  end;
end;

procedure TMainForm.ShowError(Msg: string; Error: string);
begin
  if Error <> '' then
    Msg := Msg + ' (' + Error + ')';
  MessageDlg('Error', Msg, mtError, [mbOk], 0);
end;

function TMainForm.LeftPadCh(S: string; Len: LongInt; PadCh: Char): string;
begin
  while Length(S) < Len do
    S := PadCh + S;
  Result := S;
end;

function TMainForm.GetInputCount: LongInt;
begin
  Result := FInputIndex.Count;
end;

function TMainForm.GetOutputCount: LongInt;
begin
  Result := FOutputIndex.Count;
end;

function TMainForm.SafeStrToInt(S: string): LongInt;
var
  i: Integer;
begin
  Result := 0;
  S := Trim(S);
  if S = '' then
    Exit;
  for i := 1 to Length(S) do
    if not (S[i] in ['0'..'9']) then
      Exit;
  Result := StrToInt(S);
end;

function TMainForm.ReplaceChar(S: string; Old: Char; New: Char): string;
var
  p: LongInt;
begin
  if Old <> New then begin
    p := Pos(Old, S);
    while p > 0 do begin
      S[p] := New;
      p := Pos(Old, S);
    end;
  end;
  Result := S;
end;

function TMainForm.ReplaceStr(S: string; Old: string; New: string; StartOccurrence: LongInt = 1): string;
var
  p: LongInt;
  Occurrence: LongInt;
  NewS: string;
  Offset: LongInt;
  OldLen: LongInt;
  NewLen: LongInt;
begin
  Occurrence := 0;
  Offset := 0;
  NewS := S;
  OldLen := Length(Old);
  NewLen := Length(New);
  if Old <> New then begin
    p := Pos(Old, S);
    while p > 0 do begin
      Inc(Occurrence);
      if Occurrence >= StartOccurrence then begin
        Delete(NewS, p - Offset, OldLen);
        System.Insert(New, NewS, p - Offset);
      end
      else
        Offset := Offset + NewLen - OldLen;
      Delete(S, p, OldLen);
      System.Insert(LeftPadCh('', NewLen, #1), S, p);
      p := Pos(Old, S);
    end;
  end;
  Result := NewS;
end;

procedure TMainForm.BitBtnConvertClick(Sender: TObject);
const
  BufSize = 10000;
var
  USEMProc: TProcess;
  Buf, OutputLine: string;
  NoNewLine: Boolean;

  procedure HandleOutput;
  var
    i, Count: LongInt;
    OldLine: string;
  begin
    if USEMProc.Output <> nil then
      Count := USEMProc.Output.Read(Buf[1], Length(Buf))
    else
      Count := 0;
    i := 1;
    if Count = 0 then
      Sleep(10);
    while i <= Count do begin
      if Buf[i] in [#10, #13] then begin
        if NoNewLine then begin
          OldLine := SynEditResults.Lines[SynEditResults.Lines.Count - 1];
          SynEditResults.Lines[SynEditResults.Lines.Count - 1] := OutputLine + Copy(OldLine, Length(OutputLine) + 1, Length(OldLine));
        end
        else
          SynEditResults.Lines.Add(OutputLine);
          
        NoNewLine := (Buf[i] = #13) and (i < Count) and (Buf[i + 1] <> #10);

        SynEditResults.Perform(WM_VSCROLL, SB_BOTTOM, 0);
        if (i < Count) and (Buf[i + 1] in [#10, #13]) then
          Inc(i);
        OutputLine := '';
      end
      else begin
        OutputLine := OutputLine + Buf[i];
      end;
      Inc(i);
    end;
  end;
  
begin
  USEMProc := TProcess.Create(nil);
  USEMProc.CommandLine := FileNameEditUSEMARCON.FileName +
    ' "' + FileNameEditConversion.FileName +
    '" "' + FileNameEditInput.FileName +
    '" "' + FileNameEditOutput.FileName + '"';
  USEMProc.Options := USEMProc.Options + [poUsePipes, poStderrToOutPut,
    poNoConsole];

  SynEditResults.Lines.Clear;
  SetLength(Buf, BufSize);
  OutputLine := '';
  NoNewLine := False;
  FCancelled := False;
  Running := True;
  try
    USEMProc.Execute;
    while USEMProc.Running do begin
      Application.ProcessMessages;
      if FCancelled then begin
        USEMProc.Terminate(0);
      end;

      HandleOutput;
    end;
    while (USEMProc.Output <> nil) and (USEMProc.Output.NumBytesAvailable > 0) do
      HandleOutput;
    SynEditResults.Lines.Add(OutputLine);
    SynEditResults.Perform(WM_VSCROLL, SB_BOTTOM, 0);

    if USEMProc.ExitStatus <> 0 then
      ShowError('USEMARCON Conversion failed', IntToStr(USEMProc.ExitStatus));
    USEMProc.Free;
    Sleep(50);
    ReadFiles(True, True);
  finally
    Running := False;
  end;
end;

procedure TMainForm.BitBtnFindClick(Sender: TObject);
begin
  if FContinueSearch then
    FContinueSearch := False
  else
    FindString(True);
end;

procedure TMainForm.FindString(NextHit: Boolean);
var
  Rec,
  StartRec: LongInt;
  FindStr: string;
  SaveStatus: string;
  SaveFindButtonCaption: string;
  RegExp: TRegExpr;
  InputF: TBufferedFile;
  OutputF: TBufferedFile;
begin
  if MaxRecNr < 1 then
    Exit;
  SaveStatus := StatusBar1.Panels[3].Text;
  SaveFindButtonCaption := BitBtnFind.Caption;
  BitBtnFind.Caption := 'Cancel';
  RegExp := TRegExpr.Create;
  Cursor := crHourGlass;
  InputF := nil;
  OutputF := nil;
  try
    FindStr := EditFind.Text;
    if Copy(FindStr, 1, 1) <> '/' then begin
      FindStr := ReplaceChar(FindStr, '?', '.');
      FindStr := ReplaceStr(FindStr, '*', '.*');
      FindStr := ReplaceStr(FindStr, '|', '\|');
    end
    else begin
      FindStr := Copy(FindStr, 2, Length(FindStr));
      if Copy(FindStr, Length(FindStr), 1) = '/' then
        FindStr := Copy(FindStr, 1, Length(FindStr) - 1);
    end;
    RegExp.Expression := FindStr;
    RegExp.ModifierI := True;
    StartRec := CurrentRecNr;
    if NextHit then
      Rec := StartRec + 1
    else
      Rec := StartRec - 1;

    if FIndexedInputFileName <> '' then begin
      InputF := TBufferedFile.Create;
      InputF.Open(FIndexedInputFileName);
    end;
    if FIndexedOutputFileName <> '' then begin
      OutputF := TBufferedFile.Create;
      OutputF.Open(FIndexedOutputFileName);
    end;

    FContinueSearch := True;
    while FContinueSearch and (Rec <> StartRec) do begin
      if NextHit and (Rec > MaxRecNr) then begin
        Rec := 1;
        Continue;
      end;
      if not NextHit and (Rec < 1) then begin
        Rec := MaxRecNr;
        Continue;
      end;
      if Rec mod 117 = 0 then begin
        StatusBar1.Panels[3].Text := 'Searching record ' + IntToStr(Rec) + ' for "' +
          FindStr + '"';
        Application.ProcessMessages;
      end;
      if MatchRecord(Rec, RegExp, InputF, OutputF) then begin
        SetCurrentRecNr(Rec);
        Break;
      end;
      if NextHit then
        Inc(Rec)
      else
        Dec(Rec);
    end;
    if not FContinueSearch then begin
      LabelMatch.Caption := 'Cancelled';
      LabelMatch.Font.Color := clRed;
    end
    else if Rec = StartRec then begin
      if MatchRecord(Rec, RegExp, InputF, OutputF) then begin
        LabelMatch.Caption := 'Only match';
        LabelMatch.Font.Color := clRed;
      end
      else begin
        LabelMatch.Caption := 'No match';
        LabelMatch.Font.Color := clRed;
      end;
    end
    else if NextHit and (Rec < StartRec) then begin
      LabelMatch.Caption := 'Earlier match';
      LabelMatch.Font.Color := clRed;
    end
    else if not NextHit and (Rec > StartRec) then begin
      LabelMatch.Caption := 'Later match';
      LabelMatch.Font.Color := clRed;
    end
    else begin
      LabelMatch.Caption := 'Match found';
      LabelMatch.Font.Color := clGreen;
    end;
  finally
    FreeAndNil(InputF);
    FreeAndNil(OutputF);
    FreeAndNil(RegExp);
    StatusBar1.Panels[3].Text := SaveStatus;
    BitBtnFind.Caption := SaveFindButtonCaption;
    Cursor := crDefault;
    FContinueSearch := False;
  end;
end;

procedure TMainForm.BitBtnFirstClick(Sender: TObject);
begin
  SetCurrentRecNr(1);
end;

procedure TMainForm.BitBtnLastClick(Sender: TObject);
begin
  SetCurrentRecNr(Max(InputCount, OutputCount));
end;

procedure TMainForm.BitBtnNextKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_SPACE) and not TimerNext.Enabled then begin
    if not SetCurrentRecNr(CurrentRecNr + 1) then
      Exit;
    TimerNext.Interval := 750;
    TimerNext.Enabled := True;
  end;
end;

procedure TMainForm.BitBtnNextKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  TimerNext.Enabled := False;
end;

procedure TMainForm.BitBtnNextMouseDown(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not SetCurrentRecNr(CurrentRecNr + 1) then
    Exit;
  TimerNext.Interval := 750;
  TimerNext.Enabled := True;
end;

procedure TMainForm.BitBtnNextMouseUp(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TimerNext.Enabled := False;
end;

procedure TMainForm.BitBtnPreviousKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_SPACE) and not TimerPrev.Enabled then begin
    if not SetCurrentRecNr(CurrentRecNr - 1) then
      Exit;
    TimerPrev.Interval := 750;
    TimerPrev.Enabled := True;
  end;
end;

procedure TMainForm.BitBtnPreviousKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  TimerPrev.Enabled := False;
end;

procedure TMainForm.BitBtnPreviousMouseDown(Sender: TOBject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not SetCurrentRecNr(CurrentRecNr - 1) then
    Exit;
  TimerPrev.Interval := 750;
  TimerPrev.Enabled := True;
end;

procedure TMainForm.BitBtnPreviousMouseUp(Sender: TOBject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimerPrev.Enabled := False;
end;

procedure TMainForm.ReadFiles(Input: Boolean; Output: Boolean);
begin
  if Input then begin
    if (InputCount = 0) Or
     ((FileNameEditInput.FileName <> FIndexedInputFileName) Or
     (FileAge(FileNameEditInput.FileName) <> FIndexedInputFileTimestamp)) then begin
      FIndexedInputFileTimestamp := FileAge(FileNameEditInput.FileName);
      FIndexedInputFileName := FileNameEditInput.FileName;
      ReadMARCFile(FileNameEditInput.FileName, FInputIndex);
    end;
  end;
  if Output then begin
    if (OutputCount = 0) Or
     ((FileNameEditOutput.FileName <> FIndexedOutputFileName) Or
     (FileAge(FileNameEditOutput.FileName) <> FIndexedOutputFileTimestamp)) then begin
      FIndexedOutputFileTimestamp := FileAge(FileNameEditOutput.FileName);
      FIndexedOutputFileName := FileNameEditOutput.FileName;
      ReadMARCFile(FileNameEditOutput.FileName, FOutputIndex, True);
    end;
  end;
  if CurrentRecNr > MaxRecNr then
    SetCurrentRecNr(MaxRecNr)
  else if CurrentRecNr < 1 then
    SetCurrentRecNr(1)
  else
    SetCurrentRecNr(CurrentRecNr);
  StatusBar1.Panels[0].Text := 'Records: ' + IntToStr(MaxRecNr)
end;

procedure TMainForm.UpdateInputStatus(Data: PtrInt);
begin
  UpdateStatus(SynEditInput, FInputRecordUTF8);
end;

procedure TMainForm.UpdateOutputStatus(Data: PtrInt);
begin
  UpdateStatus(SynEditOutput, FOutputRecordUTF8);
end;

procedure TMainForm.UpdateStatus(var Editor: TSynEdit; UTF8: Boolean);
var
  p: LongInt;
  Ch: string;
  Sel: string;

  function StrBytesToIntString(Str: string): string;
  var
    i: Integer;
  begin
    Result := '';
    for i := 1 to Length(Str) do begin
      if Result <> '' then
        Result := Result + ' ';
      Result := Result + IntToStr(Integer(Str[i]));
    end;
  end;

  function StrBytesToHexString(Str: string): string;
  var
    i: Integer;
  begin
    Result := '';
    for i := 1 to Length(Str) do begin
      if Result <> '' then
        Result := Result + ' ';
      Result := Result + IntToHex(Integer(Str[i]), 2);
    end;
  end;

begin
  if UTF8 then
    EditDecodedUTF8.Text := Copy(Editor.LineText, Editor.CaretX,Length(Editor.LineText))
  else
    EditDecodedUTF8.Text := Utf8ToAnsi(Copy(Editor.LineText, Editor.CaretX,Length(Editor.LineText)));
  if Editor.CaretX <= Length(Editor.LineText) then begin
    if UTF8 then
      Ch := UTF8Copy(Editor.LineText, Editor.CaretX, 1)
    else
      Ch := Copy(Utf8ToAnsi(Editor.LineText), Editor.CaretX, 1)
  end
  else
    Ch := ' ';
  StatusBar1.Panels[2].Text := 'Char: ' + Ch + ' Dec: ' +
    StrBytesToIntString(Ch) + ' Hex: ' + StrBytesToHexString(Ch);

  if Editor.SelText <> '' then begin
    Sel := '';
    for p := 1 to Length(Editor.SelText) do begin
      if UTF8 then
        Sel := Sel + StrBytesToHexString(UTF8Copy(Editor.SelText, p, 1)) + ' '
      else
        Sel := Sel + StrBytesToHexString(Copy(Utf8ToAnsi(Editor.SelText), p, 1)) + ' ';
    end;
    StatusBar1.Panels[3].Text := 'Sel: ' + Sel;
  end
  else
    StatusBar1.Panels[3].Text := '';

  try
    if SafeStrToInt(Copy(Editor.LineText, 1, 3)) >= 10 then begin
      StatusBar1.Panels[1].Text := 'Line: ' + IntToStr(Editor.CaretY) +
        ' Col: ' + IntToStr(Editor.CaretX);
      Exit;
    end;
  except
    StatusBar1.Panels[1].Text := '';
    Exit;
  end;

  p := Editor.CaretX;

  if (p > 7) then
    Dec(p, 7)
  else
    p := 0;
  StatusBar1.Panels[1].Text := 'Position: ' + IntToStr(p);
end;

function TMainForm.MatchRecord(RecNr: LongInt; var RegExp: TRegExpr; var InputF, OutputF: TBufferedFile): Boolean;
var
  Success: Boolean;
  UTF8: Boolean;
begin
  Result := False;
  if (RecNr <= InputCount) and (InputF <> nil) and (RegExp.Exec(ISO2709ToString(GetMarcData(InputF, FInputIndex.Items[RecNr - 1], Success), UTF8))) and Success then
    Result := True
  else if (RecNr <= OutputCount) and (OutputF <> nil) and (RegExp.Exec(ISO2709ToString(GetMarcData(OutputF, FOutputIndex.Items[RecNr - 1], Success), UTF8))) and Success then
    Result := True;
end;

function TMainForm.GetINIPath(const INIFile: string; const Section: string;
  const Setting: string): string;
var
  RegExp: TRegExpr;
  RegFile: TRegFile;
begin
  RegExp := TRegExpr.Create;
  RegFile := TRegFile.Create(INIFile);
  try
    Result := RegFile.ReadString(Section, Setting);
    if Result <> '' then begin
      if Pos(DirectorySeparator, Result) = 0 then begin
        RegExp.Expression := '(.*)' + DirectorySeparator;
        RegExp.ModifierI := True;
        // Escape it if it's a backslash
        if DirectorySeparator = '\' then
          RegExp.Expression := RegExp.Expression + '\';
        if RegExp.Exec(INIFile) and (RegExp.SubExprMatchCount = 1) then
          Result := RegExp.Match[1] + DirectorySeparator + Result;
      end;
    end;
  finally
    FreeAndNil(RegExp);
    FreeAndNil(RegFile);
  end;
end;

procedure TMainForm.ShowFileDialog(var Edit: TFileNameEdit);
var
  FileName: string;
begin
  if Edit.DialogKind = dkOpen then begin
    OpenDialog1.Filter := Edit.Filter;
    OpenDialog1.Options := Edit.DialogOptions;
    if OpenDialog1.Execute then begin
      FileName := OpenDialog1.FileName;
      Edit.FileName := FileName;
      if Assigned(Edit.OnAcceptFileName) then
        Edit.OnAcceptFileName(Self, FileName);
    end
  end
  else begin
    SaveDialog1.Filter := Edit.Filter;
    SaveDialog1.Options := Edit.DialogOptions;
    if SaveDialog1.Execute then begin
      FileName := SaveDialog1.FileName;
      Edit.FileName := FileName;
      if Assigned(Edit.OnAcceptFileName) then
        Edit.OnAcceptFileName(Self, FileName);
    end
  end;
end;

procedure TMainForm.LoadProject(const FileName: string);
var
  RegFile: TRegFile;
begin
  if not FileExists(FileName) then begin
    ShowMessage('Cannot load project. File ' + FileName + ' does not exist');
    Exit;
  end;

  RegFile := TRegFile.Create(FileName);

  try
    FileNameEditInput.Text := RegFile.ReadString('Files', 'InputFile');
    FileNameEditConversion.Text := RegFile.ReadString('Files', 'INIFile');
    FileNameEditUSEMARCON.Text := RegFile.ReadString('Files', 'USEMARCON');
    FileNameEditOutput.Text := RegFile.ReadString('Files', 'OutputFile');
    Caption := Application.Title + ' - ' + JustFileName(FileName);
    AddToRecentList(FileName);
    FProjectName := FileName;
  finally
    FreeAndNil(RegFile);
  end;
end;

procedure TMainForm.SaveProject(const FileName: string);
var
  RegFile: TRegFile;
begin
  RegFile := TRegFile.Create(FileName);

  try
    RegFile.WriteString('Files', 'InputFile', FileNameEditInput.Text);
    RegFile.WriteString('Files', 'INIFile', FileNameEditConversion.Text);
    RegFile.WriteString('Files', 'USEMARCON', FileNameEditUSEMARCON.Text);
    RegFile.WriteString('Files', 'OutputFile', FileNameEditOutput.Text);
    Caption := Application.Title + ' - ' + JustFileName(FileName);
    AddToRecentList(FileName);
    FProjectName := FileName;
  finally
    FreeAndNil(RegFile);
  end;
end;

procedure TMainForm.AddToRecentList(const FileName: string);
var
  i: Integer;
begin
  for i := 0 to FRecentProjects.Count - 1 do begin
    if FRecentProjects[i] = FileName then begin
      FRecentProjects.Delete(i);
      FRecentProjects.Insert(0, FileName);
      UpdateRecentProjectList;
      Exit;
    end;
  end;
  while FRecentProjects.Count >= 20 do
    FRecentProjects.Delete(FRecentProjects.Count - 1);
  FRecentProjects.Insert(0, FileName);
  UpdateRecentProjectList;
end;

function TMainForm.JustFileName(FileName: string): string;
var
  i: Integer;
begin
  Result := FileName;
  for i := Length(FileName) downto 1 do begin
    if FileName[i] = DirectorySeparator then begin
      Result := Copy(FileName, i + 1, Length(FileName));
      Break;
    end;
  end;
end;

procedure TMainForm.UpdateRecentProjectList;
var
  i: Integer;
  Item: TMenuItem;
begin
  for i := MenuItemOpenRecent.Count - 2 downto 0 do begin
    MenuItemOpenRecent.Items[i].Free;
  end;
  MenuItemClearRecent.Enabled := FRecentProjects.Count > 0;
  for i := FRecentProjects.Count - 1 downto 0 do begin
    Item := TMenuItem.Create(Self);
    Item.Caption := FRecentProjects[i];
    Item.OnClick := @MenuItemRecentClick;
    MenuItemOpenRecent.Insert(0, Item);
  end;
end;

procedure TMainForm.MenuItemRecentClick(Sender: TObject);
begin
  if not (Sender is TMenuItem) then
    Exit;
  LoadProject((Sender as TMenuItem).Caption);
end;

function TMainForm.MaxLineLength(S: string): LongInt;
var
  Len: Integer;
  i: Integer;
begin
  Len := 0;
  Result := 0;
  for i := 1 to Length(S) do begin
    Inc(Len);
    if S[i] = #13 then begin
      if Len > Result then
        Result := Len;
      Len := 0;
    end;
  end;
end;

function TMainForm.GetMarcData(Input: Boolean; Index: LongInt; out Success: Boolean): string;
const
  Lock: Boolean = False;
var
  F: TBufferedFile;
  FileName: string;
  RecPos: Int64;
begin
  Result := '';
  Success := False;
  if Lock then
    Exit;
  Lock := True;
  if Input then begin
    FileName := FIndexedInputFileName;
    RecPos := FInputIndex.Items[Index];
  end
  else begin
    FileName := FIndexedOutputFileName;
    RecPos := FOutputIndex.Items[Index];
  end;
  F := TBufferedFile.Create;
  F.BufferSize := 8192;
  try
    F.Open(FileName);
  except
    on E: Exception do begin
      ShowError('Could not open ' + FileName + ' for reading', E.Message);
      FreeAndNil(F);
      Lock := False;
      Exit;
    end;
  end;
  Result := GetMarcData(F, RecPos, Success);
  FreeAndNil(F);
  Lock := False;
end;

function TMainForm.GetMarcData(var F: TBufferedFile; RecPos: Int64; out
  Success: Boolean): string;
var
  LengthStr: array[0..5] of Char;
  RecLength: LongInt;
  RecData: Ansistring;
  Line: string;
  Data: string;
  Ch: Char;
  EndPos: SizeInt;
  BytesRead: Int64;
begin
  Success := False;
  Result := '';
  try
    F.Position := RecPos;
    F.Read(Ch, 1, BytesRead);
    if Ch = '<' then begin
      Data := Ch;
      while not F.Eof do begin
        Line := F.ReadTextLine;
        EndPos := Pos('</record>', Line);
        if EndPos > 0 then begin
          Data := Data + Copy(Line, 1, EndPos + 9);
          RecData := FConverter.Convert(Data);
          break;
        end;
        Data := Data + Line;
      end;
    end
    else begin
      LengthStr[0] := Ch;
      F.Read(LengthStr[1], 4, BytesRead);
      LengthStr[5] := #0;
      RecLength := StrToInt(LengthStr);
      SetLength(RecData, RecLength);
      RecData[1] := LengthStr[0];
      RecData[2] := LengthStr[1];
      RecData[3] := LengthStr[2];
      RecData[4] := LengthStr[3];
      RecData[5] := LengthStr[4];
      F.Read(RecData[6], RecLength - 5, BytesRead);
    end;
    Success := True;
    Result := RecData;
  except
    on E: Exception do begin
      ShowError('Could not read record', E.Message);
    end;
  end;
end;

procedure TMainForm.BitBtnStopClick(Sender: TObject);
begin
  FCancelled := True;
  BitBtnStop.Enabled := False;
end;

procedure TMainForm.CheckBoxSplitSubfieldsChange(Sender: TObject);
begin
  ShowRecord(FCurrentRecNr);
end;

procedure TMainForm.CheckBoxUTF8AutodetectChange(Sender: TObject);
begin
  ShowRecord(FCurrentRecNr);
end;

procedure TMainForm.EditFindChange(Sender: TObject);
begin
  BitBtnFind.Enabled := EditFind.Text <> '';
  LabelMatch.Caption := '';
end;

procedure TMainForm.EditFindKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) and BitBtnFind.Enabled then begin
    BitBtnFind.Click;
    Key := #0;
  end;
end;

procedure TMainForm.EditRecNrEditingDone(Sender: TObject);
var
  RecNr: LongInt;
begin
  try
    RecNr := SafeStrToInt(EditRecNr.Text);
    if (RecNr > 0) and (RecNr <= MaxRecNr) then
      SetCurrentRecNr(RecNr);
  except
  end;
end;

procedure TMainForm.EditRecNrKeyPress(Sender: TObject; var Key: char);
var
  RecNr: LongInt;
begin
  if Key = #13 then begin
    try
      RecNr := SafeStrToInt(EditRecNr.Text);
      if (RecNr > 0) and (RecNr <= MaxRecNr) then
        SetCurrentRecNr(RecNr);
    except
    end;
    Key := #0;
  end;
end;

procedure TMainForm.FileNameEditConversionChange(Sender: TObject);
begin
  BitBtnEditRuleFile.Enabled := FileNameEditConversion.FileName <> '';
end;

procedure TMainForm.FileNameEditInputAcceptFileName(Sender: TObject;
  var Value: String);
begin
  FileNameEditInput.FileName := Value;
  ReadFiles(True, False);
end;

procedure TMainForm.FileNameEditInputKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then begin
    ReadFiles(True, False);
    Key := #0;
  end;
end;

procedure TMainForm.FileNameEditOutputAcceptFileName(Sender: TObject;
  var Value: String);
begin
  FileNameEditOutput.FileName := Value;
  ReadFiles(False, True);
end;

procedure TMainForm.FileNameEditOutputKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then begin
    ReadFiles(False, True);
    Key := #0;
  end;
end;

procedure TMainForm.FormActivate(Sender: TObject);
const
  Initialized: Boolean = False;
begin
  if Initialized then
    Exit;
  InitAll;
  Initialized := True;
end;

procedure TMainForm.InitAll;
var
  WinState: TWindowState;
  i: Integer;
  TempS: string;
begin
  if Screen.Fonts.IndexOf('Arial Unicode MS') = -1 then
    EditDecodedUTF8.Font.Name := 'Microsoft Sans Serif';

  CreateDir(GetAppConfigDir(False));
  Config.FileName := GetAppConfigDir(False) + 'settings.xml';
  FileNameEditInput.FileName := Config.GetValue('InputFile', '');
  FileNameEditConversion.FileName := Config.GetValue('ConversionIni', '');
  FileNameEditUSEMARCON.FileName := Config.GetValue('USEMARCONExe', '');
  FileNameEditOutput.FileName := Config.GetValue('OutputFile', '');
  FileNameEditConversionChange(Self);

  SynEditInput.Font.Name := Config.GetValue('RecordFontName', SynEditInput.Font.Name);
  SynEditInput.Font.Size := Config.GetValue('RecordFontSize', SynEditInput.Font.Size);
  SynEditOutput.Font := SynEditInput.Font;

  WinState := TWindowState(Config.GetValue('WindowState', LongInt(Self.WindowState)));
  if WinState = wsMinimized then WinState := wsNormal;
  Self.WindowState := WinState;
  if WinState = wsNormal then begin
    Self.Left := Config.GetValue('WindowLeft', Self.Left);
    Self.Top := Config.GetValue('WindowTop', Self.Top);
    Self.Width := Config.GetValue('WindowWidth', Self.Width);
    Self.Height := Config.GetValue('WindowHeight', Self.Height);
  end;

  PanelInput.Width := Config.GetValue('InputPanelWidth', Self.ClientWidth div 2);
  PanelFiles.Height := Config.GetValue('FilePanelHeight', PanelFiles.Height);
  PanelBottom.Height := Config.GetValue('BottomPanelHeight', PanelBottom.Height);

  CheckBoxSynchronousScroll.Checked := Config.GetValue('SynchronousScroll', True);
  CheckBoxSplitSubfields.Checked := Config.GetValue('SplitSubfields', False);
  CheckBoxUTF8Autodetect.Checked := Config.GetValue('UTF8Autodetect', True);

  for i := 1 to 20 do begin
    TempS := Config.GetValue('RecentProject' + IntToStr(i), '');
    if TempS = '' then
      Break;
    FRecentProjects.Add(TempS);
  end;
  UpdateRecentProjectList;

{$IFDEF WINDOWS}
  if FileNameEditUSEMARCON.FileName = '' then
    FileNameEditUSEMARCON.FileName := 'C:\usemarcon\usemarcon.exe';
{$ENDIF}

  if ParamStr(1) <> '' then begin
    LoadProject(ParamStr(1));
  end;
end;

procedure TMainForm.ExceptionHandler(Sender: TObject; E: Exception);
begin
  ShowError(E.Message, '');
end;

procedure TMainForm.frReport1GetValue(const ParName: String;
  var ParValue: Variant);
var
  SynEdit: TSynEdit;
begin
  if FPrintInput then
    SynEdit := SynEditInput
  else
    SynEdit := SynEditOutput;

  if ParName = 'FileName' then begin
    if FPrintInput then
      ParValue := FileNameEditInput.FileName
    else
      ParValue := FileNameEditOutput.FileName;
  end
  else if ParName = 'RecNr' then
    ParValue := FCurrentPrintRecord
  else if ParName = 'FieldCode' then
    ParValue := Copy(SynEdit.Lines[FCurrentPrintLine], 1, 3)
  else if ParName = 'Indicators' then
    ParValue := Copy(SynEdit.Lines[FCurrentPrintLine], 5, 2)
  else if ParName = 'FieldData' then
    ParValue := Copy(SynEdit.Lines[FCurrentPrintLine], 7, MAXINT);
end;

procedure TMainForm.frUserDatasetRecordCheckEOF(Sender: TObject; var Eof: Boolean);
begin
  Eof := FCurrentPrintRecord <> FCurrentRecNr;
end;

procedure TMainForm.frUserDatasetRecordFirst(Sender: TObject);
begin
  FCurrentPrintRecord := FCurrentRecNr;
end;

procedure TMainForm.frUserDatasetRecordLineCheckEOF(Sender: TObject;
  var Eof: Boolean);
var
  SynEdit: TSynEdit;
begin
  if FPrintInput then
    SynEdit := SynEditInput
  else
    SynEdit := SynEditOutput;
  Eof := FCurrentPrintLine >= SynEdit.Lines.Count;
end;

procedure TMainForm.frUserDatasetRecordLineFirst(Sender: TObject);
begin
  FCurrentPrintLine := 0;
end;

procedure TMainForm.frUserDatasetRecordLineNext(Sender: TObject);
begin
  Inc(FCurrentPrintLine);
end;

procedure TMainForm.frUserDatasetRecordNext(Sender: TObject);
begin
  Inc(FCurrentPrintRecord);
end;

procedure TMainForm.BitBtnExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.BitBtnEditRuleFileClick(Sender: TObject);
begin
  EditRuleFile(0);
end;

procedure TMainForm.EditRuleFile(InitialLine: Integer);
var
  FileName: string;
begin
  FileName := GetINIPath(FileNameEditConversion.FileName, 'DEFAULT_FILES',
    'RuleFile');
  if FileName = '' then begin
    ShowMessage('Rule file not specified in INI file');
    Exit;
  end;
  if (FormRuleEditor.Execute(FileName, '', InitialLine) = mrRetry) and (BitBtnConvert.Enabled)
    then BitBtnConvert.Click;
end;

initialization
  {$I mainunit.lrs}

end.

