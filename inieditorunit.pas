{
 /***************************************************************************
                            inieditorunit.pas
                            -----------------

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

unit INIEditorUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, ComCtrls, StdCtrls, MaskEdit, Buttons;

type

  { TFormINIEditor }

  TFormINIEditor = class(TForm)
    AMainMenu1: TMainMenu;
    Bevel1: TBevel;
    BitBtnSaveAndClose: TBitBtn;
    BitBtnSave: TBitBtn;
    BitBtnClose: TBitBtn;
    CheckBoxConvertSubfieldsToLowercase: TCheckBox;
    CheckBoxHandleLinkedFields: TCheckBox;
    CheckBoxFieldOrder: TCheckBox;
    CheckBoxUTF8Mode: TCheckBox;
    CheckBoxInputLastBlockPadded: TCheckBox;
    CheckBoxOutputLastBlockPadded: TCheckBox;
    CheckBoxUpdateOrdinal: TCheckBox;
    CheckBoxVerboseMode: TCheckBox;
    CheckBoxRuleDebugMode: TCheckBox;
    CheckBoxInputSegmented: TCheckBox;
    CheckBoxOutputSegmented: TCheckBox;
    ComboBoxDuplicateSubfields: TComboBox;
    ComboBoxDuplicateFields: TComboBox;
    ComboBoxInputCharset: TComboBox;
    ComboBoxInputFormat: TComboBox;
    ComboBoxOutputFormat: TComboBox;
    ComboBoxOutputXMLFormat: TComboBox;
    ComboBoxOutputXMLType: TComboBox;
    EditErrorLogFile: TEdit;
    EditRuleFile: TEdit;
    EditOutputPaddingChar: TEdit;
    EditCharacterConversionFile: TEdit;
    EditInputFormatCheckingFile: TEdit;
    EditOutputFormatCheckingFile: TEdit;
    EditInputFixedFieldFile: TEdit;
    EditOutputFixedFieldFile: TEdit;
    EditDefaultInputFile: TEdit;
    EditDefaultOutputFile: TEdit;
    EditInputPaddingChar: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MaskEditInputBlockSize: TMaskEdit;
    MaskEditInputMinFreeData: TMaskEdit;
    MaskEditOutputBlockSize: TMaskEdit;
    MaskEditOutputMinFreeData: TMaskEdit;
    MaskEditMaxErrors: TMaskEdit;
    MaskEditOrdinal: TMaskEdit;
    MenuItemSave: TMenuItem;
    MenuItemClose: TMenuItem;
    MenuItemFile: TMenuItem;
    NotebookINI: TNotebook;
    PageFiles: TPage;
    PageFileAttributes: TPage;
    PageOptions: TPage;
    PanelTop: TPanel;
    StatusBar1: TStatusBar;
    TimerInit: TTimer;
    procedure BitBtnSaveClick(Sender: TObject);
    procedure BitBtnSaveAndCloseClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure FormINIEditorCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormINIEditorCreate(Sender: TObject);
    procedure FormINIEditorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormINIEditorKeyPress(Sender: TObject; var Key: char);
    procedure FormINIEditorShow(Sender: TObject);
    procedure MenuItemCloseClick(Sender: TObject);
    procedure MenuItemSaveClick(Sender: TObject);
    procedure PageOptionsContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure PanelTopClick(Sender: TObject);
    procedure TimerInitTimer(Sender: TObject);
  private
    procedure SetChanges(const AValue: Boolean);
  private
    FChanges: Boolean;
    FFileName: string;

    procedure LoadSettings;
    procedure SaveSettings;
  
    property Changes: Boolean read FChanges write SetChanges;
  public
    function Execute(ININame: string): Integer;
  end;
  
  { TRegFile }

  TRegFile = class(TObject)
  private
    FFileName: string;
  public
    constructor Create(FileName: string);
    destructor Destroy; override;
    
    function ReadString(Section: string; Name: string): string;
    procedure WriteString(Section: string; Name: string; Value: string);
  end;

var
  FormINIEditor: TFormINIEditor;

implementation

uses
  RegExpr;

{ TFormINIEditor }

procedure TFormINIEditor.MenuItemCloseClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormINIEditor.MenuItemSaveClick(Sender: TObject);
begin
  BitBtnSave.Click;
end;

procedure TFormINIEditor.PageOptionsContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TFormINIEditor.PanelTopClick(Sender: TObject);
begin

end;

procedure TFormINIEditor.TimerInitTimer(Sender: TObject);
begin
  TimerInit.Enabled := False;
  Changes := False;
end;

procedure TFormINIEditor.SetChanges(const AValue: Boolean);
begin
  FChanges := AValue;
  if FChanges then
    StatusBar1.SimpleText := 'Modified'
  else
    StatusBar1.SimpleText := '';
end;

procedure TFormINIEditor.LoadSettings;

  function IsTrue(St: string): Boolean;
  begin
    Result := UpperCase(St) = 'TRUE';
  end;
  
var
  RegFile: TRegFile;
begin
  RegFile := TRegFile.Create(FFileName);
  try
    EditRuleFile.Text := RegFile.ReadString('DEFAULT_FILES', 'RuleFile');
    EditCharacterConversionFile.Text := RegFile.ReadString('DEFAULT_FILES',
      'TranscodingCharacterTable');
    EditErrorLogFile.Text := RegFile.ReadString('DEFAULT_FILES', 'ErrorLogFile');

    EditInputFormatCheckingFile.Text := RegFile.ReadString('DEFAULT_FILES',
      'InputFormatCheckingTable');
    EditInputFixedFieldFile.Text := RegFile.ReadString('DEFAULT_FILES',
      'InputMarcEditConfigurationFile');
    EditDefaultInputFile.Text := RegFile.ReadString('DEFAULT_FILES',
      'MarcInputFile');

    EditOutputFormatCheckingFile.Text := RegFile.ReadString('DEFAULT_FILES',
      'OutputFormatCheckingTable');
    EditOutputFixedFieldFile.Text := RegFile.ReadString('DEFAULT_FILES',
      'OutputMarcEditConfigurationFile');
    EditDefaultOutputFile.Text := RegFile.ReadString('DEFAULT_FILES',
      'MarcOutputFile');
      
    CheckBoxInputSegmented.Checked :=
      isTrue(RegFile.ReadString('DEFAULT_MARC_ATTRIBUTES',
      'IsInputBlockSegmentedChecked'));
    MaskEditInputBlockSize.Text := RegFile.ReadString('DEFAULT_MARC_ATTRIBUTES',
      'InputMarcSizeBlock');
    EditInputPaddingChar.Text := RegFile.ReadString('DEFAULT_MARC_ATTRIBUTES',
      'InputMarcPaddingChar');
    MaskEditInputMinFreeData.Text := RegFile.ReadString('DEFAULT_MARC_ATTRIBUTES',
      'InputMarcMinDataFree');
    CheckBoxInputLastBlockPadded.Checked :=
      IsTrue(RegFile.ReadString('DEFAULT_MARC_ATTRIBUTES',
      'IsInputLastBlockPaddedChecked'));
    ComboBoxInputFormat.Text := RegFile.ReadString('DEFAULT_MARC_ATTRIBUTES',
      'InputFileFormat');
    ComboBoxInputCharset.Text := RegFile.ReadString('DEFAULT_MARC_ATTRIBUTES',
      'InputFileCharacterSet');

    CheckBoxOutputSegmented.Checked :=
      IsTrue(RegFile.ReadString('DEFAULT_MARC_ATTRIBUTES',
      'IsOutputBlockSegmentedChecked'));
    MaskEditOutputBlockSize.Text := RegFile.ReadString('DEFAULT_MARC_ATTRIBUTES',
      'OutputMarcSizeBlock');
    EditOutputPaddingChar.Text := RegFile.ReadString('DEFAULT_MARC_ATTRIBUTES',
      'OutputMarcPaddingChar');
    MaskEditOutputMinFreeData.Text := RegFile.ReadString('DEFAULT_MARC_ATTRIBUTES',
      'OutputMarcMinDataFree');
    CheckBoxOutputLastBlockPadded.Checked :=
      IsTrue(RegFile.ReadString('DEFAULT_MARC_ATTRIBUTES',
      'IsOutputLastBlockPaddedChecked'));
    ComboBoxOutputFormat.Text := RegFile.ReadString('DEFAULT_MARC_ATTRIBUTES',
      'OutputFileFormat');
    ComboBoxOutputXMLType.Text := RegFile.ReadString('DEFAULT_MARC_ATTRIBUTES',
      'OutputXMLRecordType');
    ComboBoxOutputXMLFormat.Text := RegFile.ReadString('DEFAULT_MARC_ATTRIBUTES',
      'OutputXMLRecordFormat');

    MaskEditMaxErrors.Text := RegFile.ReadString('DEFAULT_VALUES',
      'MaxErrorsToBeEncountered');
    if MaskEditMaxErrors.Text = '' then
      MaskEditMaxErrors.Text := RegFile.ReadString('DEFAULT_VALUES',
        'MaxErrorsToBeEncoutered');
    MaskEditOrdinal.Text := RegFile.ReadString('DEFAULT_VALUES',
      'OrdinalNumber');
    CheckBoxUpdateOrdinal.Checked := IsTrue(RegFile.ReadString('DEFAULT_VALUES',
      'UpdateOrdinalNumber'));
    CheckBoxVerboseMode.Checked := IsTrue(RegFile.ReadString('DEFAULT_STATES',
      'IsVerboseExecutionModeChecked'));
    CheckBoxUTF8Mode.Checked := IsTrue(RegFile.ReadString('DEFAULT_STATES',
      'UTF8Mode'));
    CheckBoxFieldOrder.Checked := IsTrue(RegFile.ReadString('DEFAULT_STATES',
      'ConvertInFieldOrder'));
    CheckBoxConvertSubfieldsToLowercase.Checked := IsTrue(RegFile.ReadString('DEFAULT_STATES',
      'ConvertSubfieldCodesToLowercase'));
    CheckBoxHandleLinkedFields.Checked := IsTrue(RegFile.ReadString('DEFAULT_STATES',
      'HandleLinkedFields'));
    ComboBoxDuplicateSubfields.Text := RegFile.ReadString('DEFAULT_VALUES',
      'DuplicateSubfields');
    ComboBoxDuplicateFields.Text := RegFile.ReadString('DEFAULT_VALUES',
      'DuplicateFields');

    CheckBoxRuleDebugMode.Checked := IsTrue(RegFile.ReadString('DEBUG',
      'IsDebugExecutionModeChecked'));
  finally
    FreeAndNil(RegFile);
  end;
  TimerInit.Enabled := True;
end;

procedure TFormINIEditor.SaveSettings;

  function BoolStr(B: Boolean): string;
  begin
    if B then
      Result := 'TRUE'
    else
      Result := 'FALSE';
  end;

var
  RegFile: TRegFile;
begin
  RegFile := TRegFile.Create(FFileName);
  try
    RegFile.WriteString('DEFAULT_FILES', 'RuleFile', EditRuleFile.Text);
    RegFile.WriteString('DEFAULT_FILES', 'TranscodingCharacterTable',
      EditCharacterConversionFile.Text);
    RegFile.WriteString('DEFAULT_FILES', 'ErrorLogFile', EditErrorLogFile.Text);

    RegFile.WriteString('DEFAULT_FILES', 'InputFormatCheckingTable',
      EditInputFormatCheckingFile.Text);
    RegFile.WriteString('DEFAULT_FILES', 'InputMarcEditConfigurationFile',
      EditInputFixedFieldFile.Text);
    RegFile.WriteString('DEFAULT_FILES', 'MarcInputFile',
      EditDefaultInputFile.Text);

    RegFile.WriteString('DEFAULT_FILES', 'OutputFormatCheckingTable',
      EditOutputFormatCheckingFile.Text);
    RegFile.WriteString('DEFAULT_FILES', 'OutputMarcEditConfigurationFile',
      EditOutputFixedFieldFile.Text);
    RegFile.WriteString('DEFAULT_FILES', 'MarcOutputFile',
      EditDefaultOutputFile.Text);

    RegFile.WriteString('DEFAULT_MARC_ATTRIBUTES', 'IsInputBlockSegmentedChecked',
      BoolStr(CheckBoxInputSegmented.Checked));
    RegFile.WriteString('DEFAULT_MARC_ATTRIBUTES', 'InputMarcSizeBlock',
      MaskEditInputBlockSize.Text);
    RegFile.WriteString('DEFAULT_MARC_ATTRIBUTES', 'InputMarcPaddingChar',
      EditInputPaddingChar.Text);
    RegFile.WriteString('DEFAULT_MARC_ATTRIBUTES', 'InputMarcMinDataFree',
      MaskEditInputMinFreeData.Text);
    RegFile.WriteString('DEFAULT_MARC_ATTRIBUTES', 'IsInputLastBlockPaddedChecked',
      BoolStr(CheckBoxInputLastBlockPadded.Checked));
    RegFile.WriteString('DEFAULT_MARC_ATTRIBUTES', 'InputFileFormat',
      ComboBoxInputFormat.Text);
    RegFile.WriteString('DEFAULT_MARC_ATTRIBUTES', 'InputFileCharacterSet',
      ComboBoxInputCharset.Text);

    RegFile.WriteString('DEFAULT_MARC_ATTRIBUTES', 'IsOutputBlockSegmentedChecked',
      BoolStr(CheckBoxOutputSegmented.Checked));
    RegFile.WriteString('DEFAULT_MARC_ATTRIBUTES', 'OutputMarcSizeBlock',
      MaskEditOutputBlockSize.Text);
    RegFile.WriteString('DEFAULT_MARC_ATTRIBUTES', 'OutputMarcPaddingChar',
      EditOutputPaddingChar.Text);
    RegFile.WriteString('DEFAULT_MARC_ATTRIBUTES', 'OutputMarcMinDataFree',
      MaskEditOutputMinFreeData.Text);
    RegFile.WriteString('DEFAULT_MARC_ATTRIBUTES', 'IsOutputLastBlockPaddedChecked',
      BoolStr(CheckBoxOutputLastBlockPadded.Checked));
    RegFile.WriteString('DEFAULT_MARC_ATTRIBUTES', 'OutputFileFormat',
      ComboBoxOutputFormat.Text);
    RegFile.WriteString('DEFAULT_MARC_ATTRIBUTES', 'OutputXMLRecordType',
      ComboBoxOutputXMLType.Text);
    RegFile.WriteString('DEFAULT_MARC_ATTRIBUTES', 'OutputXMLRecordFormat',
      ComboBoxOutputXMLFormat.Text);

    RegFile.WriteString('DEFAULT_VALUES', 'MaxErrorsToBeEncountered',
      MaskEditMaxErrors.Text);
    RegFile.WriteString('DEFAULT_VALUES', 'OrdinalNumber',
      MaskEditOrdinal.Text);
    RegFile.WriteString('DEFAULT_VALUES', 'UpdateOrdinalNumber',
      BoolStr(CheckBoxUpdateOrdinal.Checked));
    RegFile.WriteString('DEFAULT_STATES', 'IsVerboseExecutionModeChecked',
      BoolStr(CheckBoxVerboseMode.Checked));
    RegFile.WriteString('DEFAULT_STATES', 'UTF8Mode',
      BoolStr(CheckBoxUTF8Mode.Checked));
    RegFile.WriteString('DEFAULT_STATES', 'ConvertInFieldOrder',
      BoolStr(CheckBoxFieldOrder.Checked));
    RegFile.WriteString('DEFAULT_STATES', 'ConvertSubfieldCodesToLowercase',
      BoolStr(CheckBoxConvertSubfieldsToLowercase.Checked));
    RegFile.WriteString('DEFAULT_STATES', 'HandleLinkedFields',
      BoolStr(CheckBoxHandleLinkedFields.Checked));
    RegFile.WriteString('DEFAULT_VALUES', 'DuplicateSubfields',
      ComboBoxDuplicateSubfields.Text);
    RegFile.WriteString('DEFAULT_VALUES', 'DuplicateFields',
      ComboBoxDuplicateFields.Text);

    RegFile.WriteString('DEBUG', 'IsDebugExecutionModeChecked',
      BoolStr(CheckBoxRuleDebugMode.Checked));
      
    Changes := False;
  finally
    FreeAndNil(RegFile);
  end;
end;

procedure TFormINIEditor.EditChange(Sender: TObject);
begin
  Changes := True;
end;

procedure TFormINIEditor.BitBtnSaveAndCloseClick(Sender: TObject);
begin
  SaveSettings;
  ModalResult := mrOk;
end;

procedure TFormINIEditor.BitBtnSaveClick(Sender: TObject);
begin
  SaveSettings;
end;

procedure TFormINIEditor.FormINIEditorCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  CanClose := False;
  if Changes then begin
    case MessageDlg('INI Editor', 'Save changes?', mtConfirmation, mbYesNoCancel, 0) of
      mrYes: SaveSettings;
      mrNo: ;
      else Exit;
    end;
  end;
  CanClose := True;
end;

procedure TFormINIEditor.FormINIEditorCreate(Sender: TObject);
begin
  NotebookINI.PageIndex := 0;
end;

procedure TFormINIEditor.FormINIEditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

procedure TFormINIEditor.FormINIEditorKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #9 then
    Key := #0;
end;

procedure TFormINIEditor.FormINIEditorShow(Sender: TObject);
begin
  if EditRuleFile.CanFocus then
    EditRuleFile.SetFocus;
end;

function TFormINIEditor.Execute(ININame: string): Integer;
begin
  Result := mrCancel;
  FFileName := ININame;
  if not FileExists(ININame) then begin
    case MessageDlg('INI Editor', 'INI File does not exist. Create a new file?',
     mtConfirmation, mbYesNo, 0) of
      mrYes: ;
      else Exit;
    end;
  end;
  LoadSettings;

  Result := ShowModal;
end;

{ TRegFile }

constructor TRegFile.Create(FileName: string);
begin
  inherited Create;
  FFileName := FileName;
end;

destructor TRegFile.Destroy;
begin
  inherited Destroy;
end;

function TRegFile.ReadString(Section: string; Name: string): string;
var
  F: TextFile;
  Line: string;
  SectionOk: Boolean;
  RegExp: TRegExpr;
begin
  Result := '';
  if not FileExists(FFileName) then
    Exit;
  AssignFile(F, FFileName);
  Reset(F);
  RegExp := TRegExpr.Create;
  RegExp.ModifierI := True;
  try
    SectionOk := False;
    while not Eof(F) do begin
      ReadLn(F, Line);
      Line := Trim(Line);

      if Copy(Line, 1, 1) = '[' then begin
        // Section, check if it's the right one
        RegExp.Expression := '\[' + Section + '\]';
        SectionOk := RegExp.Exec(Line);
        Continue;
      end;
      if SectionOk then begin
        RegExp.Expression := '^' + Name + '=(.*)';
        if RegExp.Exec(Line) and (RegExp.SubExprMatchCount = 1) then begin
          Result := RegExp.Match[1];
          Break;
        end;
      end;
    end;
  finally
    CloseFile(F);
    FreeAndNil(RegExp);
  end;
end;

procedure TRegFile.WriteString(Section: string; Name: string; Value: string);
var
  InF: TextFile;
  OutF: TextFile;
  FullLine: string;
  Line: string;
  RegExp: TRegExpr;
  InFileOk: Boolean;
  SectionOk: Boolean;
  WasOk: Boolean;
  ValueWritten: Boolean;
  EmptyLinesNeeded: Integer;
  i: Integer;
begin
  if not FileExists(FFileName) then begin
    InFileOk := False;
    AssignFile(OutF, FFileName);
  end
  else begin
    InFileOk := True;
    AssignFile(InF, FFileName);
    Reset(InF);
    AssignFile(OutF, FFileName + '.$$tmp');
  end;
  Rewrite(OutF);
  RegExp := TRegExpr.Create;
  RegExp.ModifierI := True;
  ValueWritten := False;
  EmptyLinesNeeded := 0;
  try
    SectionOk := False;
    if InFileOk then begin
      while not Eof(InF) do begin
        ReadLn(InF, FullLine);
        Line := Trim(FullLine);

        if Copy(Line, 1, 1) = '[' then begin
          // Section, check if it's the right one
          RegExp.Expression := '\[' + Section + '\]';
          WasOk := SectionOk;
          SectionOk := RegExp.Exec(Line);
          if WasOk and not SectionOk and not ValueWritten then begin
            // There was no previous value, add this now to the end of the section
            WriteLn(OutF, Name + '=' + Value);
            ValueWritten := True;
            for i := 1 to EmptyLinesNeeded do
              WriteLn(OutF);
            EmptyLinesNeeded := 0;
          end;
        end;
        if SectionOk then begin
          RegExp.Expression := '^' + Name + '=(.*)';
          if RegExp.Exec(Line) and (RegExp.SubExprMatchCount = 1) then begin
            FullLine := Name + '=' + Value;
            ValueWritten := True;
          end;
        end;
        if Line <> '' then begin
          for i := 1 to EmptyLinesNeeded do
            WriteLn(OutF);
          EmptyLinesNeeded := 0;

          WriteLn(OutF, FullLine);
        end
        else
          Inc(EmptyLinesNeeded);
      end;
    end;
    if not ValueWritten then begin
      if not SectionOk then begin
        WriteLn(OutF);
        WriteLn(OutF, '[' + Section + ']');
      end;
      WriteLn(OutF, Name + '=' + Value);
    end;
  finally
    if InFileOk then
      CloseFile(InF);
    CloseFile(OutF);
    FreeAndNil(RegExp);

    if InFileOk then begin
      RenameFile(FFileName, FFilename + '.$$tmp2');
      try
        RenameFile(FFileName + '.$$tmp', FFileName);
      except
        RenameFile(FFileName + '.$$tmp2', FFileName);
        raise;
      end;
      DeleteFile(FFileName + '.$$tmp2');
    end;
  end;
end;

initialization
  {$I inieditorunit.lrs}

end.

