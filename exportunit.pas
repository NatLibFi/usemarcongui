{
 /***************************************************************************
                            exportunit.pas
                            --------------

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

unit ExportUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, EditBtn, Buttons, ComCtrls;

type

  { TFormExport }

  TFormExport = class(TForm)
    ButtonStop: TButton;
    ButtonExport: TButton;
    ButtonClose: TButton;
    ComboBoxFormat: TComboBox;
    EditFrom: TEdit;
    EditTo: TEdit;
    LabelFormat: TLabel;
    LabelRange: TLabel;
    Label2: TLabel;
    RadioGroupSource: TRadioGroup;
    SaveDialog: TSaveDialog;
    StatusBar1: TStatusBar;
    procedure ButtonExportClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure RadioGroupSourceClick(Sender: TObject);
  private
    FRunning: Boolean;
    FStop: Boolean;
    FRecords: LongInt;
    
    procedure WriteText(var F: file; const S: string);
  public
  end;

var
  FormExport: TFormExport;

implementation

uses
  MainUnit, RecordConverterUnit;

{ TFormExport }

procedure TFormExport.ButtonExportClick(Sender: TObject);
var
  ExportFrom: Integer;
  ExportTo: Integer;
  F: file;
  FOpen: Boolean;
  i: Integer;
  Rec: string;
  Input: Boolean;
  XML: Boolean;
  Converter: TRecordConverter;
  Success: Boolean;
begin
  if not SaveDialog.Execute or (SaveDialog.Filename = '') then
    Exit;
    
  Self.Cursor := crHourGlass;
  ButtonExport.Enabled := False;
  ButtonClose.Enabled := False;
  ButtonStop.Enabled := True;
  FRunning := True;
  FStop := False;
  Input := RadioGroupSource.ItemIndex = 0;
  XML := ComboBoxFormat.ItemIndex > 0;
  Converter := nil;
  FOpen := False;
  try
    ExportFrom := StrToInt(EditFrom.Text);
    ExportTo := StrToInt(EditTo.Text);

    if ExportFrom < 1 then begin
      ShowMessage('First record number is too low');
      Exit;
    end;
    if ExportTo > FRecords then begin
      ShowMessage('Last record number is too high');
      Exit;
    end;
    
    try
      AssignFile(F, SaveDialog.Filename);
      Rewrite(F, 1);
      FOpen := True;
    except
      on E: Exception do begin
        MainForm.ShowError('Could not open file ' + SaveDialog.Filename +
          ' for writing', E.Message);
        Exit;
      end;
    end;
    if XML then begin
      Converter := TRecordConverter.Create;
      WriteText(F,
'<?xml version="1.0"?>' + #10 +
#10 +
'<collection>' + #10);
    end;
    for i := ExportFrom to ExportTo do begin
      if i mod 117 = 0 then begin
        StatusBar1.SimpleText := 'Exporting record ' + IntToStr(i) + '/' +
          IntToStr(ExportTo) + '...';
        Application.ProcessMessages;
        if FStop then
          Break;
      end;
      Rec := MainForm.GetMarcData(Input, i - 1, Success);
      if not Success then
        Break;
      if XML then begin
        WriteText(F, Converter.ConvertToXML(Rec));
      end
      else
        WriteText(F, Rec);
    end;
    if XML then begin
      WriteText(F,
'</collection>' + #10);
    end;
    StatusBar1.SimpleText := 'Export complete';
  finally
    if FOpen then
      CloseFile(F);
    FRunning := False;
    ButtonExport.Enabled := True;
    ButtonClose.Enabled := True;
    ButtonStop.Enabled := False;
    Self.Cursor := crDefault;
    ModalResult := mrOk;
    FreeAndNil(Converter);
  end;
end;

procedure TFormExport.ButtonStopClick(Sender: TObject);
begin
  FStop := True;
end;

procedure TFormExport.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := not FRunning;
end;

procedure TFormExport.FormShow(Sender: TObject);
begin
  EditFrom.Text := IntToStr(MainForm.CurrentRecNr);
  EditTo.Text := EditFrom.Text;
  RadioGroupSourceClick(Self);
end;

procedure TFormExport.RadioGroupSourceClick(Sender: TObject);
begin
  if RadioGroupSource.ItemIndex = 1 then
    FRecords := MainForm.OutputCount
  else
    FRecords := MainForm.InputCount;
    
  LabelRange.Caption := 'Export records (' + IntToStr(FRecords) +
    ' records available)';
end;

procedure TFormExport.WriteText(var F: file; const S: string);
var
  P: PChar;
begin
  P := PChar(S);
  BlockWrite(F, P^, Length(S));
end;

initialization
  {$I exportunit.lrs}

end.

