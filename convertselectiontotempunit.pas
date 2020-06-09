{
 /***************************************************************************
                            convertselectiontotempunit.pas
                            ------------------------------

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

unit ConvertSelectionToTempUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls;

type

  { TFormConvertSelectionToTemp }

  TFormConvertSelectionToTemp = class(TForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    CheckBoxForceNew: TCheckBox;
    EditTempField: TEdit;
    EditTargetField: TEdit;
    LabelTempField: TLabel;
    LabelTargetField: TLabel;
    MemoResult: TMemo;
    PanelButtons: TPanel;
    PanelBottom: TPanel;
    PanelOptions: TPanel;
    procedure CheckBoxForceNewChange(Sender: TObject);
    procedure EditTargetFieldChange(Sender: TObject);
    procedure EditTempFieldChange(Sender: TObject);
  private
    FSourceStr: string;

    procedure UpdateResult(Initial: Boolean = False);
  public
    function Execute(var CodeStr: string): Integer;
  end; 

var
  FormConvertSelectionToTemp: TFormConvertSelectionToTemp;

implementation

uses
  RegExpr;

{ TFormConvertSelectionToTemp }

procedure TFormConvertSelectionToTemp.EditTempFieldChange(Sender: TObject);
begin
  UpdateResult;
end;

procedure TFormConvertSelectionToTemp.EditTargetFieldChange(Sender: TObject);
begin
  UpdateResult;
end;

procedure TFormConvertSelectionToTemp.CheckBoxForceNewChange(Sender: TObject);
begin
  UpdateResult;
end;

procedure TFormConvertSelectionToTemp.UpdateResult(Initial: Boolean);

  procedure Justify(var S: string; Len: Integer);
  var
    i: Integer;
  begin
    for i := Length(S) + 1 to Len do begin
      S := S + ' ';
    end;
  end;

var
  Lines: TStrings;
  Line: string;
  i, p: Integer;
  TempField: string;
  TargetField: string;
  RegExp: TRegExpr;
  Source: string;
  Dest: string;
  Rule: string;
  SourceLen: Integer;
  DestLen: Integer;
const
  Running: Boolean = False;
begin
  if Running then
    Exit;
  MemoResult.Lines.Clear;
  RegExp := TRegExpr.Create();
  Lines := TStringList.Create;
  Running := True;
  try
    Source := FSourceStr;
    i := Pos(#10, Source);
    while i > 0 do begin
      TempField := Copy(Source, 1, i - 1);
      if Copy(TempField, Length(TempField), 1) = #13 then
        TempField := Copy(TempField, 1, Length(TempField) - 1);
      Lines.Add(TempField);
      Source := Copy(Source, i + 1, MAXINT);
      i := Pos(#10, Source);
    end;
    if Source <> '' then
      Lines.Add(Source);
    SourceLen := 0;
    DestLen := 0;
    // Count column widths and set defaults for fields
    RegExp.Expression := '(.*)\|(.*)\|(.*)';
    for i := 0 to Lines.Count - 1 do begin
      Line := Lines[i];
      if RegExp.Exec(Line) then begin
        Source := RegExp.Match[1];
        Dest := RegExp.Match[2];
        if Length(Source) > SourceLen then
          SourceLen := Length(Source);
        if Length(Dest) > DestLen then
          DestLen := Length(Dest);
        if Initial then begin
          while Source[1] = ' ' do
            Source := Copy(Source, 2, MAXINT);
          while Dest[1] = ' ' do
            Dest := Copy(Dest, 2, MAXINT);
          EditTargetField.Text := Copy(Dest, 1, 3);
          if (Source[1] >= '0') and (Source[1] <= '9') then
            Dest := Chr(Ord(Source[1]) + 17) + Copy(Source, 2, 2);
          EditTempField.Text := Dest;
        end;
      end;
    end;
    if CheckBoxForceNew.Checked then begin
      if DestLen < 15 then
        DestLen := 15;
    end
    else begin
      if DestLen < 8 then
        DestLen := 8;
    end;
    // Do the work
    RegExp.Expression := '^(.*)\|(.*?)\s*\|(.*)$';
    for i := 0 to Lines.Count - 1 do begin
      Line := Lines[i];
      if RegExp.Exec(Line) then begin
        Source := RegExp.Match[1];
        Dest := RegExp.Match[2];
        Rule := RegExp.Match[3];
        p := Pos(')', Dest);
        if (p > 0) then
          Dest := Copy(Dest, p + 1, MAXINT)
        else begin
          while Copy(Dest, 1, 1) = ' ' do
            Dest := Copy(Dest, 2, MAXINT);
          Dest := Copy(Dest, 4, MAXINT);
        end;
        TempField := ' <' + EditTempField.Text + Dest + ' ';
        Justify(TempField, DestLen);
        Justify(Source, SourceLen);
        MemoResult.Lines.Add(Source + '|' + TempField + '|' + Rule);
      end
      else begin
        MemoResult.Lines.Add(Line);
      end;
    end;

    TempField := EditTempField.Text + 'I1 ';
    Justify(TempField, SourceLen);
    TargetField := ' ' + EditTargetField.Text;
    if CheckBoxForceNew.Checked then
      TargetField := TargetField + '(new)';
    TargetField := TargetField + 'I1 ';
    Justify(TargetField, DestLen);
    MemoResult.Lines.add(TempField + '|' + TargetField + '| S');

    TempField := EditTempField.Text + 'I2 ';
    Justify(TempField, SourceLen);
    TargetField := ' ' + EditTargetField.Text;
    if CheckBoxForceNew.Checked then
      TargetField := TargetField + '(newest)';
    TargetField := TargetField + 'I2 ';
    Justify(TargetField, DestLen);
    MemoResult.Lines.add(TempField + '|' + TargetField + '| S');

    TempField := EditTempField.Text + ' ';
    Justify(TempField, SourceLen);
    TargetField := ' ' + EditTargetField.Text;
    if CheckBoxForceNew.Checked then
      TargetField := TargetField + '(newest)';
    Justify(TargetField, DestLen);
    MemoResult.Lines.add(TempField + '|' + TargetField + '| S');
  finally
    FreeAndNil(RegExp);
    FreeAndNil(Lines);
    Running := False;
  end;
end;

function TFormConvertSelectionToTemp.Execute(var CodeStr: string): Integer;
begin
  FSourceStr := CodeStr;
  UpdateResult(True);
  Result := ShowModal;
  if Result = mrOk then
    CodeStr := MemoResult.Text;
end;

initialization
  {$I convertselectiontotempunit.lrs}

end.

