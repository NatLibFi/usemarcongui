{
 /***************************************************************************
                            searchreplaceunit.pas
                            ---------------------

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

unit SearchReplaceUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, SynEditTypes;

type

  { TFormSearchReplace }

  TFormSearchReplace = class(TForm)
    BitBtnReplaceAll: TBitBtn;
    BitBtnFindReplace: TBitBtn;
    BitBtnCancel: TBitBtn;
    CBCaseSensitive: TCheckBox;
    CBWholeWords: TCheckBox;
    CBRegExp: TCheckBox;
    CBMultiline: TCheckBox;
    CBPrompt: TCheckBox;
    CBBackwards: TCheckBox;
    EditFind: TEdit;
    EditReplace: TEdit;
    GroupBoxOptions: TGroupBox;
    LabelFind: TLabel;
    LabelReplace: TLabel;
    RadioGroupOrigin: TRadioGroup;
    RadioGroupScope: TRadioGroup;
    procedure FormShow(Sender: TObject);
  private
    function GetBackwards: Boolean;
    function GetCaseSensitive: Boolean;
    function GetFindText: string;
    function GetFromCursor: Boolean;
    function GetMultiline: Boolean;
    function GetPrompt: Boolean;
    function GetRegExp: Boolean;
    function GetReplaceText: string;
    function GetSearchOptions: TSynSearchOptions;
    function GetSelectionOnly: Boolean;
    function GetWholeWords: Boolean;
  public
    property FindText: string read GetFindText;
    property ReplaceText: string read GetReplaceText;
    property CaseSensitive: Boolean read GetCaseSensitive;
    property WholeWords: Boolean read GetWholeWords;
    property RegExp: Boolean read GetRegExp;
    property Multiline: Boolean read GetMultiline;
    property Prompt: Boolean read GetPrompt;
    property Backwards: Boolean read GetBackwards;
    property FromCursor: Boolean read GetFromCursor;
    property SelectionOnly: Boolean read GetSelectionOnly;
    property Options: TSynSearchOptions read GetSearchOptions;
  
    function Execute(Replace: Boolean): Integer;
  end;

var
  FormSearchReplace: TFormSearchReplace;

implementation

{ TFormSearchReplace }

procedure TFormSearchReplace.FormShow(Sender: TObject);
begin
  if EditFind.CanFocus then
    EditFind.SetFocus;
end;

function TFormSearchReplace.GetBackwards: Boolean;
begin
  Result := CBBackwards.Checked;
end;

function TFormSearchReplace.GetCaseSensitive: Boolean;
begin
  Result := CBCaseSensitive.Checked;
end;

function TFormSearchReplace.GetFindText: string;
begin
  Result := EditFind.Text;
end;

function TFormSearchReplace.GetFromCursor: Boolean;
begin
  Result := RadioGroupOrigin.ItemIndex = 0;
end;

function TFormSearchReplace.GetMultiline: Boolean;
begin
  Result := CBMultiline.Checked;
end;

function TFormSearchReplace.GetPrompt: Boolean;
begin
  Result := CBPrompt.Checked;
end;

function TFormSearchReplace.GetRegExp: Boolean;
begin
  Result := CBRegExp.Checked;
end;

function TFormSearchReplace.GetReplaceText: string;
begin
  Result := EditReplace.Text;
end;

function TFormSearchReplace.GetSearchOptions: TSynSearchOptions;
begin
  Result := [];
  if CBCaseSensitive.Checked then
    Include(Result, ssoMatchCase);
  if CBWholeWords.Checked then
    Include(Result, ssoWholeWord);
  if CBRegExp.Checked then
    Include(Result, ssoRegExpr);
  if CBMultiline.Checked then
    Include(Result, ssoRegExprMultiline);
  if CBBackwards.Checked then
    Include(Result, ssoBackwards);
  if CBPrompt.Checked then
    Include(Result, ssoPrompt);
  if RadioGroupOrigin.ItemIndex = 1 then
    Include(Result, ssoEntireScope);
  if RadioGroupScope.ItemIndex = 0 then
    Include(Result, ssoSelectedOnly);
  if CBPrompt.Enabled then
    Include(Result, ssoReplace);
end;

function TFormSearchReplace.GetSelectionOnly: Boolean;
begin
  Result := RadioGroupScope.ItemIndex = 0;
end;

function TFormSearchReplace.GetWholeWords: Boolean;
begin
  Result := CBWholeWords.Checked;
end;

function TFormSearchReplace.Execute(Replace: Boolean): Integer;
begin
  LabelReplace.Enabled := Replace;
  EditReplace.Enabled := Replace;
  BitBtnReplaceAll.Enabled := Replace;
  BitBtnReplaceAll.Visible := Replace;
  CBPrompt.Enabled := Replace;
  if Replace then begin
    Caption := 'Replace';
    BitBtnFindReplace.Caption := '&Replace';
  end
  else begin
    Caption := 'Find';
    BitBtnFindReplace.Caption := '&Find';
  end;
  Result := ShowModal;
end;

initialization
  {$I searchreplaceunit.lrs}

end.

