{
 /***************************************************************************
                            USEMARCONGUI.lpr
                            ----------------------

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

program USEMARCONGUI;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms, lazreport
  { add your units here }, MainUnit, RuleEditorUnit, INIEditorUnit, ExportUnit,
  QuestionDialog, SearchReplaceUnit, RecordConverterUnit, Int64List,
  BufferedFile, ConvertSelectionToTempUnit, SynHighlighterMARC,
  SynHighlighterRule;

{$IFDEF WIN32}
  {$R *.res}
{$ENDIF}

//{$IFDEF WINDOWS}{$R USEMARCONGUI.rc}{$ENDIF}

begin
  Application.Title:='USEMARCON GUI';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFormRuleEditor, FormRuleEditor);
  Application.CreateForm(TFormINIEditor, FormINIEditor);
  Application.CreateForm(TFormExport, FormExport);
  Application.CreateForm(TQuestionForm, QuestionForm);
  Application.CreateForm(TFormSearchReplace, FormSearchReplace);

  FormINIEditor.Position := poMainFormCenter;
  Application.CreateForm(TFormConvertSelectionToTemp, FormConvertSelectionToTemp);
  Application.Run;
end.

