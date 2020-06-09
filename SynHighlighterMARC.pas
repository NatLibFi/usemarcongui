{
 /***************************************************************************
                            SynHighlighterMARC.pas
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

unit SynHighlighterMARC;

{$I SynEdit.inc}

interface

uses
  SysUtils, Classes,
  LCLIntf, LCLType,
  Controls, Graphics,
  SynEditTypes, SynEditHighlighter;

type
  TtkTokenKind = (tkNull, tkSpace, tkText, tkField, tkIndicator, tkSubfield,
    tkFixed1, tkFixed2, tkEndField);

  TProcTableProc = procedure of object;

{$H+}

type
  { TSynMARCSyn }

  TSynMARCSyn = class(TSynCustomHighlighter)
  private
    FLine: PChar;
    FLineNumber: Integer;
    FProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    FTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FTextAttrs: TSynHighlighterAttributes;
    FFieldAttrs: TSynHighlighterAttributes;
    FIndicatorAttrs: TSynHighlighterAttributes;
    FSubFieldAttrs: TSynHighlighterAttributes;
    FEndFieldAttrs: TSynHighlighterAttributes;
    FSpaceAttrs: TSynHighlighterAttributes;
    FFixed1Attrs: TSynHighlighterAttributes;
    FFixed2Attrs: TSynHighlighterAttributes;

    function CreateHighlighterAttributes(AName: string; Foreground: TColor;
      Background: TColor): TSynHighlighterAttributes;
    
    procedure MakeMethodTables;
    procedure CRProc;
    procedure TextProc;
    procedure LFProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure SubfieldProc;
    procedure EndFieldProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
  public
    constructor Create(AOwner: TComponent); override;

    class function GetLanguageName: string; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: Integer); override;

    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  published
    property TextAttrs: TSynHighlighterAttributes read FTextAttrs write FTextAttrs;
    property FieldAttrs: TSynHighlighterAttributes read FFieldAttrs write FFieldAttrs;
    property IndicatorAttrs: TSynHighlighterAttributes read FIndicatorAttrs write FIndicatorAttrs;
    property SubFFieldAttrs: TSynHighlighterAttributes read FSubFieldAttrs write FSubFieldAttrs;
    property EndFieldAttrs: TSynHighlighterAttributes read FEndFieldAttrs write FEndFieldAttrs;
    property SpaceAttrs: TSynHighlighterAttributes read FSpaceAttrs write FSpaceAttrs;
    property Fixed1Attrs: TSynHighlighterAttributes read FFixed1Attrs write FFixed1Attrs;
    property Fixed2Attrs: TSynHighlighterAttributes read FFixed2Attrs write FFixed2Attrs;
  end;

implementation

uses
  SynEditStrConst;

procedure TSynMARCSyn.MakeMethodTables;
var
  i: Char;
begin
  for i := #0 to #255 do begin
    case i of
      #0: FProcTable[I] := @NullProc;
      #10: FProcTable[I] := @LFProc;
      #13: FProcTable[I] := @CRProc;
      '|': FProcTable[I] := @SubfieldProc;
      #$1e: FProcTable[I] := @EndFieldProc;
      #1..#9,
      #11,
      #12,
      #14..#29,
      #31..#32: FProcTable[I] := @SpaceProc;
    else
      FProcTable[I] := @TextProc;
    end;
  end;
end;

constructor TSynMARCSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTextAttrs := CreateHighlighterAttributes(SYNS_AttrText, clBlack, clNone);
  AddAttribute(FTextAttrs);

  FSpaceAttrs := CreateHighlighterAttributes(SYNS_AttrSpace, clNone, clWhite);
  AddAttribute(FSpaceAttrs);

  FFieldAttrs := CreateHighlighterAttributes('MARC Field', clBlue, clWhite);
  AddAttribute(FFieldAttrs);

  FIndicatorAttrs := CreateHighlighterAttributes('MARC Indicator', RGB(210, 0, 0), clWhite);
  AddAttribute(FIndicatorAttrs);

  FSubFieldAttrs := CreateHighlighterAttributes('MARC Subfield', RGB(192, 0, 255), clWhite);
  AddAttribute(FSubFieldAttrs);

  FEndFieldAttrs := CreateHighlighterAttributes('MARC field end', RGB(220, 220, 220), clWhite);
  AddAttribute(FEndFieldAttrs);

  FFixed1Attrs := CreateHighlighterAttributes('MARC Fixed field part 1', clBlack, clWhite);
  AddAttribute(FFixed1Attrs);

  FFixed2Attrs := CreateHighlighterAttributes('MARC Fixed field part 2', clBlack, RGB(230,230,230));
  AddAttribute(FFixed2Attrs);

  //*************************
  SetAttributesOnChange(@DefHighlightChange);
  FDefaultFilter := '*.*';
  MakeMethodTables;
end;

procedure TSynMARCSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  FLine := PChar(NewValue);
  Run := 0;
  FLineNumber := LineNumber;
  Next;
end;

procedure TSynMARCSyn.CRProc;
begin
  FTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: Inc(Run, 2);
  else Inc(Run);
  end;
end;

procedure TSynMARCSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do 
    Inc(Run);
end;

procedure TSynMARCSyn.TextProc;
begin
  if (Run >= 0) and (Run <= 2) then
    FTokenID := tkField
  else begin
    if (FLine[0] = '0') and (FLine[1] = '0') and (FLine[2] in ['0'..'9']) then begin
      { Fixed field }
      if Run < 6 then
        FTokenID := tkText
      else begin
        if (Run - 6) div 10 mod 2 = 0 then
          FTokenID := tkFixed1
        else
          FTokenID := tkFixed2;
      end;
    end
    else begin
      if (Run >= 4) and (Run <= 5) then
        FTokenID := tkIndicator
      else
        FTokenID := tkText;
    end;
  end;
  while (FLine[Run] in [#128..#191]) or // continued utf8 subcode
   ((FLine[Run] <> #0) and (FProcTable[FLine[Run]] = @TextProc)) do 
    Inc(Run);
end;

procedure TSynMARCSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynMARCSyn.NullProc;
begin
  FTokenID := tkNull;
end;

procedure TSynMARCSyn.SubfieldProc;
begin
  if (FLine[0] = '0') and (FLine[1] = '0') and (FLine[2] in ['0'..'9']) then begin
    { Fixed field }
    if Run < 6 then
      FTokenID := tkText
    else begin
      if (Run - 6) div 10 mod 2 = 0 then
        FTokenID := tkFixed1
      else
        FTokenID := tkFixed2;
    end;
  end
  else begin
    FTokenID := tkSubfield;
    if FLine[Run + 1] in ['0'..'9', 'a'..'z', 'A'..'Z'] then
      Inc(Run);
  end;
  Inc(Run);
end;

procedure TSynMARCSyn.EndFieldProc;
begin
  FTokenID := tkEndField;
  Inc(Run);
end;

procedure TSynMARCSyn.Next;
begin
  FTokenPos := Run;
  FProcTable[FLine[Run]]();
end;

function TSynMARCSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FSpaceAttrs;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttrs;
  else 
    Result := nil;
  end;
end;

function TSynMARCSyn.GetEol: Boolean;
begin
  Result := FTokenID = tkNull;
end;

function TSynMARCSyn.GetToken: string;
var
  Len: LongInt;
begin
  Result := '';
  Len := Run - FTokenPos;
  SetString(Result, (FLine + FTokenPos), Len);
end;

procedure TSynMARCSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - FTokenPos;
  TokenStart := FLine + FTokenPos;
end;

function TSynMARCSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynMARCSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkText: Result := FTextAttrs;
    tkField: Result := FFieldAttrs;
    tkIndicator: Result := FIndicatorAttrs;
    tkSubfield: Result := FSubFieldAttrs;
    tkEndField: Result := FEndFieldAttrs;
    tkSpace: Result := FSpaceAttrs;
    tkFixed1: Result := FFixed1Attrs;
    tkFixed2: Result := FFixed2Attrs;
  else
    Result := nil;
  end;
end; 

function TSynMARCSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end; 

function TSynMARCSyn.GetTokenPos: Integer;
begin
 Result := FTokenPos;
end; 

function TSynMARCSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end; 

class function TSynMARCSyn.GetLanguageName: string;
begin
  Result := 'MARC';
end;

function TSynMARCSyn.CreateHighlighterAttributes(AName: string; 
  Foreground: TColor; Background: TColor): TSynHighlighterAttributes;
begin
  Result := TSynHighlighterAttributes.Create(AName);
  if Foreground <> clNone then 
    Result.Foreground := ForeGround;
  if Background <> clNone then 
    Result.Background := Background;
  Result.Style := [];
end;

function TSynMARCSyn.GetSampleSource: String;
begin
  Result:='245 00§aTitle';
end;

end.
