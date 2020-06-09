{
 /***************************************************************************
                            SynHighlighterRule.pas
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

unit SynHighlighterRule;

{$I SynEdit.inc}

interface

uses
  SysUtils, Classes,
  LCLIntf, LCLType,
  Controls, Graphics,
  SynEditTypes, SynEditHighlighter, RegExpr;

type
  TtkTokenKind = (tkNull, tkSpace, tkText, tkString, tkSeparator, tkKey, tkComment,
    tkOperator, tkBlock);

  TProcTableProc = procedure of object;

type

  { TSynRuleSyn }

  TSynRuleSyn = class(TSynCustomHighlighter)
  private
    FLine: PChar;
    FLineNumber: Integer;
    FProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    FTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FTextAttrs: TSynHighlighterAttributes;
    FStringAttrs: TSynHighlighterAttributes;
    FSeparatorAttrs: TSynHighlighterAttributes;
    FBlockAttrs: TSynHighlighterAttributes;
    FSpaceAttrs: TSynHighlighterAttributes;
    FKeyAttrs: TSynHighlighterAttributes;
    FCommentAttrs: TSynHighlighterAttributes;
    FOperatorAttrs: TSynHighlighterAttributes;
    FKeywordRegExp: TRegExpr;

    function CreateHighlighterAttributes(AName: string; Foreground: TColor;
      Background: TColor): TSynHighlighterAttributes;

    procedure MakeMethodTables;
    procedure CRProc;
    procedure TextProc;
    procedure LFProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure SeparatorProc;
    procedure BlockProc;
    procedure OperatorProc;
    procedure IdentProc;
    procedure StringProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    class function GetLanguageName: string; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(const NewValue: string; LineNumber: Integer); override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: Integer); override;

    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  published
    property TextAttrs: TSynHighlighterAttributes read FTextAttrs write FTextAttrs;
    property StringAttrs: TSynHighlighterAttributes read FStringAttrs write FStringAttrs;
    property SeparatorsAttrs: TSynHighlighterAttributes read FSeparatorAttrs write FSeparatorAttrs;
    property BlockAttrs: TSynHighlighterAttributes read FBlockAttrs write FBlockAttrs;
    property SpaceAttrs: TSynHighlighterAttributes read FSpaceAttrs write FSpaceAttrs;
    property KeyAttrs: TSynHighlighterAttributes read FKeyAttrs write FKeyAttrs;
    property CommentAttrs: TSynHighlighterAttributes read FCommentAttrs write FCommentAttrs;
    property OperatorAttrs: TSynHighlighterAttributes read FOperatorAttrs write FOperatorAttrs;
  end;

implementation

uses
  Dialogs,
  SynEditStrConst;

procedure TSynRuleSyn.MakeMethodTables;
var
  i: Char;
begin
  for i := #0 to #255 do begin
    case i of
      #0: FProcTable[i] := @NullProc;
      #10: FProcTable[i] := @LFProc;
      #13: FProcTable[i] := @CRProc;
      '|': FProcTable[i] := @SeparatorProc;
      '/',
      ':',
      ';',
      ',',
      '.',
      '[',
      ']',
      '+',
      '-',
      '*',
      '=',
      '<',
      '>',
      '(',
      ')': FProcTable[i] := @OperatorProc;
      '{',
      '}': FProcTable[i] := @BlockProc;
      #1..#9,
      #11,
      #12,
      #14..#29,
      #31..#32: FProcTable[i] := @SpaceProc;
      'A'..'Z',
      'a'..'z',
      '#': FProcTable[i] := @IdentProc;
      '''': FProcTable[i] := @StringProc;
    else
      FProcTable[i] := @TextProc;
    end;
  end;
end;

constructor TSynRuleSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTextAttrs := CreateHighlighterAttributes(SYNS_AttrText, clBlack, clNone);
  AddAttribute(FTextAttrs);

  FStringAttrs := CreateHighlighterAttributes('String', RGB(32, 0, 96), clNone);
  AddAttribute(FStringAttrs);

  FSpaceAttrs := CreateHighlighterAttributes(SYNS_AttrSpace, clNone, clWhite);
  AddAttribute(FSpaceAttrs);

  FSeparatorAttrs := CreateHighlighterAttributes('Separator', RGB(192, 0, 255), clWhite);
  AddAttribute(FSeparatorAttrs);

  FBlockAttrs := CreateHighlighterAttributes('Block', RGB(192, 0, 255), clWhite);
  AddAttribute(FBlockAttrs);

  FKeyAttrs := CreateHighlighterAttributes('Keyword', clBlue, clWhite);
  AddAttribute(FKeyAttrs);

  FCommentAttrs := CreateHighlighterAttributes('Comment', clDkGray, clWhite);
  AddAttribute(FCommentAttrs);

  FOperatorAttrs := CreateHighlighterAttributes('Operator', clRed, clWhite);
  AddAttribute(FOperatorAttrs);

  FKeywordRegExp := TRegExpr.Create;
  FKeywordRegExp.Expression := '^(S|D|[Ii]f|[Tt]hen|[Ee]lse|[Aa]nd|[Oo]r|[Nn]ot|[Ii]n|[Ss]tr|[Vv]al|[Ll]en|[Ss]to' +
    '|[Mm]em|[Ee]xc|[Cc]lr|[Ff]rom|[Tt]o|[Bb]etween|[Ss]trict|[Dd]elete|[Rr]eplace|[Rr]eplace[Oo]cc|[Aa]t|[Bb]y' +
    '|[Bb]eginning|[Ee]nding|[Bb]oundaries|[Bb][Ff]irst|[Ee][Ff]irst|[Bb][Ll]ast|[Ee][Ll]ast' +
    '|[Nn]umber|[Ss]tring|[Ee]xists|[Pp]recedes|[Ff]ollows|[Rr]edo|[Ss]ort|[Nn]ext|[Ll]ast|[Tt]able|[Uu]pper' +
    '|[Ll]ower|[Yy]ear|[Mm]onth|[Dd]ay|[Hh]our|[Mm]inute|[Ss]econd|[Oo]rdinal|[Nn]ext[Ss]ub' +
    '|[Pp]revious[Ss]ub|[Rr]eg[Ff]ind|[Rr]eg[Rr]eplace|[Rr]eg[Mm]atch|[Ii]n[Tt]able|[Mm]ove[Bb]efore' +
    '|[Mm]ove[Aa]fter|[Ee]xists[Ii]n|[Nn]ext[Ss]ub[Ii]n|[Pp]revious[Ss]ub[Ii]n|[Rr]eg[Ff]ind[Pp]os' +
    '|[Rr]eg[Ff]ind[Nn]um|[Rr]eg[Rr]eplace[Tt]able|[Ww]hile|[Ff]or|[Cc]ondition|[Ww]ith|[Uu][Tt][Ff]8|#define|macro|version|#if|#endif)$';

  //*************************
  SetAttributesOnChange(@DefHighlightChange);
  FDefaultFilter := '*.*';
  MakeMethodTables;
end;

destructor TSynRuleSyn.Destroy;
begin
  FreeAndNil(FKeywordRegExp);
  inherited Destroy;
end;

procedure TSynRuleSyn.SetLine(const NewValue: String; LineNumber:Integer);
begin
  FLine := PChar(NewValue);
  Run := 0;
  FLineNumber := LineNumber;
  Next;
end;

procedure TSynRuleSyn.CRProc;
begin
  FTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: Inc(Run, 2);
    else Inc(Run);
  end;
end;

procedure TSynRuleSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do 
    Inc(Run);
end;

procedure TSynRuleSyn.TextProc;
begin
  FTokenID := tkText;
  inc(Run);
end;

procedure TSynRuleSyn.StringProc;
begin
  FTokenID := tkString;
  Inc(Run);
  while not (FLine[Run] in [#0, #10, #13]) do begin
    if (FLine[Run] = '''') and ((FLine[Run - 1] <> '\') or ((Run >= 2) And (FLine[Run - 1] = '\') and (FLine[Run - 2] = '\'))) then begin
      Inc(Run);
      break;
    end;
    Inc(Run);
  end;
end;

procedure TSynRuleSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynRuleSyn.NullProc;
begin
  FTokenID := tkNull;
end;

procedure TSynRuleSyn.SeparatorProc;
begin
  FTokenID := tkSeparator;
  Inc(Run);
end;

procedure TSynRuleSyn.BlockProc;
begin
  FTokenID := tkBlock;
  Inc(Run);
end;

procedure TSynRuleSyn.OperatorProc;
begin
  if (FLine[Run] = '/') and (FLine[Run + 1] = '/') then begin
    FTokenID := tkComment;
    while not (fLine[Run] in [#0, #10, #13]) do
      Inc(Run);
  end
  else begin
    FTokenID := tkOperator;
    Inc(Run);
  end;
end;

procedure TSynRuleSyn.IdentProc;
var
  Token: string;
begin
  Token := '';
  while FLine[Run] in ['0'..'9', '#', 'a'..'z', 'A'..'Z'] do begin
    Token := Token + FLine[Run];
    Inc(Run);
  end;

  if FKeywordRegExp.Exec(Token) then
    FTokenID := tkKey
  else
    FTokenID := tkText;
end;

procedure TSynRuleSyn.Next;
begin
  FTokenPos := Run;
  FProcTable[FLine[Run]]();
end;

function TSynRuleSyn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FSpaceAttrs;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttrs;
    else Result := nil;
  end;
end;

function TSynRuleSyn.GetEol: Boolean;
begin
  Result := FTokenId = tkNull;
end;

function TSynRuleSyn.GetToken: String;
var
  Len: LongInt;
begin
  Result := '';
  Len := Run - FTokenPos;
  SetString(Result, (FLine + FTokenPos), Len);
end;

procedure TSynRuleSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - FTokenPos;
  TokenStart := FLine + FTokenPos;
end;

function TSynRuleSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenId;
end; 

function TSynRuleSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkText: Result := FTextAttrs;
    tkString: Result := FStringAttrs;
    tkSeparator: Result := FSeparatorAttrs;
    tkBlock: Result := FBlockAttrs;
    tkSpace: Result := FSpaceAttrs;
    tkKey: Result := FKeyAttrs;
    tkComment: Result := FCommentAttrs;
    tkOperator: Result := FOperatorAttrs;
  else
    Result := nil;
  end;
end;

function TSynRuleSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenId);
end;

function TSynRuleSyn.GetTokenPos: Integer;
begin
 Result := FTokenPos;
end;

function TSynRuleSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['0'..'9', '#', 'a'..'z', 'A'..'Z'];
end;

class function TSynRuleSyn.GetLanguageName: string;
begin
  Result := 'Rule';
end;

function TSynRuleSyn.CreateHighlighterAttributes(AName: string; 
  Foreground: TColor; Background: TColor): TSynHighlighterAttributes;
begin
  Result := TSynHighlighterAttributes.Create(AName);
  if Foreground <> clNone then 
    Result.Foreground := ForeGround;
  if Background <> clNone then 
    Result.Background := Background;
  Result.Style := [];
end;

function TSynRuleSyn.GetSampleSource: string;
begin
  Result := '020$a  | 021(nto)$a   | S; Replace (''('' By ''$c'') // comment';
end;

end.
