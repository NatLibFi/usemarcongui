{
 /***************************************************************************
                            recordconverterunit.pas
                            -----------------------

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

unit RecordConverterUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr;

type
  { TRecordConverter }

  TRecordConverter = class
  private
    function JustifyLeftCh(S: string; Len: LongInt; Ch: Char): string;
    function JustifyRightCh(S: string; Len: LongInt; Ch: Char): string;
    function XMLDecode(S: string): string;
    function XMLEncode(S: string): string;

    function GetTag(const XML: string; StartPos: SizeInt; const Tag: string; out Attrs: string;
      out Contents: string; out NextPos: SizeInt): Boolean;
    function GetAttribute(const Attributes: string; const Attribute: string; out Value: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Convert(const XML: string): string;
    function ConvertToXML(const Data: string): string;
  end;

implementation

function TRecordConverter.JustifyLeftCh(S: string; Len: LongInt; Ch: Char): string;
begin
  S := Copy(S, 1, Len);
  while Length(S) < Len do
    S := S + Ch;
  Result := S;
end;

function TRecordConverter.JustifyRightCh(S: string; Len: LongInt; Ch: Char): string;
begin
  S := Copy(S, 1, Len);
  while Length(S) < Len do
    S := Ch + S;
  Result := S;
end;

function TRecordConverter.XMLDecode(S: string): string;
var
  EPos, OldPos, Len: SizeInt;
  Hex: Boolean;
  Entity: string;
begin
  EPos := Pos('&#', S);
  while EPos > 0 do begin
    Len := 2;
    if S[EPos + 2] in ['x', 'X'] then begin
      Hex := True;
      Inc(Len);
    end
    else begin
      Hex := False;
    end;
    Entity := '';
    while (S[EPos + Len] in ['0'..'9']) or (Hex and (S[EPos + Len] in ['a'..'f', 'A'..'F'])) do begin
      Entity := Entity + S[Epos + Len];
      Inc(Len);
    end;
    if S[EPos + Len] = ';' then begin
      // Valid entity
      if Hex then
        Entity := Chr(StrToInt('$' + Entity))
      else
        Entity := Chr(StrToInt(Entity));
      S := Copy(S, 1, EPos - 1) + Entity + Copy(S, EPos + Len + 1, Length(S));
    end;
    OldPos := EPos;
    EPos := Pos('&#', Copy(S, EPos + 1, Len));
    if EPos > 0 then
      EPos := OldPos + 1 + EPos;
  end;
  S := StringReplace(S, '&lt;', '<', [rfReplaceAll]);
  S := StringReplace(S, '&gt;', '>', [rfReplaceAll]);
  S := StringReplace(S, '&apos;', '''', [rfReplaceAll]);
  S := StringReplace(S, '&amp;', '&', [rfReplaceAll]);
  Result := S;
end;

function TRecordConverter.XMLEncode(S: string): string;
begin
  S := StringReplace(S, '&', '&amp;', [rfReplaceAll]);
  S := StringReplace(S, '''', '&apos;', [rfReplaceAll]);
  S := StringReplace(S, '<', '&lt;', [rfReplaceAll]);
  S := StringReplace(S, '>', '&gt;', [rfReplaceAll]);
  Result := S;
end;

function TRecordConverter.GetTag(const XML: string; StartPos: SizeInt; const Tag: string; out Attrs: string;
  out Contents: string; out NextPos: SizeInt): Boolean;
const
  WhiteSpaceEnd = [' ', Chr(9), Chr(10), Chr(13), '>'];
var
  TagLen: SizeInt;
  XMLLen: SizeInt;
  i: SizeInt;
begin
  Result := False;

  NextPos := 0;
  Attrs := '';
  Contents := '';
  TagLen := Length(Tag);
  XMLLen := Length(XML);
  i := StartPos;
  while i <= XMLLen do begin
    if (XML[i] = '<') and (Copy(XML, i + 1, TagLen) = Tag) and (XML[i + TagLen + 1] in WhiteSpaceEnd) then begin
      Inc(i, TagLen + 1);
      while (i <= XMLLen) and (XML[i] <> '>') do begin
        Attrs := Attrs + XML[i];
        Inc(i);
      end;
      Inc(i);
      while (i <= XMLLen) and not ((XML[i] = '<') and (Copy(XML, i, TagLen + 3) = '</' + Tag + '>')) do begin
        Contents := Contents + XML[i];
        Inc(i);
      end;
      NextPos := i + TagLen + 3;
      Result := True;
      Exit;
    end;
    Inc(i);
  end;
end;

function TRecordConverter.GetAttribute(const Attributes: string;
  const Attribute: string; out Value: string): Boolean;
const
  WhiteSpace = [' ', Chr(9), Chr(10), Chr(13)];
  Separators = [' ', Chr(9), Chr(10), Chr(13), '='];
var
  i: SizeInt;
  AttrsLen: SizeInt;
  AttrName: string;
  AttrValue: string;
begin
  Result := False;
  Value := '';
  i := 1;
  AttrsLen := Length(Attributes);
  while i <= AttrsLen do begin
    if not (Attributes[i] in WhiteSpace) then begin
      AttrName := '';
      while (i <= AttrsLen) and not (Attributes[i] in Separators) do begin
        AttrName := AttrName + Attributes[i];
        Inc(i);
      end;
      while (i <= AttrsLen) and (Attributes[i] in WhiteSpace) do begin
        Inc(i);
      end;
      if (Attributes[i] <> '=') then begin
        // Uh, it's broken
        Exit;
      end;
      Inc(i);
      while (i <= AttrsLen) and (Attributes[i] in WhiteSpace) do begin
        Inc(i);
      end;
      if (Attributes[i] <> '"') then begin
        // Uh, it's broken
        Exit;
      end;
      Inc(i);
      AttrValue := '';
      while (i <= AttrsLen) and (Attributes[i] <> '"') do begin
        AttrValue := AttrValue + Attributes[i];
        Inc(i);
      end;
      if AttrName = Attribute then begin
        Value := XMLDecode(AttrValue);
        Result := True;
        Exit;
      end;
    end;
    Inc(i);
  end;
end;

constructor TRecordConverter.Create;
begin
  inherited;
end;

destructor TRecordConverter.Destroy;
begin
  inherited Destroy;
end;

function TRecordConverter.Convert(const XML: string): Ansistring;
const
  FIELD_START = #$1F;
  FIELD_END = #$1E;
  RECORD_END = #$1D;
var
  Directory: string;
  MarcData: string;
  DataPos: Longint;
  Leader: string;
  Attrs: string;
  Contents: string;
  Tag: string;
  Ind1: string;
  Ind2: string;
  FieldData: string;
  SubAttrs: string;
  SubContents: string;
  SubCode: string;
  DataStart: LongInt;
  RecordLen: LongInt;
  RecordData: Ansistring;
  XMLPos: SizeInt;
  SubXMLPos: SizeInt;
begin
  Result := '';

  Directory := '';
  MarcData := '';
  DataPos := 0;

  GetTag(XML, 1, 'leader', Attrs, Leader, XMLPos);
  Leader := JustifyLeftCh(XMLDecode(Leader), 24, ' ');

  XMLPos := 1;
  while GetTag(XML, XMLPos, 'controlfield', Attrs, Contents, XMLPos) do begin
    GetAttribute(Attrs, 'tag', Tag);
    Contents := XMLDecode(Contents) + FIELD_END;

    Directory := Directory + JustifyRightCh(Tag, 3, '0') + JustifyRightCh(IntToStr(Length(Contents)), 4, '0') +
      JustifyRightCh(IntToStr(DataPos), 5, '0');
    MarcData := MarcData + Contents;
    DataPos := DataPos + Length(Contents);
  end;

  XMLPos := 1;
  while GetTag(XML, XMLPos, 'datafield', Attrs, Contents, XMLPos) do begin
    GetAttribute(Attrs, 'tag', Tag);
    GetAttribute(Attrs, 'ind1', Ind1);
    GetAttribute(Attrs, 'ind2', Ind2);

    FieldData := JustifyLeftCh(Ind1, 1, ' ') + JustifyLeftCh(Ind2, 1, ' ');
    SubXMLPos := 1;
    while GetTag(Contents, SubXMLPos, 'subfield', SubAttrs, SubContents, SubXMLPos) do begin
      GetAttribute(SubAttrs, 'code', SubCode);
      FieldData := FieldData + FIELD_START + SubCode + XMLDecode(SubContents);
    end;
    FieldData := FieldData + FIELD_END;

    Directory := Directory + JustifyRightCh(Tag, 3, '0') + JustifyRightCh(IntToStr(Length(FieldData)), 4, '0') +
      JustifyRightCh(IntToStr(DataPos), 5, '0');
    MarcData := MarcData + FieldData;
    DataPos := DataPos + Length(FieldData);
  end;

  Directory := Directory + FIELD_END;
  MarcData := MarcData + RECORD_END;

  DataStart := Length(Leader) + Length(Directory);
  RecordLen := Length(Leader) + Length(Directory) + Length(MarcData);
  Leader := JustifyRightCh(IntToStr(RecordLen), 5, '0') + Copy(Leader, 6, 7) +
    JustifyRightCh(IntToStr(DataStart), 5, '0') + Copy(Leader, 18, Length(Leader));
  RecordData := Leader + Directory + MarcData;
  Result := RecordData;
end;

function TRecordConverter.ConvertToXML(const Data: string): string;
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
  DirPos, FieldLen, FieldOffset: LongInt;
  FieldCode: string;
  Ind1: string;
  Ind2: string;
  FieldData: string;
  SubStart: LongInt;
  SubEnd: LongInt;
  SubCode: string;
  SubData: string;
begin
  Result := '<record>' + #10;
  Result := Result + '  <leader>' + XMLEncode(ReadStr(0, 24)) + '</leader>' + #10;
  BaseAddr := ReadInt(12, 5);
  DirPos := 24;
  while ReadStr(DirPos, 1) <> FieldEnd do begin
    FieldCode := ReadStr(DirPos, 3);
    FieldLen := ReadInt(DirPos + 3, 4);
    FieldOffset := ReadInt(DirPos + 7, 5);

    if (Copy(FieldCode, 1, 2) = '00') then begin
      FieldData := ReadStr(BaseAddr + FieldOffset, FieldLen - 1);
      Result := Result + '  <controlfield tag="' + XMLEncode(FieldCode) + '">' +
        XMLEncode(FieldData) + '</controlfield>' + #10;
    end
    else begin
      Ind1 := ReadStr(BaseAddr + FieldOffset, 1);
      Ind2 := ReadStr(BaseAddr + FieldOffset + 1, 1);
      FieldData := ReadStr(BaseAddr + FieldOffset + 2, FieldLen - 2);
      Result := Result + '  <datafield tag="' + XMLEncode(FieldCode) +
        '" ind1="' + XMLEncode(Ind1) + '" ind2="' + XMLEncode(Ind2) + '">' + #10;
      
      SubStart := Pos(FieldStart, FieldData);
      while SubStart > 0 do begin
        SubEnd := Pos(FieldStart, Copy(FieldData, SubStart + 1, Length(FieldData)));
        if SubEnd = 0 then
          SubEnd := Pos(FieldEnd, Copy(FieldData, SubStart + 1, Length(FieldData)));
        if SubEnd = 0 then
          SubEnd := Length(FieldData)
        else
          Inc(SubEnd, SubStart);
        SubCode := Copy(FieldData, SubStart + 1, 1);
        SubData := Copy(FieldData, SubStart + 2, SubEnd - SubStart - 2);
        Result := Result + '    <subfield code="' + XMLEncode(SubCode) + '">' +
          XMLEncode(SubData) + '</subfield>' + #10;
          
        FieldData := Copy(FieldData, SubEnd, Length(FieldData));
        SubStart := Pos(FieldStart, FieldData);
      end;
      Result := Result + '  </datafield>' + #10;
    end;

    Inc(DirPos, 12);
  end;
  Result := Result + '</record>' + #10;
end;

end.

