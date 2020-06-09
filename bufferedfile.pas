{
 /***************************************************************************
                            bufferedfile.pas
                            ----------------

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

unit BufferedFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TBufferedFile }

  TBufferedFile = class
  private
    FBufferSize: LongInt;
    FFile: file;
    FFileOpen: Boolean;
    FBuffer: PChar;
    FBufferPos: LongInt;
    FBuffered: LongInt;
    FBufferStartPos: Int64;
    function GetEof: Boolean;
    function GetPosition: Int64;
    procedure SetBufferSize(const AValue: LongInt);
    procedure SetPosition(Value: Int64);
  public
    constructor Create;
    destructor Destroy; override;
  
    procedure Open(const FileName: string);
    procedure Close();
    procedure Read(out Buffer; Bytes: LongInt; out BytesRead: Int64);
    function ReadTextLine: string;
    procedure Rewind;

    property BufferSize: LongInt read FBufferSize write SetBufferSize;
    property Eof: Boolean read GetEof;
    property Position: Int64 read GetPosition write SetPosition;
  end;

implementation

const
  DEF_BUFFER_SIZE = 8192 * 1024;

{ TBufferedFile }

constructor TBufferedFile.Create;
begin
  FFileOpen := False;
  FBufferStartPos := 0;
  FBuffered := 0;
  FBuffer := nil;
  FBufferSize := DEF_BUFFER_SIZE;
end;

destructor TBufferedFile.Destroy;
begin
  if FFileOpen then
    Close;
  if FBuffer <> nil then
    FreeMem(FBuffer);
  inherited;
end;

function TBufferedFile.GetEof: Boolean;
begin
  Result := (FBufferPos >= FBuffered) and System.Eof(FFile);
end;

function TBufferedFile.GetPosition: Int64;
begin
  Result := FBufferStartPos + FBufferPos;
end;

procedure TBufferedFile.SetBufferSize(const AValue: LongInt);
begin
  if FBufferSize=AValue then exit;
  FBufferSize:=AValue;
  if FBuffer <> nil then begin
    FreeMem(FBuffer);
    FBuffer := nil;
  end;
  FBuffered := 0;
end;

procedure TBufferedFile.SetPosition(Value: Int64);
begin
  if FBuffered > 0 then begin
    if FBufferStartPos + FBufferPos = Value then
      Exit;
    if (Value >= FBufferStartPos) and (Value <= FBufferStartPos + FBuffered) then begin
      FBufferPos := Value - FBufferStartPos;
      Exit;
    end;
  end;
  FBuffered := 0;
  FBufferPos := 0;
  Seek(FFile, Value);
end;

procedure TBufferedFile.Open(const FileName: string);
begin
  Assign(FFile, FileName);
  FileMode := 0;
  Reset(FFile, 1);
  FFileOpen := True;
  FBuffered := 0;
  FBufferStartPos := 0;
end;

procedure TBufferedFile.Close();
begin
  CloseFile(FFile);
  FFileOpen := False;
end;

procedure TBufferedFile.Rewind;
begin
  Reset(FFile, 1);
  FBuffered := 0;
  FBufferPos := 0;
  FBufferStartPos := 0;
end;

procedure TBufferedFile.Read(out Buffer; Bytes: LongInt; out BytesRead: Int64);
begin
  if FBuffer = nil then
    FBuffer := Getmem(FBufferSize);
  // First add as much as we have in buffer or was requested
  BytesRead := Int64(FBuffered) - FBufferPos;
  if BytesRead > Bytes then
    BytesRead := Bytes;
  Move(FBuffer[FBufferPos], Buffer, BytesRead);
  Dec(Bytes, BytesRead);
  Inc(FBufferPos, BytesRead);
  // Then read more and add as long as necessary
  while (Bytes > 0) and not System.Eof(FFile) do begin
    FBufferStartPos := FilePos(FFile);
    BlockRead(FFile, FBuffer^, FBufferSize, FBuffered);
    if FBuffered < Bytes then begin
      Move(FBuffer^, (@Buffer + BytesRead)^, FBuffered);
      FBufferPos := FBuffered;
      Dec(Bytes, FBuffered);
      Inc(BytesRead, FBuffered);
    end
    else begin
      Move(FBuffer^, (@Buffer + BytesRead)^, Bytes);
      FBufferPos := Bytes;
      Inc(BytesRead, Bytes);
      Break;
    end;
  end;
end;

function TBufferedFile.ReadTextLine: string;
var
  Ch: Char;
begin
  Result := '';
  while not Eof do begin
    if FBufferPos >= FBuffered then begin
      FBufferStartPos := FilePos(FFile);
      BlockRead(FFile, FBuffer^, FBufferSize, FBuffered);
      FBufferPos := 0;
      if FBuffered = 0 then
        break;
    end;
    Ch := FBuffer[FBufferPos];
    Inc(FBufferPos);
    if Ch = #10 then
      Break;
    if Ch <> #13 then
      Result := Result + Ch;
  end;
end;

end.

