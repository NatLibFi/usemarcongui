{
 /***************************************************************************
                            int64list.pas
                            -------------

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

unit Int64List;

interface

type

  { TInt64List }

  TInt64List = class
  private
    FCount: Cardinal;
    FMemSize: Cardinal;
    FMemPtr: Pointer;
    FLastItem: PInt64;

    function GetValue(Index: Cardinal) : Int64;
    procedure Init;

  public
    constructor Create;
    destructor Destroy; override;
    property Items[Index: Cardinal]: Int64 read GetValue; default;
    procedure Add(Value: Int64);
    procedure Clear;

    property Count: Cardinal read FCount;
  end;

implementation

uses
  SysUtils;

const
  InitSize = 100;

constructor TInt64List.Create;
begin
  Init;
end;

destructor TInt64List.Destroy;
begin
  FreeMem(FMemPtr);
  inherited;
end;

procedure TInt64List.Add(Value: Int64);
var
  NewMemPtr: Pointer;
begin
  if FCount = FMemSize then begin
    // Double the space to avoid too frequent allocations
    GetMem(NewMemPtr, 2 * FMemSize * SizeOf(Int64));
    Move(FMemPtr^, NewMemPtr^, FMemSize * SizeOf(Int64));
    FreeMem(FMemPtr);
    FMemPtr := NewMemPtr;
    FMemSize := FMemSize * 2;
    FLastItem := FMemPtr;
    Inc(FLastItem, FCount - 1);
  end;
  if FLastItem = nil then
    FlastItem := FMemPtr
  else
    Inc(FLastItem);
  FLastItem^ := Value;
  Inc(FCount);
end;

procedure TInt64List.Clear;
begin
  FreeMem(FMemPtr);
  Init;
end;

function TInt64List.GetValue(Index: Cardinal): Int64;
var
  Int64Ptr: PInt64;
begin
  if Index >= FCount then
    Raise(Exception.Create('Index out of range'));

  Int64Ptr := FMemPtr;
  Inc(Int64Ptr, Index);
  Result := Int64Ptr^;
end;

procedure TInt64List.Init;
begin
  FCount := 0;
  GetMem(FMemPtr, InitSize * SizeOf(Int64));
  FMemSize := InitSize;
  FLastItem := nil;
end;

end.
