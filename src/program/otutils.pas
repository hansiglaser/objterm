(**
 * ObjTerm - Terminal with more than Text
 *
 * Utilities
 *
 * (c) 2021 by Johann Glaser
 *)
Unit OTUtils;

{$mode objfpc}
{$H+}

Interface

Uses Classes, SysUtils, FPJSON;

Type

  { TRingBuffer }

  generic TRingBuffer<T> = class
  private
    FSize  : Integer;
    FItems : array of T;
    FWrPtr : Integer;
    FRdPtr : Integer;
    FEventAdd : PRTLEvent;
  public
    Constructor Create(ASize : Integer);
    Destructor Destroy; override;
    Function IsEmpty : Boolean;
    Function IsFull : Boolean;
    Function Add(AItem:T) : Boolean;
    Function Get : T;
    property EventAdd : PRTLEvent read FEventAdd write FEventAdd;
  End;

  TStringRingBuffer = specialize TRingBuffer<String>;

Function StrArr2JSONArray(AStrArr:Array of String) : TJSONArray;
Function JSONArray2StrArr(AJSONArray:TJSONArray) : TStringArray;
Function GetGUIDStr : String;

Implementation

{ TRingBuffer }

Constructor TRingBuffer.Create(ASize : Integer);
Begin
  inherited Create;
  FSize := ASize;
  SetLength(FItems, FSize);
  FEventAdd := Nil;
End;

Destructor TRingBuffer.Destroy;
Begin
  inherited Destroy;
End;

Function TRingBuffer.IsEmpty : Boolean;
Begin
  Result := (FWrPtr = FRdPtr);
End;

Function TRingBuffer.IsFull : Boolean;
Begin
  Result := (((FWrPtr + 1) mod FSize) = FRdPtr);
End;

Function TRingBuffer.Add(AItem : T) : Boolean;
Var NextWrPtr : Integer;
Begin
  NextWrPtr := (FWrPtr + 1) mod FSize;
  if NextWrPtr = FRdPtr then
    Exit(False);
  FItems[FWrPtr] := AItem;
  FWrPtr := NextWrPtr;
  if assigned(FEventAdd) then
    RTLEventSetEvent(FEventAdd);   // optionally notify if another thread is waiting
  Result := True;
End;

Function TRingBuffer.Get : T;
Begin
  if IsEmpty then
    raise Exception.Create('Ring buffer is empty.');
  Result := FItems[FRdPtr];
  FRdPtr := (FRdPtr + 1) mod FSize;
End;


Function StrArr2JSONArray(AStrArr:Array of String) : TJSONArray;
Var St : String;
Begin
  Result := TJSONArray.Create;
  For St in AStrArr do
    Result.Add(St);
End;

Function JSONArray2StrArr(AJSONArray : TJSONArray) : TStringArray;
Var I : Integer;
Begin
  SetLength(Result, AJSONArray.Count);
  For I := 0 to AJSONArray.Count-1 do
    Result[I] := AJSONArray.Strings[I];
End;

Function GetGUIDStr : String;
Var GUID : TGUID;
Begin
  CreateGUID(GUID);
  Result := GUIDToString(GUID);
  Result := Copy(Result, 2, 36);   // remove leading and trailing '{}'
End;

End.

