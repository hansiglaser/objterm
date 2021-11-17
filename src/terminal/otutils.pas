(**
* ObjTerm - Terminal with more than Text
*
* Utilities
*
* (c) 2021 by Johann Glaser
*)
Unit OTUtils;

{$mode objfpc}

Interface

Uses JS, SysUtils;

Function JSArrayOfStr2Pas(A:TJSArray) : TStringArray;
Function GetGUIDStr : String;

Implementation

Function JSArrayOfStr2Pas(A:TJSArray) : TStringArray;
Var I : Integer;
Begin
  SetLength(Result, A.Length);
  For I := 0 to A.Length-1 do
    Begin
      if not isString(A[I]) then
        raise Exception.Create('Entry '+IntToStr(I)+' in cssclasses is not a string');
      Result[I] := String(A[I]);
    End;
End;

Function GetGUIDStr : String;
Var GUID : TGUID;
Begin
  CreateGUID(GUID);
  Result := GUIDToString(GUID);
  Result := Copy(Result, 2, 36);   // remove leading and trailing '{}'
End;

End.

