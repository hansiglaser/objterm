(**
 * ObjTerm - Terminal with more than Text
 *
 * Main Client-Side Unit
 *
 * Implements a simple WebSocket server, see https://github.com/Warfley/LazWebsockets
 *
 * (c) 2021 by Johann Glaser
 *)
Unit OTOBase;

{$mode objfpc}
{$H+}

Interface

Uses {$IFDEF UNIX}
  cthreads, {$ENDIF}
  classes, SysUtils,
  fpjson, jsonparser,
  ObjTerm,
  OTUtils,
  wsutils,
  wsmessages,
  wsstream,
  websocketserver;

Type

  { TOTOHTML }

  TOTOHTML = class(TOTObject)
    FHTML : String;
    Constructor Create(AHTML : String);
    Procedure CmdAppend(AMsg : TJSONObject); override;
    class Function GetOTType : String; override;
  End;

  { TOTOString }

  TOTOString = class(TOTObject)
    FSt : String;
    Constructor Create(ASt : String);
    Procedure CmdAppend(AMsg : TJSONObject); override;
    Procedure Append(ASt : String);
    Function GetLength : Integer;
    class Function GetOTType : String; override;
  End;

  { TOTODygraph }

  TOTODygraph = class(TOTObject)
    FWidth   : Integer;
    FHeight  : Integer;
    FData    : TJSONArray;
    FOptions : TJSONData;
    Constructor Create(AWidth : Integer; AHeight : Integer; AData : TJSONArray; AOptions : TJSONData);
    Procedure CmdAppend(AMsg : TJSONObject); override;
    Procedure AppendPoints(AData : TJSONArray; ABlockRedraw : Boolean = False);
    class Function GetOTType : String; override;
  End;

Implementation

{ TOTOHTML }

Constructor TOTOHTML.Create(AHTML : String);
Begin
  inherited Create;
  FHTML := AHTML;
End;

Procedure TOTOHTML.CmdAppend(AMsg : TJSONObject);
Begin
  Inherited CmdAppend(AMsg);
  AMsg.Add('html', FHTML);
End;

Class Function TOTOHTML.GetOTType : String;
Begin
  Result := 'otohtml';
End;

{ TOTOString }

Constructor TOTOString.Create(ASt : String);
Begin
  inherited Create;
  FSt := ASt;
End;

Procedure TOTOString.CmdAppend(AMsg : TJSONObject);
Begin
  Inherited CmdAppend(AMsg);
  AMsg.Add('st', FSt);
End;

Procedure TOTOString.Append(ASt : String);
Var Msg : TJSONObject;
Begin
  FSt := FSt + ASt;
  Msg := FObjTerm.CreateMsgCmd('objcmd');
  Msg.Add('uniqueid', FUniqueID);
  Msg.Add('subcmd',   'append');
  Msg.Add('st', ASt);
  FObjTerm.SendMessage(Msg);
  Msg.Free;
End;

Function OnGetLengthCmdStatus(AMsg : TJSONObject; ACmdId, AStatus, AMessage : String; AOnCmdStatusData : Pointer) : Boolean;
Var I : Integer;
Begin
  if AStatus = 'ok' then
    Begin
      I := AMsg.Integers['length'];
      PInteger(AOnCmdStatusData)^ := I;
    End
  else
    PInteger(AOnCmdStatusData)^ := -2;  // states that the cmdstatus shows an error
  Result := False;
End;

Function TOTOString.GetLength : Integer;
Var Msg      : TJSONObject;
    RTLEvent : PRTLEvent;
    Length   : Integer;
Begin
  Length := -1;   // states that no information is available or an error occured
  RTLEvent := RTLEventCreate;
  Msg := FObjTerm.CreateMsgCmd('objcmd', RTLEvent, @OnGetLengthCmdStatus, @Length);
  Msg.Add('uniqueid', FUniqueID);
  Msg.Add('subcmd',   'getlength');
  FObjTerm.SendMessage(Msg);
  Msg.Free;
  // wait for return message (the RTLEvent will be set _after_ OnGetCmdStatus is executed)
  RTLEventWaitFor(RTLEvent, 1000);
  RTLEventDestroy(RTLEvent);
  // TODO: handle timeout
  Result := Length;
End;

Class Function TOTOString.GetOTType : String;
Begin
  Result := 'otostring';
End;

{ TOTODygraph }

Constructor TOTODygraph.Create(AWidth : Integer; AHeight : Integer; AData : TJSONArray; AOptions : TJSONData);
Begin
  inherited Create;
  FWidth   := AWidth;
  FHeight  := AHeight;
  FData    := AData;
  FOptions := AOptions;
End;

Procedure TOTODygraph.CmdAppend(AMsg : TJSONObject);
Begin
  Inherited CmdAppend(AMsg);
  AMsg.Add('width',   FWidth);
  AMsg.Add('height',  FHeight);
  AMsg.Add('data',    FData.Clone);       // have to clone, otherwise these TJSONObjects would get freed with AMsg
  AMsg.Add('options', FOptions.Clone);
End;

Procedure TOTODygraph.AppendPoints(AData : TJSONArray; ABlockRedraw:Boolean=False);
Var Msg : TJSONObject;
    I   : Integer;
Begin
  Msg := FObjTerm.CreateMsgCmd('objcmd');
  Msg.Add('uniqueid',    FUniqueID);
  Msg.Add('subcmd',      'appendpoints');
  Msg.Add('data',        AData.Clone);   // always clone, because the sub-array would be freed with the message below
  Msg.Add('blockredraw', ABlockRedraw);
  FObjTerm.SendMessage(Msg);
  Msg.Free;
  // append to local copy
  For I := 0 to AData.Count-1 do
    FData.Add(AData.Arrays[I]);
End;

Class Function TOTODygraph.GetOTType : String;
Begin
  Result := 'otodygraph';
End;

End.

