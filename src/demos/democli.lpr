(**
 * ObjTerm Demo implementing a Command Line Interface via ObjTerm
 *
 * (c) 2021 by Johann Glaser
 *)
Program DemoCLI;

{$mode objfpc}
{$H+}

(* The commands entered by the user are either executed
 *  - in the receiving thread, or
 *  - in the main thread
 * If the following $DEFINE is not defined, then they are executed in the
 * receiving thread. This is the simpler implementation, but it has limitations
 * (see CliCmdStl).
 * If the $DEFINE is defined, then the commands are executed in the main thread.
 * This is more complicated, but also more powerful. A thread-safe string list
 * is created, and the receive thread only places the commands in that list.
 * In the main thread, a loop is waiting for commands, and then executes them.
 *)
{ $ DEFINE ExecuteInMain}

Uses {$IFDEF UNIX}
  cthreads, {$ENDIF}
  classes, SysUtils, Types, BaseUnix, Base64,
{$IFDEF ExecuteInMain}
  Generics.Collections,
{$ENDIF}
  ObjTerm, OTOBase, FPJSON;

{$IFDEF ExecuteInMain}
Type TThreadStringList = specialize TThreadList<String>;
{$ENDIF}

Var OT : TObjTerm;
    OS : TOTOString;
    OG : TOTODygraph;
{$IFDEF ExecuteInMain}
    CmdQueue : TThreadStringList;
    CmdAvail : PRTLEvent;
{$ENDIF}

Procedure OnConnect(AAddress:String);
Begin
  WriteLn('Connected from ', AAddress);
  OT.AppendOTString(
    'Welcome to DemoCLI!'#10+
    'Type "help" for a list of commands.'
  );
End;

Procedure OnDisconnect(AAddress:String);
Begin
  WriteLn('Disconnected from ', AAddress, '.');
  WriteLn('Waiting for connection to port ',ParamStr(1));
End;

Procedure OnHello(AHelloMsg, AHelloReplyMsg : TJSONObject);
Begin
  AHelloReplyMsg.Strings['inputenable'] := 'true';
  AHelloReplyMsg.Strings['inputprompt'] := 'DemoCLI>';
  AHelloReplyMsg.Strings['promptstyle'] := '';
  AHelloReplyMsg.Strings['inputstyle']  := '';
End;

Procedure CliCmdHelp(AText:String; AWords:TStringDynArray);
Begin
  OT.AppendOTString(
    'DemoCLI Commands:'#10+
    '  help                   this list of commands'#10+
    '  st [text ...]          create an OTOString'#10+
    '  sta [text ...]         append to previous OTOString'#10+
    '  stl                    query length of previous OTOString'#10+
    '  html [html ...]        create an OTOHTML'#10+
    '  dygraph                create an OTODygraph'#10+
    '  dygrapha               append points to the previous OTODygraph'#10+
    '  msg [svty] [text ...]  create a message with severity note, warning, error, or fatal'#10+
    '  img (filename)         create an OTOHTML with an image'#10+
    '  close                  close the WebSocket connection'#10+
    '  exit                   exit the DemoCLI program'
  );
  // TODO: support specific help per command
End;

Procedure CliCmdSt(AText:String; AWords:TStringDynArray);
Begin
  OS.Free;
  //OS := TOTOString(OT.AppendOTString(Copy(AText, 4, Length(St))));
  OS := TOTOString.Create(Copy(AText, 4, Length(AText)));
  //OS.FCSSStyle := 'font-weight:bold;';
  OT.AppendObj(OS);
End;

Procedure CliCmdSta(AText:String; AWords:TStringDynArray);
Begin
  if not assigned(OS) then
    Begin
      OT.AppendMsg(msError, 'Error: There was no previous string object.');
      // TODO: this also can't work if the object has vanished from the terminal, e.g., because it was reloaded in the browser
      Exit;
    End;
  OS.Append(Copy(AText, 5, Length(AText)));
End;

Procedure CliCmdStl(AText:String; AWords:TStringDynArray);
Begin
  if not assigned(OS) then
    Begin
      OT.AppendMsg(msError, 'Error: There was no previous string object');
      // TODO: use otomsgerror
      // TODO: this also can't work if the object has vanished from the terminal, e.g., because it was reloaded in the browser
      Exit;
    End;
{$IFDEF ExecuteInMain}
  OT.AppendOTString('The length is '+IntToStr(OS.GetLength));  // this doesn't work if executed in the receive thread
{$ELSE}
  OT.AppendMsg(msWarning,
    'The length can not be queried with DemoCLI. The reason is that each'#10+
    'command is directly executed in the receive thread. If another command is'#10+
    'executed, which waits for a reply (e.g., TOTOString.GetLength), then'#10+
    'this blocks the receive thread, which would itself be required to receive'#10+
    'the result.'#10+
    'A solution would be to implement this command in the main thread, i.e.,'#10+
    'outside of the receive thread, similar to DemoTest.'
    );
{$ENDIF}
End;

Procedure CliCmdHtml(AText:String; AWords:TStringDynArray);
Var OH : TOTOHTML;
Begin
  // OT.AppendOTHTML(Copy(AText, 6, Length(AText))).Free;
  OH := TOTOHTML.Create(Copy(AText, 6, Length(AText)));
  //OH.FCSSClasses := ['otmsgnote'];
  OT.AppendObj(OH);
  OH.Free;
End;

Procedure CliCmdDygraph(AText:String; AWords:TStringDynArray);
Var Data    : TJSONArray;
    I       : Integer;
    base    : Real;
    Options : TJSONObject;
Begin
  // add a graph
  Data := TJSONArray.Create;
  For I := 0 to 999 do
    Begin
      base := 10.0*cos(i / 90.0);
      Data.Add(TJSONArray.Create([I, base, base + sin(i / 2.0)]));
    End;
  Options := TJSONObject.Create(['labels',TJSONArray.Create(['X', 'Est.', 'Actual'])]);
  OG := TOTODygraph(OT.AppendOTDygraph(700, 400, Data, Options));
End;

Procedure CliCmdDygrapha(AText:String; AWords:TStringDynArray);
Var Data    : TJSONArray;
    I       : Integer;
Begin
  // append points to previous graph
  if not assigned(OG) then
    Begin
      OT.AppendMsg(msError, 'Error: There was no previous Dygraph object');
      Exit;
    End;
  Data := TJSONArray.Create;
  For I := 0 to 19 do
    Begin
      Data.Add(TJSONArray.Create([OG.FData.Count+I, Random*20.0-10.0, Random*20.0-10.0]));
    End;
  OG.AppendPoints(Data);
End;

Procedure CliCmdMsg(AText:String; AWords:TStringDynArray);
Var Severity : TMsgSeverity;
Begin
  if Length(AWords) < 3 then
    Begin
      OT.AppendMsg(msError, 'Error: There syntax is "msg [svty] [text ...]"');
      Exit;
    End;
  Case AWords[1] of
    'note'    : Severity := msNote;
    'warning' : Severity := msWarning;
    'error'   : Severity := msError;
    'fatal'   : Severity := msFatal;
  else
    OT.AppendMsg(msError, 'Error: There allowed values for severity are note, warning, error, or fatal');
    Exit;
  End;
  // extracting the actual text is too simple here, because it takes the wrong
  // section if multiple spaces are used around the first two words
  OT.AppendMsg(Severity, Copy(AText, 1+3+1+Length(AWords[1])+1, Length(AText)));
End;

Function ReadFileToString(const Filename: string): string;
Var F : File;
Begin
  Result := '';
  //try
    Assign(F, Filename);
    Reset(F, 1);
    SetLength(Result, FileSize(F));
    try
      BlockRead(F, Result[1], FileSize(F));
    finally
      Close(F);
    End;
  //Except
    // ignore errors, Result string will be empty
  //End;
End;

Procedure CliCmdImg(AText:String; AWords:TStringDynArray);
Var ImgType : String;
    St      : String;
Begin
  if Length(AWords) = 1 then
    Begin
      // only 'img', send a fixed image
      OT.AppendOTHTML(
        '<p>Image from <a href="https://wiki.selfhtml.org/wiki/Grafik/Grafiken_mit_Data-URI">SelfHTML</a>'#10+
        '<img width="16" height="16" alt="tick" src="data:image/gif;base64,R0lGODdhEAAQAMwAAPj7+FmhUYjNfGuxYY'+
        'DJdYTIeanOpT+DOTuANXi/bGOrWj6CONzv2sPjv2CmV1unU4zPgI/Sg6DJnJ3ImTh8Mtbs00aNP1CZSGy0YqLEn47RgXW8amasW'+
        '7XWsmmvX2iuXiwAAAAAEAAQAAAFVyAgjmRpnihqGCkpDQPbGkNUOFk6DZqgHCNGg2T4QAQBoIiRSAwBE4VA4FACKgkB5NGReAS'+
        'FZEmxsQ0whPDi9BiACYQAInXhwOUtgCUQoORFCGt/g4QAIQA7">');
    End
  else if Length(AWords) = 2 then
    Begin
      // command was "img filename", read the file and show it as image
      if      AWords[1].EndsWith('.png', true) then ImgType := 'png'
      else if AWords[1].EndsWith('.jpg', true) then ImgType := 'jpg'
      else if AWords[1].EndsWith('.gif', true) then ImgType := 'gif'
      else
        Begin
          OT.AppendMsg(msError, 'Error: There file type of '+AWords[1]+' is not supported. Use .png, .jpg, or .gif.');
          Exit;
        End;
      try
        St := ReadFileToString(AWords[1]);
        St := EncodeStringBase64(St);
        OT.AppendOTHTML('<img src="data:image/'+ImgType+';base64,'+St+'">');
      except on E : Exception do
        OT.AppendMsg(msError, 'Error showing '+AWords[1]+': '+E.Message);
      End;
    End
  else
    Begin
      OT.AppendMsg(msError, 'Error: There syntax is "img [<filename>]"');
      Exit;
    End;
End;

Procedure CliCmdClose(AText:String; AWords:TStringDynArray);
Begin
  OT.AppendMsg(msNote, 'Disconnecting WebSocket connection');
  Sleep(100);
  OT.FWSH.CloseAll;
End;

Procedure CliCmdExit(AText:String; AWords:TStringDynArray);
Begin
  OT.AppendMsg(msNote, 'Exiting DemoCLI program');
  Sleep(100);
  OT.FWSH.CloseAll;    // close cleanly
  Sleep(100);
  OT.FWSS.Stop(True);
End;

Procedure CliExecute(AText : String);
Var Words : TStringDynArray;
Begin
  AText := Trim(AText);
  OT.AppendOTString('DemoCLI> '+AText);
  if AText = '' then
    Exit;
  Words := AText.Split(' ', TStringSplitOptions.ExcludeEmpty);
  if Length(Words) < 1 then
    Exit;  // this shouldn't happen with the precautions above, but better safe than sorry
  Case Words[0] of
    'help'     : CliCmdHelp    (AText, Words);
    'st'       : CliCmdSt      (AText, Words);
    'sta'      : CliCmdSta     (AText, Words);
    'stl'      : CliCmdStl     (AText, Words);
    'html'     : CliCmdHtml    (AText, Words);
    'dygraph'  : CliCmdDygraph (AText, Words);
    'dygrapha' : CliCmdDygrapha(AText, Words);
    'msg'      : CliCmdMsg     (AText, Words);
    'img'      : CliCmdImg     (AText, Words);
    'close'    : CliCmdClose   (AText, Words);
    'exit'     : CliCmdExit    (AText, Words);
  else
    OT.AppendMsg(msError, 'Error: Invalid command "'+Words[0]+'". Use "help" for a list of the supported commands.');
  End;
End;

Function OnInput(AMsg : TJSONObject; AText : String) : Boolean;
Begin
  Result := False;  // message fully handled, therefore don't queue to FRecvBuf
  if OT.Verbose then
    WriteLn('The user entered command '+AText);
{$IFDEF ExecuteInMain}
  CmdQueue.Add(AText);
  RTLEventSetEvent(CmdAvail);
{$ELSE}
  CliExecute(AText);
{$ENDIF}
End;

Procedure SigIntHandler(Sig:CInt); cdecl;
Begin
  WriteLn;
  WriteLn('Received ^C SigInt, exiting.');
  OT.AppendMsg(msWarning, 'Received ^C SigInt, exiting DemoCLI program');
  Sleep(100);
  OT.FWSH.CloseAll;    // close cleanly
  Sleep(100);
  OT.FWSS.Stop(True);
  Sleep(100);
  // Warning: This does not yet fully work.
  // The good news is that the connection is cleanly closed to the browser
  // (i.e., ObjTerm server).
  // The bad news is that TWebsocketCommunicator.SocketStream is already Nil
  // while its property "Open" is called at various places (e.g., in
  // TOTWSHandler.DoHandleCommunication, TWebsocketCommunicator.Close).
End;

Var SigAct : PSigActionRec;
{$IFDEF ExecuteInMain}
    Cmds   : specialize TList<String>;
{$ENDIF}

Begin
  if ParamCount <> 1 then
    Begin
      WriteLn('Usage: ',ParamStr(0),' port');
      Halt(1);
    End;
  WriteLn('DemoCLI');
  WriteLn('Waiting for connection to port ',ParamStr(1));

  // create an ObjTerm instance
  OT := TObjTerm.Create(StrToInt(ParamStr(1)));
  OT.Verbose       := true;
  OT.HelloIdent    := 'DemoCLI';
  OT.OnConnect     := @OnConnect;
  OT.OnDisconnect  := @OnDisconnect;
  OT.OnHello       := @OnHello;
  OT.OnInput       := @OnInput;

  // prepare
  OS := Nil;
  OG := Nil;
{$IFDEF ExecuteInMain}
  CmdQueue := TThreadStringList.Create;
  CmdQueue.Duplicates := dupAccept;
  CmdAvail := RTLEventCreate;
{$ENDIF}

  // install signal handler for ^C
  New(SigAct);
  SigAct^.sa_handler := SigActionHandler(@SigIntHandler);
  FillChar(SigAct^.sa_mask, SizeOf(SigAct^.sa_mask), #0);
  SigAct^.sa_flags := 0;
{$ifdef Linux}               // Linux specific
  SigAct^.sa_restorer := Nil;
{$endif}
  if fpSigAction(SigInt, SigAct, Nil) <> 0 then
    Begin
      Writeln('Error: ',fpGetErrNo,'.');
      Halt(1);
    End;

{$IFDEF ExecuteInMain}
  // start WebSocket server in background thread
  OT.StartWebSocketServerThread;
  while not OT.FWST.Finished do
    Begin
      RTLEventWaitFor(CmdAvail, 100);   // use timeout so that we don't overlook when the connection is closed
      Cmds := CmdQueue.LockList;
      try
        While Cmds.Count > 0 do
          Begin
            CliExecute(Cmds.First);
            Cmds.Delete(0);
          End;
      finally
        CmdQueue.UnlockList;
      End;
    End;
{$ELSE}
  // start WebSocket server in the main thread
  OT.StartWebSocketServer;
{$ENDIF}

  // clean up
  OT.Free;
{$IFDEF ExecuteInMain}
  CmdQueue.Free;
  RTLEventDestroy(CmdAvail);
{$ENDIF}
End.

