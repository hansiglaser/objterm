(**
 * ObjTerm Demo for testing with a conventional Command Line Interface at the shell
 *
 * (c) 2021 by Johann Glaser
 *)
Program DemoTest;

{$mode objfpc}
{$H+}

Uses {$IFDEF UNIX}
  cthreads, {$ENDIF}
  classes, SysUtils,
  ObjTerm, OTOBase, OTUtils, FPJSON;

Var OT : TObjTerm;

Procedure OnConnect(AAddress:String);
Begin
  WriteLn('Connected to ', AAddress);
End;

Procedure OnDisconnect(AAddress:String);
Begin
  WriteLn('Connection to ', AAddress, ' closed');
End;

Procedure OnInvalidMessage(AMsg : TJSONObject; AError : String);
Begin
  WriteLn(AError);
End;

Procedure OnHello(AHelloMsg, AHelloReplyMsg : TJSONObject);
Begin
  WriteLn('Connected to server version "'+OT.ServerVersion+'" with features '+StrArr2JSONArray(OT.ServerFeatures).AsJSON); // TODO
  WriteLn('  Directly from message: server version is "', AHelloMsg.Strings['version'],'"');
  // overwrite existing fields
  AHelloReplyMsg.Strings['inputenable'] := 'true';
  AHelloReplyMsg.Strings['inputprompt'] := 'DemoTest>';
  AHelloReplyMsg.Strings['promptstyle'] := 'color:#FF4020';
  AHelloReplyMsg.Strings['inputstyle']  := 'color:#80C0FF';
  // add a custom field
  AHelloReplyMsg.Add('custom', 'Custom OnHello Info');
End;

Function OnCmdStatus(AMsg : TJSONObject; ACmdId, AStatus, AMessage : String; AOnCmdStatusData : Pointer) : Boolean;
Begin
  if OT.Verbose then
    WriteLn('OnCmdStatus');
  Result := False;  // message is (or will be) fully handled, therefore don't queue to FRecvBuf
  if AStatus = 'ok' then
    // ignore if everything went fine
    Exit;
  WriteLn('Error with previous command (ID '+ACmdId+'): '+AMessage);
End;

Function OnInput(AMsg : TJSONObject; AText : String) : Boolean;
Begin
  WriteLn('The user entered command '+AText);
  Result := False;  // message fully handled, therefore don't queue to FRecvBuf
End;

Function OnNotify(AMsg : TJSONObject) : Boolean;
Begin
  WriteLn('OnNotify');
  Result := False;  // message fully handled, therefore don't queue to FRecvBuf
End;

Var St      : String;
    Msg     : String;
    Data    : TJSONArray;
    I       : Integer;
    base    : Real;
    Options : TJSONObject;
    OS      : TOTOString;
    OH      : TOTOHTML;
    OG      : TOTODygraph;

Begin
  if ParamCount <> 1 then
    Begin
      WriteLn('Usage: ',ParamStr(0),' port');
      Halt(1);
    End;
  WriteLn('DemoTest');
  WriteLn('  If a client connects or disconnects, a respective message is printed.');
  WriteLn('  Type text and press Enter. It is only sent if a client is connected');
  WriteLn('  Type "exit" to exit.');
  WriteLn('  st String Object');
  WriteLn('  sta Append to previous String Object');
  WriteLn('  stl Query the current length of the previous String Object');
  WriteLn('  html HTML Object');
  WriteLn('  dygraph Dygraph Object');
  WriteLn('  dygrapha Append points to previous Dygraph Object');
  WriteLn('  close Close the connection');
  // create an ObjTerm instance
  OT := TObjTerm.Create(StrToInt(ParamStr(1)));
  OT.Verbose          := true;                 // enable debugging output of the WebSocket server to stdout
  OT.HelloIdent       := ParamStr(0);          // provide identification string for the HelloReply message
  OT.OnConnect        := @OnConnect;           // register callbacks for various events
  OT.OnDisconnect     := @OnDisconnect;
  OT.OnInvalidMessage := @OnInvalidMessage;
  OT.OnHello          := @OnHello;
  OT.OnCmdStatus      := @OnCmdStatus;
  OT.OnInput          := @OnInput;
  OT.OnNotify         := @OnNotify;
  // start WebSocket server in background thread
  OT.StartWebSocketServerThread;
  WriteLn('Waiting for connection to port ',ParamStr(1));
  // conventional command line handling
  while True do
    Begin
      // prompt user for input on conventional terminal via stdin
      Write('> ');
      ReadLn(St);
      // check if an exception was cought in a background thread
      if Assigned(OT.FWST.FatalException) then
        Begin
          WriteLn('Exception in Web Socket Thread');
          raise OT.FWST.FatalException;
          // TODO: react immediately, see https://wiki.freepascal.org/Logging_exceptions#Handling_thread_exceptions
        End;
      // check if we received something from ObjTerm
      While not OT.FRecvBuf.IsEmpty do
        Begin
          Msg := OT.FRecvBuf.Get;
          WriteLn('Got message ''',Msg,''' which was not yet handled');
          Continue;
        End;
      // ignore empty input
      if St = '' then
        Continue;
      // don't proceed without a connection
      if not OT.IsConnected then
        Begin
          WriteLn('No connection.');
          Continue;
        End;
      // execute the user command
      if St = 'exit' then
        Begin
          WriteLn('Ok, stopping the WebSocket server and exiting.');
          OT.FWSH.CloseAll;    // close cleanly
          Sleep(100);
          OT.FWSS.Stop(True);
          Break;
        End;
      if Copy(St,1, 3) = 'st ' then
        Begin
          OS.Free;
          //OS := TOTOString(OT.AppendOTString(Copy(St, 4, Length(St))));
          OS := TOTOString.Create(Copy(St, 4, Length(St)));
          //OS.FCSSStyle := 'font-weight:bold;';
          OT.AppendObj(OS);
        End
      else if Copy(St,1, 4) = 'sta ' then
        Begin
          if not assigned(OS) then
            Begin
              WriteLn('No previous string object');
              Continue;
            End;
          // TODO: this also can't work if the object has vanished from the terminal, e.g., because it was reloaded in the browser
          OS.Append(Copy(St, 5, Length(St)));
        End
      else if Copy(St,1, 3) = 'stl' then
        Begin
          if not assigned(OS) then
            Begin
              WriteLn('No previous string object');
              Continue;
            End;
          // TODO: this also can't work if the object has vanished from the terminal, e.g., because it was reloaded in the browser
          I := OS.GetLength;
          WriteLn('The length is ',I);
        End
      else if Copy(St,1, 5) = 'html ' then
        Begin
          // OT.AppendOTHTML(Copy(St, 6, Length(St))).Free;
          OH := TOTOHTML.Create(Copy(St, 6, Length(St)));
          //OH.FCSSClasses := ['otmsgnote'];
          OT.AppendObj(OH);
          OH.Free;
        End
      else if St = 'dygraph' then
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
        End
      else if St = 'dygrapha' then
        Begin
          // append points to previous graph
          if not assigned(OG) then
            Begin
              WriteLn('No previous graph object');
              Continue;
            End;
          Data := TJSONArray.Create;
          For I := 0 to 19 do
            Begin
              Data.Add(TJSONArray.Create([OG.FData.Count+I, Random*20.0-10.0, Random*20.0-10.0]));
            End;
          OG.AppendPoints(Data);
        End
      else if St = 'close' then
        Begin
          OT.FWSH.CloseAll;
        End
      else
        Begin
          WriteLn('Unknown command in St="'+St+'"');
        End;
    End;
  WriteLn('Waiting for Web Socket Thread to finish.');
  OT.Free;
  WriteLn('End.');
End.

