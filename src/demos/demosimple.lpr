Program DemoSimple;
Uses
  {$IFDEF UNIX} CThreads, {$ENDIF}
  Classes, SysUtils, Base64,
  ObjTerm, OTOBase;

Var OT : TObjTerm;
    OS : TOTOString;

Begin
  // create an ObjTerm instance
  OT := TObjTerm.Create(5005);
  // start WebSocket server in background thread
  OT.StartWebSocketServerThread;
  WriteLn('Waiting for connection to port 5005');
  // everything prepared, now wait until the ObjTerm connects
  OT.WaitForConnect;
  // print plain text string
  OS := TOTOString(OT.AppendOTString('Hello World! as plain text'));
  // print HTML
  OT.AppendOTHTML('<b>Hello World!</b> as HTML');
  // add more text to the string from above
  OS.Append(' and some later appended text');
  // query the total length of that text
  WriteLn('The string now has a length of ',OS.GetLength);
  // show an image (using HTML and data URL)
  OT.AppendOTHTML(
    '<p>Image from <a href="https://wiki.selfhtml.org/wiki/Grafik/Grafiken_mit_Data-URI">SelfHTML</a>'#10+
    '<img width="16" height="16" alt="tick" src="data:image/gif;base64,R0lGODdhEAAQAMwAAPj7+FmhUYjNfGuxYY'+
    'DJdYTIeanOpT+DOTuANXi/bGOrWj6CONzv2sPjv2CmV1unU4zPgI/Sg6DJnJ3ImTh8Mtbs00aNP1CZSGy0YqLEn47RgXW8amasW'+
    '7XWsmmvX2iuXiwAAAAAEAAQAAAFVyAgjmRpnihqGCkpDQPbGkNUOFk6DZqgHCNGg2T4QAQBoIiRSAwBE4VA4FACKgkB5NGReAS'+
    'FZEmxsQ0whPDi9BiACYQAInXhwOUtgCUQoORFCGt/g4QAIQA7">');
//  OT.AppendOTHTML('<img style="margin:5px;" src="data:image/png;base64,'+EncodeStringBase64({... image data variable ...})+'">');
  // show a message with severity (which is just an TOTOString with predefined formatting)
  OT.AppendMsg(msNote,    'This is a note');
  OT.AppendMsg(msWarning, 'This is a warning');
  OT.AppendMsg(msError,   'This is a error');
  OT.AppendMsg(msFatal,   'This is a fatal error');
  // show an interactive graph
//  OG := OT.AppendOTDygraph({... graph data variables ...});
//  OG.AppendPoints({... more graph data ...});

  // close connection
  Sleep(100);
  OT.FWSH.CloseAll;
  Sleep(100);
  OT.FWSS.Stop(True);
  OT.Free;
End.
