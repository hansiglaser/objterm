(**
 * ObjTerm Demo using a scope and showing a screenshot and waveform via ObjTerm
 *
 * (c) 2021 by Johann Glaser
 *)
Program DemoPasGPIB;

{$mode objfpc}
{$H+}

// either communicate via USB-TMC or TCP, define one of these two
{$DEFINE USBTMC}
{ $ DEFINE TCP}


Uses
  {$IFDEF UNIX} CThreads, {$ENDIF}
  Classes, SysUtils, BaseUnix, Base64,
  ObjTerm, OTOBase, FPJSON,
  PasGpibUtils,
{$IFDEF USBTMC}
  LibUsbOop, UsbTmc, DevComUSBTMC,
{$ENDIF USBTMC}
{$IFDEF TCP}
  DevComTCP,
{$ENDIF TCP}
  AgilentMSOX3000A,
  Math, FTFont, FPImage, FPCanvas, FPImgCanv, FPWritePNG;

Const
{$IFDEF USBTMC}
  idVendor  = $0957;
  idProduct = $17A6;
{$ENDIF USBTMC}
{$IFDEF TCP}
  Host = '192.168.87.166';
  Port = 5025;
{$ENDIF TCP}

/// Helper Function to convert TAgilentMSOX3000A.Screen for use with EncodeStringBase64
Function ToString(A:TDynByteArray):String;
Begin
  SetLength(Result, Length(A));
  Move(A[0], Result[1], Length(A));
End;

/// Helper Function to convert a TWaveform for use with Dygraph
Function Waveform2JSONArray(AWaveform:TWaveform) : TJSONArray;
Var I : Integer;
Begin
  if (Length(AWaveform.FRealData) = 0) or (Length(AWaveform.FTimes) = 0) then
    raise Exception.Create('No real and/or time data available, don''t forget ConvToReal and ConvTimes.');
  Result := TJSONArray.Create;
  For I := 0 to Length(AWaveform.FRealData)-1 do
    Result.Add(TJSONArray.Create([AWaveform.FTimes[I], AWaveform.FRealData[I]]));
End;

/// Helper Function to create a Histogram from a TWaveform and return it as PNG data in a string
Function MakeHistogramPNG(AWaveform:TWaveform) : String;
Var Histogram : Array[0..255] of Integer;
    I         : Integer;
    HistMax   : Integer;
    AFont     : TFreeTypeFont;
    Canvas    : TFPCustomCanvas;
    Image     : TFPCustomImage;
    Writer    : TFPWriterPNG;
    SS        : TRawByteStringStream;
Begin
  // calculate histogram
  FillChar(Histogram, SizeOf(Histogram), 0);
  For I := 0 to Length(AWaveform.FByteData)-1 do
    Inc(Histogram[AWaveform.FByteData[I]]);
  HistMax := MaxIntValue(Histogram);

  // using fcl-image to draw the image, see https://wiki.lazarus.freepascal.org/fcl-image
  // initialize free type font manager
  ftfont.InitEngine;
  FontMgr.SearchPath:='/usr/share/fonts/truetype/dejavu/';
  AFont:=TFreeTypeFont.Create;
  // Create an image object
  Image := TFPMemoryImage.Create (500, 300);
  // Attach the image to the canvas
  Canvas := TFPImageCanvas.Create(Image);
  // draw
  With Canvas do
    Begin
      Pen.Mode      := pmCopy;
      Pen.Style     := psSolid;
      Pen.Width     := 1;
      Pen.FPColor   := colBlack;
      Brush.Style   := bsSolid;
      Brush.FPColor := colWhite;
      Font          := AFont;
      Font.Name     := 'DejaVuSans';
      Font.Size     := 8;
      // fill white
      FillRect(0, 0, Width, Height);
      // draw x axes
      Line(30-5,     Height-30, Width-30+5, Height-30);
      Line(Width-30, Height-30, Width-30,   Height-30+5);
      // draw y axes
      Line(30,   5,             30, Height-30+5);
      Line(30-5, Height-30-255, 30, Height-30-255);  // tick for maximum value
      // x axis text
      TextOut(30      -(TextWidth('0')               div 2), Height-30+5+4+TextHeight('0'), '0');
      TextOut(Width-30-(TextWidth(IntToStr(HistMax)) div 2), Height-30+5+4+TextHeight('0'), IntToStr(HistMax));
      // y axis text
      TextOut(30-5-2-TextWidth(  '0'), Height-30    +((TextHeight('0'))   shr 1), '0');
      TextOut(30-5-2-TextWidth('255'), Height-30-255+((TextHeight('255')) shr 1), '255');
      // title (centered)
      Font.Size     := 12;
      TextOut((Width - TextWidth('Histogram')) shr 1,5 + TextHeight('Histogram'),'Histogram');
      // draw histogram (horizontally)
      Pen.FPColor := colBlue;
      For I := 0 to 255 do
        Begin
          // line from x=0 to the height
          Line(30, Height-30-I, 30+Round((Histogram[I]*1.0/(HistMax*1.0))*((Width-30-30)*1.0)), Height-30-I);
          // green dot at the maximum value
          DrawPixel(30+Round((Histogram[I]*1.0/(HistMax*1.0))*((Width-30-30)*1.0)), Height-30-I, colDkGreen);
        End;
    End;
  // Create the PNG writer
  Writer := TFPWriterPNG.Create;
  Writer.Indexed := True;
  // store to string stream
  SS := TRawByteStringStream.Create;
  Writer.ImageWrite(SS, Image);
  // return the data
  Result := SS.DataString;
  AFont.Free;
  SS.Free;
  Writer.Free;
  Canvas.Free;
  Image.Free;
End;

Var OT         : TObjTerm;
    InputEvent : PRTLEvent;

Procedure OnHello(AHelloMsg, AHelloReplyMsg : TJSONObject);
Begin
  AHelloReplyMsg.Strings['inputenable'] := 'true';
  AHelloReplyMsg.Strings['inputprompt'] := 'DemoPasGPIB>';
  AHelloReplyMsg.Strings['promptstyle'] := '';
  AHelloReplyMsg.Strings['inputstyle']  := '';
End;

Function OnInput(AMsg : TJSONObject; AText : String) : Boolean;
Begin
  Result := False;  // message fully handled, therefore don't queue to FRecvBuf
  RTLEventSetEvent(InputEvent);
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

Var
{$IFDEF USBTMC}
  Context  : TLibUsbContext;
  Intf     : TUSBTMCIntfInfos;
  Tmc      : TUSBTMCUSB488;
  Comm     : TUSBTMCCommunicator;
{$ENDIF USBTMC}
{$IFDEF TCP}
  Comm     : TTCPCommunicator;
{$ENDIF TCP}
  MSOX     : TAgilentMSOX3000A;
  I        : Integer;
  Waveform : TWaveform;

{$IFDEF USBTMC}
Procedure USBTMCErrorHandler;
Var I    : Integer;
    Code : Integer;
    Msg  : String;
Begin
  if not assigned(MSOX) then Exit;    // prevent accessing the device before its constructor has finished
  // print all errors in the queue
  For I := 0 to 20 do       // 34410A and 34461A can store up to 20 errors
    Begin
      {$IFDEF USBTMC}
      if (Tmc.ReadStatusByte and IEEE488_StatusByte_ErrorQueue) = 0 then Break;
      {$ENDIF}
      if I = 0 then
        WriteLn('Error Queue after last command '''+Comm.LastSend+''':');
      MSOX.GetNextError(Code,Msg);
      WriteLn('  ',Code,': ',Msg);
      if Code = 0 then Break;
    End;
End;
{$ENDIF USBTMC}

Var SigAct  : PSigActionRec;
    OS      : TOTOString;
    MA      : TDynMeasureResultArray;
    Data    : TJSONArray;
    Options : TJSONObject;

Begin
  if ParamCount <> 1 then
    Begin
      WriteLn('Usage: ',ParamStr(0),' port');
      Halt(1);
    End;
  WriteLn('DemoPasGPIB');

  // create an ObjTerm instance
  OT := TObjTerm.Create(StrToInt(ParamStr(1)));
  OT.HelloIdent    := 'DemoPasGPIB';
  OT.OnHello       := @OnHello;
  OT.OnInput       := @OnInput;

  // prepare
  InputEvent := RTLEventCreate;

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

  // start WebSocket server in background thread
  OT.StartWebSocketServerThread;

  { device communicator }
{$IFDEF USBTMC}
  // device connector via USB-TMC
  Context := TLibUsbContext.Create;
  Intf := TUSBTMCUSB488.Scan(Context);
  if Length(Intf) = 0 then
    Begin
      WriteLn('Error: No USB devices with TMC and GPIB found');
      Halt;
    End;
  // search appropriate device in list and create UsbTmc handler
  For I := 0 to Length(Intf)-1 do
    if (Intf[I].DevDescr.idVendor  = idVendor) and
       (Intf[I].DevDescr.idProduct = idProduct) then
      Begin
        Tmc := TUSBTMCUSB488.Create(Context,Intf[I]);
        break;
      End;
  if not assigned(Tmc) then
    Begin
      WriteLn('Error: No matching USB devices ',IntToHex(idVendor,4),':',IntToHex(idProduct,4),' found');
      Halt;
    End;
  Comm := TUSBTMCCommunicator.Create(Tmc);
  Comm.SetTimeout(2000000{us});
  Comm.ErrorHandler := @USBTMCErrorHandler;
{$ENDIF USBTMC}
{$IFDEF TCP}
  // device connector via TCP/IP
  Comm := TTCPCommunicator.Create(Host,Port);
{$ENDIF TCP}
  Comm.SetTimeout(5000000{us});
  { remote instrument }
  MSOX := TAgilentMSOX3000A.Create(Comm);

  WriteLn('Waiting for connection to port ',ParamStr(1));

  // in the mean time setup the scope
  MSOX.Reset;
  Sleep(1000);
  MSOX.Stop;   // stop acquisition, because it disrupts the setup commands below
  MSOX.SetTimebaseMode(tmMain);
  MSOX.Run;
  MSOX.Channel[CH2].Display(false);  // switch off channel 2
  MSOX.Channel[CH3].Display(false);  // switch off channel 3
  MSOX.Channel[CH4].Display(false);  // switch off channel 4
  MSOX.Channel[CH1].SetCoupling(cpDC);
  MSOX.Channel[CH1].SetBWLimit(True);
  MSOX.Channel[CH1].SetVDiv(0.5);  // 0.5V/div

  MSOX.Channel[CH1].SetVDiv(0.5);  // 0.5V/div
  MSOX.Channel[CH1].SetOffset(1.0);   // set base line to 1.0V = 2 div below center
  MSOX.SetTDiv(0.0005);  // 500us/div
  MSOX.SetTriggerSource(tsCH1);
  MSOX.SetTriggerType(ttEdge);
  MSOX.SetTriggerSlope(tsPositive);
  MSOX.SetTriggerLevel(1.25);   // at 1.0V (half of 2.5Vpp signal)

  // setup measurement: Amplitude, Frequency, Duty Cycle, Rise Time
  MSOX.MeasureClear;
  MSOX.MeasureAdd(mtVAmplitude, msCH1);
  MSOX.MeasureAdd(mtFrequency,  msCH1);
  MSOX.MeasureAdd(mtDutyCycle,  msCH1);
  MSOX.MeasureAdd(mtRisetime,   msCH1);

  // start acquisition
  MSOX.SetTriggerMode(tmNormal);

  // everything prepared, now wait until the ObjTerm connects
  OT.WaitForConnect;

  // user information
  Sleep(300);   // wait until Hello messages are over and the ObjTerm has printed its note of the connection
  OT.AppendOTString('Test program demonstrating remote control of the Agilent InfiniiVision MSO-X 3000A scopes'#10);
  OT.AppendOTString('  Identity: '+MSOX.Identify);
  OT.AppendOTString('  Current date at scope: '+MSOX.GetDateTime+#10);
  OT.AppendOTString(
    #10+
    'Please prepare the test by connecting the probe of channel 1 to the'#10+
    'square wave calibration output. Then press [Enter] to save a'#10+
    'screenshot.');

  // wait until the user sends any command
  RTLEventWaitFor(InputEvent);

  // clear statistics, all four start with 0 counts
  MSOX.MeasureStatisticsReset;

  OS := TOTOString(OT.AppendOTString(#10'Acquisition'));
  For I := 0 to 4 do
    Begin
      OS.Append('.');
      Sleep(500);
    End;
  OS.Append(#10'Done.');

  // query measurements
  MSOX.SetMeasureStatistics(stAll);
  MA := MSOX.GetMeasureResults;
  // make a nice table
  OT.AppendOTHTML(
    '<style type="text/css">'#10+
      'table.demopasgpib {'#10+
        'margin: 5px;'#10+
        'padding: 3px;'#10+
        'background:#808080;'#10+
      '}'#10+
      '.demopasgpib th {'#10+
        'background: #C0FFC0;'#10+
        'padding:3px;'#10+
        'color:#000000;'#10+
        'font-weight:bold;'#10+
      '}'#10+
      '.demopasgpib td {'#10+
        'background: #C0C0FF;'#10+
        'padding:3px;'#10+
        'color:#000000;'#10+
        'text-align:right;'#10+
      '}'#10+
    '</style>'#10+
    '<p>Measurement Results:</p>'#10+
    '<table class="demopasgpib">'#10+
      '<tr>'#10+
        '<th></th>'#10+
        '<th>Amplitude</th>'#10+
        '<th>Frequency</th>'#10+
        '<th>Duty Cycle</th>'#10+
        '<th>Rise Time</th>'#10+
      '</tr>'#10+
      '<tr>'#10+
        '<th>Current</th>'#10+
        '<td class="demopasgpib">'+FloatToStrSI(MA[0].Current, FormatSettings, True, ffFixed, 0, 2)+'V</td>'#10+
        '<td>'+FloatToStrSI(MA[1].Current, FormatSettings, True, ffFixed, 0, 4)+'Hz</td>'#10+
        '<td>'+FloatToStrSI(MA[2].Current, FormatSettings, True, ffFixed, 0, 2)+'%</td>'#10+
        '<td>'+FloatToStrSI(MA[3].Current, FormatSettings, True, ffFixed, 0, 2)+'s</td>'#10+
      '</tr>'#10+
      '<tr>'#10+
        '<th>Min</th>'#10+
        '<td>'+FloatToStrSI(MA[0].Min, FormatSettings, True, ffFixed, 0, 2)+'V</td>'#0+
        '<td>'+FloatToStrSI(MA[1].Min, FormatSettings, True, ffFixed, 0, 4)+'Hz</td>'#0+
        '<td>'+FloatToStrSI(MA[2].Min, FormatSettings, True, ffFixed, 0, 2)+'%</td>'#0+
        '<td>'+FloatToStrSI(MA[3].Min, FormatSettings, True, ffFixed, 0, 2)+'s</td>'#0+
      '</tr>'#0+
      '<tr>'#0+
        '<th>Max</th>'#0+
        '<td>'+FloatToStrSI(MA[0].Max, FormatSettings, True, ffFixed, 0, 2)+'V</td>'#0+
        '<td>'+FloatToStrSI(MA[1].Max, FormatSettings, True, ffFixed, 0, 4)+'Hz</td>'#0+
        '<td>'+FloatToStrSI(MA[2].Max, FormatSettings, True, ffFixed, 0, 2)+'%</td>'#0+
        '<td>'+FloatToStrSI(MA[3].Max, FormatSettings, True, ffFixed, 0, 2)+'s</td>'#0+
      '</tr>'#0+
      '<tr>'#0+
        '<th>Mean</th>'#0+
        '<td>'+FloatToStrSI(MA[0].Mean, FormatSettings, True, ffFixed, 0, 2)+'V</td>'#0+
        '<td>'+FloatToStrSI(MA[1].Mean, FormatSettings, True, ffFixed, 0, 4)+'Hz</td>'#0+
        '<td>'+FloatToStrSI(MA[2].Mean, FormatSettings, True, ffFixed, 0, 2)+'%</td>'#0+
        '<td>'+FloatToStrSI(MA[3].Mean, FormatSettings, True, ffFixed, 0, 2)+'s</td>'#0+
      '</tr>'#10+
      '<tr>'#10+
        '<th>StdDev</th>'#10+
        '<td>'+FloatToStrSI(MA[0].StdDev, FormatSettings, True, ffFixed, 0, 2)+'V</td>'#10+
        '<td>'+FloatToStrSI(MA[1].StdDev, FormatSettings, True, ffFixed, 0, 4)+'Hz</td>'#10+
        '<td>'+FloatToStrSI(MA[2].StdDev, FormatSettings, True, ffFixed, 0, 2)+'%</td>'#10+
        '<td>'+FloatToStrSI(MA[3].StdDev, FormatSettings, True, ffFixed, 0, 2)+'s</td>'#10+
      '</tr>'#10+
      '<tr>'#10+
        '<th>Count</th>'#10+
        '<td>'+FloatToStrSI(MA[0].Count, FormatSettings)+'</td>'#10+
        '<td>'+FloatToStrSI(MA[1].Count, FormatSettings)+'</td>'#10+
        '<td>'+FloatToStrSI(MA[2].Count, FormatSettings)+'</td>'#10+
        '<td>'+FloatToStrSI(MA[3].Count, FormatSettings)+'</td>'#10+
      '</tr>'#10+
    '</table>');
  (* The above HTML snipped places a <style> element in the surrounding <div>.
   * While these should normally be in the <head>, HTML 5 added support to place
   * them also in the <body>. However, the support for scoped (i.e., local)
   * <style> was withdrawn. See also
   *  - https://stackoverflow.com/questions/28633038/what-is-the-current-state-of-the-scoped-attribute-for-the-style-element-in-htm
   *  - https://developer.mozilla.org/en-US/docs/Web/CSS/:scope1
   * Therefore the solution above uses the class "demopasgpib" for the <table>,
   * and the inner elements.
   * However, the style still applies to the whole ObjTerm. I.e., if another
   * <table class="demopasgpib"> is or was added, then the style also applies.
   *)

  // get a screenshot
  MSOX.SetHardcopyOptions(false);  // inksaver off: image is not inverted, i.e., mostly black
  // and show as a HTML <img> with included data on ObjTerm
  OT.AppendOTHTML('<img style="margin:5px;" src="data:image/png;base64,'+EncodeStringBase64(ToString(MSOX.Screen(ifPng, ipColor)))+'">');

  // get raw data
  MSOX.MeasureStatisticsReset;
  MSOX.Single;
  MSOX.TriggerForce;              // force trigger, because trigger mode is always "normal" in single shot mode
  Sleep(100);
  MSOX.SetWaveformFormat(wfByte);
  MSOX.SetWaveformPointsMode(wpmRaw);
  MSOX.SetWaveformPointsCount(1000);
  Waveform := MSOX.GetWaveformPreamble;
  MSOX.GetWaveformData(Waveform);
  Waveform.ConvToReal;
  Waveform.ConvTimes;
  // and show as Dygraph on ObjTerm
  Data    := Waveform2JSONArray(Waveform);
  Options := TJSONObject.Create(['labels',TJSONArray.Create(['t [s]', 'V [V]'])]);
  OT.AppendOTDygraph(700, 400, Data, Options);
  // calculate an (admittedly not very useful) colorful histogram of the values
  if Length(Waveform.FByteData) > 0 then
  // and show as a HTML <img> with included data on ObjTerm
  OT.AppendOTHTML('<img style="margin:5px;" src="data:image/png;base64,'+EncodeStringBase64(MakeHistogramPNG(Waveform))+'">');
  Waveform.Free;

  // give the background threads some time to complete the communication
  Sleep(1000);

  // clean up
  WriteLn('Cleaning up');
  // pas-gpib
  MSOX.Free;
  Comm.Free;
{$IFDEF TCP}
{$ENDIF TCP}
{$IFDEF USBTMC}
  Context.Free;
{$ENDIF USBTMC}
  // ObjTerm
  Sleep(100);
  OT.FWSH.CloseAll;
  Sleep(100);
  OT.FWSS.Stop(True);
  OT.Free;
  RTLEventDestroy(InputEvent);
End.

