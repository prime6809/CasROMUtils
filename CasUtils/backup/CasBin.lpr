program CasBin;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp ,DragonCoCoCasUnit,DragonCoCoCasNameBlockUnit,
  RamothMemoryStream,DragonCoCoBasic,contnrs, DragonCoCoBasicTokens,
  DragonCoCoCASDefsUnit, ConsoleUtils, RamothCustomApplicationUnit
  { you can add units after this };

type
  { TCasBin }

  TCasBin = class(TRamothCustomApplication)
  protected
    AppendToCas     : BOOLEAN;
    CasFile         : TDragonCoCoCas;
    CasHead         : TDragonCoCoCasNameBlock;
    Data            : TRamothMemoryStream;
    CasFileName     : STRING;
    BinFileName     : STRING;
    NameOnTape      : STRING;

    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    PROCEDURE Process;
  end;

CONST   Major   = 1;
        Minor   = 1;

        DefHeadSyncSize = 128;          { Default header sync size }
        DefBlockSyncSize= 16;           { Default interblock sync size }

        { Short options }
                OptSVerbose = 'v';          { Be verbose! }
                OptSHelp    = 'h';          { Get some help }
                OptSAppend  = 'a';          { Append to cas output file }
                OptSLoad    = 'l';          { Load address }
                OptSExec    = 'e';          { Exec address }

        { Long options }
                OptLVerbose = 'verbose';
                OptLHelp    = 'help';
                OptLAppend  = 'append';
                OptLLoad    = 'load';
                OptLExec    = 'exec';

        { Option arrays for CheckOpts }
                ShortOpts : string = OptSVerbose+   '::' +
                                     OptSHelp+      '::' +
                                     OptSAppend+    '::' +
                                     OptSLoad+      ':' +
                                     OptSExec+      ':' ;

                LongOptsArray : array [1..5] OF AnsiSTRING =
                    (OptLVerbose+       '::',
                     OptLHelp+          '::',
                     OptLAppend+        '::',
                     OptLLoad+          ':',
                     OptLExec+          ':'
                    );


{ TCasBin }

PROCEDURE TCasBin.Process;

BEGIN;
  IF(AppendToCas) THEN
    CasFile:=TDragonCoCoCas.CreateAppendCasFile(CasFileName)
  ELSE
    CasFile:=TDragonCoCoCas.Create;

  CasHead:=TDragonCoCoCasNameBlock.Create;
  Data:=TRamothMemoryStream.Create;
  TRY
    WITH CasHead DO
    BEGIN;
      IF (FileExists(BinFileName)) THEN
        Data.LoadFromFile(BinFileName);

      FileName:=NameOnTape;
      LoadAddress:=StrToIntDef(GetOptionValue(OptSLoad,OptLLoad),$400);
      ExecAddress:=StrToIntDef(GetOptionValue(OptSExec,OptLExec),$400);
      FileID:=FtMachineCode;
    END;
    CasFile.WriteFile(CasHead,Data,DefHeadSyncSize,DefBlockSyncSize);
    CasFile.SaveToFile(CasFileName);
  FINALLY
    CasFile.Free;
    CasHead.Free;
    Data.Free;
  END

END;

procedure TCasBin.DoRun;
begin
  // quick check parameters
  LogError(CheckOptions(ShortOpts,LongOptsArray));

  // parse parameters
  IF HasOption('h', 'help')THEN
  BEGIN;
    WriteHelp;
    Terminate;
    Exit;
  end;

  {Verbose flag}
  IF (HasOption(OptSVerbose, OptLVerbose)) THEN
    DebugLevel:=StrToIntDef(GetOptionValue(OptSVerbose, OptLVerbose),$0000);

  IF (ParamCount<3) THEN
    LogError('Error: Must supply: cas file, binary file, name on tape');

  {Collect parameters}
  CasFileName:=ParamStr(1);
  BinFileName:=ParamStr(2);
  NameOnTape:=ParamStr(3);
  AppendToCas:=HasOption(OptSAppend,OptLAppend);

  IF (NOT FileExists(BinFileName)) THEN
    LogError('Error: BinFile '+BinFileName+' does not exist!');

  IF (Errors.Count=0) THEN
    Process;

  CheckErrorTerminate;
{  IF (Errors.Count<>0) THEN
  BEGIN;
    ShowException(Exception.Create(Errors.Text));
    Terminate;
    Exit;
  END;
}
  // stop program loop
  Terminate;
end;

constructor TCasBin.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TCasBin.Destroy;
begin
  inherited Destroy;
end;

procedure TCasBin.WriteHelp;
begin
  WriteLn(Format('CasBin version %2.2d.%2.2d Copyright 2011 Phill Harvey-Smith',[Major,Minor]));
  WriteLn('This is free software, and must not be distributed for profit !');
  WriteLn;
  WriteLnFmt('%s CasFileName BinFileName NameOnTape [<params>]',[ExeName]);
  WriteLn('Note: addresses in decimal, unless prefixed by $');
  WriteLn;
  WriteLn('The following optional parameters may be specified : ');
  WriteLn(' -a, --append     : Append to output cas file');
  WriteLn(' -e, --exec=      : Specify exec address');
  WriteLn(' -h, --help       : Display this help.');
  WriteLn(' -l, --load=      : Specify load address');
  WriteLn(' -v, --verbose=   : Set verbosity level');
end;

var
  Application: TCasBin;
begin
  Application:=TCasBin.Create(nil);
  Application.Title:='CasBin';
  Application.Run;
  Application.Free;
end.

