program CasBas;

{*************************************************************************}
{**                                                                     **}
{** Program : CasBas                                                    **}
{**                                                                     **}
{** Author : P.Harvey-Smith, 2006-10-28.                                **}
{**                                                                     **}
{** Purpose : To Translate Dragon BASIC CAS files to CoCo or CoCo to    **}
{**            Dragon, allowing the exchange of BASIC programs between  **}
{**            the two machines.                                        **}
{**                                                                     **}
{*************************************************************************}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, RamothCustomApplicationUnit,
  DragonCoCoCasUnit,DragonCoCoCasNameBlockUnit,
  RamothMemoryStream,DragonCoCoBasic,contnrs,
  DragonCoCoBasicTokens,DragonCoCoCASDefsUnit, ConsoleUtils
  { you can add units after this };

type

  { TCasBas }

  TCasBas = class(TRamothCustomApplication)
  protected
    InFileName      : STRING;       { Filename of input Cas file }
    OutFileName     : STRING;       { Filname of output cas name }
    HeadSyncSize    : INTEGER;      { No of sync bytes before header }
    BlockSyncSize   : INTEGER;      { No of sync bytes between blocks }
    SourceType      : CHAR;         { Source basic, C,D,M,T }
    TransType       : CHAR;         { Destination basic, C,D,M,T }

    LastByte        : BYTE;         { LastByte processed }
    BytesToSkip     : INTEGER;      { Bytes to skip at begining of basic line }

    procedure DoRun; override;
    FUNCTION GetType(TypeStr    : STRING) : CHAR;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

CONST   Major   = 1;
        Minor   = 0;

        DefHeadSyncSize = 128;          { Default header sync size }
        DefBlockSyncSize= 16;           { Default interblock sync size }

        BasicDragon     = 'D';
        BasicCoCo       = 'C';
        BasicMicroCoCo  = 'M';
        BasicText       = 'T';

        Debug           = TRUE;         { Set to true for Debug output }

  { Short options }
          OptSVerbose   = 'v';          { Be verbose! }
          OptSHelp      = 'h';          { Get some help }
          OptSHex       = 'x';          { Hex dump blocks }
          OptSMicrodeal = 'm';          { Dump microdeal blocks }
          OptSBData     = 'b';          { Dump block data }

  { Long options }
          OptLVerbose   = 'verbose';
          OptLHelp      = 'help';
          OptLHex       = 'hexdump';
          OptLMicrodeal = 'microdeal';
          OptLBData     = 'bdata';

  { Option arrays for CheckOpts }
          ShortOpts : string = OptSVerbose+     '::' +
                               OptSHelp+        '::' +
                               OptSHex+         '::' +
                               OptSMicrodeal+   '::' +
                               OptSBData+       '::';

          LongOptsArray : array [1..5] OF AnsiSTRING =
              (OptLVerbose+     '::',
               OptLHelp+        '::',
               OptLHex+         '::',
               OptLMicrodeal+   '::',
               OptLBData+       '::'
              );

{ TCasBas }

FUNCTION TCasBas.GetType(TypeStr    : STRING) : CHAR;

BEGIN;
  TypeStr:=Trim(TypeStr);
  Result:=TypeStr[1];
END;

procedure TCasBas.DoRun;

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

  IF (ParamCount<4) THEN
    LogError('Error: Must supply: input file, output file, to and from basic types');

  InFileName:=ParamStr(1);
  OutFileName:=ParamStr(2);
  SourceType:=GetType(ParamStr(3));
  TransType:=GetType(ParamStr(4));

  IF (NOT FileExists(InFileName)) THEN
    LogError('Error filename %s does not exist',[InFileName]);

  { add your program here }

  // stop program loop
  Terminate;
end;

constructor TCasBas.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TCasBas.Destroy;
begin
  inherited Destroy;
end;

procedure TCasBas.WriteHelp;
begin
  WriteLn(Format('CasBas version %2.2d.%2.2d Copyright 2007 Phill Harvey-Smith',[Major,Minor]));
  WriteLn('This is free software, and must not be distributed for profit !');
  WriteLn;
  WriteLn('CasBas CasFileNameIn CasFileNameOut SourceBasicType DestBasicType');
  WriteLn;
  WriteLn('DestBasicType must be either "Dragon", "CoCo", "MC10" or "Text"');
end;

var
  Application: TCasBas;
begin
  Application:=TCasBas.Create(nil);
  Application.Title:='CasBas';
  Application.Run;
  Application.Free;
end.

