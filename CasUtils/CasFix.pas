program CasFix;

{*************************************************************************}
{**                                                                     **}
{** Program : CasFix                                                    **}
{**                                                                     **}
{** Author : P.Harvey-Smith, 2006-10-28, 2021-11-16.                    **}
{**                                                                     **}
{** Purpose : To allow the manipulation of Dragon 32/64/Alpha and Tandy **}
{**            CoCo cassette image files, so that they may be optimised **}
{**            for use on an emulator, or to write to a real cassette.  **}
{**                                                                     **}
{*************************************************************************}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp , RamothCustomApplicationUnit,
  ConsoleUtils, DragonCoCoCasUnit,DragonCoCoCasNameBlockUnit,
  RamothMemoryStream,DragonCoCoCASDefsUnit
  { you can add units after this };

type

  { TCasFix }

  TCasFix = class(TRamothCustomApplication)
  protected
    HeadSyncSize    : INTEGER;
    BlockSyncSize   : INTEGER;
    FixChecksum     : BOOLEAN;
    InFileName      : STRING;
    OutFileName     : STRING;

    procedure DoRun; override;
    PROCEDURE Process;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

CONST   Major   = 1;
        Minor   = 1;

        DefHeadSyncSize = 128;          { Default header sync size }
        DefBlockSyncSize= 16;           { Default interblock sync size }

        { Short options }
                OptSVerbose   = 'v';          { Be verbose! }
                OptSHelp      = 'h';          { Get some help }
                OptSHeadSync  = 'H';          { Head sync count }
                OptSSync      = 's';          { Sync count }
                OptSFix       = 'f';          { Fix checksum }

        { Long options }
                OptLVerbose   = 'verbose';
                OptLHelp      = 'help';
                OptLHeadSync  = 'headsync';
                OptLSync      = 'sync';
                OptLFix       = 'fix';

        { Option arrays for CheckOpts }
                ShortOpts : string = OptSVerbose+     '::' +
                                     OptSHelp+        '::' +
                                     OptSHeadSync+    ':' +
                                     OptSSync+        ':' +
                                     OptSFix+         '::';

                LongOptsArray : array [1..5] OF AnsiSTRING =
                    (OptLVerbose+     '::',
                     OptLHelp+        '::',
                     OptLHeadSync+    ':',
                     OptLSync+        ':',
                     OptLFix+         '::'
                    );

{ TCasFix }

{*************************************************************************}
{**                                                                     **}
{** Process the input cas file                                          **}
{**                                                                     **}
{** Generate blocks of sync bytes, as specified by the user             **}
{** Scan the input file for cassette blocks                             **}
{** For each block found write it to the output file, plus sync bytes   **}
{** Write the output file back to disk.                                 **}
{**                                                                     **}
{*************************************************************************}

PROCEDURE TCasFix.Process;

VAR     InCasFile       : TDragonCoCoCas;
        OutCasFile      : TDragonCoCoCas;
        Data            : TRamothMemoryStream;
        HeadSyncData    : TRamothMemoryStream;
        BlockSyncData   : TRamothMemoryStream;
        Count           : INTEGER;

BEGIN;
  InCasFile:=TDragonCoCoCas.Create;
  OutCasFile:=TDragonCoCoCas.Create;

  {Init Sync byte blocks }
  HeadSyncData:=TRamothMemoryStream.Create;
  BlockSyncData:=TRamothMemoryStream.Create;

  HeadSyncData.WriteRepetedChars(CHR(SyncByte),HeadSyncSize);
  BlockSyncData.WriteRepetedChars(CHR(SyncByte),BlockSyncSize);

  {Process the blocks}
  TRY
    InCasFile.LoadCasFile(InFileName);

    Count:=0;
    {Scan each block in file}
    WITH InCasFile DO
    BEGIN;
      WHILE (InCasFile.MoreBlocks) DO
      BEGIN;

        {Get a block from input}
        Data:=InCasFile.GetNextRawBlock;
        TRY
          IF (Data.Size<>0) THEN
          BEGIN;
            IF (FixChecksum) THEN
                RecalcChecksum(Data,0);

            {If filename block, write initial sync count, else interblock}
            {also write initial sync count for first block in file}
            IF ((InCasFile.BlockType=BtFileName) OR (Count=1)) THEN
            BEGIN;
              OutCasFile.WriteRawBlock(Data,HeadSyncData);
              Count:=Count+1;
            END
            ELSE
            BEGIN;
              OutCasFile.WriteRawBlock(Data,BlockSyncData);
            END;
          END;
        FINALLY
          Data.Free;
        END;
      END;
    END;

    {Write the output file}
    OutCasFile.SaveToFile(OutFileName);
  FINALLY
    InCasFile.Free;
    OutCasFile.Free;
    HeadSyncData.Free;
    BlockSyncData.Free;
  END;
END;


procedure TCasFix.DoRun;

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

  IF (ParamCount < 2) THEN
    LogError('Error: must supply input and output filenames');

  InFileName:=ParamStr(1);
  OutFileName:=ParamStr(2);

  IF (NOT FileExists(InFileName)) THEN
    LogError('Error: infile %s does not exist',[InFileName]);

  { Process options }
  HeadSyncSize:=StrToIntDef(GetOptionValue(OptSHeadSync,OptLHeadSync),DefHeadSyncSize);
  BlockSyncSize:=StrToIntDef(GetOptionValue(OptSSync,OptLSync),DefBlockSyncSize);
  FixChecksum:=HasOption(OptSFix,OptLFix);

  IF (Errors.Count=0) THEN
    Process;

  { add your program here }

  CheckErrorTerminate;

  // stop program loop
  Terminate;
end;

constructor TCasFix.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TCasFix.Destroy;
begin
  inherited Destroy;
end;

procedure TCasFix.WriteHelp;
begin
  WriteLn(Format('CasFix version %2.2d.%2.2d Copyright 2007-2010 Phill Harvey-Smith',[Major,Minor]));
  WriteLn('This is free software, and must not be distributed for profit !');
  WriteLn;
end;

var
  Application: TCasFix;
begin
  Application:=TCasFix.Create(nil);
  Application.Title:='CasFix';
  Application.Run;
  Application.Free;
end.

