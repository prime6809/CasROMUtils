program CasCat;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp , RamothCustomApplicationUnit,ConsoleUtils,
  DragonCoCoCasUnit,DragonCoCoCasNameBlockUnit,
  RamothMemoryStream,DragonCoCoCASDefsUnit, MemMapUnit
  { you can add units after this };

type

  { TCasCat }

  TCasCat = class(TRamothCustomApplication)
  protected
    Extract         : BOOLEAN;
    Microdeal       : BOOLEAN;
    HexDumpBlocks   : BOOLEAN;
    BlockData       : BOOLEAN;
    Errors          : TStringList;

    MemMap          : TMemMap;

    procedure DoRun; override;
    PROCEDURE LogError(ErrorMsg : STRING);
    FUNCTION StrPad(ToPad   : STRING;
                    PadChar : CHAR;
                    Len     : INTEGER) : STRING;

    PROCEDURE DumpBlocks(CasFile : TDragonCoCoCas);
    PROCEDURE CatCas(FileName       : STRING);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

CONST   Major   = 1;
        Minor   = 1;

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
{ TCasCat }

FUNCTION TCasCat.StrPad(ToPad   : STRING;
                        PadChar : CHAR;
                        Len     : INTEGER) : STRING;

VAR CurrentLen  : INTEGER;

BEGIN;
  CurrentLen:=Length(ToPad);
  Result:=ToPad;
  IF (CurrentLen<Len) THEN
  BEGIN;
    SetLength(Result,Len);
    FillChar(Result[CurrentLen+1],Len-CurrentLen,PadChar);
  END;
END;

PROCEDURE TCasCat.DumpBlocks(CasFile : TDragonCoCoCas);

VAR     Found	        : TFindBlock;
        LocalChecksum   : BYTE;
        LogLine         : STRING;
        FileNameBlock   : TDragonCoCoCasNameBlock;
        HexLines        : TStringList;
        HexLine         : INTEGER;
        BlockPos        : Int64;
        EndBlockPos     : Int64;
        MDCount         : BYTE;
        MDAddr          : WORD;
        BlockNo         : INTEGER;

BEGIN;
  FileNameBlock:=TDragonCoCoCasNameBlock.Create(NameBlockLen);
  TRY
    WITH CasFile DO
    BEGIN;
      Position:=0;
      BlockNo:=-1;
      Found:=FindNextBlock;
      WHILE (Found<>BlkNoMore) DO
      BEGIN;
        BlockNo:=BlockNo+1;
        LogLine:=Format('BlockNo=%d, Type=%s, Len=%4.4X, File offset=%8.8X, ',[BlockNo,DecodeBlockType,BlockLen,CasFile.Position]);

        BlockPos:=Position;

        IF (BlockType=BtFileName) THEN
        BEGIN;
          WriteLn(LogLine);
          FileNameBlock.ReadFromStream(CasFile,FALSE);
          WITH FileNameBlock DO
            LogLine:=Format('    Filename=%s, Type=%s, ',[FileName,DecodeFileID]);
        END;

        LocalChecksum:=ChecksumFoundBlock;

        IF (CksumValid) THEN
          LogLine:=LogLine+Format('Checksum valid=%2.2X',[Checksum])
        ELSE
          LogLine:=LogLine+Format('Checksum invalid : Sum=%2.2X, Calculated=%2.2X',[Checksum,LocalChecksum]);

        WriteLn(LogLine);
        IF (HexDumpBlocks) THEN
        BEGIN;
          HexLines:=HexDump(BlockPos,BlockLen);
          FOR HexLine:=0 TO (HexLines.Count-1) DO
            WriteLn(HexLines.Strings[HexLine]);

          HexLines.Free;
        END;

        IF ((Microdeal) AND (BlockNo > 3)) THEN
        BEGIN;
          EndBlockPos:=Position;
          Position:=BlockPos;
          MDCount:=ReadByte;
          MDAddr:=ReadMotorolaWord;
          WriteLnFmt('Load $%02X bytes at $%04X',[MDCount,MDAddr]);
          Position:=EndBlockPos;
          MemMap.SetVal(MDAddr,MDCount);
        END;

        Found:=FindNextBlock
      END;
      Position:=0;
    END;
  FINALLY
    FileNameBlock.Free;
  END;
END;

PROCEDURE TCasCat.CatCas(FileName       : STRING);

VAR     CasFile         : TDragonCoCoCas;
        Idx             : INTEGER;
        Line            : STRING;
        Line2           : STRING;
        Data            : TRamothMemoryStream;
        OutFileName     : STRING;
        NextBlkAddr     : WORD;

BEGIN;
  CasFile:=TDragonCoCoCas.Create;
  TRY
    CasFile.LoadCasFile(FileName);
    WriteLn('Directory of ',FileName);
    WriteLn(Format('Found %d files',[CasFile.FilesInCas]));

    WITH CasFile DO
    BEGIN;
      WriteLn('FileName ID                ASCII         Gap           Load Exec Len');
      FOR Idx:=0 TO (FilesInCas-1) DO
      BEGIN;
        Data:=GetFileData(Idx,0);
        TRY
          WITH TDragonCoCoCasNameBlock(Filenames.Items[Idx]) DO
          BEGIN;
            Line:=StrPad(FileName,' ',9);
            Line:=Line+Format('%2.2X:%s %2.2X:%s    %2.2X:%s  %4.4X %4.4X %4.4X',
                          [FileID,DecodeFileId,AsciiFlag,DecodeAsciiFlag,
                           GapFlag,DecodeGapFlag,LoadAddress,ExecAddress,
                           Data.Size]);

            OutFileName:=Trim(FileName);
            IF ((Microdeal) AND (Data.Size = $0E)) THEN
            BEGIN;
              Data.Seek($0C,soFromBeginning);
              NextBlkAddr:=Data.ReadMotorolaWord;
              Line2:=Format('Following block loads at : %4.4X',[NextBlkAddr]);
            END
            ELSE
              Line2:='';

          END;

          WriteLn(Line);
          IF (Line2<>'') THEN
            WriteLn(Line2);

          IF (Extract) THEN
          BEGIN;
            IF (OutFileName='') THEN
              OutFileName:=Format('FILE-%D.DBN',[Idx])
            ELSE
              OutFileName:=Format('%s-%d.DBN',[OutFileName,Idx]);

            Data.SaveToFile(OutFileName);
          END;
        FINALLY
          Data.Free;
        END;
      END;

      IF (BlockData) THEN
        DumpBlocks(CasFile);

      IF (Microdeal) THEN
        WriteLn(MemMap.GetMap);
    END;
  FINALLY
    WriteLn;
    CasFile.Free;
  END;
END;

procedure TCasCat.DoRun;

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

  IF (ParamCount<1) THEN
    LogError('Error: Must supply: control file, menu ROM file and output file names');

  IF (NOT FileExists(ParamStr(1))) THEN
    LogError('Error filename '+Paramstr(1)+' does not exist');

  HexDumpBlocks:=HasOption(OptSHex,OptLHex);
  Microdeal:=HasOption(OptSMicrodeal,OptLMicrodeal);
  BlockData:=HasOption(OptSBData,OptLBData) OR HexDumpBlocks OR Microdeal;

  CatCas(ParamStr(1));

  { add your program here }
  IF (Errors.Count<>0) THEN
  BEGIN;
    ShowException(Exception.Create(Errors.Text));
    Terminate;
    Exit;
  END;

  // stop program loop
  Terminate;
end;

PROCEDURE TCasCat.LogError(ErrorMsg : STRING);

BEGIN;
  IF (Trim(ErrorMsg)<>'') THEN
    Errors.Add(ErrorMsg);
END;

constructor TCasCat.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  Errors:=TStringList.Create;
  MemMap:=TMemMap.Create($8000);
end;

destructor TCasCat.Destroy;
begin
  Errors.Free;
  inherited Destroy;
end;

procedure TCasCat.WriteHelp;
begin
  { add your help code here }
  WriteLnFmt('CasCat version %2.2d.%2.2d Copyright 2007-2017 Phill Harvey-Smith',[Major,Minor]);
  WriteLn('This is free software, and must not be distributed for profit !');
  WriteLn;
  WriteLnFmt('%s <casfile> [<params>]',[ExeName]);
  WriteLn;
  WriteLn;
  WriteLn('The following optional parameters may be specified : ');
  WriteLn(' -h, --help       : Display this help.');
  WriteLn(' -v, --verbose=   : Set verbosity level');
  WriteLn(' -b, --bdata      : Output each block''s metadata');
  WriteLn(' -x, --hexdump    : Output a hexdump of each tape block');
  WriteLn(' -m, --microdeal  : Interpret microdeal gapped blocks');
end;

var
  Application: TCasCat;
begin
  Application:=TCasCat.Create(nil);
  Application.Title:='CasCat';
  Application.Run;
  Application.Free;
end.

