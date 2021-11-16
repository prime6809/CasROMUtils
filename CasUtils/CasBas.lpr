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
    FUNCTION TranslateByte(ToTranslate      : BYTE) : BYTE;
    PROCEDURE DumpBlock(Block : TRamothMemoryStream);
    PROCEDURE TranslateBlock(Block  : TRamothMemoryStream);
    PROCEDURE NewCasTranslate;
    PROCEDURE ListCas(SourceDialect : CHAR);

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
        BasicError      = 'E';

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
  TypeStr:=UpperCase(Trim(TypeStr));

  IF (TypeStr[1] IN [BasicCoCo, BasicDragon, BasicMicroCoCo, BasicText]) THEN
    Result:= TypeStr[1]
  ELSE
    Result:=BasicError;
END;

FUNCTION TCasBas.TranslateByte(ToTranslate      : BYTE) : BYTE;

BEGIN;
  IF (LastByte=BasicFn) THEN
  BEGIN;
    IF (TransType=BasicDragon) THEN
      Result:=CoCoToDragonFunction(ToTranslate)
    ELSE
      Result:=DragonToCoCoFunction(ToTranslate);
  END
  ELSE
  BEGIN;
    IF (TransType=BasicDragon) THEN
      Result:=CoCoToDragonCommand(ToTranslate)
    ELSE
      Result:=DragonToCoCoCommand(ToTranslate);
  END;
END;

PROCEDURE TCasBas.DumpBlock(Block : TRamothMemoryStream);

VAR       Lines           : TStringList;
          Idx             : INTEGER;

BEGIN;
  Lines:=Block.HexDump(0);
  FOR Idx:=0 TO (Lines.Count-1) DO
    WriteLn(Lines.Strings[Idx]);

  Lines.Free;
END;

PROCEDURE TCasBas.TranslateBlock(Block  : TRamothMemoryStream);

VAR     ToTranslate     : BYTE;
        Translated      : BYTE;

BEGIN;
  IF (Debug) THEN WriteLn(Format('BytesToSkip=%d, LastByte=%2.2X',[BytesToSkip,LastByte]));
  WITH Block DO
  BEGIN;
    IF (Debug) THEN DumpBlock(Block);
    Position:=BytesToSkip+2;            { skip any bytes, plus blocktyp and len }
    BytesToSkip:=0;                     { As we have just skipped, set skip to 0 !!!! }
    WHILE (Position<(Size-1)) DO        {Don't process checksum !}
    BEGIN;

      Read(ToTranslate,1);              { Get a byte }
      IF (BytesToSkip=0) THEN
      BEGIN;
        CASE ToTranslate OF
          { Skip line no and link }
          $00      : BEGIN;
                       BytesToSkip:=4;
                       Translated:=ToTranslate;
                       LastByte:=0;
                     END;

          { Basic text, leave alone }
          $01..$7F : Translated:=ToTranslate;

          { Is it a command or function ?, yes translate }
          $80..$FE : BEGIN;
                       Translated:=TranslateByte(ToTranslate);
                       Position:=Position-1;
                       Write(Translated,1);
                     END;

          {Function marker, do nothing}
          $FF      : Translated:=ToTranslate;
        END;
        LastByte:=ToTranslate;            { Set lastbyte=current }
      END
      ELSE
      BEGIN;
        BytesToSkip:=BytesToSkip-1;
        LastByte:=0;
      END;

    END;
    IF (Debug) THEN DumpBlock(Block);
  END;
END;

{*************************************************************************}
{**                                                                     **}
{** Process the input cas file                                          **}
{**                                                                     **}
{*************************************************************************}

PROCEDURE TCasBas.NewCasTranslate;

VAR     InCasFile       : TDragonCoCoCas;
        OutCasFile      : TDragonCoCoCas;
        FileIdx         : INTEGER;
        Head            : TDragonCoCoCasNameBlock;
        NewProgBin      : TRamothMemoryStream;
        BytesToCopy     : Int64;
        Prog            : TBasicProg;

BEGIN;
  InCasFile:=TDragonCoCoCas.Create;
  OutCasFile:=TDragonCoCoCas.Create;

  TRY
    InCasFile.LoadCasFile(InFileName);

    WITH InCasFile DO
    BEGIN;
      FOR FileIdx:=0 TO (FilesInCas-1) DO
      BEGIN;
        {Get the name block}
        Head:=GetNameBlock(FileIdx);
        IF (IsFileType(FileIdx,FtBasic)) THEN
        BEGIN;
          {Load the program}
          Prog:=TBasicProg.LoadFromCas(InCasFile,FileIdx,SourceType);
          Prog.Retokenize(TransType,FALSE);             {Retokenize in destination dialect}
          NewProgBin:=Prog.ToBinImage;                  {Generate binary image}

          {Write new file to output cas}
          OutCasFile.WriteFile(Head,NewProgBin,HeadSyncSize,BlockSyncSize);

          {Freeup temp vars}
          Prog.Free;
          NewProgBin.Free;
        END
        ELSE
        BEGIN;
          {If this is the last file in the cas, copy all bytes from the }
          {current position till the end of the cas file, if it is not }
          {the last, copy all bytes up till the begining of the next file}
          IF (FileIdx=(FilesInCas-1)) THEN
            BytesToCopy:=Size-Position
          ELSE
            BytesToCopy:=GetNameBlock(FileIdx+1).CasFileOffset-Position;

          {Copy any non basic blocks over}
          OutCasFile.CopyBytesFromStream(InCasFile,BytesToCopy);
        END;
      END;
    END;

    {Write the output file}
    OutCasFile.SaveToFile(OutFileName);
  FINALLY
    InCasFile.Free;
    OutCasFile.Free;
  END;
END;

PROCEDURE TCasBas.ListCas(SourceDialect : CHAR);

VAR     Cas             : TDragonCoCoCas;
        Prog            : TBasicProg;
        CasIdx          : INTEGER;
        OutLines        : TStringList;
        List            : TStringList;

BEGIN;
  Cas:=TDragonCoCoCas.CreateLoadCasFile(InFileName);
  OutLines:=TStringList.Create;

  TRY
    FOR CasIdx:=0 TO (Cas.FilesInCas-1) DO
    BEGIN;
      IF (Cas.IsFileType(CasIdx,FtBasic)) THEN
      BEGIN;

        Prog:=TBasicProg.LoadFromCas(Cas,CasIdx,SourceDialect);
        List:=Prog.List;
        Prog.Free;
        OutLines.Add(Format('[Program no : %d]',[CasIdx]));
        OutLines.Add('');
        OutLines.AddStrings(List);
        List.Free;
      END;
    END;

    IF (OutLines.Count>0) THEN
    BEGIN;
      OutLines.SaveToFile(OutFileName);
    END
    ELSE
      WriteLn(Format('No basic programs contained in %s',[InFileName]));
  FINALLY
    IF (OutLines<>NIL) THEN OutLines.Free;
    IF (Cas<>NIL) THEN Cas.Free;
  END;
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

  IF ((SourceType = BasicError) OR (TransType = BasicError)) THEN
    LogError('Error in basic types, valid types are : C, D, M, T');

  IF (Errors.Count=0) THEN
  BEGIN;
    IF (TransType = BasicText) THEN
      ListCas(SourceType)
    ELSE
      NewCasTranslate;
  END;

  CheckErrorTerminate;
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

