unit DragonCoCoCasNameBlockUnit;

{*************************************************************************}
{**                                                                     **}
{** DragonCoCoCasNameBlockUnit, unit for manipulating the filename      **}
{**     blocks of a Dragon 32/64/Alpha and Tandy CoCo cassete image file**}
{**                                                                     **}
{** Author : P.Harvey-Smith, 2004-2006.                                 **}
{**                                                                     **}
{**                                                                     **}
{**                                                                     **}
{*************************************************************************}

interface

USES RamothMemoryStream,DragonCoCoCASDefsUnit;

CONST   NameBlockLen    = 13;
        NameLen         = 8;
        FileIDPos       = 8;
        AsciiFlagPos    = 9;
        GapFlagPos      = 10;
        ExecAddrPos     = 11;
        LoadAddrPos     = 13;

        DreamBlockLen   = 8;    {Dream filename blocks contain just the filename}

TYPE    TDragonCoCoCasNameBlock	= Class(TObject)
        PROTECTED
          FBlockLen     : BYTE;
        PUBLIC
          FileName	: STRING;
          FileID	: BYTE;
          AsciiFlag	: BYTE;
          GapFlag	: BYTE;
          ExecAddress	: WORD;
          LoadAddress	: WORD;
          CasFileOffset	: Int64;
          CasDataOffset	: Int64;

          PROPERTY BlockLen     : BYTE READ FBlockLen;

          CONSTRUCTOR Create(CreateBlockLen   : BYTE = NameBlockLen);

          FUNCTION DecodeFileID		: STRING;
          FUNCTION DecodeAsciiFlag      : STRING;
          FUNCTION DecodeGapFlag	: STRING;
          PROCEDURE ReadFromStream(    ReadFrom   : TRamothMemoryStream;
                                       BlockType  : BYTE;
                                   VAR Checksum   : BYTE;
                                   UpdatePointer  : BOOLEAN = TRUE);  overload;

          PROCEDURE ReadFromStream(    ReadFrom   : TRamothMemoryStream;
                                   UpdatePointer  : BOOLEAN = TRUE);  overload;

          PROCEDURE WriteToStream(WriteTo     : TRamothMemoryStream);
        END;

implementation

CONSTRUCTOR TDragonCoCoCasNameBlock.Create(CreateBlockLen   : BYTE = NameBlockLen);

BEGIN;
  INHERITED Create;
  FBlockLen:=CreateBlockLen;
END;


FUNCTION TDragonCoCoCasNameBlock.DecodeFileID		: STRING;

BEGIN;
  CASE FileID OF
    FtBasic		    : Result:='Basic program ';
    FtDataFile		: Result:='Data file     ';
    FtMachineCode   : Result:='Machine code  ';
    FtBinary		: Result:='Binary file   ';
    FtDream         : Result:='Dream source  ';
    FtHeaderless    : Result:='Headerless    ';
  ELSE
    Result:=                  'Unknown ftype ';
  END;
END;

FUNCTION TDragonCoCoCasNameBlock.DecodeAsciiFlag	: STRING;

BEGIN;
  CASE AsciiFlag OF
    AsASCII	    : Result:='Ascii  ';
    AsBinary	: Result:='Binary ';
  ELSE
    Result:=              'Unknown';
  END;
END;

FUNCTION TDragonCoCoCasNameBlock.DecodeGapFlag		: STRING;

BEGIN;
  CASE GapFlag OF
    GfUngapped	: Result:='Ungapped ';
    GfGapped	: Result:='Gapped   ';
  ELSE
    Result:=              'Unknown  ';
  END;
END;

PROCEDURE TDragonCoCoCasNameBlock.ReadFromStream(    ReadFrom   : TRamothMemoryStream;
                                                     BlockType  : BYTE;
                                                 VAR Checksum   : BYTE;
                                                 UpdatePointer  : BOOLEAN = TRUE);

VAR     OldPointer      : Int64;

BEGIN;
  WITH ReadFrom DO
  BEGIN;
    OldPointer:=Position;
    CasFileOffset:=Position-2;

    {Set defaults}
    FileName:='';
    AsciiFlag:=AsBinary;      { Assume binary }
    GapFlag:=GfUngapped;
    ExecAddress:=$0000;
    LoadAddress:=$400;        { So as not to trample on sysvars }

    IF(FBlockLen = DreamBlockLen) THEN
      FileID:=FtDream;

    {If this is a valid Filename block, then read the data from it}
    IF (BlockType=BtFileName) THEN
    BEGIN;
      IF (FBlockLen >= NameLen) THEN
        FileName:=ReadFixedLenString(NameLen);

      IF (FBlockLen >= (FileIDPos+1)) THEN
        Read(FileID,1);

      IF (FBlockLen >= (AsciiFlagPos+1)) THEN
        Read(AsciiFlag,1);

      IF (FBlockLen >= (GapFlagPos+1)) THEN
        Read(GapFlag,1);

      IF (FBlockLen >= (ExecAddrPos+1)) THEN
        ExecAddress:=ReadMotorolaWord;

      IF (FBlockLen >= (LoadAddrPos+1)) THEN
        LoadAddress:=ReadMotorolaWord;

      Read(CheckSum,1);
      CasDataOffset:=Position;
    END
    ELSE        {Headerless file assume some defaults}
    BEGIN;
      FileID:=FtHeaderless;     { Flag it as headerless }
      CasDataOffset:=CasFileOffset-1;
    END;

    IF (NOT UpdatePointer) THEN
      Position:=OldPointer;
  END;
END;

PROCEDURE TDragonCoCoCasNameBlock.ReadFromStream(    ReadFrom   : TRamothMemoryStream;
                                                 UpdatePointer  : BOOLEAN = TRUE);

VAR     Checksum        : BYTE = 0;

BEGIN;
  ReadFromStream(ReadFrom,BtFileName,Checksum,UpdatePointer);
END;

PROCEDURE TDragonCoCoCasNameBlock.WriteToStream(WriteTo     : TRamothMemoryStream);

BEGIN;
  WITH WriteTo DO
  BEGIN;
    WriteFixedLenString(FileName,8);
    Write(FileID,1);
    Write(AsciiFlag,1);
    Write(GapFlag,1);
    WriteMotorolaWord(ExecAddress);
    WriteMotorolaWord(LoadAddress);
  END;
END;
end.
