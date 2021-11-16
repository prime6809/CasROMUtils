unit DragonCoCoCasUnit;

{*************************************************************************}
{**                                                                     **}
{** DragonCoCoCasUnit, a unit to handle .CAS cassette file images as    **}
{**     used by Dragon 32/64/Alpha and Tandy CoCo emulators.            **}
{**                                                                     **}
{** Author : P.Harvey-Smith 2004-2006.                                  **}
{**                                                                     **}
{** The unit has methods to deal with reading files from cassette files **}
{**     extracting individual  casssette blocks, and writing cassete    **}
{**     blocks. This unit depends on RamothMemoryStream and             **}
{**     DragonCoCoNameBlockUnit.                                        **}
{**                                                                     **}
{** A CAS file consists of a number of sections, and is for the most    **}
{** part a direct image of the bytes that would be encoded on a physical**}
{** cassette.                                                           **}
{**                                                                     **}
{** This is as follows :                                                **}
{**                                                                     **}
{** 1) one or more sync bytes, value $55 'U'                            **}
{** 2) a block begining byte, value $3C '<'                             **}
{** 3) a data block, consisting of                                      **}
{**     a) Block type byte $00=filename, $01=data, $FF=eof              **}
{**     b) Block length in bytes                                        **}
{**     c) Data bytes                                                   **}
{**     d) Block checksum.                                              **}
{** 4) Inter-block sync bytes                                           **}
{** 5) As many datablocks as needed                                     **}
{**                                                                     **}
{** 2010-08-17, Replaced some simple function calls that only returned  **}
{** private variables with properties.                                  **}
{**                                                                     **}
{**                                                                     **}
{**                                                                     **}
{**                                                                     **}
{**                                                                     **}
{*************************************************************************}

interface

uses
  SysUtils, Variants, Classes,
  RamothMemoryStream,DragonCoCoCasNameBlockUnit,Contnrs,
  DragonCoCoCASDefsUnit;

TYPE    TFindBlock	= (BlkFound,BlkEOF,BlkNoMore,BlkBegin);

        { TDragonCoCoCas }

  TDragonCoCoCas        = Class(TRamothMemoryStream)
  PRIVATE
    FCurrentByte	    : BYTE;
    FLastByte	        : BYTE;
    FLastBlockType      : BYTE;
    FBlockType	        : BYTE;
	FBlockLen	        : BYTE;
    FCheckSum	        : BYTE;
    FFilesInCas		    : INTEGER;
    FMoreBlocks         : BOOLEAN;
    FCksumValid         : BOOLEAN;

    PROCEDURE ScanCas;
    FUNCTION InternalChecksumBlock(Block  : TRamothMemoryStream;
                                   BType  : BYTE;
                                   BLen   : BYTE) : BYTE;
    FUNCTION GetName(Index   : INTEGER) : STRING;

  PUBLIC
    FileNames			: TObjectList;

    PROPERTY BlockType      : BYTE READ FBlockType;
    PROPERTY BlockLen       : BYTE READ FBlockLen;
    PROPERTY Checksum       : BYTE READ FCheckSum;
    PROPERTY FilesInCas     : INTEGER READ FFilesInCas;
    PROPERTY MoreBlocks     : BOOLEAN READ FMoreBlocks;
    PROPERTY CksumValid     : BOOLEAN READ FCksumValid;
    PROPERTY Names[Index : INTEGER] : STRING READ GetName;

    CONSTRUCTOR Create;
    CONSTRUCTOR CreateLoadCasFile(FileName	: STRING);
    CONSTRUCTOR CreateAppendCasFile(FileName	: STRING);

    PROCEDURE LoadCasFile(FileName	: STRING);
    FUNCTION FindNextBlock(SkipCount   : Int64 = 0) : TFindBlock;

    FUNCTION GetNextRawBlock    : TRamothMemoryStream;
    FUNCTION DecodeBlockType	: STRING;
    FUNCTION ChecksumBlock(Block  : TRamothMemoryStream;
                           Pos    : Int64) : BYTE;
    FUNCTION CalcChecksum(Pos	: INT64) : BYTE;
    FUNCTION ChecksumFoundBlock : BYTE;
	FUNCTION GetFileData(FileNum	: INTEGER;
                         Offset     : Int64)  : TRamothMemoryStream; overload;
	PROCEDURE GetFileData(FileNum   : INTEGER;
                          Offset    : Int64;
                          Data      : TRamothMemoryStream); overload;

    FUNCTION GetFileList : TStringList;
    PROCEDURE WriteRawBlock(BlockData     : TRamothmemoryStream;
                            SyncData      : TRamothmemoryStream); overload;
    PROCEDURE RecalcChecksum(Block        : TRamothmemoryStream;
                             Pos          : Int64); overload;
    PROCEDURE RecalcChecksum(Pos  : Int64);
    PROCEDURE Reset;
    PROCEDURE WriteFileNameBlock(Head        : TDragonCoCoCasNameBlock;
                                       SyncSize    : INTEGER);
                                                       
    PROCEDURE WriteDataBlock(Data    : TRamothMemoryStream;
                             BlkType : BYTE;
                             NoBytes : BYTE;
                             SyncSize: INTEGER);
    PROCEDURE WriteFile(Header            : TDragonCoCoCasNameBlock;
                        Data              : TRamothMemoryStream;
                        HeadSyncSize      : INTEGER;
                        DataSyncSize      : INTEGER);
    PROCEDURE WriteFilePos(Pos               : Int64;
                           Header            : TDragonCoCoCasNameBlock;
                           Data              : TRamothMemoryStream;
                           HeadSyncSize      : INTEGER;
                           DataSyncSize      : INTEGER);

    PROCEDURE WriteEOFBlock(SyncSize : INTEGER);

    FUNCTION IsFileType(FileNo    : INTEGER;
                        FileType  : BYTE) : BOOLEAN;
    FUNCTION GetNameBlock(FileNo  : INTEGER) : TDragonCoCoCasNameBlock;

  END;

implementation


{*************************************************************************}
{**                                                                     **}
{** Create a new cassette file, and initialise vars                     **}
{**                                                                     **}
{*************************************************************************}

CONSTRUCTOR TDragonCoCoCas.Create;

BEGIN;
  INHERITED Create;
  FileNames:=TObjectList.Create;
  FCurrentByte:=0;
  FLastByte:=0;
  FMoreBlocks:=TRUE;
  FCksumValid:=FALSE;
END;

{*************************************************************************}
{**                                                                     **}
{** Create a new cassette file, initialise vars, then load.             **}
{**                                                                     **}
{*************************************************************************}

CONSTRUCTOR TDragonCoCoCas.CreateLoadCasFile(FileName	: STRING);
BEGIN;
  Create;
  LoadCasFile(FileName);
END;

{*************************************************************************}
{**                                                                     **}
{** Create a new cassette file, initialise vars, load cas from file,    **}
{**     move to the end ready to write more data to the file.           **}
{**                                                                     **}
{*************************************************************************}

CONSTRUCTOR TDragonCoCoCas.CreateAppendCasFile(FileName	: STRING);

BEGIN;
  CreateLoadCasFile(FileName);
  Self.Seek(0,soFromEnd);
END;

{*************************************************************************}
{**                                                                     **}
{** Load a cassette image from tape and scan it for files.              **}
{**                                                                     **}
{*************************************************************************}

PROCEDURE TDragonCoCoCas.LoadCasFile(FileName	: STRING);

BEGIN;
  IF (FileExists(FileName)) THEN
  BEGIN;
    LoadFromFile(FileName);
    ScanCas;
  END;
END;

{*************************************************************************}
{**                                                                     **}
{** Find next block in the cassette, leaves the stream pointer          **}
{**     positiond *AFTER* the Block type and length bytes, also updates **}
{**     block type and length properties. Returns a value indcating     **}
{**     if a block was found or not.                                    **}
{**                                                                     **}
{*************************************************************************}

FUNCTION TDragonCoCoCas.FindNextBlock(SkipCount   : Int64 = 0) : TFindBlock;

BEGIN;
  Position:=Position+SkipCount;

  FLastBlockType:=FBlockType;
  Read(FCurrentByte,1);

  WHILE (Not(Self.EOS) AND (FCurrentByte<>BlockBegin)) DO
  BEGIN;
    Read(FCurrentByte,1);
  END;

  //WriteLn(Format('Position=%X [%X], Current=%d, Last=%d',[position,Position,CurrentByte,LastByte]));
  IF (NOT EOS) THEN
  BEGIN;
    Read(FBlockType,1);
    Read(FBlockLen,1);

    Result:=BlkFound;
  END
  ELSE
  BEGIN;
    FMoreBlocks:=FALSE;
    Result:=BlkNoMore;
  END;
END;

{*************************************************************************}
{**                                                                     **}
{** Get the raw data for the next block, return it as a memory stream,  **}
{**     also updates the MoreBlocks property, so the caller can tell    **}
{**     if we have reached the end of file. Note, block data does NOT   **}
{**     include the beginig block marker.                               **}
{**                                                                     **}
{*************************************************************************}

FUNCTION TDragonCoCoCas.GetNextRawBlock : TRamothMemoryStream;

VAR     Find    : TFindBlock;

BEGIN;
  Find:=FindNextBlock;
  Result:=TRamothMemoryStream.Create; { Create output buf }

  IF (Find<>BlkNoMore) THEN
  BEGIN;
    Position:=Position-2;               { Seek back past Len & Type }
    Result.CopyBytesFromStream(Self,FBlockLen+3);  { Get block, len, type & checksum }
  END;
END;

{*************************************************************************}
{**                                                                     **}
{** Decode the block type and return a string describing it.            **}
{**                                                                     **}
{*************************************************************************}

FUNCTION TDragonCoCoCas.DecodeBlockType	: STRING;

BEGIN;
  CASE FBlockType OF
    BtFileName	: Result:='Filename block';
    BtData	    : Result:='Data block';
    BtEOF	    : Result:='End of file block';
  ELSE
    Result:=Format('Unknown block: $%2.2X',[FBlockType]);
  END;
END;

{*************************************************************************}
{**                                                                     **}
{** Calculate the checksum for a block, pointed to by Pos in the stream **}
{**                                                                     **}
{*************************************************************************}

FUNCTION TDragonCoCoCas.CalcChecksum(Pos	: INT64) : BYTE;

BEGIN;
  Result:=ChecksumBlock(Self,Pos);
END;

{*************************************************************************}
{**                                                                     **}
{** Calculate the checksum for a block, in any stream                   **}
{**                                                                     **}
{*************************************************************************}

FUNCTION TDragonCoCoCas.ChecksumBlock(Block  : TRamothMemoryStream;
                                      Pos    : Int64) : BYTE;

VAR  BType  : BYTE;
     BLen   : BYTE;

BEGIN;
  Block.Position:=Pos;
  BType:=Block.ReadByte;
  BLen:=Block.ReadByte;
  Result:=InternalChecksumBlock(Block,BType,BLen);
END;


FUNCTION TDragonCoCoCas.ChecksumFoundBlock : BYTE;

BEGIN;
  Result:=InternalChecksumBlock(Self,FBlockType,FBlockLen);
END;

FUNCTION TDragonCoCoCas.InternalChecksumBlock(Block  : TRamothMemoryStream;
                                              BType  : BYTE;
                                              BLen   : BYTE) : BYTE;

VAR	Idx	: INTEGER;
	Sum	: WORD;

BEGIN;
  WITH Block DO
  BEGIN;
    Sum:=BType+BLen;       	        { All blocks have these }
    IF ({(BType<>btEOF) AND} (BLen<>0)) THEN
      FOR Idx:=0 TO BLen-1 DO
        Sum:=Sum+ReadByte;

    FCheckSum:=ReadByte;              { Skip checksum }
  END;
  Result:=BYTE(Sum);
  FCksumValid:=(Result=FCheckSum);
END;

{*************************************************************************}
{**                                                                     **}
{** Get the complete file data for a file within the cas file.          **}
{**                                                                     **}
{*************************************************************************}

FUNCTION TDragonCoCoCas.GetFileData(FileNum	    : INTEGER;
                                    Offset      : Int64) : TRamothMemoryStream;

BEGIN;
  Result:=TRamothMemoryStream.Create;
  GetFileData(FileNum,Offset,Result);
END;

PROCEDURE TDragonCoCoCas.GetFileData(FileNum    : INTEGER;
                                     Offset     : Int64;
                                     Data       : TRamothMemoryStream);

VAR	Found	        : TFindBlock;
    TempData        : TRamothMemoryStream;

BEGIN;
  {Create buffers}
  TempData:=TRamothMemoryStream.Create;
  Data.Clear;

  TRY
    {Loop through blocks in file, reading ito buffer}
    Position:=TDragonCoCoCasNameBlock(FileNames.Items[FileNum]).CasDataOffset;

   //WriteLn(Format('TDragonCoCoCas.GetFileData(%d,$%8.8X) Position=%8.8X',[FileNum,Offset,Position]));

    Found:=FindNextBlock;
    WHILE (Found<>BlkNoMore) DO
    BEGIN;
      CASE FBlockType OF
        BtData  :
        BEGIN;
          TempData.CopyBytesFromStream(Self,FBlockLen);
          Found:=FindNextBlock(1);				{ Skip checksum }
        END;

        BtEOF :
        BEGIN;
          Position:=Position+FBlockLen+1;
          Found:=BlkNoMore;
        END;
        ELSE
          Found:=FindNextBlock(FBlockLen+1);
      END;
    END;

    {Copy data from temp buffer, to output, skipping offset bytes}
    IF (Offset<TempData.Size) THEN
      TempData.Position:=Offset
    ELSE
      TempData.Position:=0;

    Data.CopyBytesFromStream(TempData,TempData.Size-Offset);
  FINALLY
    TempData.Free
  END;
END;

{*************************************************************************}
{**                                                                     **}
{** Scan the loaded cas file for files, and build the file header table.**}
{**                                                                     **}
{*************************************************************************}

PROCEDURE TDragonCoCoCas.ScanCas;

VAR	Found	: TFindBlock;
	FBlock	: TDragonCoCoCasNameBlock;

BEGIN;
  Self.Position:=0;
  FFilesInCas:=0;
  FBlockType:=BtEOF;
  FileNames.Clear;
  Found:=Self.FindNextBlock;
  WHILE (Found<>BlkNoMore) DO
  BEGIN;
    IF (FLastBlockType=BtEOF) THEN
    BEGIN;
      FBlock:=TDragonCoCoCasNameBlock.Create(BlockLen);
      //IF (BlockLen>13) THEN		{Check block i long enough}
      //BEGIN;
        FBlock.ReadFromStream(Self,FBlockType,FCheckSum);
        FileNames.Add(FBlock);
        FFilesInCas:=FFilesInCas+1;
      //END;
    END
    ELSE
      Position:=Position+FBlockLen;

    Found:=FindNextBlock(1);
  END;
  Self.Position:=0;
  FMoreBlocks:=TRUE;
END;

{*************************************************************************}
{**                                                                     **}
{** Reset the file, as if nothing had been loaded, same as free/create  **}
{**                                                                     **}
{*************************************************************************}

PROCEDURE TDragonCoCoCas.Reset;

BEGIN;
  FCurrentByte:=0;
  FLastByte:=0;
  FileNames.Free;
  FileNames:=TObjectList.Create;
  Position:=0;
  Size:=0;
END;

{*************************************************************************}
{**                                                                     **}
{** Return a list of files in the file                                  **}
{**                                                                     **}
{*************************************************************************}

FUNCTION TDragonCoCoCas.GetFileList : TStringList;

VAR 	Idx	: INTEGER;

BEGIN;
  Result:=TStringList.Create;
  FOR Idx:=0 TO (FileNames.Count-1) DO
    Result.Add(TDragonCoCoCasNameBlock(FileNames.Items[Idx]).FileName);
END;

{*************************************************************************}
{**                                                                     **}
{** Write a block to the cassete stream, preceeding it with sync bytes  **}
{**                                                                     **}
{*************************************************************************}

PROCEDURE TDragonCoCoCas.WriteRawBlock(BlockData     : TRamothmemoryStream;
                                       SyncData      : TRamothmemoryStream);

BEGIN;
  IF (SyncData<>NIL) THEN
  BEGIN;
    SyncData.Position:=0;
    Self.CopyBytesFromStream(SyncData,SyncData.Size);
  END;

  IF (BlockData<>NIL) THEN
  BEGIN;
    BlockData.Position:=0;
    WriteByte(BlockBegin);
    CopyBytesFromStream(BlockData,BlockData.Size);
  END;
END;

{*************************************************************************}
{**                                                                     **}
{** Recalculate and update the checksum for a block.                    **}
{**                                                                     **}
{*************************************************************************}

PROCEDURE TDragonCoCoCas.RecalcChecksum(Block        : TRamothmemoryStream;
                                        Pos          : Int64); overload;

VAR     NewSum  : BYTE;

BEGIN;
  NewSum:=ChecksumBlock(Block,Pos);
  Block.Position:=Block.Position-1;
  Block.WriteByte(NewSum);
END;

PROCEDURE TDragonCoCoCas.RecalcChecksum(Pos  : Int64);

BEGIN;
  RecalcChecksum(Self,Pos);
END;

{*************************************************************************}
{**                                                                     **}
{** WriteFileNameBlock, writes out a filename block, taking care of     **}
{**     writing the block type,size, and checksum                       **}
{**                                                                     **}
{*************************************************************************}

PROCEDURE TDragonCoCoCas.WriteFileNameBlock(Head        : TDragonCoCoCasNameBlock;
                                            SyncSize    : INTEGER);
                                            
VAR Block       : TRamothMemoryStream;
    SyncData    : TRamothMemoryStream;
    Sum         : BYTE;
    
BEGIN;
  Block:=TRamothMemoryStream.Create;
  SyncData:=TRamothMemoryStream.Create;
  
  SyncData.WriteRepetedChars(CHR(SyncByte),SyncSize);        {Generate sync block}
  Sum:=0;                               {so it's initialised....}

  WITH Block DO
  BEGIN;
    WriteByte(FtBasic);                 {Write filetype}
    WriteByte(FNameBlockLen);           {Write file length}
    Head.WriteToStream(Block);          {Write out the filename block}
    WriteByte(Sum);                     {Dummy}
    Sum:=ChecksumBlock(Block,0);        {Calc checksum}
    Position:=Position-1;
    WriteByte(Sum);                     {Write real checksum}
  END;
  WriteRawBlock(Block,SyncData);        {Write the block}
  
  Block.Free;
  SyncData.Free;
END;

{*************************************************************************}
{**                                                                     **}
{** WriteDataBlock, writes out a filename block, taking care of         **}
{**     writing the block type,size, and checksum                       **}
{**                                                                     **}
{*************************************************************************}

PROCEDURE TDragonCoCoCas.WriteDataBlock(Data    : TRamothMemoryStream;
                                        BlkType : BYTE;
                                        NoBytes : BYTE;
                                        SyncSize: INTEGER);

VAR Block       : TRamothMemoryStream;
    SyncData    : TRamothMemoryStream;
    Sum         : BYTE;

BEGIN;
  Block:=TRamothMemoryStream.Create;
  SyncData:=TRamothMemoryStream.Create;

  SyncData.WriteRepetedChars(CHR(SyncByte),SyncSize);        {Generate sync block}
  Sum:=0;                               {so it's initialised....}

  WITH Block DO
  BEGIN;
    WriteByte(BlkType);                 {Write filetype}
    WriteByte(NoBytes);                 {Write file length}
    CopyBytesFromStream(Data,NoBytes);  {Write the data}
    WriteByte(Sum);                     {Dummy}
    Sum:=ChecksumBlock(Block,0);        {Calc checksum}
    Position:=Position-1;
    WriteByte(Sum);                     {Write real checksum}
  END;
  WriteRawBlock(Block,SyncData);        {Write the block}

  Block.Free;
  SyncData.Free;
END;

{*************************************************************************}
{**                                                                     **}
{** Write a file to the cas file, takes care of blocking, setting block **}
{**     types and lengths, calculating checksums etc.                   **}
{**                                                                     **}
{*************************************************************************}

PROCEDURE TDragonCoCoCas.WriteFile(Header            : TDragonCoCoCasNameBlock;
                                   Data              : TRamothMemoryStream;
                                   HeadSyncSize      : INTEGER;
                                   DataSyncSize      : INTEGER);

BEGIN;
  WriteFilePos(Self.Position,Header,Data,HeadSyncSize,DataSyncSize);
END;

PROCEDURE TDragonCoCoCas.WriteFilePos(Pos               : Int64;
                                      Header            : TDragonCoCoCasNameBlock;
                                      Data              : TRamothMemoryStream;
                                      HeadSyncSize      : INTEGER;
                                      DataSyncSize      : INTEGER);



VAR     SizeToWrite     : Int64;

BEGIN;
  FileNames.Add(Header);                        {Save header to file list}
  
  Position:=Pos;                                {Set output pos}
  WriteFileNameBlock(Header,HeadSyncSize);      {Write out the header}
  
  Data.Position:=0;                             {Start at begining of data}
  
  {Itterate over data, writing it to cas file}
  WHILE (Data.Position<Data.Size) DO
  BEGIN;
    {Work out the size of the current block, last block may be smaller}
    IF ((Data.Size-Data.Position)>DefBlockSize) THEN
      SizeToWrite:=DefBlockSize
    ELSE
      SizeToWrite:=Data.Size-Data.Position;
      
    {If we are writing the first data block, write a header sized sync}
    IF (Data.Position=0) THEN
      WriteDataBlock(Data,BtData,BYTE(SizeToWrite),HeadSyncSize)
    ELSE
      WriteDataBlock(Data,BtData,BYTE(SizeToWrite),DataSyncSize);
  END;
  WriteEOFBlock(DataSyncSize);
END;

PROCEDURE TDragonCoCoCas.WriteEOFBlock(SyncSize : INTEGER);

BEGIN;
  WriteRepetedChars(CHR(SyncByte),SyncSize); {Write sync data}
  WriteByte(BlockBegin);                {Mark beginning of block}
  WriteByte(BtEOF);                     {EOF block}
  WriteByte(0);                         {Zero lenghth}
  WriteByte(BtEOF);                     {Checksum, $FF+$00=$FF !}
END;

{*************************************************************************}
{**                                                                     **}
{** IsFileType, returns true if the specified file, is of the specified **}
{**      type, else returns false.                                      **}
{**                                                                     **}
{*************************************************************************}

FUNCTION TDragonCoCoCas.IsFileType(FileNo    : INTEGER;
                                   FileType  : BYTE) : BOOLEAN;
BEGIN;
  IF (TDragonCoCoCasNameBlock(FileNames.Items[FileNo]).FileID=FileType) THEN
    Result:=TRUE
  ELSe
    Result:=FALSE;
END;

{*************************************************************************}
{**                                                                     **}
{** GetNameBlock, Get the filename block for the specified file.        **}
{**                                                                     **}
{*************************************************************************}

FUNCTION TDragonCoCoCas.GetNameBlock(FileNo  : INTEGER) : TDragonCoCoCasNameBlock;

BEGIN;
  Result:=TDragonCoCoCasNameBlock(FileNames.Items[FileNo]);
END;

FUNCTION TDragonCoCoCas.GetName(Index   : INTEGER) : STRING;

BEGIN;
  Result:='';

  IF (Index < FileNames.Count) THEN
  BEGIN;
    Result:=TDragonCoCoCasNameBlock(FileNames[Index]).FileName;

    IF (Result='') THEN
      Result:=Format('NONAME%2.2d',[Index]);
  END;
END;

end.
