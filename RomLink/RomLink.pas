program RomLink;

{***************************************************************************}
{**                                                                       **}
{** RomLink, builds a paged rom file for the Dragon/CoCo MegaCart.        **}
{**                                                                       **}
{** 2006-10-19, P.Harvey-Smith, Version 0.1.                              **}
{**                                                                       **}
{** I have decided to use FreePascal for this, as it has some nice high   **}
{** level structures for dealing with files and memory blocks, and can    **}
{** re-use some of my Delphi code. Also it is more cross platform than    **}
{** Delphi, and without all the tedious mucking about of C/C++.           **}
{**                                                                       **}
{** This should compile cleanly on the Win 32 and Linux versions of       **}
{** FreePascal, it was built with 2.0.4, but should work on any 2.x and   **}
{** later version. It should also be relitivly easy to backport to Delphi **}
{** I may st some point produce a Mac version if I can get access to a    **}
{** Mac to compile it on :)                                               **}
{**                                                                       **}
{**                                                                       **}
{**                                                                       **}
{**                                                                       **}
{**                                                                       **}
{**                                                                       **}
{***************************************************************************}


USES contnrs,classes,RomList,RamothMemoryStream,SysUtils,Graphics,IntfGraphics,
     interfaces, RLECompressUnit, CustApp, ConsoleUtils, DragonDOSMMCBinFileUnit,
     RamothCustomApplicationUnit;

TYPE
  TRomLink = class(TRamothCustomApplication)
  protected
    CtrlFileName        : STRING;
    MenuCodeFileName    : STRING;
    OutputFileName      : STRING;
    MainRomList         : TRomList;
    RomImage            : TRamothMemoryStream;
    RomTable            : TRamothMemoryStream;
    BlockNo             : INTEGER;
    UncompressedSize    : Int64;
    CompressedSize      : Int64;

    procedure DoRun; override;
    PROCEDURE Usage;
    FUNCTION LoadSplashFile(SplashFileName  : STRING;
                            VAR SplashFile  : TRamothMemoryStream) : BOOLEAN;
    FUNCTION LoadMenuCode(MenuFileName      : STRING;
                          SplashFileName    : STRING) : BOOLEAN;
    PROCEDURE CopyRomData(Entry     : TRomEntry);
    FUNCTION FileNumber(FileName    : STRING;
                        FileNo      : INTEGER) : STRING;
    PROCEDURE Process;
    procedure DoHelp;
  PUBLIC
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  END;

CONST   Major           = 0;
        Minor           = 98;

        MenuMagic       = $12210968;    { Magic ID for menu file }
        MenuMagicOfs    = 4;            { Offset of ID }
        MenuDataPtrOfs  = 8;            { Offset of data pointer to menu table }
        SplashPtrOfs    = 10;           { Offset of pointer to splashscreen data }

        RomBase         = $C000;        { Base address of Dragon/CoCo cart rom }

{ Short options }
        OptSVerbose     = 'v';          { Be verbose! }
        OptSHelp        = 'h';          { Get some help }
        OptSSort        = 's';          { Sort control file, to specified file }

{ Long options }
        OptLVerbose     = 'verbose';
        OptLHelp        = 'help';
        OptLSort        = 'sort';

{ Option arrays for CheckOpts }
        ShortOpts : string = OptSVerbose+   '::' +
                             OptSHelp+      '::' +
                             OptSSort+      ':';

        LongOptsArray : array [1..3] OF AnsiSTRING =
            (OptLVerbose+   '::',
             OptLHelp+      '::',
             OptSSort+      ':'
            );

PROCEDURE TRomLink.Usage;

BEGIN;
  WriteLn(Format('RomLink Version %2.2d.%2.2d Copyright 2007-2010 Phill Harvey-Smith',[Major,Minor]));
  WriteLn('This is free software, and must not be distributed for profit !');
  WriteLn;
  WriteLn('  RomLink <CtrlFile> <MenuCodeFile> <OutputFile>');
END;
{***************************************************************************}
{**                                                                       **}
{** Load splashscrren file, either as raw 256x192 bitmap or as a graphics **}
{** file as supported by TPicture e.g. .bmp, .gif, .png etc.              **}
{** Graphics file will be converted to the correct format.                **}
{**                                                                       **}
{***************************************************************************}

FUNCTION TRomLink.LoadSplashFile(SplashFileName  : STRING;
                                 VAR SplashFile  : TRamothMemoryStream) : BOOLEAN;

VAR     Image           : TPicture;
        Xc              : INTEGER;
        Yc              : INTEGER;
        PicByte         : BYTE;

BEGIN;
  IF ((Pos('.BIN',UpperCase(SplashFileName))>0) OR
      (Pos('.RAW',UpperCase(SplashFileName))>0)) THEN
  BEGIN;
    SplashFile.LoadFromFile(SplashFileName);
    Result:=True;
  END
  ELSE
  BEGIN;
    Image:=TPicture.Create;
    TRY
      Image.LoadFromFile(SplashFileName);
      WriteLn(Format('width=%d, height=%d',[Image.Width,Image.Height]));
      PicByte:=0;

      FOR Yc:=0 TO 191 DO
      BEGIN;
        FOR Xc:=0 TO 255 DO
        BEGIN;
           IF (Image.Bitmap.Canvas.Pixels[Xc,Yc]<>clBlack) THEN
            PicByte:=PicByte OR $01;

          IF (((Xc+1) MOD 8)=0) THEN
          BEGIN;
            SplashFile.WriteByte(PicByte);
            PicByte:=0;
          END
          ELSE
            PicByte:=PicByte SHL 1;
        END;
      END;
      Result:=True;
    FINALLY
      Image.Free;
    END;
  END;
END;

{***************************************************************************}
{**                                                                       **}
{** LoadMenuCode, loads the binary containing the menu code that will be  **}
{**               in the first block of the linked rom, checks for the    **}
{**               Magic number at offset 4 withing the file.              **}
{**                                                                       **}
{**                                                                       **}
{***************************************************************************}

FUNCTION TRomLink.LoadMenuCode(MenuFileName      : STRING;
                               SplashFileName    : STRING) : BOOLEAN;

VAR     MagicCheck      : LongWord;
        SplashFile      : TRamothMemoryStream;

BEGIN;
  RomImage:=TRamothMemoryStream.Create;
  SplashFile:=TRamothMemoryStream.Create;
  MagicCheck:=0;

  TRY
    IF (FileExists(MenuFileName)) THEN
    BEGIN;
      WITH RomImage DO
      BEGIN;
        LoadFromFile(MenuFileName);
        WriteLn('Read ',Size,' bytes from MenuFile:',MenuFileName);
        IF (RomImage.Size<>RomSize) THEN
          PadToMultiple(RomSize);

        IF (FileExists(SplashFileName)) THEN
        BEGIN;
          IF (NOT LoadSplashFile(SplashFileName,SplashFile)) THEN
            WriteLn('Error could not load splashfile : ',SplashFileName)
          ELSE
            WriteLn('Read ',SplashFile.Size,' bytes from SplashFile:',SplashFileName);

          IF (SplashFile.Size>RomSize) THEN
            SplashFile.Size:=RomSize;

          SplashFile.Seek(0,soFromBeginning);
          RomImage.Seek(0,soFromEnd);
          RomImage.CopyBytesFromStream(SplashFile,SplashFile.Size);
          IF (RomImage.Size<>(RomSize*2)) THEN
            PadToMultiple(RomSize*2);

          RomImage.Position:=SplashPtrOfs;
          RomImage.WriteMotorolaWord(RomBase+RomSize);
        END
        ELSE
          WriteLn('Could not open Splashscreen file :',SplashFileName);

        Position:=MenuMagicOfs;
        Read(MagicCheck,SizeOf(MagicCheck));
      END;

      IF (MagicCheck=MenuMagic) THEN
        Result:=TRUE
      ELSE
        Result:=FALSE;
    END
    ELSE
    BEGIN;
      WriteLn(Format('Error file %s does not exist',[MenuFileName]));
      Result:=FALSE;
    END;
  FINALLY
    SplashFile.Free;
  END;
END;

{***************************************************************************}
{**                                                                       **}
{** Copy Rom data to output stream, and update block pointer              **}
{**                                                                       **}
{***************************************************************************}

PROCEDURE TRomLink.CopyRomData(Entry     : TRomEntry);

VAR RomStream   : TRamothMemoryStream;

BEGIN;
  IF (Entry.TryCompress AND (Entry.RomType <> RTRom) AND (Entry.FCompress.Ratio < 100)) THEN
    RomStream:=Entry.FCompress.OutputMem
  ELSE
    RomStream:=Entry.RomData;

  RomImage.Position:=RomImage.Size;
  RomStream.Position:=0;
  Entry.StartOffset:=RomImage.Position MOD RomSize;
  RomImage.CopyBytesFromStream(RomStream,RomStream.Size);
  Entry.Written:=TRUE;
  Entry.StartBlock:=BlockNo;

  IF (DebugRom) THEN
    WriteLn(Format('Wrote %d bytes',[RomStream.Size]));

  //BlockNo:=BlockNo+(Entry.RomData.Size DIV RomSize);
  BlockNo:=RomImage.Position DIV RomSize;

  WriteLnFmt('Copied data at pos %2.2d, offset=%4.4X, start=%4.4X ,len=%4.4X, entry=%4.4X, size=%d, comp=%d, ratio=%d%%, %s',
                 [Entry.StartBlock,Entry.StartOffset,Entry.StartAddr,
                  Entry.CodeLength,Entry.ExecAddr,
                  Entry.RomData.Size,RomStream.Size, ((RomStream.Size * 100) DIV Entry.RomData.Size),
                  Entry.Text]);


  CompressedSize:=CompressedSize+RomStream.Size;
  UncompressedSize:=UncompressedSize+Entry.RomData.Size;
END;

FUNCTION TRomLink.FileNumber(FileName    : STRING;
                             FileNo      : INTEGER) : STRING;

VAR     Ext     : STRING;
        FName   : STRING;
        ExtPos  : INTEGER;

BEGIN;
  Ext:=ExtractFileExt(FileName);
  FName:=ExtractFileName(FileName);

  ExtPos:=Pos(Ext,FName);
  IF (ExtPos>0) THEN
    SetLength(FName,ExtPos-1);

  Result:=Format('%s%s%d%s',[ExtractFilePath(FileName),FName,FileNo,Ext]);
END;

{***************************************************************************}
{**                                                                       **}
{** Main process loop :                                                   **}
{**     Load in List of roms to process.                                  **}
{**     Load in menu code, and pad to 8K                                  **}
{**     Copy menu data to output stream                                   **}
{**     Copy each rom to output stream, taking care of block sizes        **}
{**     Create list of roms for menu, and patch into menu code            **}
{**     Write completed rom to output file                                **}
{**                                                                       **}
{***************************************************************************}


PROCEDURE TRomLink.Process;

VAR     Entry       : TRomEntry;
        Idx         : INTEGER;
        Len         : BYTE;
        MenuAd      : WORD;
        RomSizeK    : Int64;
        Rom2SizeK   : int64;
        LenPos      : int64;
        EndHeadPos  : int64;
        CompRatio   : REAL;
        CmdParams   : TStringArray;

BEGIN;
  CmdParams:=GetNonOptions(ShortOpts, LongOptsArray);

  IF (Length(CmdParams)>=3) THEN
  BEGIN;
    CtrlFileName:=CmdParams[0];
    MenuCodeFileName:=CmdParams[1];
    OutputFileName:=CmdParams[2];
  END
  ELSE
    Raise Exception.Create('Error: not enough parameters');

  UncompressedSize:=0;

  {Load in list of roms}
  writeln('reading romlist');
  MainRomList:=TRomList.ReadFromFile(CtrlFileName);
  writeln('read romlist');

  TRY
    {Load and validate menu code}
    IF (LoadMenuCode(MenuCodeFileName,MainRomList.SplashFileName)) THEN
    BEGIN;
      RomImage.PadToMultiple(RomSize);
    END
    ELSE
    BEGIN
      Raise Exception.Create('Error: Menu file is invalid');
    END;

    {Initialise the in memory copy of the rom directory}
    {Set start block based on current ROM size as menu code is in block 0}
    {and if present splash screen code is in block 1}
    RomTable:=TRamothmemoryStream.Create;
    BlockNo:=RomImage.Size DIV RomSize;

    {Process the list of roms and copy them to the output stream}
    WITH MainRomList DO
    BEGIN;
      {Even if we have 16K blocks, we write the first 8k block first}
      {as this ensures that 16K blocks start on a even boundry}
      IF((RomImage.Size<>(RomSize*2)) AND (First8K<>FirstInvalid)) THEN
      BEGIN;
        Entry:=GetBlock(First8K);
        CopyRomData(Entry);
      END;

      {Make sure we are on a multiple of 16K before outputting 16K roms}
      IF (RomImage.Size<>(RomSize*2)) THEN
        RomImage.PadToMultiple(RomSize*2);

      {Now we are on a 16K boundry, write out 16K roms}
      REPEAT;
        Entry:=FindNextBlock(RTRom,(RomSize*2));
        IF (Entry<>NIL) THEN
          CopyRomData(Entry);
      UNTIL (Entry=NIL);

      {Now write out the rest of the 8K ones}
      REPEAT;
        Entry:=FindNextBlock(RTRom,RomSize);
        IF (Entry<>NIL) THEN
          CopyRomData(Entry);
      UNTIL (Entry=NIL);

      {Now write out any cassette images}
      REPEAT;
        Entry:=FindNextBlock(RTCasAny,RomSize);
        IF (Entry<>NIL) THEN
          CopyRomData(Entry);
      UNTIL (Entry=NIL);

      {Finally we build and patch the menu entries into the menu code}
      SortMenu;
      FOR Idx:=0 TO (Count-1) DO
      BEGIN;
        Entry:=GetBlock(Idx);
        Len:=Length(Entry.Text);
        WITH RomTable DO
        BEGIN;
          LenPos:=Position;                     { Save position to write len byte }

          WriteByte(Len);                       { First write length }
          WriteByte(Entry.GetWriteRomType);     { Then the type }
          WriteByte(Entry.GetWriteRomFlags);    { And flags }
          WriteMotorolaWord(Entry.StartAddr);   { Rom start address }
          WriteMotorolaWord(Entry.ExecAddr);    { Entry address }
          WriteMotorolaWord(Entry.CodeLength);  { Length, only used for CAS }
          WriteByte(Entry.StartBlock);          { Start block no }
          WriteMotorolaWord(Entry.StartOffset); { Start offset within block }
          WriteMotorolaWord(Entry.FSamBits);    { Write out SAM bits }
          WriteByte(Entry.FVDGBits);            { Write out VDG bits }
          WriteMotorolaWord(Entry.InitStack);   { Write out initial stack pointer }
          WriteByte(Entry.FCompress.RunMarker);  { Write RLE marker }
          WriteFixedLenString(Entry.Text,Length(Entry.Text));     { ROM title for menu}

          { We record the start and end of header and then go back and re-write}
          { the length, as this means we don't have to know the header length in}
          { advance }
          EndHeadPos:=Position;                 { Save position of end of table entry }
          Seek(LenPos,soFromBeginning);         { Go back to beggining of header }
          Len:=(EndHeadPos-LenPos);
          WriteByte(Len);                       { Update length }
          Seek(EndHeadPos,soFromBeginning);     { Go back to end of header }
        END;
      END;
      RomTable.WriteByte(0);              { Zero terminator }
    END;

    MenuAd:=RomSize-RomTable.Size;        { Place rom table at end of menu area }

    { Copy menu table into Rom image }
    RomTable.Position:=0;
    RomImage.Position:=MenuAd;
    RomImage.CopyBytesFromStream(RomTable,RomTable.Size);

    MenuAd:=Word(MenuAd+RomBase);         { Work out address of menu table }
    RomImage.Position:=MenuDataPtrOfs;    { Write out data pointer }
    RomImage.WriteMotorolaWord(MenuAd);

    { Write out the finished file ! }

    IF (RomImage.Size<=(MainRomList.RomSizeK*1024)) THEN
    BEGIN;
      RomImage.SaveToFile(OutputFileName);
      RomSizeK:=RomImage.Size DIV 1024;
      WriteLn(Format('Linked rom is %dK, %dK free ',[RomSizeK,MainRomList.RomSizeK-RomSizeK]));
      IF (UncompressedSize <> 0) THEN
        CompRatio :=(CompressedSize / UncompressedSize) * 100
      ELSE
        CompRatio:=0;

      WriteLn(Format('Uncompressed data size %dK, Compressed data size %dK, Ratio %3.2F',
                      [UncompressedSize DIV 1024, CompressedSize DIV 1024,
                       CompRatio]));

      IF ((MainRomList.Rom2SizeK > 0) AND (RomSizeK > (MainRomList.RomSizeK - MainRomList.Rom2SizeK))) THEN
      BEGIN;
        Rom2SizeK:=MainRomList.Rom2SizeK * 1024;
        RomImage.SaveBlockToFile(FileNumber(OutputFileName,0),0,Rom2SizeK-1);
        RomImage.SaveBlockToFile(FileNumber(OutputFileName,1),Rom2SizeK,(RomImage.Size-Rom2SizeK));
      END;
    END
    ELSE
      WriteLn(Format('ERRROR, Linked rom is bigger than specified rom size of %dK',[MainRomList.RomSizeK]));

    IF (HasOption(OptSSort,OptLSort)) THEN
      MainRomList.OutputSortedFile(GetOptionValue(OptSSort,OptLSort));

  FINALLY
    { Free up variables }
    RomImage.Free;
    RomTable.Free;
    MainRomList.Free;
  END;
END;
procedure TRomLink.DoRun;

BEGIN;
  { quick check parameters }
  LogError(CheckOptions(ShortOpts, LongOptsArray));

  { parse parameters }
  if HasOption(OptSHelp, OptLHelp) then
  begin;
    DoHelp;
    Exit;
  end;

  {Verbose flag}
  IF (HasOption(OptSVerbose, OptLVerbose)) THEN
    DebugLevel:=StrToIntDef(GetOptionValue(OptSVerbose, OptLVerbose),$0000);

  IF (ParamCount<3) THEN
    LogError('Error: Must supply: control file, menu ROM file and output file names');

  {Process if no errors}
  if (Errors.Count=0) then
    Process;

  {If we have errors, display them and terminate}
  if (Errors.Count<>0) then
  begin
    Errors.Add('use -h or --help for help');
    ShowException(Exception.Create(Errors.Text));
    Terminate;
    Exit;
  end;

  // stop program loop
  Terminate;
END;

{Setup defaults}
constructor TRomLink.Create(TheOwner: TComponent);

begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TRomLink.Destroy;
begin
  inherited Destroy;
end;

PROCEDURE TRomLink.DoHelp;

BEGIN;
  WriteHelp;
  Terminate;
  Exit;
END;
procedure TRomLink.WriteHelp;
begin
  { add your help code here }
  WriteLnFmt('%s <ctrlfile> <menu_rom_image> <output_rom> [<params>]',[ExeName]);
  WriteLn;
  WriteLn;
  WriteLn('The following optional parameters may be specified : ');
  WriteLn(' -h, --help       : Display this help.');
  WriteLn(' -s, --sort=      : Sort ctrlfile and output to specified file.');
  WriteLn(' -v, --verbose=   : Set verbosity level');
end;

var
  Application: TRomLink;

begin
  Application:=TRomLink.Create(nil);
  Application.Title:='';
  Application.Run;
  Application.Free;
end.


