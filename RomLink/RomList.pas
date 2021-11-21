unit RomList;

{***************************************************************************}
{**                                                                       **}
{** RomList, maintains a list of roms read from control file, and rom     **}
{**     binary images.                                                    **}
{**                                                                       **}
{**                                                                       **}
{**                                                                       **}
{***************************************************************************}


interface

uses contnrs,classes,SysUtils,RamothMemoryStream,DragonCoCoCasUnit,
     DragonCoCoCasNameBlockUnit,Utils,RamothStringListUnit,
     DragonCoCoCASDefsUnit,RLECompressUnit,fpexprpars,ConsoleUtils,
     DragonDOSMMCBinFileUnit;

CONST
     {Section begin/end within config file}
     {Config section}
     SecBeginConfig     = 'BEGINCONFIG';
     SecEndConfig       = 'ENDCONFIG';
     {Old format rom config}
     SecBeginRom        = 'BEGINROM';
     SecEndRom          = 'ENDROM';

     RTRom      = $00;  { Binary rom, org $C000 }
     RTCasBin   = $01;  { Cassette file image, binary }
     RTCasBas   = $02;  { Cassette file image, BASIC }
     RTCasAny   = $03;  { Combination of both cas types, for search }
     RTBinary   = $04;  { Binary memory image }
     RTSnapMMC  = $05;  { DragonMMC snapshot file }
     RTDDMMCBin = $06;  { Dragon DOS / DragonMMC binary file }
     RTDDMMCBas = $07;  { Dragon DOS / DragonMMC basic file }

     RTFlagCompress     = $08;  { Entry is compressed }
     RTFlagDisableFIRQ  = $10;  { Disable FIRQ in CPLD before running }
     RTFlagNoCas        = $20;  { Don't preserve cassette buffer }
     RTFlagNoOK         = $40;  { Don't print ok after loading CASBIN files }
     RTFlagCoCoBas      = $80;  { CoCo basic file }

     {These are the ascii representations of the above that will}
     {exist in the control file}
     KeyRTRom           = 'ROM';
     KeyRTCasBin        = 'CASBIN';
     KeyRTCasBasDragon  = 'CASBASD';
     KeyRTCasBasCoCo    = 'CASBASC';
     KeyRTBinaryImage   = 'BINIMG';
     KeyRTDragonMMCSnap = 'MMCSNAP';
     KeyRTDDMMCBin      = 'DDMMCBIN';
     KeyRTDDMMCBas      = 'DDMMCBAS';

     {If this is a BASIC program, is it a Dragon or CoCo one}
     {Setting this allows the basic loader to re-tokenize if}
     {loading on a non-native machine type, e.g. Dragon program on a CoCo}
     CTDragon           = 'D';
     CTCoCo             = 'C';

     NoRomFields= 6;    { Minimum No of fields in TRomEntry, may be more if patches exist }

     RomSize : integer  = (8*1024);     {8K rom banks}

     FirstInvalid       = -1;

     DefaultRomSize     = 64;           {Default Total rom size in kilobytes}
     DefaultRom2Size    = 0;            {Default rom 2 size in kilobytes}

     {Tokens for parsing lines in the control file}
     TokHeadStart       = 'HS';         { Use start address from cas header }
     TokHeadLen         = 'HL';         { Use length from cas header }
     TokHeadExec        = 'HE';         { Use entry address from cas header }
     TokNoOKPrint       = 'NO';         { Don't print OK once loaded, just exec }
     TokNoCas           = 'NC';         { Don't preserve cassette buffer }
     TokDisableFIRQ     = 'DF';         { Disable FIRQ (in cart CPLD) }
     TokLoad            = 'LOAD';       { File to load, new format entry }
     TokEnd             = 'END';        { End address from file line }
     TokExec            = 'EXEC';       { Exec address from command line }
     TokPatch           = 'PATCH';      { Patch entry token }
     TokFill            = 'FILL';       { Fill entry token }
     TokWrite           = 'WRITE';      { File area to write }
     TokFlags           = 'FLAGS';      { Flags to loader }
     TokMDGap           = 'MDGAP';      { Microdeal gapped file }

     TokVidBase         = 'VIDBASE';    { Video base / 512, as programmed to SAM }
     TokSAMMode         = 'SAMMODE';    { SAM video mode 0..7 }
     TokVDGMode         = 'VDGMODE';    { VDG video mode, 8 bit value and $F8 }
     TokStack           = 'STACK';      { Initial stack pointer address }

     TokCSS             = 'CSS';        { Specify CSS bit should be set }
     TokAG              = 'AG';         { Specify that the A/G bit should be set }

     {Header names}
     TokRomSize         = 'RomSize';    { Total Rom size in Kb }
     TokRom2Size        = 'Rom2Size';   { Rom 2 size }
     TokSplashFile      = 'SplashFile'; { Splashscreen filename }
     TokSort            = 'Sort';       { Should we sort the program names in the menu? }
     TokCompress        = 'Compress';   { Should we attempt to compress (default) or not }

     {Masks for SAM / VDG bits}
     SAMModeMask        = $0007;        { Mode bits in SAM word}
     SAMBaseMask        = $01F8;        { Base address mask }
     SAMMask            = (SAMBaseMask OR SAMModeMask);
     VDGMask            = $F8;          { VDG bits Mask }

     {Bits in VDG Word }
     VDGCSS             = $08;          { Colour set select }
     VDGGM0             = $10;          { Graphics mode bit 0 }
     VDGGM1             = $20;          { Graphics mode bit 1 }
     VDGGM2             = $40;          { Graphics mode bit 2 }
     VDGAG              = $80;          { Alpha / Graphics }

     SAMInvalid         = $FFFF;        { Flag SAM bits invalid }
     VDGInvalid         = $FF;          { Flag VDG bits invalid }
     StackInvalid       = $FFFF;        { Flag init stack invalid }

     TextVDUCursAddr    = $88;          { Address of text cursor location }

     DefStartAddr       = $0000;        { Default Start addess }
     DefEndAddr         = $7FFF;        { Default End address }
     DefExecAddr        = $0000;        { Default Exec address }

     DefStartAddrMMC    = $0000;        { Default Start addess DragonMMC snapshot }
     DefEndAddrMMC      = $8001;        { Default End address DragonMMC snapshot }

type
    TCasBinType    = (
       cbCasBin,
       cbMicrodealGapped
       );

     {Class for each entry in the rom list}
     TRomEntry  = class(TObject)
     PROTECTED
       {Only valid for current file being loaded }
       FHeadStart       : BOOLEAN;
       FHeadLen         : BOOLEAN;
       FHeadExec        : BOOLEAN;
       FCasBinType      : TCasBinType;  { Binary cassette type }
       FLoadAddr        : WORD;         { Address to load current file at }
       FStartAddr       : WORD;         { Address of start of block to write }
       FEndAddr         : WORD;         { Last address to write  }
       FExecAddr        : WORD;         { Execution address }
       FInitStack       : WORD;         { Set stack pointer to this *BEFORE* loading }
       FTryCompress     : BOOLEAN;      { Should we try compressing? }

       PROCEDURE GetParams(Line     : STRING;
                           Token    : STRING;
                           VAR List : TRamothStringList);
       FUNCTION GetParams(Line: STRING; Token: STRING) : TRamothStringList;
       FUNCTION GetLength : WORD;
       PROCEDURE GetSamMode(Line        : STRING);
       PROCEDURE GetMemBase(Line        : STRING);
       PROCEDURE GetVDGMode(Line        : STRING);
       PROCEDURE SetStartEndExec(Line        : STRING);
       PROCEDURE SetFlags(Line        : STRING);
       PROCEDURE GetInitStack(Line        : STRING);
     PUBLIC
       FileNames        : TStringList;  { Filenames for new format entries }
       Patches          : TStringList;  { Patches to the loaded data }
       ControlSection   : TStringList;  { Section of the control file }
       Text             : STRING;       { Text to be displayed in menu }
       RomType          : BYTE;         { Rom type, Rom, CasBin or CasBas, Binary }

       StartBlock       : BYTE;         { Start block within linked rom }
       StartOffset      : WORD;         { Start offset within block }
       RomData          : TRamothMemoryStream;  { Rom image, loaded from disk }
       Written          : BOOLEAN;      { Has been written to image flag }
       Skip             : WORD;         { How many bytes to skip at begining of file }
       PatchesApplied   : INTEGER;      { No of patches applied to data }
       CasBasType       : CHAR;         { If basic prog is it Dragon or CoCo }
       FNoOK            : BOOLEAN;      { If CasBIN, should we print OK once loaded ? }
       FNoCas           : BOOLEAN;      { If CasBIN, should we not preserve cas buffer }
       FDisableFIRQ     : BOOLEAN;      { If CasBIN, should we disable FIRQ in the CPLD }
       SnapStack        : WORD;         { Stack pointer of DragonMMC snapshot }
       BinBase          : WORD;         { Base address of BINIMG }

       BaseAddr         : WORD;         { Base address of memory image }

       FSamBits         : WORD;         { SAM video bits }
       FVDGBits         : BYTE;         { VDG bits }
       FVidValid        : Boolean;      { Are above valid }

       FCompress        : TRLECompressor;

       PROPERTY CodeLength  : WORD READ GetLength;        { Length of code, padded for roms }
       PROPERTY StartAddr   : WORD READ FStartAddr;       { Start address for code to save }
       PROPERTY EndAddr     : WORD READ FEndAddr;         { End address for code to save }
       PROPERTY ExecAddr    : WORD READ FExecAddr;        { Entry address of code }
       PROPERTY InitStack   : WORD READ FInitStack;       { Initial stack pointer }
       PROPERTY TryCompress : BOOLEAN READ FTryCompress;  { Should we try to compress when reading ?}

       CONSTRUCTOR Create(InTryCompress   : BOOLEAN);
       DESTRUCTOR Destroy; override;

       { Function to decode ascii rom type read from control file }
       Function DecodeRomType(RomTypeDecode : STRING) : BYTE;

       FUNCTION DoLoadROMorBin(LocalFileName      : STRING) : BOOLEAN;

       FUNCTION DoLoadCAS(LocalFileName      : STRING;
                          FileNo             : INTEGER) : BOOLEAN;
       FUNCTION DoLoadDragonMMCSnap(LocalFileName      : STRING) : BOOLEAN;
       FUNCTION DoLoadDDragonDosDMMC(LocalFileName      : STRING) : BOOLEAN;

       Function FixupFileName(InFileName       : STRING) : STRING;
       Function GetWriteRomType : BYTE;
       Function GetWriteRomFlags : BYTE;
       PROCEDURE SetupFromRomType;

       FUNCTION  ProcessFile(FileNo     : INTEGER) : BOOLEAN;
       PROCEDURE ProcessPatch(PatchNo   : INTEGER);

     end;

     {Class to hold the List of roms}
     TRomList   = class(TObjectList)
     PROTECTED
       FileText         : TRamothStringList;  { loaded control file }
       FCompress        : BOOLEAN;

       Function HexConv(Line   : STRING) : WORD;
       PROCEDURE ReadNextEntry;
       FUNCTION FindSection(Token           : STRING) : BOOLEAN;
       FUNCTION GetSection(TokenBegin       : STRING;
                           TokenEnd         : STRING;
                           IncludeTokens    : BOOLEAN = FALSE) : TRamothStringList;
       PROCEDURE CleanSection(TokenBegin    : STRING;
                              TokenEnd      : STRING;
                              VAR ToClean   : TRamothStringList);
     PUBLIC
       First8K          : INTEGER;      { Index of first 8K rom }
       First16K         : INTEGER;      { Index of first 16K rom }
       FirstCas         : INTEGER;      { Index of first Cas block }
       RomSizeK         : INTEGER;      { Total Rom size in Kbytes }
       Rom2SizeK        : INTEGER;      { Second Rom size in K, must be lower than total! }
       SplashFileName   : STRING;       { Filename of splashscreen, pmode 4, 6k}
       MenuSorted       : BOOLEAN;      { Should output menu be sorted }

       PROPERTY Compress : BOOLEAN READ FCompress;

       Constructor ReadFromFile(FileName     : STRING);
       Function GetBlock(BlockNo     : INTEGER) : TRomEntry;
       Function FindNextBlock(BType  : Byte;
                              Size   : Int64) : TRomEntry;
       PROCEDURE SortMenu;
       PROCEDURE OutputSortedFile(OutFileName   : STRING);
    end;

     TPatch     = class(TRamothMemoryStream)
       PatchAddr     : WORD;         { Address to patch }
       PatchLength   : WORD;         { No of bytes to patch }
       Constructor Create(Line       : STRING);
       PROCEDURE Apply(ToPatch : TRomEntry);   { Apply the patch }
     PROTECTED
       PROCEDURE CreatePatch(Line   : STRING);
       PROCEDURE CreateFill(Line   : STRING);
       FUNCTION GetIntVal16(Expression    : STRING;
                            OUT IntVal    : INTEGER) : BOOLEAN;
     end;


{ Set this to true to enable debugging output}
VAR     DebugRom        : BOOLEAN = FALSE;
        DebugLevel      : INTEGER = 0;

implementation

FUNCTION RomListCompare(Item1: Pointer;
                        Item2: Pointer):Integer;

BEGIN;
  Result:=CompareText(TRomEntry(Item1).Text, TRomEntry(Item2).Text);
END;


{***************************************************************************}
{**                                                                       **}
{** Initiate the rom list :                                               **}
{**     Create the list and initialise vars                               **}
{**     Read in the control file                                          **}
{**     Loop over control file, reading in rom blocks                     **}
{**             Check for first 8k,16k and cas blocks                     **}
{**                                                                       **}
{**                                                                       **}
{**                                                                       **}
{***************************************************************************}

Constructor TRomList.ReadFromFile(FileName      : STRING);

VAR     Config  : TRamothStringList;

begin;
  Inherited Create;

  {Initialise the first indexes, as if none present}
  First8K:=FirstInvalid;
  First16K:=FirstInvalid;
  FirstCas:=FirstInvalid;
  RomSizeK:=DefaultRomSize;
  Rom2SizeK:=0;
  SplashFileName:='';
  MenuSorted:=FALSE;

  WriteDebugLn(DbgInfo,'TRomList.ReadFromFile',[]);

  FileText:=TRamothStringList.Create;

  WITH FileText DO
  BEGIN;
    LoadFromFile(FileName);
    First;

    {Read in config section and set global parameters}
    First;
    Config:=GetSection(SecBeginConfig,SecEndConfig);
    CleanSection(SecBeginConfig,SecEndConfig,Config);

    TRY
      IF (Config.Count>0) THEN
      BEGIN;
        RomSizeK:=StrToIntDef(Config.Values[TokRomSize],DefaultRomSize);
        Rom2SizeK:=StrToIntDef(Config.Values[TokRom2Size],DefaultRom2Size);
        SplashFileName:=Trim(Config.Values[TokSplashFile]);
        MenuSorted:=StrToBoolDef(Config.Values[TokSort],False);
        FCompress:=StrToBoolDef(Config.Values[TokCompress],True);
      END;
    FINALLY
      Config.Free;
    END;

    IF (Rom2SizeK > RomSizeK) THEN
      WriteLn(Format('Error, Rom2Size: %d is bigger than Total rom size:%d',[Rom2SizeK,RomSizeK]));

    IF (NOT FileExists(SplashFileName)) THEN
      SplashFileName:='';

;    Add('');                    { Make sure at least 1 line after last patch }
    {Scan the control file for New style entries}
    First;
    while (StrIndex<Count) do
    begin;

      {Check we have enough lines for next record, skip if not }
      if((Count-StrIndex) >= NoRomFields) then
      begin;
        ReadNextEntry;
      end
      else
        NextLine;
    END;

  END;
END;

{***************************************************************************}
{**                                                                       **}
{** Read a next program, entry from the control file (new style)          **}
{**                                                                       **}
{***************************************************************************}
{** New type entry format :

        LineNo  Use
        1       Entry type      CASBIN, CASBAS, BINIMG, ROM, MMCSNAP
        2       Entry name      Text displayed in menu
        3..n    Files to load   One or more files to load + addresses and lengths
        n+1     Write line      WRITE=start addr, end addr, entry addr
        n+2     Further flags   e.g. SAM, VDG etc
**}

PROCEDURE TRomList.ReadNextEntry;

VAR     Entry       : TRomEntry;
        Param       : TStringList;
        Line        : STRING;
        WriteLine   : STRING;
        UpLine      : STRING;
        Section     : TRamothStringList;
        Idx         : INTEGER;

BEGIN;
  Param:=TStringList.Create;
  Section:=TRamothStringList.Create;
  TRY
    Section:=GetSection(SecBeginRom,SecEndRom,TRUE);

    {As we are now getting begin and end markers, then the returned section}
    {must have more than 2 lines (markers + data lines}
    IF (Section.Count>2) THEN
    BEGIN;
      {Create an entry to read the next rom}
      Entry:=TRomEntry.Create(FCompress);
      WITH Entry DO
      BEGIN;
        {Copy control file section into RomEntry}
        ControlSection.AddStrings(Section);
        CleanSection(SecBeginRom,SecEndRom,Section);

        {Read in fields from control file, ROM type and entry name first}
        RomType:=DecodeRomType(Section.Next);
        Text:=Section.Next;
        SetupFromRomType;

        {Check for files, patches, VDG/SAM Modes etc}
        PatchesApplied:=0;
        WriteLine:='';
        WHILE (NOT Section.Eof) DO
        BEGIN;
          Line:=Section.Next;
          UpLine:=UpperCase(Line);

          IF (Pos(TokLoad,UpLine)=1) THEN
            FileNames.Add(Line);

          IF ((Pos(TokPatch,UpLine)=1) OR (Pos(TokFill,UpLine)=1)) THEN
            Patches.Add(Line);

          IF (Pos(TokSAMMode,UpLine)=1) THEN
            Entry.GetSamMode(Line);

          IF (Pos(TokVidBase,UpLine)=1) THEN
            Entry.GetMemBase(Line);

          IF (Pos(TokVDGMode,UpLine)=1) THEN
            Entry.GetVDGMode(Line);

          IF (Pos(TokWrite,UpLine)=1) THEN
            WriteLine:=Line;

          IF (Pos(TokFlags,UpLine)=1) THEN
            Entry.SetFlags(Line);

          IF (Pos(TokStack,UpLine)=1) THEN
            Entry.GetInitStack(Line);
        END;

        FOR Idx:=0 TO (FileNames.Count-1) DO
          ProcessFile(Idx);

        FOR Idx:=0 TO (Patches.Count-1) DO
          ProcessPatch(Idx);

        IF (WriteLine<>'') THEN
          Entry.SetStartEndExec(WriteLine);

        IF (RomType=RTRom) THEN
          RomData.PadToMultiple(RomSize)
        ELSE
        bEGIN
          //WriteLn(Format('Cropping to Start:%4.4X, End:%4.4X, Length: %4.4X',[StartAddr,EndAddr,(EndAddr-StartAddr)]));
          RomData.Crop(StartAddr,CodeLength);
          //WriteLn('New Rom Size : ',RomData.Size);
        end;

        WriteDebugLn(DbgInfo,'New Rom Size : %d',[RomData.Size]);

        {Setup written flag, so all roms start as not yet written}
        Written:=FALSE;

      END;


      Self.Add(Entry);

      IF ((Entry.RomType <> RTRom) AND FCompress) THEN
      BEGIN;
        Entry.FCompress.Compress(Entry.RomData);

        WITH Entry.FCompress.Stats DO
          WriteDebugLn(DbgCompress,'MaxRunChar=%d, MaxRun=%d, Most frequent=%d ($%2.2X), Least frequent=%d ($%2.2X)',[
                                 MaxRunChar,MaxRunValue,
                                 MostFreqent,MostFreqent,
                                 LeastFrequent,LeastFrequent]);

        WriteDebugLn(DbgCompress,'Compression ratio : %3.2F, Orig size : %d, Compressed size %d',
                          [Entry.FCompress.Ratio,Entry.RomData.Size,Entry.FCompress.OutputMem.Size]);

      END;

      {Update FirstCas index, if this is the first cas file}
      IF((Entry.RomType IN [RTCasBin,RTCasBas]) AND (FirstCas=FirstInvalid)) THEN
      BEGIN
        FirstCas:=Self.Count-1;
      END
      ELSE
      BEGIN;
        {If not a cas file, update rom first indexes if needed}
        IF((Entry.RomData.Size=RomSize) AND (First8K=FirstInvalid)) THEN
          First8K:=Self.Count-1;

        IF((Entry.RomData.Size=(RomSize*2)) AND (First16K=FirstInvalid)) THEN
          First16K:=Self.Count-1;
      END;
    END;
  FINALLY
    Param.Free;
    Section.Free;
  END;
END;

FUNCTION TRomList.FindSection(Token       : STRING) : BOOLEAN;

BEGIN;
  Result:=FALSE;
  Token:=UpperCase(Token);
  WHILE ((NOT FileText.EOF) AND (UpperCase(FileText.Current(TRUE))<>Token)) DO
  BEGIN;
    WriteDebugLn(DbgSection,'[%d] %s',[FileText.StrIndex,FileText.Current(TRUE)]);

    FileText.NextLine;
  END;

  Result:=(UpperCase(FileText.Current(TRUE))=Token);
END;

FUNCTION TRomList.GetSection(TokenBegin     : STRING;
                             TokenEnd       : STRING;
                             IncludeTokens  : BOOLEAN = FALSE) : TRamothStringList;

VAR     SectionBegin    : INTEGER;
        SectionEnd      : INTEGER;
        LineIdx         : INTEGER;

BEGIN;
  Result:=TRamothStringList.Create;

  SectionBegin:=-1;
  SectionEnd:=-1;

  IF (FindSection(TokenBegin)) THEN
    SectionBegin:=FileText.StrIndex;

  IF (FindSection(TokenEnd)) THEN
    SectionEnd:=FileText.StrIndex;

  IF (DebugROM) THEN
  BEGIN;
    WriteDebugLn(DbgSection,'Section %s begin %d',[TokenBegin,SectionBegin]);
    WriteDebugLn(DbgSection,'Section %s end %d',[TokenEnd,SectionEnd]);
  END;

  { We found a section begin, without an end, assume EOF=end of section}
  IF ((SectionBegin <> -1) AND (SectionEnd = -1)) THEN
    SectionEnd:=FileText.Count-1;

  {Check that we found the section!}
  IF ((SectionBegin <> -1) AND (SectionEnd <> -1)) THEN
  BEGIN;
    IF (NOT IncludeTokens) THEN
    BEGIN;
      SectionBegin:=SectionBegin+1;
      SectionEnd:=SectionEnd-1;
    END;


    FOR LineIdx:=SectionBegin TO SectionEnd DO
    BEGIN;
      Result.Add(FileText.Strings[LineIdx]);
      WriteDebugLn(DbgSection,FileText.Strings[LineIdx],[]);
    END;
  END;

  Result.First;
END;
{***************************************************************************}
{**                                                                       **}
{** CleanSection takes a raw control file section, removes the specified  **}
{** bigin and end tokens, removes comment lines and trims leading and     **}
{** trailing spaces from the remainder of the lines.                      **}
{** We do this now on a per section basis rather than globally as it      **}
{** allows us to preserve section layout if we want to output a sorted    **}
{** control file.                                                         **}
{**                                                                       **}
{***************************************************************************}

PROCEDURE TRomList.CleanSection(TokenBegin  : STRING;
                                TokenEnd    : STRING;
                                VAR ToClean : TRamothStringList);
VAR Idx     : INTEGER;
    Line    : STRING;
    ULine   : STRING;

BEGIN;
  FOR Idx:=(ToClean.Count-1) DOWNTO 0 DO
  BEGIN;
    Line:=Trim(ToClean.Strings[Idx]);
    ULine:=UpperCase(Line);
    IF ((Line='') OR (ULine=TokenBegin) OR (ULine=TokenEnd) OR (ULine[1] IN [';','#'])) THEN
      ToClean.Delete(Idx)
    ELSE
      ToClean.Strings[Idx]:=Line;
  END;

  ToClean.First;
END;

Function TRomList.HexConv(Line   : STRING) : WORD;

VAR     IntRes  : INTEGER;      { }

begin;
  Line:=Trim(Line);                     { Trim Spaces }
  if (Line[1] <> '$') then              { Add a '$' if not already present }
    Line:='$'+Line;

  if(NOT TryStrToInt(Line,IntRes)) then { If conversion error, display message }
    WriteLn(Format('Error in control file %s is an invalid hex number',[Line]));

  Result:=IntRes;
end;


{***************************************************************************}
{**                                                                       **}
{** Get the rom blocka at specified index in rom list, return as TRomEntry**}
{**     Returns nil, if specified index is invalid.                       **}
{**                                                                       **}
{***************************************************************************}

Function TRomList.GetBlock(BlockNo      : INTEGER) : TRomEntry;

BEGIN;
  IF ((BlockNo>=0) AND (BlockNo<Self.Count)) THEN
    Result:=TRomEntry(Items[BlockNo])
  ELSE
    Result:=NIL;
END;

{***************************************************************************}
{**                                                                       **}
{** FindNextBlock, scans list of roms for the next block of a specified   **}
{**     type that has not been written, and returns it's entry.           **}
{**                                                                       **}
{***************************************************************************}

Function TRomList.FindNextBlock(BType  : Byte;
                                Size   : Int64) : TRomEntry;

VAR Idx         : INTEGER;      { Index to itterate list }
    Entry       : TRomEntry;    { Entry from list }

BEGIN;
  WriteDebugLn(DbgBlock,'FindNextBlock(%d,%4.4X)',[BType,Size]);

  Result:=NIL;  { Asume we'll fail }
  Idx:=0;

  {Loop over list}
  WHILE ((Idx<Self.Count) AND (Result=NIL)) DO
  BEGIN;
    { Get next entry to check }
    Entry:=TRomEntry(Items[Idx]);

    { Only check if we have not already written it }
    IF (NOT Entry.Written) THEN
    BEGIN;
      {Check for any cas, Raw memory dumps or Snapshots}
      IF ((BType=RTCasAny) AND
          (Entry.RomType IN [RTCasBin,RTCasBas,RTBinary,RTSnapMMC,RTDDMMCBas,RTDDMMCBin])) THEN
        Result:=Entry;

      {Check for rom files}
      IF ((BType=RTRom) AND (Entry.RomType=RTRom) AND
          (Entry.RomData.Size=Size)) THEN
        Result:=Entry;
    END;
    Idx:=Idx+1; { Increment index }
  END;
END;

PROCEDURE TRomList.SortMenu;

BEGIN;
  IF (MenuSorted) THEN
  BEGIN;
    Sort(@RomListCompare);
    WriteLn('Menu sorted.....');
  END;
END;

PROCEDURE TRomList.OutputSortedFile(OutFileName   : STRING);

VAR OutputFile      : TRamothStringList;
    ConfigSection   : INTEGER;
    Idx             : INTEGER;
    SectionList     : TRamothStringList;
    RomEntry        : TRomEntry;

BEGIN;
  OutputFile:=TRamothStringList.Create;
  SectionList:=TRamothStringList.Create;
  TRY
    { Find the config section as it is first }
    FileText.First;
    FindSection(SecEndConfig);
    ConfigSection:=FileText.StrIndex;

    FileText.First;
    FileText.CopyNToList(@OutputFile,ConfigSection+1);

    {Sort the list if not already sorted.... since this happens after the ROM}
    {has been output, this shouldn't force sorting where not required}
    IF (NOT MenuSorted) THEN
      Sort(@RomListCompare);

    FOR Idx:=0 TO (Self.Count-1) DO
    BEGIN;
      OutputFile.Add('');
      RomEntry:=GetBlock(Idx);

      IF (RomEntry<>NIL) THEN
      BEGIN;
        OutputFile.AddStrings(RomEntry.ControlSection);
      END;
    END;
    WriteLnFmt('Write Output to : %s',[OutFileName]);
    OutputFile.SaveToFile(OutFileName);
  FINALLY
    OutputFile.Free;
    SectionList.Free;
  END;
END;

CONSTRUCTOR TRomEntry.Create(InTryCompress   : BOOLEAN);

BEGIN;
  INHERITED Create;
  FileNames:=TStringList.Create;
  Patches:=TStringList.Create;
  ControlSection:=TStringList.Create;
  RomData:=TRamothMemoryStream.Create;
  FCompress:=TRLECompressor.Create;
  BaseAddr:=$0000;
  FSamBits:=SAMInvalid;
  FVDGBits:=VDGInvalid;
  FNoCas:=FALSE;
  FNoOK:=FALSE;
  FDisableFIRQ:=FALSE;
  FCasBinType:=cbCasBin;
  FInitStack:=StackInvalid;
  FTryCompress:=InTryCompress;
END;

DESTRUCTOR TRomEntry.Destroy;

BEGIN;
  FileNames.Free;
  Patches.Free;
  ControlSection.Free;
  RomData.Free;
  FCompress.Free;

  INHERITED Destroy;
END;

PROCEDURE TRomEntry.GetParams(Line     : STRING;
                              Token    : STRING;
                              VAR List : TRamothStringList);

BEGIN;
  Line:=Copy(Line,Length(Token)+1,MaxInt);
  List.Split(Line,',');
END;

FUNCTION TRomEntry.GetParams(Line: STRING;
                             Token: STRING) : TRamothStringList;

BEGIN;
  Result:=TRamothStringList.Create;
  GetParams(Line,Token,Result);
END;

FUNCTION TRomEntry.GetLength : WORD;

BEGIN;
  Result:=(EndAddr-StartAddr)+1;
END;

{***************************************************************************}
{**                                                                       **}
{** DecodeRomType, convert ascii rom type to integer rom type, also sets  **}
{**                CasBasType, if this is a cassette file, and BASIC      **}
{**                                                                       **}
{***************************************************************************}

Function TRomEntry.DecodeRomType(RomTypeDecode : STRING) : BYTE;
BEGIN;
  RomTypeDecode:=Trim(UpperCase(RomTypeDecode));
  Result:=RTRom;                {Assume it will be a rom}

  IF (RomTypeDecode=KeyRTCasBin) THEN Result:=RTCasBin;
  IF ((RomTypeDecode=KeyRTCasBasDragon) OR (RomTypeDecode=KeyRTCasBasCoCo)) THEN
  BEGIN;
    Result:=RTCasBas;
    IF (RomTypeDecode=KeyRTCasBasDragon) THEN CasBasType:=CTDragon;
    IF (RomTypeDecode=KeyRTCasBasCoCo) THEN CasBasType:=CTCoCo;
  END;
  IF (RomTypeDecode=KeyRTBinaryImage) THEN
    Result:=RTBinary;
  IF (RomTypeDecode=KeyRTDragonMMCSnap) THEN
    Result:=RTSnapMMC;
  IF (RomTypeDecode=KeyRTDDMMCBin) THEN
    Result:=RTDDMMCBin;
  IF (RomTypeDecode=KeyRTDDMMCBas) THEN
    Result:=RTDDMMCBas;
END;
FUNCTION TRomEntry.DoLoadROMorBin(LocalFileName      : STRING) : BOOLEAN;

VAR
    Buffer          : TRamothMemoryStream;

BEGIN;
  Buffer:=TRamothMemoryStream.Create;
  TRY
    IF (FileExists(LocalFileName)) THEN
      Buffer.LoadFromFile(LocalFileName);     { Load the data }

    IF (RomType = RTRom) THEN
      RomData.Seek(0,soFromBeginning)
    ELSE
      RomData.Seek(FLoadAddr,soFromBeginning);

    RomData.CopyBytesFromStream(Buffer,0,Buffer.Size);
  FINALLY
    Buffer.Free;
  END;
  Result:=TRUE;
END;

FUNCTION TRomEntry.DoLoadCAS(LocalFileName      : STRING;
                             FileNo             : INTEGER) : BOOLEAN;

VAR
    CasFile         : TDragonCoCoCas; { CAS file image }
    Head            : TDragonCoCoCasNameBlock;
    Buffer          : TRamothMemoryStream;
    ByteCount       : BYTE;
    WriteAddr       : WORD;
    Found           : TFindBlock;

BEGIN;
  FDisableFIRQ:=TRUE;

  {Load the cas file}
  CasFile:=TDragonCoCoCas.Create;
  Buffer:=TRamothMemoryStream.Create;
  TRY
    WriteDebugLn(DbgLoad,'DoLoadCas(%s)',[LocalFileName]);
    CasFile.LoadCasFile(LocalFileName);
    {Check that the filenumber specified is valid}
    {if invalid return an error}
    IF (FileNo>CasFile.FilesInCas) THEN
      Result:=FALSE
    ELSE
    BEGIN;
      {Get the data from the cas file}
      CasFile.GetFileData(FileNo-1,0,Buffer);
      Head:=TDragonCoCoCasNameBlock(CasFile.FileNames.Items[FileNo-1]);

      WITH Head DO
        WriteDebugLn(DbgHead,'Name=%s, Load=%04X, Exec=%04X',[FileName,LoadAddress,ExecAddress]);

      {If Basic, then always take len from file & set skip=0}
      IF (Head.FileID=FtBasic) THEN
      BEGIN;
        FHeadStart:=TRUE;
        FHeadLen:=TRUE;
        FHeadExec:=TRUE;
        Skip:=0;
      END;

      {Set start,length and exec based on flags}
      IF (FHeadStart) THEN
      BEGIN;
        FStartAddr:=Head.LoadAddress;
        FLoadAddr:=FStartAddr;
      END;
      IF (FHeadExec) THEN FExecAddr:=Head.ExecAddress;
      IF (FHeadLen) THEN FEndAddr:=StartAddr+Buffer.Size;

      CASE FCasBinType OF
        cbCasBin :
          BEGIN;
            {Copy data to cas stream}
            RomData.Seek(FLoadAddr,soFromBeginning);
            RomData.CopyBytesFromStream(Buffer,0,Buffer.Size);
          END;
        cbMicrodealGapped :
          BEGIN
            Buffer.Seek(0,soFromBeginning);
            CasFile.Seek(Head.CasDataOffset,soFromBeginning);
            Found:=CasFile.FindNextBlock;
            WHILE (Found<>BlkNoMore) DO
            BEGIN;
              IF (CasFile.BlockType = BtData) THEN
              BEGIN;
                ByteCount:=CasFile.ReadByte;
                WriteAddr:=CasFile.ReadMotorolaWord;
                IF (CasFile.BlockLen < ByteCount) THEN ByteCount:=CasFile.BlockLen;
                RomData.Seek(WriteAddr,soFromBeginning);
                RomData.CopyBytesFromStream(CasFile,ByteCount);
                Found:=CasFile.FindNextBlock(1);
              END
              ELSE
                Found:=BlkNoMore;
            END;
          END;
      END;

      Result:=TRUE;
    END;
  FINALLY
    CasFile.Free;
    Buffer.Free;
  END;
END;

{***************************************************************************}
{**                                                                       **}
{** Load a DragonMMC snapshot file. The snapshot consists of a 32K image  **}
{** of the RAM preceeded by 2 bytes containing the 6809's system stack    **}
{** pointer. The stack contains the following values :                    **}
{** High:                                                                 **}
{**     PC,U,Y,X,DP,B,A,CC,                                               **}
{**     PIA0(CRA,DDRA,DA,CRB,DDRB,DB),                                    **}
{**     PIA1(CRA,DDRA,DA,CRB,DDRB,DB),                                    **}
{**     SAMBITS(2 bytes)                                                  **}
{**     VDU cursor address (2 bytes)                                      **}
{**     64 bytes of saved screen data for bottom 2 lines of text screen   **}
{**     11 bytes of saved registers + return addresses                    **}
{***************************************************************************}

FUNCTION TRomEntry.DoLoadDragonMMCSnap(LocalFileName      : STRING) : BOOLEAN;

BEGIN;
  //RomData:=TRamothMemoryStream.Create;
  RomData.LoadFromFile(LocalFileName);     { Load the data }

  { Get stack pointer from Snapshot}
  RomData.Seek(0,soFromBeginning);
  SnapStack:=RomData.ReadMotorolaWord;

  { Remove saved regs + addresses of DragonMMC menu code, so that on loading }
  { we just return straight to the program }
  RomData.Seek(0,soFromBeginning);
  RomData.WriteMotorolaWord(SnapStack+11);

  IF ((FStartAddr <> DefStartAddrMMC) OR (FEndAddr <> DefEndAddrMMC)) THEN
  BEGIN;
    RomData.Crop(2,RomData.Size-2);
    RomType:=RTBinary;
  END;

  Result:=TRUE;
END;

{***************************************************************************}
{**                                                                       **}
{** Load a DragonDOS or DragonMMC format file.                            **}
{**                                                                       **}
{***************************************************************************}
FUNCTION TRomEntry.DoLoadDDragonDosDMMC(LocalFileName      : STRING) : BOOLEAN;

VAR DosMMCBin   : TDragonDOSMMCBinFile;
    Buffer      : TRamothMemoryStream;

BEGIN
  DosMMCBin:=TDragonDOSMMCBinFile.Create;
  Buffer:=TRamothMemoryStream.Create;

  TRY
    DosMMCBin.ReadFromFile(LocalFileName);
    WITH DosMMCBin DO
    BEGIN;
      WriteLnFmt('Type=%02X, Load=%04X, Len=%04X, Exec=%04X',[FileType,LoadAddr,FLen,ExecAddr]);

      IF (Valid) THEN
      BEGIN;
        GetFileData(Buffer);

        { If basic, use start, len and exec from header }
        IF (FileType = DDFTypeBas) THEN
        BEGIN;
          FHeadStart:=TRUE;
          FHeadLen:=TRUE;
          FHeadExec:=TRUE;
          Skip:=0;
        END;

        {Set start,length and exec based on flags}
        IF (FHeadStart) THEN
        BEGIN;
          FStartAddr:=LoadAddr;
          FLoadAddr:=FStartAddr;
        END;
        IF (FHeadExec) THEN FExecAddr:=ExecAddr;
        IF (FHeadLen) THEN FEndAddr:=StartAddr+Buffer.Size;

        {Copy data From disk file}
        RomData.Seek(FLoadAddr,soFromBeginning);
        RomData.CopyBytesFromStream(Buffer,0,Buffer.Size);
      END;
      Result:=Valid;
    END;
  FINALLY
    DosMMCBin.Free;
    Buffer.Free;
  END;
END;

{***************************************************************************}
{**                                                                       **}
{** FixupFileName, fix the filename, remove leading trailing quotes.      **}
{**                                                                       **}
{***************************************************************************}

Function TRomEntry.FixupFileName(InFileName       : STRING) : STRING;

VAR     Last    : INTEGER;

BEGIN;
  InFileName:=Trim(InFileName);
  IF (System.Length(InFileName)>0) THEN
  BEGIN;
    Last:=System.Length(InFileName);
    IF (InFileName[1]='"') THEN InFileName[1]:=' ';
    IF (InFileName[Last]='"') THEN InFileName[Last]:=' ';
    InFileName:=Trim(InFileName);
  END;

  Result:=InFileName;
END;

{***************************************************************************}
{**                                                                       **}
{** GetWriteRomType, get the rom type that will be written to linked file **}
{**                                                                       **}
{***************************************************************************}

Function TRomEntry.GetWriteRomType : BYTE;

BEGIN;
  {Binary written as if they where tape images}
  IF (RomType IN [RTBinary]) THEN
    Result:=RTCasBin
  ELSE
    Result:=RomType;
END;

Function TRomEntry.GetWriteRomFlags : BYTE;

BEGIN;
  Result:=0;

  CASE RomType OF
    RTCasBas :
    BEGIN;
      IF (CasBasType=CTCoCo) THEN
        Result:=Result OR RTFlagCoCoBas;
      IF ((FCompress.Ratio < 100) AND TryCompress) THEN
        Result:=Result OR RTFlagCompress;
    END;
    RTCasBin,
    RTBinary,
    RTSnapMMC :
    BEGIN;
      IF (FDisableFIRQ) THEN
        Result:=Result OR RTFlagDisableFIRQ;
      IF (FNoOK) THEN
        Result:=Result OR RTFlagNoOK;
      IF (FNoCas) THEN
        Result:=Result OR RTFlagNoCas;
      IF ((FCompress.Ratio < 100) AND TryCompress) THEN
        Result:=Result OR RTFlagCompress;
    END;
  END;
END;


PROCEDURE TRomEntry.SetupFromRomType;

BEGIN;
  FStartAddr:=DefStartAddr;
  FEndAddr:=DefEndAddr;

  { Roms are based at $C000, all other types at $0000}
  IF (RomType = RTRom) THEN
    BaseAddr:=$C000
  ELSE
    BaseAddr:=$0000;

  { For everything except ROM, fill the first 32K of memory image}
  IF (RomType <> RTRom) THEN
    RomData.WriteRepetedChars(CHR(0),32*1024);

  IF (RomType = RTSnapMMC) THEN
  BEGIN;
    {All snapshots start at 0 and end at $7FFF}
    FStartAddr:=DefStartAddrMMC;
    FEndAddr:=DefEndAddrMMC;
    FExecAddr:=DefExecAddr;

    {Flag don't print OK, or save tape buffer}
    FNoOK:=TRUE;
    FNoCas:=TRUE;
    FDisableFIRQ:=TRUE;
  END;
END;


PROCEDURE TRomEntry.GetSamMode(Line        : STRING);

VAR     Mode    : Integer;

BEGIN;
  Line:=Trim(Copy(Line,Length(TokSAMMode)+1,MaxInt));

  IF (TryStrToInt(Line,Mode)) THEN
  BEGIN;
    IF (FSamBits=SAMInvalid) THEN
      FSamBits:=(Mode AND SAMModeMask)
    ELSE
      FSamBits:=(FSamBits AND NOT SAMModeMask) OR (Mode AND SAMModeMask);
  END;
END;

PROCEDURE TRomEntry.GetMemBase(Line        : STRING);

VAR     Base    : Integer;

BEGIN;
  Line:=Trim(Copy(Line,Length(TokVidBase)+1,MaxInt));

  IF (TryStrToInt(Line,Base)) THEN
  BEGIN;
    Base:=Base DIV 512;

    IF (FSamBits=SAMInvalid) THEN
      FSamBits:=((Base SHL 3) AND SAMBaseMask)
    ELSE
      FSamBits:=(FSamBits AND NOT SAMBaseMask) OR ((Base SHL 3) AND SAMBaseMask);
  END;
END;

PROCEDURE TRomEntry.GetVDGMode(Line        : STRING);

VAR     Mode    : Integer;
        Split   : TRamothStringList;

BEGIN;
  Split:=GetParams(Line,TokVDGMode);
  TRY
    IF (TryStrToInt(Split[0],Mode)) THEN
    BEGIN;
      Mode:=(Mode AND $07) SHL 4;

      IF (Split.IndexOf(TokCSS)>-1) THEN
        Mode:=Mode OR VDGCSS;

      IF (Split.IndexOf(TokAG)>-1) THEN
        Mode:=Mode OR VDGAG;

      FVDGBits:=Mode  AND VDGMask;
    END;
  FINALLY
    Split.Free;
  END;
END;

PROCEDURE TRomEntry.SetStartEndExec(Line        : STRING);

VAR     Split   : TRamothStringList;

BEGIN;
  Split:=GetParams(Line,TokWrite);
  TRY
    IF (Split.Count>0) THEN
      FStartAddr:=StrToIntDef(Split[0],FStartAddr);

    IF (Split.Count>1) THEN
    BEGIN;
      FEndAddr:=StrToIntDef(Split[1],FEndAddr);
      //IF (FEndAddr > 0) THEN
      //  FEndAddr:=FEndAddr-1;
    END;

    IF (Split.Count>2) THEN
      FExecAddr:=StrToIntDef(Split[2],FExecAddr);
  FINALLY
    Split.Free;
  END;
END;

PROCEDURE TRomEntry.SetFlags(Line        : STRING);

VAR     Split   : TRamothStringList;
        Idx     : INTEGER;
        Up      : STRING;

BEGIN;
  Split:=GetParams(Line,TokFlags);
  TRY
    FOR Idx:=0 TO (Split.Count-1) DO
    BEGIN
      Up:=Trim(UpperCase(Split.Strings[Idx]));
      IF (Up=TokNoOKPrint) THEN FNoOK := TRUE;
      IF (Up=TokNoCas)     THEN FNoCas:= TRUE;
      IF (Up=TokDisableFIRQ) THEN FDisableFIRQ:=TRUE;
    END;
  FINALLY
    Split.Free;
  END;
END;

PROCEDURE TRomEntry.GetInitStack(Line        : STRING);

VAR     Split   : TRamothStringList;

BEGIN;
  Split:=GetParams(Line,TokStack);
  TRY
    FInitStack:=StrToIntDef(Split[0],StackInvalid);
    WriteLn('SPLIT='+Split[0]);
    IF (FInitStack >= $8000) THEN
      FInitStack:=StackInvalid;
  FINALLY
    Split.Free;
  END;
END;

FUNCTION TRomEntry.ProcessFile(FileNo     : INTEGER) : BOOLEAN;

VAR Params          : TRamothStringList;
    LocalFileName   : STRING;
    Up              : STRING;
    CasFileNo       : INTEGER;
    Idx             : INTEGER;

BEGIN;
  Params:=GetParams(FileNames.Strings[FileNo],TokLoad);
  TRY
    FHeadStart:=FALSE;
    FHeadLen:=FALSE;
    FHeadExec:=FALSE;
    CasFileNo:=1;

    LocalFileName:=FixupFileName(Params.Strings[0]);
    WriteDebugLn(DbgLoad,'Loading : %s',[LocalFileName]);

    {Scan params and set flags }
    FOR Idx:=0 TO (Params.Count-1) DO
    BEGIN;
      Up:=Trim(UpperCase(Params.Strings[Idx]));
      IF (Up=TokHeadStart) THEN FHeadStart:=TRUE;
      IF (Up=TokHeadLen)   THEN FHeadLen:=TRUE;
      IF (Up=TokHeadExec)  THEN FHeadExec:=TRUE;
      IF (Up=TokMDGap)     THEN FCasBinType:=cbMicrodealGapped;

      WriteDebugLn(DbgLoad,'Param %d=%s',[Idx,Params.Strings[Idx]]);
    END;

    {Check for Load, End, Exec specified on command line}
    IF (Params.IndexOfName(TokLoad)>-1) THEN
    BEGIN;
      FLoadAddr:=StrToIntDef(Params.Values[TokLoad],DefStartAddr);
      FStartAddr:=FLoadAddr;
    END;

    IF (Params.IndexOfName(TokEnd)>-1) THEN
      FEndAddr:=StrToIntDef(Params.Values[TokEnd],DefEndAddr);

    IF (Params.IndexOfName(TokExec)>-1) THEN
      FExecAddr:=StrToIntDef(Params.Values[TokExec],DefExecAddr);

    IF ((RomType IN [RTCasBas, RTCasBin]) AND (Params.Count>1)) THEN
      CasFileNo:=StrToIntDef(Params.strings[1],1);

    CASE RomType OF
      {ROM or binary dump from MESS}
      RTRom,
      RTBinary          : Result:=DoLoadROMorBin(LocalFileName);

      {Cas image either binary or Basic}
      RTCasBas,
      RTCasBin          : Result:=DoLoadCAS(LocalFileName,CasFileNo);

      {DragonMMC snapshot file}
      RTSnapMMC         : Result:=DoLoadDragonMMCSnap(LocalFileName);

      RTDDMMCBin,
      RTDDMMCBas        : Result:=DoLoadDDragonDosDMMC(LocalFileName);
    END;

    IF (NOT Result) THEN
    BEGIN;
      Raise Exception.Create(Format('Error: unable to load %s, aborting',[LocalFileName]));
    END;
  FINALLY
    Params.Free;
  END;
END;

PROCEDURE TRomEntry.ProcessPatch(PatchNo   : INTEGER);

VAR Patch   : TPatch;

BEGIN
  Patch:=TPatch.Create(Patches.Strings[PatchNo]);
  TRY
    Patch.Apply(Self);
  FINALLY
    Patch.Free;
  END;
END;


{***************************************************************************}
{**                                                                       **}
{** Create a patch from a patch line, to patch a binary file              **}
{**                                                                       **}
{***************************************************************************}

TYPE    TPatchState     = (psAddr,psCount,psData,psDone);

Constructor TPatch.Create(Line  : STRING);

VAR     SpacePos        : INTEGER;              { Pos of first space after token }
        Token           : STRING;               { Patch token }

BEGIN;
  INHERITED Create;

  Line:=Trim(Line);
  SpacePos:=Pos(' ',Line);
  Token:=Copy(Line,0,SpacePos-1);
  Line:=Trim(Copy(Line,SpacePos+1,MaxInt));

  IF (UpperCase(Token)=TokPatch) THEN CreatePatch(Line);
  IF (UpperCase(Token)=TokFill) THEN CreateFill(Line);
END;

PROCEDURE TPatch.CreatePatch(Line   : STRING);

VAR     PatchList       : TRamothStringList;    { Temp patch list }
        Idx             : INTEGER;              { List index }
        State           : TPatchState;          { Patch scanning state }
        Conv            : INTEGER;              { Converted word }
        ConvByte        : BYTE;                 { Converted byte }
        ConvStr         : STRING;               { Tempory string }
        FirstChar       : CHAR;                 { First char of each evaluated line }

BEGIN;
  PatchList:=TRamothStringList.Create;        { Create a temp list for the patch }

  TRY
    PatchList.Split(Line,',',FALSE);    { Break up line on commas }
    State:=psAddr;                       { Look for address first }

    WITH PatchList DO
    BEGIN;
      FOR Idx:=0 TO (PatchList.Count-1) DO         { Itterate through params }
      BEGIN;
        Strings[Idx]:=Trim(Strings[Idx]);          { Trim spaces }
        IF (Length(Strings[Idx])>0) THEN
        BEGIN;
          FirstChar:=Strings[Idx][1];
          IF (FirstChar='"') THEN
          BEGIN;          { String patch }
            ConvStr:=StripChars(Strings[Idx],'"');
            IF (Length(ConvStr)>0) THEN
              Write(ConvStr[1],Length(ConvStr));
          END
          ELSE
          BEGIN;
            IF (GetIntVal16(Strings[Idx],Conv)) THEN
            BEGIN;
              CASE (State) OF
                psAddr : BEGIN; { Looking for start addr }
                           PatchAddr:=Word(Conv);
                           State:=psData;
                         END;
                psData : BEGIN; { Looking for Data as hex }
                           ConvByte:=BYTE(Conv);
                           Self.Write(ConvByte,1);
                         END;
              END;
            END
          END;
        END;
      END;
    END;
  FINALLY
    PatchList.Free;
  END;
END;

PROCEDURE TPatch.CreateFill(Line   : STRING);

VAR     PatchList       : TRamothStringList;    { Temp patch list }
        Idx             : INTEGER;              { List index }
        State           : TPatchState;          { Patch scanning state }
        Conv            : INTEGER;              { Converted word }
        ConvByte        : BYTE;                 { Converted byte }
        ByteCount       : INTEGER;              { Fill Count }

BEGIN;
  PatchList:=TRamothStringList.Create;          { Create a temp list for the patch }

  TRY
    PatchList.Split(Line,',',FALSE);            { Break up line on commas }
    State:=psAddr;                              { Look for address first }
    ByteCount:=0;                                   { Count defaults to zero }

    WITH PatchList DO
    BEGIN;
      FOR Idx:=0 TO (PatchList.Count-1) DO         { Itterate through params }
      BEGIN;
        Strings[Idx]:=Trim(Strings[Idx]);          { Trim spaces }
        IF (Length(Strings[Idx])>0) THEN
        BEGIN;
          IF (GetIntVal16(Strings[Idx],Conv)) THEN
          BEGIN;
            CASE (State) OF
              psAddr : BEGIN; { Looking for start addr }
                         PatchAddr:=Word(Conv);
                         State:=psCount;
                       END;
              psCount : BEGIN;
                          ByteCount:=Conv;
                          State:=psData;
                        END;
              psData : BEGIN; { Looking for Data as hex }
                         ConvByte:=BYTE(Conv);
                         Self.WriteRepetedBytes(ConvByte,ByteCount);
                         State:=psData;
                       END;
            END
          END;
        END;
      END;
    END;
  FINALLY
    PatchList.Free;
  END;
END;

FUNCTION TPatch.GetIntVal16(Expression    : STRING;
                            OUT IntVal    : INTEGER) : BOOLEAN;

VAR FParser         : TFPExpressionParser;

BEGIN;
  FParser:=TFPExpressionParser.Create(nil);

  TRY
    // Try simple conversion
    IntVal:=StrToIntDef(Expression,-1);

    // If that fails try evaluating as an expression
    IF (IntVal = -1) THEN
    BEGIN;
      FParser.Expression:=Expression;
      IntVal:=FParser.AsInteger;
    END;

    Result:=((IntVal > -1) AND (IntVal < $10000));
  FINALLY
    FParser.Free;
  END;
END;

{***************************************************************************}
{**                                                                       **}
{** Apply a patch to a rom.                                               **}
{**                                                                       **}
{***************************************************************************}


Procedure TPatch.Apply(ToPatch  : TRomEntry);

VAR     ApplyAddr       : Int64;

BEGIN;
  Self.Position:=0;
  IF ((PatchAddr>=ToPatch.BaseAddr) AND
      (PatchAddr<(ToPatch.BaseAddr+ToPatch.RomData.Size))) THEN
  BEGIN;
    ApplyAddr:=PatchAddr-ToPatch.BaseAddr;
    WITH ToPatch.RomData DO
    BEGIN;
      Position:=ApplyAddr;
      CopyBytesFromStream(Self,Self.Size);
    END;
    ToPatch.PatchesApplied:=ToPatch.PatchesApplied+1;
  END;
END;

end.



