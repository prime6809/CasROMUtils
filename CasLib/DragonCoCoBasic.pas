unit DragonCoCoBasic;

interface

uses SysUtils,RamothMemoryStream,contnrs,DragonCoCoCasUnit,
     DragonCoCoCasNameBlockUnit,Classes,DragonCoCoBasicTokens,
     DragonCoCoCASDefsUnit;

const LinkSize  = 2;    {2 linking bytes at the beginning of each basic line}

      MessNotBasic      = 'Error, not a basic program';
      MessInvalidFileNo = 'Invalid file number';

TYPE  TBasicLine  = class(Tobject)
                    LineNo        : WORD;
                    Line          : STRING;
                    constructor ReadFromStreamPos(ReadFrom  : TRamothMemoryStream;
                                                  StartPos  : Int64);
                    constructor ReadFromStream(ReadFrom  : TRamothMemoryStream);
                    FUNCTION ReTokenizeLine(SrcDialect     : CHAR;
                                            DstDialect     : CHAR) : INTEGER;
                  END;
                  
      TBasicProg  = class(TObject)
                    Lines       : TObjectList;
                    BasDialect  : CHAR;
                    Constructor LoadFromCas(CasFile     : TDragonCoCoCas;
                                            FileNo      : INTEGER;
                                            Dialect     : CHAR);
                    FUNCTION Retokenize(Dialect         : CHAR;
                                        IgnoreInvalid   : BOOLEAN) : BOOLEAN;
                    FUNCTION List : TStringList;
                    FUNCTION ToBinImage : TRamothMemoryStream;
                    FUNCTION GetLineLen(LineNo  : INTEGER) : WORD;
                  END;

      EInvalidFileNo    = class(Exception);     { File number out of range }
      ENotBasic         = class(Exception);     { Specified file not basic }

implementation

{*****************************************************************************}
{**                                                                         **}
{** TBasicLine, holds a single tokenized line of basic code.                **}
{**                                                                         **}
{*****************************************************************************}

constructor TBasicLine.ReadFromStreamPos(ReadFrom  : TRamothMemoryStream;
                                         StartPos  : Int64);

BEGIN;
  INHERITED Create;
  WITH ReadFrom DO
  BEGIN;
    Position:=StartPos+LinkSize;        { Set start pos, ignore link bytes }
    LineNo:=ReadMotorolaWord;           { Get line number }
    Line:=ReadASCIIZ;                   { Read the zero terminated line }
  END;
END;

constructor TBasicLine.ReadFromStream(ReadFrom  : TRamothMemoryStream);

BEGIN;
  ReadFromStreamPos(ReadFrom,ReadFrom.Position);
END;

{*****************************************************************************}
{**                                                                         **}
{** Retokenise a basic line in the specified dialect, returns index of      **}
{**   first invalid token in the new specified dialect, returns 0 if all    **}
{**   tokens are valid.                                                     **}
{**   For example the MC10 has no hi-res commands so these would be invalid.**}
{**                                                                         **}
{*****************************************************************************}

FUNCTION TBasicLine.ReTokenizeLine(SrcDialect     : CHAR;
                                   DstDialect     : CHAR) : INTEGER;

VAR     NewLine : STRING;       { Re-Tokenized line }
        ChIdx   : INTEGER;      { Index of character within line being processed }
        Last    : BYTE;         { Last byte from line, used to detect functions (Dragon/CoCo) }
        Tok     : TToken;       { Token to re-tokenize }
        NewTok  : TToken;       { Re-tokenized token }
        
BEGIN;
  ChIdx:=1;                                   {Init scanning vars}
  Tok:=TToken.Create;
  Tok.TokenType:=0;
  Last:=0;
  NewLine:='';

  {Itterate ofver all chars in line}
  WHILE (ChIdx<=Length(Line)) DO
  BEGIN;
    Tok.Token:=ORD(Line[ChIdx]);        {Get a char from line}
    Tok.TokenType:=Last;

    NewTok:=Tok.Retokenize(SrcDialect,DstDialect);
    
    IF (NewTok.Token<>0) THEN
      NewLine:=NewLine+NewTok.AsString; {Add re-tokenized character to new line}

    NewTok.Free;

    Last:=Tok.Token;                    {record last char}
    ChIdx:=ChIdx+1;                     {Increment index}
  END;
  Line:=NewLine;                        {Replace line, with retokenized line}
  Tok.Free;
END;

{*****************************************************************************}
{**                                                                         **}
{** TBasicProg, holds a tokenized basic program, this has metods for reading**}
{**     and writing cas files, dumping the code as ascii, and translating   **}
{**     between basic token types.                                          **}
{**                                                                         **}
{*****************************************************************************}

Constructor TBasicProg.LoadFromCas(CasFile     : TDragonCoCoCas;
                                   FileNo      : INTEGER;
                                   Dialect     : CHAR);

VAR     RawCas  : TRamothMemoryStream;          { Buffer for raw cas data }
        Head    : TDragonCoCoCasNameBlock;      { Header block for cas file }
        Line    : TBasicLine;                   { Basic line read from cas }
        
BEGIN;
  INHERITED Create;
  Lines:=TObjectList.Create;

  WITH CasFile DO
  BEGIN;
    IF (FileNo<=FilesInCas) THEN
    BEGIN;
      {Get file header for specified file}
      Head:=TDragonCoCoCasNameBlock(FileNames.Items[FileNo]);
      
      {Only attempt to process basic program files}
      IF (Head.FileID=FtBasic) THEN
      BEGIN;
        RawCas:=CasFile.GetFileData(FileNo,0);          { Get the data }
        RawCas.position:=0;
        
        {Make sure there's enough left for a line of basic, there should be}
        {at least, Link:word, LineNo:word, Terminator:byte, so 5 bytes total}
        WHILE ((RawCas.Position+5)<RawCas.Size) DO
        BEGIN;
          Line:=TBasicLine.ReadFromStream(RawCas);      { Read a line }
          Lines.Add(Line);                              { Add to list }
        END;
        RawCas.Free;                                    { Free raw block }
        
        BasDialect:=Dialect;                            { Set the dialect }
      END
      ELSE
        Raise ENotBasic.Create(MessNotBasic);           { Error, not basic }
    END
    ELSE
      Raise EInvalidFileNo.Create(MessInvalidFileNo);   { Specified fileno too big }
  END;
END;

{*****************************************************************************}
{**                                                                         **}
{**  List, produces an ASCII listing of the BASIC program, it returns a     **}
{**        stringlist containing the ascii text of the program.             **}
{**        This procedure will automatically take care of the dialect,      **}
{**        providing this was set correctly at creation time.               **}
{**                                                                         **}
{*****************************************************************************}

FUNCTION TBasicProg.List : TStringList;

VAR     Idx     : INTEGER;      { Line Index }
        BasLine : TBasicLine;   { Temp var for the line being processed }
        Line    : STRING;       { Text version of line, to be output }
        ChIdx   : INTEGER;      { Index of character within line being processed }
        Current : BYTE;         { Current byte from line }
        Last    : BYTE;         { Last byte from line, used to detect functions (Dragon/CoCo) }
        DeTok   : STRING;       { DeTokenized token text }

BEGIN;
  Result:=TStringList.Create;

  {Itterate through all basic lines in program}
  FOR Idx:=0 TO (Lines.Count-1) DO
  BEGIN;
    BasLine:=TBasicLine(Lines.Items[Idx]);      {Get a line to process}
    
    Line:=Format('%d ',[BasLine.LineNo]);       {Convert line no}

    ChIdx:=1;                                   {Init scanning vars}
    Last:=0;

    {Itterate ofver all chars in line}
    WHILE (ChIdx<=Length(BasLine.Line)) DO
    BEGIN;
      Current:=ORD(BasLine.Line[ChIdx]);        {Get a char from line}
      CASE Current OF
        $01..$7F        : DeTok:=CHR(Current);  {Normal ascii, do nothing to it}
        $80..$FE        : BEGIN;                {Token, so detokenize}
                            DeTok:=TokenToText(Current,BasDialect,Last);
                          END;
        $FF             : DeTok:='';            {Function marker, don't add anything}
      END;
      Line:=Line+DeTok;                         {Add de-tokenized character to line}
      
      Last:=Current;                            {record last char}
      ChIdx:=ChIdx+1;                           {Increment index}
    END;
    Result.Add(Line);                           {Add de-tokenized line to results}
  END;
END;

{*****************************************************************************}
{**                                                                         **}
{** Retokenize, retokenizes a whole program in the specified dialect, will  **}
{**    halt program with an error, if an invalid token encounterd, that     **}
{**    has no equivelent in the destination dialect, e.g. hi-res commands   **}
{**    in MC10 basic.                                                       **}
{**                                                                         **}
{*****************************************************************************}


FUNCTION TBasicProg.Retokenize(Dialect         : CHAR;
                               IgnoreInvalid   : BOOLEAN) : BOOLEAN;

VAR     Idx     : INTEGER;      { Line Index }
        BasLine : TBasicLine;   { Temp var for the line being processed }
        Invalid : INTEGER;      { Index of first invalid token }

BEGIN;
  Result:=TRUE;

  {Itterate through all basic lines in program}
  FOR Idx:=0 TO (Lines.Count-1) DO
  BEGIN;
    BasLine:=TBasicLine(Lines.Items[Idx]);      {Get a line to process}
    Invalid:=BasLine.ReTokenizeLine(BasDialect,Dialect);   {Re-tokenize the line}

    IF (Invalid>0) THEN
    BEGIN;
      WriteLn(Format('Error, token %2.2X has no equivelent in destination basic dialect',
                     [BasLine.Line[Invalid]]));
      HALT(0);
    END;
  END;
END;

{*****************************************************************************}
{**                                                                         **}
{** ToBinImage, Converts the current program to a binary image suitable for **}
{**    writing to a cas file for loading.                                   **}
{**                                                                         **}
{*****************************************************************************}

FUNCTION TBasicProg.ToBinImage : TRamothMemoryStream;

CONST   BaseAddr                = $1E01;                { Base address }
        Terminator : BYTE       = $00;

VAR     LineIdx         : INTEGER;
        Line            : TBasicLine;
        LineAddr        : WORD;
        
BEGIN;
  Result:=TRamothMemoryStream.Create;

  LineAddr:=BaseAddr;
  {Itterate through all the lines in the program}
  FOR LineIdx:=0 TO (Lines.Count-1) DO
  BEGIN;
    Line:=TBasicLine(Lines.Items[LineIdx]);             {Get the line}
    LineAddr:=WORD(LineAddr+GetLineLen(LineIdx));       {Work out addr of next line}
    
    WITH Result DO
    BEGIN;
      WriteMotorolaWord(LineAddr);                      {Write address of next line}
      WriteMotorolaWord(Line.LineNo);                   {Write line number}
      WriteFixedLenString(Line.Line,Length(Line.Line)); {Write basic code}
      Write(Terminator,SizeOf(Terminator));
    END;
  END;
  Result.WriteMotorolaWord($0000);                      {Write program terminator}
  Result.Position:=0;
END;

{*****************************************************************************}
{**                                                                         **}
{** GetLineLen, get the length of the specified line of the basic program   **}
{**     The returned length *INCLUDES* the 2 bytes for the line number and  **}
{**     the $00 byte line terminator. If supplied with an invalid line no   **}
{**     we return a length of zero.                                         **}
{**                                                                         **}
{*****************************************************************************}

FUNCTION TBasicProg.GetLineLen(LineNo  : INTEGER) : WORD;

VAR     Line            : TBasicLine;

BEGIN;
  {Validate line no}
  IF ((LineNo>=0) AND (LineNo<Lines.Count)) THEN
  BEGIN;
    Line:=TBasicLine(Lines.Items[LineNo]);
    Result:=Length(Line.Line)+3;                { 2 bytes for line no, 1 for $00 terminator }
  END
  ELSE
    Result:=0;
END;

end.

