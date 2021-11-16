unit RLECompressUnit;
{***************************************************************************}
{**                                                                       **}
{** Simple compression of a stream using 8 bit run length encoding.       **}
{**                                                                       **}
{** First version 2017-08-10.                                             **}
{**                                                                       **}
{**                                                                       **}
{***************************************************************************}

{$mode objfpc}

interface

uses
  Classes, SysUtils, RamothMemoryStream;

CONST
  MinRunLength  = $04;  { Runs shorter than this are more efficient as raw bytes }
  MaxRunLength  = $FF;  { Run length counter maximum as it's a byte }


TYPE
  {User top hold the frequency statistics for each character in the stream}
  TCharStat = RECORD
    Count   : INTEGER;      { Count of character }
    MaxRun  : INTEGER;      { Maximum run of this character }
  END;

  {Object used to collect frequency statistics of the stream      }
  {This is used to choose the run length marker to be the least   }
  {frequent character, decresing the chances of needing to increse}
  {the length of the stream due to having to escape the RL marker.}
  TRLEStats = class(TObject)
  PROTECTED
    FMaxRunChar     : BYTE;
    FMaxRunValue    : INTEGER;
    FMostFreqent    : BYTE;
    FLeastFrequent  : BYTE;

    PROCEDURE CalcFrequencies;
    PROCEDURE AddCount(CharNo   : BYTE);
    PROCEDURE SetMaxRun(CharNo  : BYTE;
                        Run     : INTEGER);
  PUBLIC
    CharStats   : ARRAY[0..255] OF TCharStat;

    PROPERTY MaxRunChar     : BYTE READ FMaxRunChar;
    PROPERTY MaxRunValue    : INTEGER READ FMaxRunValue;
    PROPERTY MostFreqent    : BYTE READ FMostFreqent;
    PROPERTY LeastFrequent  : BYTE READ FLeastFrequent;

    PROCEDURE ClearStats;
    PROCEDURE GetStats(Block    : TRamothMemoryStream);

    CONSTRUCTOR Create;
  END;

  {Run length compressor class, before compressing the stream is analized to }
  {determine the optimum run length marker to use so as to minimize the need }
  {to escape the marker character}
  TRLECompressor = class(TObject)
  PROTECTED
    FStats          : TRLEStats;
    FOriginalSize   : Int64;
    FRunMarker      : BYTE;

    FUNCTION GetCompressionRatio : REAL;
    PROCEDURE OutputRun(RunChar : BYTE;
                        Length  : BYTE);
  PUBLIC
    OutputMem   : TRamothMemoryStream;

    PROPERTY Stats      : TRLEStats READ FStats;
    PROPERTY Ratio      : REAL READ GetCompressionRatio;
    PROPERTY RunMarker  : BYTE READ FRunMarker;

    CONSTRUCTOR Create;
    DESTRUCTOR Destroy; override;
    PROCEDURE Compress(Source   : TRamothMemoryStream);
    PROCEDURE Decompress(Source : TRamothMemoryStream);
  END;

implementation

CONSTRUCTOR TRLEStats.Create;

BEGIN;
  INHERITED Create;
  ClearStats;
END;


PROCEDURE TRLEStats.ClearStats;

VAR Idx : INTEGER;

BEGIN;
  FOR Idx:=0 TO 255 DO
  BEGIN;
    CharStats[Idx].Count:=0;
    CharStats[Idx].MaxRun:=0;
  END;
END;

PROCEDURE TRLEStats.GetStats(Block    : TRamothMemoryStream);

VAR Last        : BYTE = 0;
    Current     : BYTE = 0;
//    InRun       : BOOLEAN;
    RunLength   : INTEGER = 0;

BEGIN;
  ClearStats;
  IF (Block.Size > 2) THEN
  BEGIN;
    Block.Seek(0,soFromBeginning);
//    InRun:=FALSE;

    WITH Block DO
    BEGIN;
      Read(Last,1);
      AddCount(Last);
      SetMaxRun(Last,1);
      RunLength:=1;

      REPEAT;
        Read(Current,1);
        AddCount(Current);

        IF (Current = Last) THEN
        BEGIN;
//          InRun:=TRUE;
          RunLength:=RunLength+1;
          SetMaxRun(Current,RunLength);
        END
        ELSE
        BEGIN
//          InRun:=FALSE;
          RunLength:=1;
        END;

        Last:=Current;
      UNTIL (EOS);
    END;
  END;
  CalcFrequencies;
END;

PROCEDURE TRLEStats.AddCount(CharNo   : BYTE);

BEGIN;
  CharStats[CharNo].Count:=CharStats[CharNo].Count+1;
END;

PROCEDURE TRLEStats.SetMaxRun(CharNo  : BYTE;
                              Run     : INTEGER);
BEGIN;
  IF (Run > CharStats[CharNo].MaxRun) THEN
    CharStats[CharNo].MaxRun:=Run;
END;

PROCEDURE TRLEStats.CalcFrequencies;

VAR Idx     : INTEGER;

BEGIN;
  FMaxRunValue:=CharStats[0].MaxRun;
  FMaxRunChar:=0;
  FMostFreqent:=0;
  FLeastFrequent:=0;

  FOR Idx:=0 TO 255 DO
  BEGIN;
    IF (CharStats[Idx].MaxRun > FMaxRunValue) THEN
    BEGIN;
      FMaxRunValue:=CharStats[Idx].MaxRun;
      FMaxRunChar:=Idx;
    END;

    IF (CharStats[Idx].Count > CharStats[FMostFreqent].Count) THEN
      FMostFreqent:=Idx;

    IF (CharStats[Idx].Count < CharStats[FLeastFrequent].Count) THEN
      FLeastFrequent:=Idx;
  END;
END;

CONSTRUCTOR TRLECompressor.Create;

BEGIN;
  INHERITED Create;
  FStats:=TRLEStats.Create;
  OutputMem:=TRamothMemoryStream.Create;
END;

DESTRUCTOR TRLECompressor.Destroy;

BEGIN;
  FStats.Free;
  OutputMem.Free;
  INHERITED Destroy;
END;

FUNCTION TRLECompressor.GetCompressionRatio : REAL;

BEGIN;
  IF (FOriginalSize > 0) THEN
    Result:=(OutputMem.Size / FOriginalSize) * 100
  ELSE
    Result:=100;
END;

PROCEDURE TRLECompressor.OutputRun(RunChar : BYTE;
                                   Length  : BYTE);

VAR Count : INTEGER;

BEGIN;
  WITH OutputMem DO
  BEGIN;
    IF ((Length > MinRunLength) OR (RunChar = FRunMarker)) THEN
    BEGIN;
      WriteByte(FRunMarker);
      WriteByte(RunChar);
      WriteByte(Length);
    END
    ELSE
      FOR Count:=1 TO Length DO
        WriteByte(RunChar);
  END;
END;

PROCEDURE TRLECompressor.Compress(Source   : TRamothMemoryStream);

VAR Last        : BYTE = 0;
    Current     : BYTE = 0;
    RunLength   : BYTE = 0;

BEGIN;
  FOriginalSize:=Source.Size;
  FStats.GetStats(Source);
  FRunMarker:=FStats.LeastFrequent;

  Source.Seek(0,soFromBeginning);
  OutputMem.Clear;

  WITH Source DO
  BEGIN;
    Read(Last,1);
    RunLength:=1;

    REPEAT;
      Read(Current,1);

      IF (Current = Last) THEN
      BEGIN;
        IF ((RunLength = MaxRunLength) OR EOS) THEN
        BEGIN;
          OutputRun(Current,RunLength);
          RunLength:=0;
        END;
        RunLength:=RunLength+1;
      END
      ELSE
      BEGIN
        IF ((RunLength > 1) OR (Last = FRunMarker)) THEN
        BEGIN
          OutputRun(Last,RunLength);
          RunLength:=1;
        END
        ELSE
          OutputMem.WriteByte(Last);
      END;

      Last:=Current;
    UNTIL (EOS);

    IF (Current = Last) THEN
      OutputRun(Current,RunLength)
    ELSE
      OutputMem.WriteByte(Current);
  END;
END;

PROCEDURE TRLECompressor.Decompress(Source : TRamothMemoryStream);

BEGIN;
END;


end.

