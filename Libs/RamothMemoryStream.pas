unit RamothMemoryStream;

{TRamothMemoryStream, and extended TMemoryStream, with methods to read and     }
{write, some basic types directly}

interface

uses
   SysUtils, Variants, Classes;


TYPE TRamothMemoryStream =	class (TMemoryStream)
        PRIVATE
	 PUBLIC
          CONSTRUCTOR Create;
          PROCEDURE WriteByte(ToWrite   : BYTE);
          FUNCTION ReadByte : BYTE;
          PROCEDURE WriteInteger(ToWrite	: INTEGER);
          FUNCTION ReadInteger : INTEGER;
          PROCEDURE WriteDWord(ToWrite	: DWORD);
          FUNCTION ReadDWord : DWord;

          PROCEDURE WriteString(ToWrite	: STRING);
          FUNCTION ReadString : STRING;
          PROCEDURE WriteFixedLenString(ToWrite	: STRING;
               				Count	: INTEGER);
          FUNCTION ReadFixedLenString(Count	: INTEGER) : STRING;
          FUNCTION ReadASCIIZ : STRING;
          PROCEDURE WriteRawString(ToWrite	: STRING);
          PROCEDURE CopyBytesFromStream(From	: TStream;
              				Count	: Int64); overload;
          PROCEDURE CopyBytesFromStream(From	: TStream;
                                        Start   : Int64;
               				Count	: Int64); overload;
          PROCEDURE CopyBytesFromStreamWithFilter(From		: TStream;
              		            		  Count		: Int64;
                                    		  Filter	: STRING);

          FUNCTION ReadMotorolaWord : WORD;
          PROCEDURE WriteMotorolaWord(ToWrite	: WORD);
          PROCEDURE WriteRepetedChars(ToWrite	: CHAR;
           			      Count	: INTEGER);
          PROCEDURE WriteRepetedBytes(ToWrite	: Byte;
                                      Count	    : INTEGER);
          PROCEDURE SaveBlockToFile(FileName	: STRING;
              			            StartPos	: Int64;
                                    Count	    : Int64);
	      PROCEDURE LoadBlockFromFile(FileName	: STRING;
                   	                  StartPos	: Int64;
                                      Count	    : Int64);
          FUNCTION PadToMultiple(Multiple      : Int64) : Int64;
          PROCEDURE Crop(StartPos	: Int64;
                         Count	    : Int64);
          FUNCTION EOS : BOOLEAN;
          FUNCTION HexDump(Pos	: Int64) : TStringList; overload;
          FUNCTION HexDump(Pos	: Int64;
                           Len  : iNT64) : TStringList; overload;
        END;

implementation


CONSTRUCTOR TRamothMemoryStream.Create;

BEGIN;
  INHERITED Create;
END;

PROCEDURE TRamothMemoryStream.WriteByte(ToWrite   : BYTE);
BEGIN;
  Write(ToWrite,Sizeof(ToWrite));
END;

FUNCTION TRamothMemoryStream.ReadByte : BYTE;

BEGIN;
  Read(Result,SizeOf(Result));
END;

PROCEDURE TRamothMemoryStream.WriteInteger(ToWrite	: INTEGER);

BEGIN;
  Write(ToWrite,Sizeof(ToWrite));
END;

FUNCTION TRamothMemoryStream.ReadInteger : INTEGER;
BEGIN;
  Read(Result,SizeOf(Result));
END;

PROCEDURE TRamothMemoryStream.WriteDWord(ToWrite	: DWORD);

BEGIN;
  Write(ToWrite,Sizeof(ToWrite));
END;

FUNCTION TRamothMemoryStream.ReadDWord : DWord;

BEGIN;
  Read(Result,SizeOf(Result));
END;

PROCEDURE TRamothMemoryStream.WriteString(ToWrite	: STRING);

BEGIN;
  WriteInteger(Length(ToWrite));
  Write(ToWrite[1],Length(ToWrite));
END;

FUNCTION TRamothMemoryStream.ReadString : STRING;

VAR	StringLen	: INTEGER;
        TempStr		: STRING;

BEGIN;
  StringLen:=ReadInteger;
  SetLength(TempStr,StringLen);
  Read(TempStr[1],StringLen);
  Result:=TempStr;
END;

PROCEDURE TRamothMemoryStream.WriteFixedLenString(ToWrite	: STRING;
						  Count		: INTEGER);

BEGIN;
  Write(ToWrite[1],Count);
END;

FUNCTION TRamothMemoryStream.ReadFixedLenString(Count	: INTEGER) : STRING;

BEGIN;
  SetLength(Result,Count);
  Read(Result[1],Count);
END;

FUNCTION TRamothMemoryStream.ReadASCIIZ : STRING;

VAR	Ch	: CHAR;

BEGIN;
  Result:='';
  IF (Position<Size) THEN
  BEGIN;
    REPEAT;
      Read(Ch,SizeOf(Ch));
      IF (Ch<>#0) THEN
        Result:=Result+Ch;
    UNTIL ((Ch=#0) OR (Position=Size));
  END;
END;

PROCEDURE TRamothMemoryStream.WriteRawString(ToWrite	: STRING);

BEGIN;
  WriteFixedLenString(ToWrite,Length(ToWrite));
END;

PROCEDURE TRamothMemoryStream.CopyBytesFromStream(From	: TStream;
                                  		  Count	: Int64);

VAR	Idx	: INTEGER;
      	CByte	: BYTE;

BEGIN;
  FOR Idx:=1 TO Count DO
  BEGIN;
    From.Read(CByte,1);
    Self.Write(CByte,1);
  END;
END;
PROCEDURE TRamothMemoryStream.CopyBytesFromStream(From  : TStream;
                                                  Start : Int64;
                                  		  Count	: Int64);

BEGIN;
  From.Position:=Start;
  CopyBytesFromStream(From,Count);
END;


PROCEDURE TRamothMemoryStream.CopyBytesFromStreamWithFilter(From	: TStream;
                                  		            Count	: Int64;
                                                            Filter	: STRING);

VAR	Idx	: INTEGER;
      	CByte	: CHAR;

BEGIN;
  FOR Idx:=1 TO Count DO
  BEGIN;
    From.Read(CByte,1);
    IF (Pos(CByte,Filter)=0) THEN
      Self.Write(CByte,1);
  END;
END;


FUNCTION TRamothMemoryStream.ReadMotorolaWord : WORD;

VAR	Temp	: WORD;

BEGIN;
  Read(Temp,Sizeof(Temp));
  Result:=Swap(Temp);
END;

PROCEDURE TRamothMemoryStream.WriteMotorolaWord(ToWrite	: WORD);
BEGIN;
  ToWrite:=Swap(ToWrite);
  Write(ToWrite,Sizeof(ToWrite));
END;

PROCEDURE TRamothMemoryStream.WriteRepetedChars(ToWrite	: CHAR;
                                  		        Count	: INTEGER);

VAR	Idx	: INTEGER;

BEGIN;
  FOR Idx:=1 TO Count DO
    Write(ToWrite,SizeOf(ToWrite));
END;

PROCEDURE TRamothMemoryStream.WriteRepetedBytes(ToWrite	: Byte;
                                  		        Count	: INTEGER);

VAR	Idx	: INTEGER;

BEGIN;
  FOR Idx:=1 TO Count DO
    Write(ToWrite,SizeOf(ToWrite));
END;

PROCEDURE TRamothMemoryStream.SaveBlockToFile(FileName	: STRING;
                                  	      StartPos	: Int64;
                                              Count	: Int64);

VAR	OutFile	: TFileStream;
	Buff	: Pointer;

BEGIN;
  OutFile:=TFileStream.Create(FileName,fmCreate);
  Self.Seek(StartPos,soFromBeginning);
  GetMem(Buff,Count+1);
  Self.Read(Buff^,Count);
  OutFile.Write(Buff^,Count);
  FreeMem(Buff);
  OutFile.Free;
END;

PROCEDURE TRamothMemoryStream.LoadBlockFromFile(FileName	: STRING;
                                  	        StartPos	: Int64;
                                                Count		: Int64);

VAR	InFile	: TFileStream;
        Buff	: Pointer;
        FileLen	: Int64;
        ToWrite	: Int64;

BEGIN;
  InFile:=TFileStream.Create(FileName,fmOpenRead);
  FileLen:=InFile.Size;
  GetMem(Buff,FileLen+1);
  InFile.Read(Buff^,FileLen);
  InFile.Free;

  IF ((Count>FileLen) OR (Count<0)) THEN
    ToWrite:=FileLen
  ELSE
    ToWrite:=Count;

  Self.Position:=StartPos;
  Self.Write(Buff^,ToWrite);
  FreeMem(Buff);
END;

FUNCTION TRamothMemoryStream.EOS : BOOLEAN;

BEGIN;
  IF (Position<Size) THEN
    Result:=FALSE
  ELSE
    Result:=TRUE;
END;

FUNCTION TRamothMemoryStream.PadToMultiple(Multiple      : Int64) : Int64;

VAR     Blocks  : Int64;
        Rem     : Int64;
        NoPad   : Int64;

BEGIN;
  Blocks:=Self.Size DIV Multiple;
  Rem:=Self.Size MOD Multiple;
  NoPad:=0;
  IF(Rem<>0) THEN
  BEGIN;
    NoPad:=Int64(((Blocks+1)*Multiple)-Self.Size);
    Self.Position:=Self.Size;
    Self.WriteRepetedChars(CHR($FF),NoPad);
  END;

  Result:=NoPad;
END;

PROCEDURE TRamothMemoryStream.Crop(StartPos	: Int64;
                                   Count	: Int64);

VAR     Buffer  : TRamothMemoryStream;

BEGIN;
  Buffer:=TRamothMemoryStream.Create;
  TRY
    IF (StartPos<Size) THEN
    BEGIN;
      Buffer.CopyBytesFromStream(Self,StartPos,Count);
      Self.Clear;
      Self.CopyBytesFromStream(Buffer,0,Count);
    END;
  FINALLY
    Buffer.Free;
  END;
END;

FUNCTION TRamothMemoryStream.HexDump(Pos	: Int64) : TStringList;

BEGIN;
  Result:=HexDump(Pos,(Size-Pos));
END;


FUNCTION TRamothMemoryStream.HexDump(Pos	: Int64;
                                     Len    : iNT64) : TStringList;

CONST	LineLen	= 16;

VAR    	Line	    : INTEGER;
        LByte	    : INTEGER;
	    NoLines	    : Int64;
        {Remain	    : INTEGER;        }
    	OutBuff	    : STRING;
        OutBuff2    : STRING;
        Current	    : Byte;
        StartPos    : Int64;
        Last        : Int64;

BEGIN;
  Result:=TStringList.Create;
  StartPos:=Position;
  Last:=StartPos+Len;

  IF ((Size>0) AND (Pos<Last)) THEN
  BEGIN;
    NoLines:=Len DIV LineLen;
    Result.Add('');
    Position:=Pos;
    FOR Line:=0 TO NoLines DO
    BEGIN;
      OutBuff:=IntToHex((Line*LineLen),4)+' ';
      OutBuff2:='';
      FOR LByte:=(Line*LineLen) TO ((Line+1)*LineLen)-1 DO
      BEGIN;
        Current:=Self.ReadByte;
        OutBuff:=OutBuff+Format('%2.2X ',[Current]);
        IF ((Current>31) AND (Current<127)) THEN
          OutBuff2:=OutBuff2+CHR(Current)
        ELSE
          OutBuff2:=OutBuff2+'-';
      END;
      Result.Add(OutBuff+OutBuff2);
    END;
    Position:=StartPos;
  END;
END;

end.
