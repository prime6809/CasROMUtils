unit RamothStringListUnit;

interface

USES Classes,SysUtils,StrUtils;

TYPE    PStrings	= ^TStrings;

	TRamothStringList = class(TStringList)
	PRIVATE
      FStrIndex : INTEGER;
    PUBLIC
      PROPERTY StrIndex	: INTEGER READ FStrIndex WRITE FStrIndex;
      FUNCTION AddFormat(FormatStr	: STRING;
         		         Params	    : ARRAY OF CONST) : INTEGER;

      PROCEDURE First;
      FUNCTION Current(DoTrim   : BOOLEAN = FALSE) : STRING;
      FUNCTION Next(DoTrim   : BOOLEAN = FALSE) : STRING;
      FUNCTION NextLine : INTEGER;
      FUNCTION Eof : BOOLEAN;
      PROCEDURE CopyNToList(Dest		: PStrings;
       			            NoToCopy	: INTEGER);
      PROCEDURE Skip(NoToSkip	: INTEGER);
      PROCEDURE AddMultiple(Params	: ARRAY OF STRING);
      PROCEDURE ClearAddMultiple(Params	: ARRAY OF STRING);
      PROCEDURE Split(ToSplit       : STRING;
                      RemoveQuote   : BOOLEAN = TRUE); overload;
      PROCEDURE Split(ToSplit       : STRING;
                      NewDelimiter  : CHAR;
                      RemoveQuote   : BOOLEAN = TRUE); overload;

      PROCEDURE ClearAndSplit(ToSplit       : STRING);
      PROCEDURE Dump;
	END;

implementation
FUNCTION TRamothStringList.AddFormat(FormatStr	: STRING;
          		             Params	: ARRAY OF CONST) : INTEGER;

BEGIN;
  Result:=Add(Format(FormatStr,Params));
END;

PROCEDURE TRamothStringList.First;

BEGIN;
  FStrIndex:=0;
END;

FUNCTION TRamothStringList.Current(DoTrim   : BOOLEAN = FALSE) : STRING;

BEGIN;
  Result:='';
  IF (FStrIndex<Count) THEN
    Result:=Strings[FStrIndex];

  IF (DoTrim) THEN
    Result:=Trim(Result);
END;

FUNCTION TRamothStringList.Next(DoTrim   : BOOLEAN = FALSE) : STRING;

BEGIN;
  Result:=Current(DoTrim);
  NextLine;
END;

FUNCTION TRamothStringList.NextLine : INTEGER;

BEGIN;
  IF (FStrIndex<Count) THEN
    FStrIndex:=FStrIndex+1;

  Result:=FStrIndex;
END;

FUNCTION TRamothStringList.Eof : BOOLEAN;

BEGIN;
  IF (FStrIndex>=Count) THEN
    Result:=TRUE
  ELSE
    Result:=FALSE;
END;

PROCEDURE TRamothStringList.CopyNToList(Dest		: PStrings;
  			                            NoToCopy	: INTEGER);

VAR	Idx	: INTEGER;

BEGIN;
  FOR Idx:=0 TO (NoToCopy-1) DO
    Dest^.Add(Next);
END;

PROCEDURE TRamothStringList.Skip(NoToSkip	: INTEGER);

BEGIN;
  FStrIndex:=FStrIndex+NoToSkip;
END;

PROCEDURE TRamothStringList.AddMultiple(Params	: ARRAY OF STRING);

VAR	StringNo	: INTEGER;

BEGIN;
  FOR StringNo:=0 TO High(Params) DO
    Add(Params[StringNo]);
END;

PROCEDURE TRamothStringList.ClearAddMultiple(Params	: ARRAY OF STRING);

BEGIN;
  Clear;
  AddMultiple(Params);
END;

{ Split a string into components using *ONLY* the specified delimiter    }
{ note this is not the same as setting DelimitedText, as doing so always }
{ also uses space, which is not what is required in most cases !         }

PROCEDURE TRamothStringList.Split(ToSplit       : STRING;
                                  RemoveQuote   : BOOLEAN = TRUE);

VAR     SrcIdx          : INTEGER;
        Dest            : STRING;
        InQuote         : BOOLEAN;
        CurrentChar     : CHAR;

BEGIN;
  Dest:='';
  InQuote:=FALSE;

  FOR SrcIdx:=1 TO Length(ToSplit) DO
  BEGIN;
    CurrentChar:=ToSplit[SrcIdx];

    IF (CurrentChar=QuoteChar) THEN
      InQuote:=NOT InQuote;

    IF ((CurrentChar=Delimiter) AND (NOT InQuote)) THEN
    BEGIN;
      Add(Dest);
      Dest:='';
    END;

    IF (((CurrentChar<>Delimiter) AND (CurrentChar<>QuoteChar)) OR
        ((CurrentChar=QuoteChar) AND NOT RemoveQuote)) THEN
      Dest:=Dest+CurrentChar;
  END;
  IF (Length(Dest)>0) THEN
    Add(Dest);
END;

PROCEDURE TRamothStringList.Split(ToSplit       : STRING;
                                  NewDelimiter  : CHAR;
                                  RemoveQuote   : BOOLEAN = TRUE);

BEGIN;
  Delimiter:=NewDelimiter;
  Split(ToSplit,RemoveQuote);
END;

PROCEDURE TRamothStringList.ClearAndSplit(ToSplit       : STRING);

BEGIN;
  Clear;
  Split(ToSplit);
END;

PROCEDURE TRamothStringList.Dump;

VAR Idx : INTEGER;

BEGIN;
  FOR Idx:=0 TO (Count-1) DO
    WriteLn(Format('string [%d] : %s',[Idx, Strings[Idx]]));
END;


end.
