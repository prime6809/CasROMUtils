unit Utils;

{*************************************************************************}
{**                                                                     **}
{** Misc utility functions, P.Harvey-Smith, 2006-10-28.                 **}
{**                                                                     **}
{**                                                                     **}
{**                                                                     **}
{**                                                                     **}
{**                                                                     **}
{**                                                                     **}
{*************************************************************************}


interface

uses SysUtils;

Function ToHexWord(    Line     : STRING;
                   VAR HexWord  : WORD) : INTEGER; overload;

Function ToHexWord(    Line     : STRING;
                       Default  : WORD;
                   VAR HexWord  : WORD) : INTEGER; overload;


Function StripChars(Line        : STRING;
                    ToStrip     : STRING) : STRING;

Function StrToIntDef(Line       : STRING;
                     DefVal     : INTEGER) : INTEGER;

implementation

Function ToHexWord(    Line     : STRING;
                   VAR HexWord  : WORD) : INTEGER;


BEGIN;
  Result:=ToHexWord(Line,0,HexWord);
END;

Function ToHexWord(    Line     : STRING;
                       Default  : WORD;
                   VAR HexWord  : WORD) : INTEGER; overload;

BEGIN;
  Line:=Trim(Line);

  HexWord:=Default;             { Default, if error }

  IF (Length(Line)>0) THEN
  BEGIN
    IF (Line[1]<>'$') THEN      { Make sure hex marker at start }
      Line:='$'+Line;

    VAL(Line,HexWord,Result);   { Do conversion and set error }
  END
  ELSE
    Result:=-1;                 { Nothing in line, error }
END;


Function StripChars(Line        : STRING;
                    ToStrip     : STRING) : STRING;

VAR     LineIdx         : INTEGER;      { Index for Line }
        StripIdx        : INTEGER;      { Index for strip chars }
        Found           : BOOLEAN;      { Did we find a strip char }

BEGIN;
  Result:='';                           { Start with empty string }
  FOR LineIdx:=1 TO Length(Line) DO     { Check each char }
  BEGIN;
    Found:=FALSE;
    FOR StripIdx:=1 TO Length(ToStrip) DO { Check each strip char }
    BEGIN
      IF (Line[LineIdx]=ToStrip[StripIdx]) THEN
        Found:=TRUE;                            { Flag found }
    END;
    IF (NOT Found) THEN                   { If not strip char, move to result }
      Result:=Result+Line[LineIdx];
  END;
END;

Function StrToIntDef(Line       : STRING;
                     DefVal     : INTEGER) : INTEGER;

VAR     Err     : INTEGER;      { Error flag }

BEGIN;
  VAL(Line,Result,Err);
  IF (Err<>0) THEN
    Result:=DefVal;
END;

end.

