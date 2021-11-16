unit MemMapUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

TYPE
  TMemMap = Class(TObject)
  PROTECTED
    Memory  : ARRAY OF BOOLEAN;
    Size    : INTEGER;
  PUBLIC
    CONSTRUCTOR Create(MemorySize   : INTEGER);
    PROCEDURE SetToVal(Start    : INTEGER;
                       Count    : INTEGER;
                       State    : BOOLEAN);
    PROCEDURE SetVal(Start  : INTEGER;
                     Count  : INTEGER);
    PROCEDURE ClearVal(Start  : INTEGER;
                       Count  : INTEGER);
    FUNCTION GetMap : STRING;

  END;

implementation

CONSTRUCTOR TMemMap.Create(MemorySize   : INTEGER);

BEGIN;
  SetLength(Memory,MemorySize);
  Size:=MemorySize;
END;

PROCEDURE TMemMap.SetToVal(Start    : INTEGER;
                           Count    : INTEGER;
                           State    : BOOLEAN);

VAR Idx : INTEGER;

BEGIN;
  FOR Idx:=Start TO (Start+Count)-1 DO
    Memory[Idx]:=State;
END;

PROCEDURE TMemMap.SetVal(Start  : INTEGER;
                         Count  : INTEGER);
BEGIN;
  SetToVal(Start,Count,TRUE);
END;
PROCEDURE TMemMap.ClearVal(Start  : INTEGER;
                           Count  : INTEGER);
BEGIN;
  SetToVal(Start,Count,FALSE);
END;

FUNCTION TMemMap.GetMap : STRING;

CONST Invalid   = -1;

VAR First   : INTEGER;
    Last    : INTEGER;
    Current : INTEGER;
    Map     : TStringList;

BEGIN;
  First:=Invalid;
  Last:=Invalid;
  Map:=TStringList.Create;
  TRY
    FOR Current:=0 TO Size DO
    BEGIN;
      IF (Memory[Current]) THEN
      BEGIN;
        IF (First=Invalid) THEN
        BEGIN;
          First:=Current;
          Last:=Current;
        END
        ELSE IF (Last<>Invalid) THEN
          Last:=Current;
      END
      ELSE
        IF (Last<>Invalid) THEN
        BEGIN;
          Map.Add(Format('Memory block start : $%4.4X, Memory block end $%4.4X',[First,Last]));
          First:=Invalid;
          Last:=Invalid;
        END;
    END;
  FINALLY
    Result:=Map.Text;
    Map.Free;
  END;
END;

end.

