unit RamothCustomApplicationUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustApp;

TYPE
  TRamothCustomApplication = class(TCustomApplication)
  PROTECTED
    Errors          : TStringList;

    PROCEDURE LogError(ErrorMsg : STRING); overload;
    PROCEDURE LogError(ErrorMsg : STRING;
                       EParams  : ARRAY OF CONST); overload;

    PROCEDURE CheckErrorTerminate;
  PUBLIC
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  END;

implementation

PROCEDURE TRamothCustomApplication.LogError(ErrorMsg : STRING);

BEGIN;
  IF (Trim(ErrorMsg)<>'') THEN
    Errors.Add(ErrorMsg);
END;

PROCEDURE TRamothCustomApplication.LogError(ErrorMsg : STRING;
                                            EParams  : ARRAY OF CONST);

BEGIN;
  LogError(Format(ErrorMsg,EParams));
END;

PROCEDURE TRamothCustomApplication.CheckErrorTerminate;

BEGIN;
  IF (Errors.Count<>0) THEN
  BEGIN;
    ShowException(Exception.Create(Errors.Text));
  END;
END;

constructor TRamothCustomApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  Errors:=TStringList.Create;
end;

destructor TRamothCustomApplication.Destroy;
begin
  Errors.Free;
  inherited Destroy;
end;

end.

