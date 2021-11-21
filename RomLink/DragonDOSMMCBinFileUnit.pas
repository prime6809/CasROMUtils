unit DragonDOSMMCBinFileUnit;

{$mode objfpc}

interface

uses
  Classes, SysUtils, RamothMemoryStream;
{
; Offsets within file header, compatible with DragonDos/SuperDos
HdrID55			    EQU	    $00			            ; magic marker byte #1
HdrType			    EQU	    $01			            ; file type see filetypes below
HdrLoad			    EQU	    $02			            ; Load address of file
HdrLen			    EQU	    $04			            ; length of file
HdrExec			    EQU	    $06			            ; entry address of file
HdrIDAA			    EQU	    $08			            ; magic marker byte #2

FileHeadLen		    EQU	    $09

; File types

FTypeBas		    EQU	    $01			            ; Basic files
FTypeBin		    EQU	    $02			            ; Binary file types

; Marker bytes
MarkerHeadStart     EQU     $55
MarkerHeadEnd       EQU     $AA
}
        { Header Offsets }
CONST   OfsHeaderStart  = $00;
        OfsHeaderType   = $01;
        OfsHeaderLoad   = $02;
        OfsHeaderLen    = $04;
        OfsHeaderExec   = $06;
        OfsHeaderEnd    = $08;

        { Header marker bytes }
        MarkHeaderStart = $55;
        MarkHeaderEnd   = $AA;

        { Types for DragonDos file types }
        DDFTypeBas      = $01;
        DDFTypeBin      = $02;

        HeaderLength    = $09;

TYPE    TDragonDOSMMCBinFile = class(TObject)
        PROTECTED
          FValid    : BOOLEAN;      { Is header valid? }
          FHStart   : BYTE;         { Header start byte }
          FFileType : BYTE;         { File type }
          FLoadAddr : WORD;         { Load address }
          FFLen     : WORD;         { File length }
          FExecAddr : WORD;         { Exec Address }
          FHEnd     : BYTE;         { Header end byte }

          FFileData : TRamothMemoryStream;
          FFileName : STRING;
        PUBLIC
          PROPERTY FileType : BYTE READ FFileType WRITE FFileType;
          PROPERTY LoadAddr : WORD READ FLoadAddr WRITE FLoadAddr;
          PROPERTY FLen     : WORD READ FFLen WRITE FFLen;
          PROPERTY ExecAddr : WORD READ FExecAddr WRITE FExecAddr;
          PROPERTY Valid    : BOOLEAN READ FValid;

          PROCEDURE ReadFromFile(LoadFileName   : STRING);
          PROCEDURE BeginFile;
          PROCEDURE GetFileData(Data      : TRamothMemoryStream);
          Constructor Create;
          Destructor Destroy; override;
        END;

implementation

Constructor TDragonDOSMMCBinFile.Create;

BEGIN;
  FValid:=FALSE;
  FFileData:=TRamothMemoryStream.Create;
END;

Destructor TDragonDOSMMCBinFile.Destroy;
BEGIN;
  FFileData.Free;
  INHERITED Destroy;
END;

{ Open file and read header, leaves position set to first byte of file data }
PROCEDURE TDragonDOSMMCBinFile.ReadFromFile(LoadFileName   : STRING);
BEGIN;
  FValid:=FALSE;
  IF (FileExists(LoadFileName)) THEN
  BEGIN;
    FFileName:=LoadFileName;
    FFileData.LoadFromFile(FFileName);

    {Load and Validate header}
    IF (FFileData.Size > HeaderLength) THEN
    BEGIN;
      FHStart:=FFileData.ReadByte;
      FFileType:=FFileData.ReadByte;
      FLoadAddr:=FFileData.ReadMotorolaWord;
      FFLen:=FFileData.ReadMotorolaWord;
      FExecAddr:=FFileData.ReadMotorolaWord;
      FHEnd:=FFileData.ReadByte;

      FValid:=((FHStart = MarkHeaderStart) AND (FHEnd = MarkHeaderEnd));
    END;
  END;
END;

PROCEDURE TDragonDOSMMCBinFile.BeginFile;

BEGIN;
  IF (FFileData.Size > HeaderLength) THEN
    FFileData.Seek(HeaderLength,soFromBeginning);
END;

PROCEDURE TDragonDOSMMCBinFile.GetFileData(Data      : TRamothMemoryStream);

BEGIN;
  IF (FValid) THEN
  BEGIN;
    BeginFile;
    Data.CopyBytesFromStream(Self.FFileData,FLen);
  END;
END;

end.

