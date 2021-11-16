unit DragonCoCoCASDefsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

CONST	SyncByte	= $55; 	{Sync byte in cassete files}
	BlockBegin	= $3C;	{Begining of block marker}

        {Block Types}
        BtFileName	= $00;	{File name block}
        BtData		= $01;	{Data block}
        BtEOF		= $FF;	{End of file block}

        FNameBlockLen   = 15;   {15 bytes in header block}

        DefBlockSize    = 250;  {Default block size}

        {File Types, as stored in filename block}
        FtBasic		= $00;	{Basic program}
        FtDataFile	= $01;	{Data file}
        FtMachineCode	= $02;	{Machine code program}
        FtBinary	= $03;	{Binary file}
        FtDream         = $88;  {Dream Assembler source file}
        FtHeaderless    = $FF;  {Headerless}

        {Ascii/Binary flag from filename block}
        AsAscii		= $FF;	{ASCII file}
        AsBinary	= $00;	{Binary file (tokenised basic)}

        {Gap Flag from filename block}
        GfUngapped	= $00;	{No gaps}
        GfGapped	= $FF;	{Gaps between blocks}



implementation

end.

