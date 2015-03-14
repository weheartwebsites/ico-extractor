///////////////////////////////////////////////////////////////////////////
//
//  Written by:  Igor Savkic (igors233@gmail.com)
//
//  Last change: 08/03/2015
//
//  Desc:        General purpose icon extraction unit, handles executables
//               and file extensions: C:\MyApp.exe; *.txt
//
////////////////////////////////////////////////////////////////////////////

unit ExtractHelper;

interface

 function ExtractLargestIconFrom(const ASourceFile, AExtractedIconPath: WideString): Boolean;
 function WideChangeFileExt(const AFileName, AExt: WideString): WideString;

implementation

{$IF CompilerVersion > 18.5} // From D2009 PngImage was included in VCL, for earlier versions you must add it yourself to project
  {$DEFINE PNG_SUPPORT}
{$IFEND}

uses
  Windows, SysUtils, Classes, Math, ShellApi, CommCtrl, ActiveX,
  Graphics {$IFDEF PNG_SUPPORT}, PngImage, ImgList{$ENDIF};

// Declaration of used functions and constants, older Delphi versions has wrong declaration for ShGetFileInfoW and several missing constants

const
  shell32   = 'shell32.dll';
  kernel32  = 'kernel32.dll';
  shlwapi32 = 'shlwapi.dll';

  VER_GREATER_EQUAL = 3;

  VER_MINORVERSION  = $0000001;
  VER_MAJORVERSION  = $0000002;

  SHIL_EXTRALARGE   = 2;
  SHIL_JUMBO        = 4;

  ASSOCSTR_EXECUTABLE = 2;
  ASSOCF_NOTRUNCATE   = $00000020;

  IID_IImageList: TGUID = '{46EB5926-582E-4017-9FDF-E8998DAA0950}';

{$IF CompilerVersion <= 15}
type
  UInt64 = Int64;
{$IFEND}

{$IFDEF PNG_SUPPORT}
  {$IF CompilerVersion < 20}
  type
    // Delphi prior to 2009 doesn't have accompanying png lib, so some external must be used (for example pngdelphi.sourceforge.net)
    // Note that D7 has bug in Icon handling and it will trim colors info 
    TPngImage = TPngObject;
  {$IFEND}
{$ENDIF}

function SHGetFileInfoW(pszPath: PWideChar; dwFileAttributes: DWORD;  var psfi: TSHFileInfoW; cbFileInfo, uFlags: UINT): DWORD; stdcall; external shell32 name 'SHGetFileInfoW';

function VerifyVersionInfo(var lpVersionInformation: TOSVersionInfo; dwTypeMask: DWORD; dwlConditionMask: UInt64): BOOL; stdcall; external kernel32 name 'VerifyVersionInfoA';
function VerSetConditionMask(ConditionMask: Uint64; TypeMask: DWORD; Condition: BYTE): Uint64; stdcall; external kernel32 name 'VerSetConditionMask';

function AssocQueryStringW(flags, str: DWORD; pszAssoc, pszExtra: LPCWSTR; pszOut: LPWSTR; pcchOut: PDWORD): HRESULT; stdcall; external shlwapi32 name 'AssocQueryStringW';

type
  PResHelper = ^TResHelper;
  TResHelper = record
    Index: Integer;
    ResName: string;
  end;

  // https://msdn.microsoft.com/en-us/library/ms997538.aspx
  PGrpIconDIrEntry = ^TGrpIconDirEntry;
  TGrpIconDirEntry = packed record
    bWidth: Byte;          // Width, in pixels, of the image
    bHeight: Byte;         // Height, in pixels, of the image
    bColorCount: Byte;     // Number of colors in image (0 if >=8bpp
    bReserved: Byte;
    wPlanes: Word;         // Color Planes
    wBitCount: Word;       // Bits per pixel
    dwBytesInRes: DWORD;   // how many bytes in this resource?
    nId: Word;             // the ID
  end;

  PGrpIconDir = ^TGrpIconDir;
  TGrpIconDir = packed record
    idReserved: Word;   // Reserved (must be 0)
    idType: Word;       // Resource type (1 for icons)
    idCount: Word;      // How many images?
    idEntries: array [0..MaxInt div SizeOf(TGrpIconDirEntry) - 2] of TGrpIconDirEntry; // The entries for each image
  end;

  PIconDirEntry = ^TIconDirEntry;
  TIconDirEntry = packed record
    bWidth: Byte;          // Width, in pixels, of the image
    bHeight: Byte;         // Height, in pixels, of the image
    bColorCount: Byte;     // Number of colors in image (0 if >=8bpp)
    bReserved: Byte;       // Reserved ( must be 0)
    wPlanes: Word;         // Color Planes
    wBitCount: Word;       // Bits per pixel
    dwBytesInRes: DWord;   // How many bytes in this resource?
    dwImageOffset: DWord;  // Where in the file is this image?
  end;

function WideChangeFileExt(const AFileName, AExt: WideString): WideString;
var
  b: Integer;
begin
  b := Length(AFileName);

  while b >= 1 do
  begin
    if Char(AFileName[b]) in ['.', ':', '\'] then
      Break;

    Dec(b);
  end;

  if (b > 0) and (AFileName[b] = '.') then
    Result := Copy(AFileName, 1, b - 1) + AExt
  else
    Result := AFileName + AExt;
end;

function SaveToFileW(AStream: TCustomMemoryStream; const ADestFile: WideString): Integer; overload
var
  hFile: THandle;
  Bytes: DWORD;
begin
  DeleteFileW(PWideChar(ADestFile));
  Result := S_OK;
  hFile := CreateFileW(PWideChar(ADestFile), GENERIC_WRITE, FILE_SHARE_READ, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);

  if hFile <> INVALID_HANDLE_VALUE then
  begin
    WriteFile(hFile, AStream.Memory^, AStream.Size, Bytes, nil);
    CloseHandle(hFile);
    WriteLn('Icon extracted and saved');
  end
  else begin
    Result := GetLastError;
    WriteLn(SysErrorMessage(Result));
  end;
end;

function SaveToFileW(AIcon: TIcon; const ADestFile: WideString): Integer; overload;
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    AIcon.SaveToStream(ms);
    Result := SaveToFileW(ms, ADestFile);
  finally
    ms.Free;
  end;
end;

function SaveResIcoToFileW(AIcoData: TCustomMemoryStream; const ADestFile: WideString): Integer; overload;
var
  ms: TMemoryStream;
  IcoDir: TCursorOrIcon;
  IcoHdr: TIconDirEntry;
  bmpHdr: PBitmapInfoHeader;
begin
  ms := TMemoryStream.Create;
  try
    // Icon on disk has same format as on disk, first goes GrpHeader and then icons (in our case only one icon is stored)
    IcoDir.Reserved := 0;
    IcoDir.wType := 1; // Icon
    IcoDir.Count := 1;
    // Fill icon header fields so they contains correct valus for icon in file format (take values from bmpHeader)
    bmpHdr := PBitmapInfoHeader(AIcoData.Memory);

    IcoHdr.bWidth := IfThen(bmpHdr.biWidth <= MAXBYTE, bmpHdr.biWidth, 0);
    IcoHdr.bHeight := IfThen(bmpHdr.biHeight div 2 <= MAXBYTE, bmpHdr.biHeight div 2, 0);
    IcoHdr.bReserved := 0;
    IcoHdr.wPlanes := bmpHdr.biPlanes;
    IcoHdr.wBitCount := bmpHdr.biBitCount;

    if IcoHdr.wPlanes * IcoHdr.wBitCount >= 8 then
      IcoHdr.bColorCount := 0
    else
      IcoHdr.bColorCount := 1 shl (IcoHdr.wPlanes * IcoHdr.wBitCount);

    IcoHdr.dwBytesInRes := AIcoData.Size;
    IcoHdr.dwImageOffset := SizeOf(TCursorOrIcon) + SizeOf(TIconRec); // File pos where icon's image data begins

    ms.WriteBuffer(IcoDir, SizeOf(TCursorOrIcon));
    ms.WriteBuffer(IcoHdr, SizeOf(TIconRec));
    ms.CopyFrom(AIcoData, 0);
    Result := SaveToFileW(ms, ADestFile);
  finally
    ms.Free;
  end;
end;

function GetIconFromResource(ASrcFile, ADestFile: WideString; const AIconIndex: Integer): Integer;
var
  hResFile: HMODULE;
  ResHelper: TResHelper;
  rs: TResourceStream;
  ico: TIcon;
  SaveAsIs: Boolean;

  function EnumResProc(hModule: HINST; AResType, AResName: PChar; AParam: DWORD): BOOL; stdcall;
  var
    ResHlp: PResHelper;
  begin
    ResHlp := PResHelper(AParam);

    if ResHlp.Index <= 1 then
    begin
      if HiWord(DWORD(AResName)) = 0 then
        ResHlp.ResName := Format('#%d', [DWORD(AResName)])
      else
        ResHlp.ResName := AResName;

      Result := FALSE;
    end
    else
      Result := TRUE;

    Dec(ResHlp.Index);
  end;

  function FindBestIconId(const AIconDir: PGrpIconDir): string;
  var
    i: Integer;
    LargestSize: Cardinal;
  begin
    // Best icon could be found either by looking for highest width/height or by looking for largest icon size
    Result := '';
    LargestSize := 0;

    for i := AIconDir.idCount - 1 downto 0 do
      if (AIconDir.idEntries[i].dwBytesInRes > LargestSize) then
      begin
        LargestSize := AIconDir.idEntries[i].dwBytesInRes;
        Result := '#' + IntToStr(AIconDir.idEntries[i].nId);
      end;
  end;

  function IsPngFile(const AFileStart: Pointer): Boolean;
  const
    PNG_HEADER: array[0..7] of Byte = (137, 80, 78, 71, 13, 10, 26, 10);
  begin
    Result := CompareMem(rs.Memory, @PNG_HEADER[0], SizeOf(PNG_HEADER));
  end;

  function GetAsIcon(const AStream: TResourceStream): TIcon;
  {$IFDEF PNG_SUPPORT}
  var
    png: TPngImage;

    procedure ConvertPngToIco(APng: TPngImage; AIco: TIcon);
    var
      Bmp: TBitmap;
      ImageList: TCustomImageList;
    begin
      Bmp  := TBitmap.Create;
      try
        Bmp.Assign(APng);
        ImageList := TCustomImageList.CreateSize(Bmp.Width, Bmp.Height);
        try
          ImageList.AddMasked(Bmp, Bmp.TransparentColor);
          ImageList.GetIcon(0, AIco);
        finally
          ImageList.Free;
        end;
      finally
        Bmp.Free;
      end;
    end;
    {$ENDIF}

  begin
    Result := TIcon.Create;
    try
      // Try loading icon from a stream, it could be .ico format or png (for 256x256 icons)
      if IsPngFile(AStream.Memory) then
      begin
      {$IFDEF PNG_SUPPORT}
        png := TPngImage.Create;
        try
          png.LoadFromStream(AStream);
          ConvertPngToIco(png, Result);
        finally
          png.Free;
        end;
      {$ENDIF}
      end
      else
        Result.LoadFromStream(AStream);
    except
      Result.Handle := 0;
    end;
  end;
begin
  Result := S_FALSE;
  hResFile := LoadLibraryExW(PWideChar(ASrcFile), 0, LOAD_LIBRARY_AS_DATAFILE);
  if hResFile <> 0 then
  begin
    // IconIndex is index of GROUP_Icon resource from which we need to get largest available icon,
    // traverse all group_icons and find name of the one we need
    ResHelper.Index := AIconIndex;
    ResHelper.ResName := '';

    EnumResourceNames(hResFile, RT_GROUP_ICON, @EnumResProc, Longint(@ResHelper));
    try
      // Now load group resource icon and find the largest (best) icon within
      rs := TResourceStream.Create(hResFile, ResHelper.ResName, RT_GROUP_ICON);
      ResHelper.ResName := FindBestIconId(rs.Memory);
      rs.Free;
      rs := TResourceStream.Create(hResFile, ResHelper.ResName, RT_ICON);
      SaveAsIs := (ExtractFileExt(ADestFile) = '') or (not IsPngFile(rs.Memory) and (ExtractFileExt(ADestFile) = '.ico'));
      ADestFile := WideChangeFileExt(ADestFile, '.ico');

      if not SaveAsIs then
      begin
        ico := GetAsIcon(rs);
        try
          if ico.Handle <> 0 then
            Result := SaveToFileW(ico, ADestFile)
          else
            SaveAsIs := True; // Conversion from png to ico failed, save file as is (as png image)
        finally
          ico.Free;
        end;
      end;

      if SaveAsIs then
      begin
        if IsPngFile(rs) then
        begin
          ADestFile := WideChangeFileExt(ADestFile, '.png');
          Result := SaveToFileW(rs, ADestFile);
        end
        else
          Result := SaveResIcoToFileW(rs, ADestFile);
      end;

      rs.Free;
    except

    end;

    FreeLibrary(hResFile);
  end
  else
    Result := GetLastError;
end;

function IsWinVistaUp: boolean;
var
  osvi: TOSVersionInfo;
  ConditionMask: UInt64;
begin
	// Initialize OsVersionInfo
	ZeroMemory(@osvi, SizeOf(TOSVersionInfo));
	osvi.dwOSVersionInfoSize := sizeof(TOSversionInfo);

	osvi.dwMajorVersion := 6; // set Vista major and minor fields
	osvi.dwMinorVersion := 0;

	// Initialize condition mask for comparison
  ConditionMask := 0;
	ConditionMask := VerSetConditionMask(ConditionMask, VER_MAJORVERSION, VER_GREATER_EQUAL);
	ConditionMask := VerSetConditionMask(ConditionMask, VER_MINORVERSION, VER_GREATER_EQUAL);

	// Perform the test.
	Result := VerifyVersionInfo(osvi, VER_MAJORVERSION or VER_MINORVERSION, ConditionMask);
end;

function GetImageList: HIMAGELIST;
type
  TShGetImageList = function(iImageList: Integer; const riid: TGUID; out ppvObj: Pointer): HRESULT; stdcall;
var
  SHGetImageList: TSHGetImageList;
  hLib: HMODULE;
  Flags: Integer;
begin
  Result:= 0;

  if IsWinVistaUp then
    Flags := SHIL_JUMBO
  else
    Flags := SHIL_EXTRALARGE;

  hLib := LoadLibrary('Shell32.dll');
  if hLib <> 0 then
  try
    SHGetImageList:= GetProcAddress(hLib, PChar(727));

    if Assigned(SHGetImageList) then
      if SHGetImageList(Flags, IID_IImageList, Pointer(Result)) <> S_OK then
        Writeln(SysErrorMessage(GetLastError));
  finally
    FreeLibrary(hLib);
  end;
end;

function FindAppFromExt(const AExt: WideString): WideString;
var
  Size: DWORD;
begin
  Size := 0;
  if AssocQueryStringW(ASSOCF_NOTRUNCATE, ASSOCSTR_EXECUTABLE, PWideChar(AExt), 'open', nil, @Size) = S_FALSE then
  begin
    SetLength(Result, Size);
    if AssocQueryStringW(ASSOCF_NOTRUNCATE, ASSOCSTR_EXECUTABLE, PWideChar(AExt), 'open', PWideChar(Result), @Size) <> S_OK then
      Result := ''
    else
      Result := PWideChar(Result);
  end;
end;

function ExtractLargestIconFrom(const ASourceFile, AExtractedIconPath: WideString): Boolean;
var
  sfi: TSHFileInfoW;
  hImgList    : HIMAGELIST;
  ResultIco: TIcon;
  Res: Integer;
  HandlerPath: WideString;
begin
  CoInitialize(nil);
  Result := False;

  // Retrieve icon index of a handler for a source file, despite MSDN docs SHGFI_ICONLOCATION doesn't return path to
  // executable that handles ext (if we pass .txt it will not return PathTo\Notepad.exe)
  ZeroMemory(@sfi, SizeOf(TSHFileInfoW));
  if SHGetFileInfoW(PWideChar(ASourceFile), FILE_ATTRIBUTE_NORMAL, sfi, SizeOF(TSHFileInfoW), SHGFI_ICONLOCATION or SHGFI_USEFILEATTRIBUTES) = 0 then
    Exit;

  // If AsourceFile is an executable file, try to load resource from it
  Res := GetIconFromResource(ASourceFile, AExtractedIconPath, sfi.iIcon);

  if Res in [ERROR_FILE_NOT_FOUND, ERROR_BAD_FORMAT] then
  begin
    // File doesn't exists or it's not a valid exe file, most likely we've been sent file extension, try to find associated application for given extension
    HandlerPath := FindAppFromExt(ExtractFileExt(ASourceFile));
    Res := GetIconFromResource(HandlerPath, AExtractedIconPath, sfi.iIcon);
  end;

  if Res = S_OK then
    Exit;

  // Retriving icon from resource failed, try now getting it from system image list, first get index of it in system list
  if SHGetFileInfoW(PWideChar(ASourceFile), FILE_ATTRIBUTE_NORMAL, sfi, SizeOF(TSHFileInfoW), SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES) = 0 then
    Exit;

  ResultIco := TIcon.Create;
  try
    hImgList := GetImageList; // Create image list of appropriate size
    ResultIco.Handle := ImageList_GetIcon(hImgList, sfi.iIcon, ILD_TRANSPARENT);

    if ResultIco.Handle <> 0 then
      SaveToFileW(ResultIco, WideChangeFileExt(AExtractedIconPath, '.ico'));
  finally
    ResultIco.Free;
  end;

  CoUninitialize;
//  ReadLn;
end;

end.


