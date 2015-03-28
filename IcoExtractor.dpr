program IcoExtractor;

{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  ShellApi,
  ExtractHelper in 'ExtractHelper.pas';

var
  Args, TempArgs: PPWideChar;
  ArgCount: Integer;

  SrcFile, DestPicFile: WideString;
begin
  // We're expecting two parameters, first is source file icon whose icon is seeked and second is destination file to save icon to
  Args := CommandLineToArgvW(GetCommandLineW, ArgCount);

  if Assigned(Args) then
  begin
    TempArgs := Args;
    Inc(TempArgs);

    if ArgCount >= 3 then // first argument is path to exe
    begin
      SrcFile := TempArgs^;
      Inc(TempArgs);
      DestPicFile := TempArgs^;
    end;

    LocalFree(HLOCAL(Args));
  end;

  if SrcFile = '' then
    Exit;

  if DestPicFile = '' then
    DestPicFile := WideChangeFileExt(SrcFile, '.png');

  ExtractLargestIconFrom(SrcFile, DestPicFile);
end.

