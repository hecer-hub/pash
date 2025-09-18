unit PNI;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, URIParser;

procedure PNI(const AUrl, AFolder: string);

implementation

uses
  process, BaseUnix;

function ForceDirs(const DirName: string): boolean;
var
  Path: string;
  i: integer;
begin
  Result := True;
  Path := IncludeTrailingPathDelimiter(DirName);
  i := 1;
  while i <= Length(Path) do
  begin
    if Path[i] = PathDelim then
    begin
      if i > 1 then
      begin
        if not DirectoryExists(Copy(Path, 1, i - 1)) then
        begin
          if not CreateDir(Copy(Path, 1, i - 1)) then
          begin
            Result := False;
            Exit;
          end;
        end;
      end;
    end;
    Inc(i);
  end;
end;

function DownloadFile(const AUrl, AFolder: string): Boolean;
var
  Proc: TProcess;
  Downloader: string;
begin
  Result := False;
  Downloader := 'wget';
  
  WriteLn('Downloading file using ' + Downloader + '...');
  
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := Downloader;
    Proc.Options := [poWaitOnExit];
    
    Proc.Parameters.Add('-P');
    Proc.Parameters.Add(AFolder);
    Proc.Parameters.Add(AUrl);
    ForceDirs(AFolder);
    
    Proc.Execute;
    
    if Proc.ExitStatus <> 0 then
    begin
      Proc.Executable := 'curl';
      Proc.Parameters.Clear;
      
      ForceDirs(AFolder);
      Proc.Parameters.Add('-o');
      Proc.Parameters.Add(AFolder + PathDelim + ExtractFileName(AUrl));
      Proc.Parameters.Add(AUrl);
      
      WriteLn('wget failed, trying curl...');
      
      Proc.Execute;
    end;
    
    Result := Proc.ExitStatus = 0;
    
  finally
    Proc.Free;
  end;
end;

function ParseURL(const URL: string; out Host, Path: string; out Port: Word; out UseSSL: Boolean; out Protocol: string): Boolean;
var
  URI: TURI;
begin
  Result := False;
  try
    URI := ParseURI(URL);
    Protocol := LowerCase(URI.Protocol);
    
    if not ((Protocol = 'http') or (Protocol = 'https') or (Protocol = 'ftp')) then
      Exit;
      
    Host := URI.Host;
    Path := URI.Path + URI.Document;
    if Path = '' then
      Path := '/';
      
    UseSSL := (Protocol = 'https');
    
    if URI.Port <> 0 then
      Port := URI.Port
    else if UseSSL then
      Port := 443
    else if Protocol = 'ftp' then
      Port := 21
    else
      Port := 80;
      
    Result := Host <> '';
  except
    Result := False;
  end;
end;

procedure PNI(const AUrl, AFolder: string);
var
  Host, Path, Protocol: string;
  Port: Word;
  UseSSL: Boolean;
  Result: Boolean;
begin
  Result := False;
  
  WriteLn('=== PNI Downloader ===');
  
  if not ParseURL(AUrl, Host, Path, Port, UseSSL, Protocol) then
  begin
    WriteLn('ERROR: Unsupported protocol or invalid URL');
    Exit;
  end;
  
  WriteLn('URL: ', AUrl);
  WriteLn('Protocol: ', UpperCase(Protocol));
  WriteLn('Folder: ', AFolder);
  WriteLn;

  Result := DownloadFile(AUrl, AFolder);
  
  WriteLn;
  if Result then
    WriteLn('=== COMPLETED SUCCESSFULLY ===')
  else
    WriteLn('=== COMPLETED WITH ERROR ===');
end;

end.
