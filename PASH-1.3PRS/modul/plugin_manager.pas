unit plugin_manager;
{$mode objfpc}{$H+}

interface

uses
  SysUtils, process, trpo, glvar, BaseUnix, Classes;

function RunPlugin(const PluginName: string; const Arguments: string): boolean;
procedure ListPlugins;
function IsExecutableFile(const FileName: string): boolean;

implementation

type
  TSystemModule = record
    name: string;
    enabled: boolean;
  end;

const
  SYSTEM_MODULES: array[0..0] of TSystemModule = (
    (name: 'prl'; enabled: True)
  );

function GetModuleStatus(const name: string): string;
var
  module: TSystemModule;
begin
  Result := '';
  for module in SYSTEM_MODULES do
  begin
    if (LowerCase(module.name) = LowerCase(name)) then
    begin
      if module.enabled then
        Result := '*'
      else
        Result := '-';
      Exit;
    end;
  end;
end;

function IsExecutableFile(const FileName: string): boolean;
var
  FileInfo: stat;
begin
  Result := False;
  if not FileExists(FileName) then
    Exit;
  
  if fpStat(FileName, FileInfo) = 0 then
  begin
    if fpS_ISREG(FileInfo.st_mode) then
    begin
      Result := (FileInfo.st_mode and (S_IXUSR or S_IXGRP or S_IXOTH)) <> 0;
    end;
  end;
end;

function RunPlugin(const PluginName: string; const Arguments: string): boolean;
var
  Proc: TProcess;
  pluginPath: string;
  OutputBuffer: AnsiString;
  BytesRead: longint;
begin
  Result := False;
  
  pluginPath := glvar.startupPath + 'binp/' + PluginName;
  
  if not FileExists(pluginPath) then
  begin
    trpoWriteLn('ERROR: Plugin "' + PluginName + '" not found in binp...');
    Exit;
  end;
  
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := pluginPath;
    
    if Arguments <> '' then
      Proc.Parameters.Add(Arguments);
    
    Proc.Options := [poWaitOnExit, poUsePipes];
    
    trpoWriteLn('Running plugin: ' + PluginName);
    
    try
      Proc.Execute;
      
      while Proc.Running or (Proc.Output.NumBytesAvailable > 0) do
      begin
        SetLength(OutputBuffer, 1024);
        BytesRead := Proc.Output.Read(OutputBuffer[1], Length(OutputBuffer));
        if BytesRead > 0 then
        begin
          SetLength(OutputBuffer, BytesRead);
          trpoWriteLn(OutputBuffer);
        end;
      end;
      
      while Proc.Running or (Proc.StdErr.NumBytesAvailable > 0) do
      begin
        SetLength(OutputBuffer, 1024);
        BytesRead := Proc.StdErr.Read(OutputBuffer[1], Length(OutputBuffer));
        if BytesRead > 0 then
        begin
          SetLength(OutputBuffer, BytesRead);
          trpoWriteLn('Error from plugin: ' + OutputBuffer);
        end;
      end;
      
    except
      on E: Exception do
      begin
        trpoWriteLn('ERROR: Failed to execute plugin "' + PluginName + '": ' + E.Message);
        trpoWriteLn('Make sure the plugin has execute permissions: chmod +x ' + pluginPath);
        Result := False;
        Exit;
      end;
    end;
  finally
    Proc.Free;
  end;
  
  Result := (Proc.ExitStatus = 0);
  
end;

procedure ListPlugins;
var
  sr: TSearchRec;
  searchPath: string;
  pluginCount: integer;
  pluginPath: string;
  moduleStatus: string;
begin
  trpoWriteLn('Available plugins in binp folder:');
  trpoWriteLn('');
  
  searchPath := glvar.startupPath + 'binp/*';
  pluginCount := 0;
  
  if FindFirst(searchPath, faAnyFile, sr) = 0 then
  begin
    repeat
      if (sr.Name <> '.') and (sr.Name <> '..') and 
         ((sr.Attr and faDirectory) = 0) then
      begin
        pluginPath := glvar.startupPath + 'binp/' + sr.Name;
        moduleStatus := GetModuleStatus(sr.Name);
        
        if IsExecutableFile(pluginPath) then
        begin
          if moduleStatus = '*' then
            trpoWriteLn('  [+] ' + sr.Name + ' (system, * enabled)')
          else if moduleStatus = '-' then
            trpoWriteLn('  [!] ' + sr.Name + ' (system, - disabled)')
          else
            trpoWriteLn('  [+] ' + sr.Name);
            
          Inc(pluginCount);
        end
        else
        begin
          trpoWriteLn('  [!] ' + sr.Name + ' (not executable)');
        end;
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
  
  trpoWriteLn('');
  trpoWriteLn('Total plugins: ' + IntToStr(pluginCount));
  trpoWriteLn('Usage: p [plugin_name] [arguments]');
  trpoWriteLn('Example: p myplugin --help');
end;

end.
