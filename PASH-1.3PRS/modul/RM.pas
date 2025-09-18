unit RM;

interface

uses
  trpo,
  glvar,
  sysutils;

procedure RunRM(const Name: String);

implementation

procedure RunRM(const Name: String);
var
  fullPath: String;

begin
  fullPath := glvar.currentPath + '/' + Name;

  {$I-}
  if FileExists(fullPath) then
  begin
    DeleteFile(fullPath);
  end
  else if DirectoryExists(fullPath) then
  begin
    RemoveDir(fullPath);
  end
  else
  begin
    trpoWriteLn('ERROR: failed to determine element.');
    exit;
  end;
  {$I+}

  if IOResult = 0 then
  else
  begin
    trpoWriteLn('ERROR: failed to delete folder/file.');
  end;
end;

end.
