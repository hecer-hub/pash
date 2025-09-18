unit RN;

interface

uses
  sysutils,
  trpo,
  glvar;

procedure RunRN(const OldName: String; const NewName: String);

implementation

procedure RunRN(const OldName: String; const NewName: String);
var
  fullOldPath: String;
  fullNewPath: String;
begin
  fullOldPath := glvar.currentPath + '/' + OldName;
  fullNewPath := glvar.currentPath + '/' + NewName;

  {$I-}
  if FileExists(fullOldPath) then
  begin
    RenameFile(fullOldPath, fullNewPath);
  end
  else if DirectoryExists(fullOldPath) then
  begin
    RenameFile(fullOldPath, fullNewPath);
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
    trpoWriteLn('ERROR: failed to rename element.');
  end;
end;

end.
