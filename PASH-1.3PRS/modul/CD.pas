unit CD;

interface

uses
  sysutils,
  trpo,
  glvar;

procedure ChangeDirectory(const dir: String);

implementation

procedure ChangeDirectory(const dir: String);
begin
  if SetCurrentDir(dir) then
  begin
    glvar.currentPath := GetCurrentDir;
  end
  else
  begin
    trpoWriteLn('ERROR: folder "' + dir + '" not found or error');
  end;
end;

end.
