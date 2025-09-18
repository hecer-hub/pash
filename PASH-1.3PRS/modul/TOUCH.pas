unit TOUCH;

interface

uses
  sysutils,
  trpo,
  glvar;

procedure RunTOUCH(const fileName: String);

implementation

procedure RunTOUCH(const fileName: String);
var
  myFile: TextFile;
  fullPath: String;
begin
  if fileName = '' then
  begin
    exit;
  end;
  
  fullPath := glvar.currentPath + '/' + fileName;
  
  Assign(myFile, fullPath);
  {$I-}
  Rewrite(myFile);
  Close(myFile);
  {$I+}
end;

end.