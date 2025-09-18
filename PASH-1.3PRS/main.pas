program pash;

uses
  trpo,
  LS,
  CD,
  TOUCH,
  MKDIR,
  RN,
  RM,
  PTE,
  PNI,
  glvar,
  crt,
  sysutils,
  auth,
  plugin_manager;

var
  userInput: String;
  shouldExit: Boolean;
  prompt: String;
  command: String;
  argument: String;
  argument2: String;

procedure ParseUserInput(const input: String);
var
  pos1, pos2: Integer;
  tempInput: String;
begin
  command := '';
  argument := '';
  argument2 := '';

  tempInput := Trim(input);

  pos1 := Pos(' ', tempInput);
  if pos1 > 0 then
  begin
    command := LowerCase(Copy(tempInput, 1, pos1 - 1));
    tempInput := Trim(Copy(tempInput, pos1 + 1, Length(tempInput)));

    pos2 := Pos(' ', tempInput);
    if pos2 > 0 then
    begin
      argument := Copy(tempInput, 1, pos2 - 1);
      argument2 := Trim(Copy(tempInput, pos2 + 1, Length(tempInput)));
    end
    else
    begin
      argument := tempInput;
    end;
  end
  else
  begin
    command := LowerCase(tempInput);
  end;
end;

procedure ShowHelp;
begin
  trpoWriteLn('');
  trpoWriteLn(' _____ _____ _____ _____ ');
  trpoWriteLn('|  _  |  _  |   __|  |  |');
  trpoWriteLn('|   __|     |__   |     |');
  trpoWriteLn('|__|  |__|__|_____|__|__|');
  trpoWriteLn('');
  trpoWriteLn('[Pash ' + glvar.version + '] - a simple shell for Linux.');
  trpoWriteLn('');
  trpoWriteLn('Commands:');
  trpoWriteLn('  ls [path]      - List files and directories.');
  trpoWriteLn('  cd [path]      - Change current directory.');
  trpoWriteLn('  rn [old] [new] - Rename file or directory.');
  trpoWriteLn('  rm [name]      - Remove file or directory.');
  trpoWriteLn('  mkdir [name]   - Create a new directory.');
  trpoWriteLn('  touch [name]   - Create a new file or update timestamp.');
  trpoWriteLn('  pte [file]     - Simple text editor.');
  trpoWriteLn('  pni [url] [dir]- Plugin Network Installer.');
  trpoWriteLn('  listp          - List available plugins.');
  trpoWriteLn('  p [name] [arg] - Run a plugin.');
  trpoWriteLn('  clear          - Clear the screen.');
  trpoWriteLn('  ver            - Show version.');
  trpoWriteLn('  auth           - Show auth info.');
  trpoWriteLn('  help           - Show this help message.');
  trpoWriteLn('  exit           - Exit the shell.');
  trpoWriteLn('');
  trpoWriteLn('Modul commands:');
  trpoWriteLn('  prl            - (Pascal Run Archiver) archiver.');
  trpoWriteLn('');
end;

procedure HandleCommand;
begin
  case command of
    'cd':
      ChangeDirectory(argument);
    'ls':
      RunLS(argument);
    'rn':
      if (argument = '') or (argument2 = '') then
        trpoWriteLn('ERROR: invalid arguments. Usage: rn [old_name] [new_name].')
      else
        RunRN(argument, argument2);
    'rm':
      if argument = '' then
        trpoWriteLn('ERROR: invalid argument. Usage: rm [file_or_dir_name].')
      else
        RunRM(argument);
    'mkdir':
      RunMKDIR(argument);
    'touch':
      RunTOUCH(argument);
    'pte':
      if argument = '' then
        trpoWriteLn('ERROR: invalid argument. Usage: pte [file_name].')
      else
        RunPTE(argument);
    'pni':
      if (argument = '') or (argument2 = '') then
        trpoWriteLn('ERROR: invalid arguments. Usage: pni ["URL"] ["folder"].')
      else
        PNI.PNI(argument, argument2);
    'listp':
      ListPlugins;
    'p':
      if argument = '' then
        trpoWriteLn('ERROR: no plugin specified. use `p` `plugin_name` [options].')
      else
        RunPlugin(argument, argument2);
    'ver':
      trpoWriteLn('Pash ' + glvar.version);
    'auth':
      RunAuth;
    'clear':
      ClrScr;
    'help':
      ShowHelp;
    'exit':
      shouldExit := True;
  else
    trpoWriteLn('ERROR: command not found: "' + command + '". For help, type `help`.');
  end;
end;

begin
  glvar.startupPath := ExtractFilePath(ParamStr(0));
  glvar.currentPath := GetCurrentDir;
  
  trpoInit(prompt);
  RunAuth;
  
  if not DirectoryExists(glvar.startupPath + 'binp') then
    CreateDir(glvar.startupPath + 'binp');

  shouldExit := False;
  while not shouldExit do
  begin
    prompt := '[pash ' + glvar.currentPath + ']$ ';
    trpoInput(prompt, userInput);
    ParseUserInput(userInput);
    HandleCommand;
  end;
end.
