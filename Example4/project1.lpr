{Sample of code for "UnTerminal", in a console program.
 This a very basic sample test.
 Just opening a CMD command.}

program project1;
uses Interfaces, UnTerminal,TermVT,SysUtils;
type
  TProc=class
    proc: TConsoleProc;
    procedure procLineCompleted(const lin: string);
    procedure procLineInit(const grilla: TtsGrid; fIni, fFin: integer);
    procedure procReadData(nDat: integer; const lastLin: string);
    constructor Create;
    destructor Destroy; override;
  end;
constructor TProc.Create;
begin
  proc := TConsoleProc.Create(nil);
  proc.LineDelimSend := LDS_CRLF;
  proc.OnInitScreen:= @procLineInit;
  proc.OnLineCompleted:=@procLineCompleted;
  proc.OnReadData:=@procReadData;
end;

destructor TProc.Destroy;
begin
  proc.Destroy;
end;

procedure TProc.procLineInit(const grilla: TtsGrid; fIni, fFin: integer);
var
  i: Integer;
begin
  for i:=fIni to fFin do write((grilla[i]));
end;

procedure TProc.procReadData(nDat: integer; const lastLin: string);
begin
  Write(lastLin);
end;

procedure TProc.procLineCompleted(const lin: string);
begin
  //Line received
  writeln('--'+lin);
  //To respond, do something like: proc.SendLn('dir');
end;

var
  p: TProc;
  i: Integer;
  input: string;
begin
  writeln('Executing process');
  writeln('Type quit to leave');
  p := TProc.Create;
 {$ifdef linux}
 p.proc.Open('bash','');
 {$endif}
 {$ifdef WINDOWS}
  p.proc.Open('cmd','');
 {$endif}
  repeat
    Readln(input);
    writeln('You input '+ input);
    if input = 'quit' then
      break;
    p.proc.SendLn(input);
    p.proc.Loop(1);
  until p.proc.State = ECO_STOPPED;
  writeln('Finished');
  p.proc.Close;
  p.Destroy;
end.
