{Basic code to launch and capture the output of a process using the unit "UnTerminal".}
unit Unit1;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,UnTerminal, Classes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    txtProcess: TEdit;
    label1: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  outText: String;
  proc: TConsoleProc;
begin
  proc:= TConsoleProc.Create(nil);
  //proc.RunInLoop(txtProcess.Text,'', -1, outText);

  proc.RunInLoop('cmd /c dir | more','', -1, outText);


  Memo1.Text := outText;
  proc.Destroy;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$ifdef linux}
  txtProcess.Text:= 'bash -c "ls"';
  {$endif}
end;

end.

