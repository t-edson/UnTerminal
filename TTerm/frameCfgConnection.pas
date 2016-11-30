unit frameCfgConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, uResaltTerm,
  SynEdit, SynEditHighlighter, UnTerminal,
  ConfigFrame;

type

  { TfraConnection }

  TfraConnection = class(TCfgFrame)
    chkDetecPrompt: TCheckBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblOtro: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    txtCadFin: TEdit;
    txtCadIni: TEdit;
    txtOtro: TEdit;
    procedure chkDetecPromptChange(Sender: TObject);
  private
    ed: TSynEdit;
    proc: TConsoleProc;
  public
    //parámetros de detección de prompt
    detecPrompt: boolean;
    prIni     : string;
    prFin     : string;
    TipDetec : TPrompMatch;
    Command  : string;   //command to execute
    maxLinTer : integer;  //máxima cantidad de líneas que se nantienen en el terminal
    TpoMax    : integer;  //Timeout for connections
    procedure Iniciar(secINI0: string; ed0: TSynEdit; proc0: TConsoleProc); //Inicia el frame
    procedure ConfigCambios;
  end;
implementation
{$R *.lfm}

{ TfraConnection }
procedure TfraConnection.Iniciar(secINI0: string; ed0: TSynEdit; proc0: TConsoleProc
  );
//necesita referencias al editor y al terminal para actualizar la detección de prompt
begin
  secINI := secINI0;  //sección INI
  //asigna referencia necesarias
  ed := ed0;
  proc := proc0;
  OnUpdateChanges := @ConfigCambios;  //manejador de cambios
  //crea las relaciones variable-control
  Asoc_Bol_TChkBox(@detecPrompt, chkDetecPrompt,'DetecPrompt', false);
  Asoc_Str_TEdit(@prIni,txtCadIni,'cadIni','');
  Asoc_Str_TEdit(@prFin,txtCadFin,'cadFin','');
  Asoc_Enum_TRadBut(@TipDetec, SizeOf(TipDetec),
         [RadioButton1, RadioButton2, RadioButton3, RadioButton4],'TipDetec', 0);
  Asoc_Str_TEdit(@Command, txtOtro, 'Command', 'bash -i');
  proc.LineDelimSend :=  LDS_LF;   //configura salto de línea
  proc.LineDelimRecv :=  LDR_LF;   //configura salto de línea
  proc.TerminalWidth:=10000;
  maxLinTer := 32000;
  TpoMax := 180;
end;

procedure TfraConnection.ConfigCambios;
{Configura al resaltador con la detección de prompt indicada}
var
  hlTerm: TResaltTerm;
begin
  //configura el resaltador con la detección del prompt
  if ed.Highlighter.ClassName='TResaltTerm' then begin
    //Solo se aplica, a 'TResaltTerm'
    hlTerm := TResaltTerm(ed.Highlighter);
    if DetecPrompt then begin  //hay detección
      hlTerm.detecPrompt:=true;
      hlTerm.prIni:=prIni;
      hlTerm.prFin:=prFin;
    end else begin //sin detección
      hlTerm.detecPrompt:=false;
    end;
    ed.Invalidate;  //para actualizar
  end;
  //configura detección en proceso
  if DetecPrompt then begin  //hay detección
    proc.detecPrompt:=true;
    proc.promptIni:= prIni;
    proc.promptFin:= prFin;
    proc.promptMatch := TipDetec;
  end else begin //sin detección
    proc.detecPrompt:=false;
  end;
end;

procedure TfraConnection.chkDetecPromptChange(Sender: TObject);
begin
  GroupBox1.Enabled:=chkDetecPrompt.Checked;
  chkDetecPrompt.Enabled:=true;  //porque también se deshabilitaría
end;

end.

