unit FrameCfgConex;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons,
  globales, UnTerminal, MisUtils, Graphics, Dialogs, ExtCtrls,
  ConfigFrame;

type
  { TfraConexion }

  TfraConexion = class(TCfgFrame)
    lblOtro: TLabel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    txtOtro: TEdit;
  private
    proc: TConsoleProc;
  public
    //parámetros
    Command     : String;   //Ruta del aplicativo (solo válido con el tipo TCON_OTHER)
    LineDelimSnd: TUtLineDelSend;  //Tipo de delimitaodr de línea para envío
    LineDelimRcv: TUtLineDelRecv;  //Tipo de delimitaodr de línea para recepción
    procedure PropToWindow; override;
    procedure WindowToProp; override;
    procedure Iniciar(secINI0: string; proc0: TConsoleProc);
    procedure UpdateChanges;
  end;

implementation
{$R *.lfm}

{ TfraConexion }
procedure TfraConexion.Iniciar(secINI0: string; proc0: TConsoleProc);
begin
  secINI := secINI0;  //sección INI
  proc := proc0;
  OnUpdateChanges:=@UpdateChanges;
  //crea las relaciones variable-control
  Asoc_Str_TEdit(@Command, txtOtro, 'Command', '');
  Asoc_Enum_TRadGroup(@LineDelimSnd, SizeOf(LineDelimSnd), RadioGroup1, 'LineDelimSnd', 0);
  Asoc_Enum_TRadGroup(@LineDelimRcv, SizeOf(LineDelimRcv), RadioGroup2, 'LineDelimRcv', 2);
end;
procedure TfraConexion.UpdateChanges;
//Configura el proceso de acuerdo a los parámetros de la conexión.
begin
  proc.LineDelimSend := LineDelimSnd;   //configura salto de línea
  proc.LineDelimRecv := LineDelimRcv;   //configura salto de línea
end;

procedure TfraConexion.PropToWindow;
begin
  inherited;
end;

procedure TfraConexion.WindowToProp;
begin
  inherited WindowToProp;
end;

end.

