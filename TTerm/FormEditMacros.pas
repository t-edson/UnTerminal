unit FormEditMacros;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs, LCLProc,
  Menus, ComCtrls, ActnList, StdActns,
  MisUtils, Parser, Globales;

type

  { TfrmEditMacros }

  TfrmEditMacros = class(TForm)
  published
    acArcAbrir: TAction;
    acArcGuaCom: TAction;
    acArcGuardar: TAction;
    acArcNuevo: TAction;
    acArcSalir: TAction;
    acEdiCopy: TEditCopy;
    acEdiCut: TEditCut;
    acEdiPaste: TEditPaste;
    acEdiRedo: TAction;
    acEdiUndo: TAction;
    AcHerConfig: TAction;
    AcHerEjec: TAction;
    AcHerDeten: TAction;
    ActionList: TActionList;
    acVerPanArc: TAction;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    mnArchivo: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    mnRecientes: TMenuItem;
    mnHerram: TMenuItem;
    mnEdicion: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ed: TSynEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    acVerBarEst: TAction;
    acVerNumLin: TAction;
    procedure acArcNuevoExecute(Sender: TObject);
    procedure acArcSalirExecute(Sender: TObject);
    procedure acEdiRedoExecute(Sender: TObject);
    procedure acEdiSelecAllExecute(Sender: TObject);
    procedure acEdiUndoExecute(Sender: TObject);
    procedure AcHerConfigExecute(Sender: TObject);
    procedure AcHerDetenExecute(Sender: TObject);
    procedure AcHerEjecExecute(Sender: TObject);
  private
    procedure MarcarError(nLin, nCol: integer);
  public
    { public declarations }
    procedure Ejecutar(arc: string);
    procedure DetenerEjec;
    procedure SetLanguage(lang: string);
  end;

var
  frmEditMacros: TfrmEditMacros;

implementation
uses FormConfig;
{$R *.lfm}

{ TfrmEditMacros }

/////////////////// Acciones de Archivo /////////////////////
procedure TfrmEditMacros.acArcNuevoExecute(Sender: TObject);
begin
  ed.Lines.Add('disconnect    ');
  ed.Lines.Add('connect');
  ed.Lines.Add('wait "login: "');
  ed.Lines.Add('sendln "usuario"');
  ed.Lines.Add('wait "password: "');
  ed.Lines.Add('sendln "clave"');
  ed.Lines.Add('pause 3    '+dic('//espera 3 segundos'));
  ed.Lines.Add('sendln "cd /folder"');
end;
procedure TfrmEditMacros.acArcSalirExecute(Sender: TObject);
begin
  frmEditMacros.Close;
end;
procedure TfrmEditMacros.MarcarError(nLin, nCol: integer);
begin
  //posiciona curosr
  ed.CaretX := nCol;
  ed.CaretY := nLin;
  //define línea con error
  ed.Invalidate;  //refresca
end;

//////////// Acciones de Edición ////////////////
procedure TfrmEditMacros.acEdiUndoExecute(Sender: TObject);
begin
end;
procedure TfrmEditMacros.acEdiRedoExecute(Sender: TObject);
begin
end;
procedure TfrmEditMacros.acEdiSelecAllExecute(Sender: TObject);
begin
  ed.SelectAll;
end;
//////////// Acciones de Herramientas  ////////////////
procedure TfrmEditMacros.AcHerEjecExecute(Sender: TObject);
begin
  cxp.Compilar('Noname', ed.Lines);
  if cxp.HayError then begin
    MarcarError(cxp.ErrorLine, cxp.ErrorCol);
    cxp.ShowError;
  end;
end;
procedure TfrmEditMacros.AcHerDetenExecute(Sender: TObject);
begin
  DetenerEjec;
end;
procedure TfrmEditMacros.AcHerConfigExecute(Sender: TObject);
begin
  config.Configurar;
end;
procedure TfrmEditMacros.DetenerEjec;
//Detiene la ejecución de la macro en curso
begin
  if not cxp.ejecProg then exit;
  DetEjec := true;  //manda mensaje para detener la macro
end;
procedure TfrmEditMacros.Ejecutar(arc: string);
//Permite ejecutar una macro almacenada en un archivo externo
var
  larc: TStringList;
begin
  larc := Tstringlist.Create;
  larc.LoadFromFile(arc);
  cxp.Compilar(arc, larc);
  if cxp.HayError then begin
    self.Show;   //por si no estaba visible
    //muestra error en el editor
    MarcarError(cxp.ErrorLine,cxp.ErrorCol);
    cxp.ShowError;
  end;
  larc.Free;
end;

procedure TfrmEditMacros.SetLanguage(lang: string);
//Rutina de traducción
begin
  case lowerCase(lang) of
  'es': begin
    acArcNuevo.Caption := '&Nuevo';
    acArcAbrir.Caption := '&Abrir...';
    acArcGuardar.Caption := '&Guardar';
    acArcGuaCom.Caption := 'G&uardar Como...';
    acArcSalir.Caption := '&Salir';
    acEdiUndo.Caption := '&Deshacer';
    acEdiRedo.Caption := '&Rehacer';
    acEdiCut.Caption := 'Cor&tar';
    acEdiCopy.Caption := '&Copiar';
    acEdiPaste.Caption := '&Pegar';
    acVerNumLin.Caption := 'Ver &Núm. de Línea';
    acVerBarEst.Caption := 'Ver Barra de &Estado';
    acVerPanArc.Caption := 'Panel de &Archivos';
    AcHerEjec.Caption := '&Ejecutar';
    AcHerDeten.Caption := '&Detener';
    AcHerConfig.Caption := 'C&onfigurar';
    //menús
    mnArchivo.Caption := '&Archivo';
    mnRecientes.Caption:='&Recientes';
    mnEdicion.Caption:='&Edición';
    mnHerram.Caption:='&Herramientas';
    //textos
    dicClear;  //ya está en español
    end;
  'en': begin
    acArcNuevo.Caption := '&New';
    acArcAbrir.Caption := '&Open...';
    acArcGuardar.Caption := '&Save';
    acArcGuaCom.Caption := 'Sa&ve As...';
    acArcSalir.Caption := '&Quit';
    acEdiUndo.Caption := '&Undo';
    acEdiRedo.Caption := '&Redo';
    acEdiCut.Caption := 'Cu&t';
    acEdiCopy.Caption := '&Copy';
    acEdiPaste.Caption := '&Paste';
    acVerNumLin.Caption := 'View Line &Number';
    acVerBarEst.Caption := 'View &Statusbar';
    acVerPanArc.Caption := '&File Panel';
    AcHerEjec.Caption := '&Execute';
    AcHerDeten.Caption := '&Stop';
    AcHerConfig.Caption := '&Setup';
    //menús
    mnArchivo.Caption := '&File';
    mnRecientes.Caption:='&Recents';
    mnEdicion.Caption:='&Edit';
    mnHerram.Caption:='&Tools';
    //textos
    dicSet('//espera 3 segundos','//wait for 3 seconds');
    dicSet('//Limpia la pantalla','//Clear the terminal');
    dicSet('//Inicia conexión','//Start connection');
    end;
  end;
end;

end.

