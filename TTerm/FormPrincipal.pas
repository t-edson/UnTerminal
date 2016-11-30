{Define a la ventana de sesión. Esta ventana permite mostrar el texto que va llegando
 de un proceso. Servirá para visualizar como se interactúa con la sesión y para poder
 iniciar conexiones a sqlplus mediante el telnet.}
unit FormPrincipal;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, SynEdit, Forms, Controls, Graphics, Menus, ActnList, ExtCtrls,
  ComCtrls, LCLType, LCLIntf, SynEditKeyCmds, SynEditMarkupHighAll, SynEditMiscClasses,
  UnTerminal, FormConfig, FormEditMacros, MisUtils, Globales, TermVT, uResaltTerm;

type

  { TfrmPrincipal }

  TfrmPrincipal = class(TForm)
  published
    AcArcSalir: TAction;
    AcHerCfg: TAction;
    AcTerDetPrm: TAction;
    AcTerDescon: TAction;
    AcTerConec: TAction;
    AcTerLimBuf: TAction;
    AcPCmEnvLin: TAction;
    acPCmEnvCtrC: TAction;
    AcTerEnvCtrlC: TAction;
    AcTerEnvCR: TAction;
    AcTerEnvLF: TAction;
    acHerMac: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
    mnTerSend: TMenuItem;
    MenuItem68: TMenuItem;
    MenuItem70: TMenuItem;
    MenuItem72: TMenuItem;
    MenuItem73: TMenuItem;
    MenuItem75: TMenuItem;
    MenuItem79: TMenuItem;
    MenuItem80: TMenuItem;
    MenuItem81: TMenuItem;
    MenuItem47: TMenuItem;
    mnPanCom: TMenuItem;
    mnHerram: TMenuItem;
    MenuItem40: TMenuItem;
    mnTerminal: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu2: TPopupMenu;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    edTerm: TSynEdit;
    edPCom: TSynEdit;
    tbPCom: TToolBar;
    tbTerm: TToolBar;
    Timer1: TTimer;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton6: TToolButton;
    ToolButton9: TToolButton;
    procedure AcArcSalirExecute(Sender: TObject);
    procedure acPCmEnvCtrCExecute(Sender: TObject);
    procedure AcPCmEnvLinExecute(Sender: TObject);
    procedure AcHerCfgExecute(Sender: TObject);
    procedure AcTerConecExecute(Sender: TObject);
    procedure AcTerDesconExecute(Sender: TObject);
    procedure AcTerDetPrmExecute(Sender: TObject);
    procedure AcTerEnvCRExecute(Sender: TObject);
    procedure AcTerEnvCtrlCExecute(Sender: TObject);
    procedure AcTerEnvLFExecute(Sender: TObject);
    procedure AcTerLimBufExecute(Sender: TObject);
    procedure AcVerEdiMacExecute(Sender: TObject);
    procedure edPComKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edPComKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edPComSpecialLineMarkup(Sender: TObject; Line: integer;
      var Special: boolean; Markup: TSynSelectedColor);
    procedure edTermSpecialLineMarkup(Sender: TObject; Line: integer;
      var Special: boolean; Markup: TSynSelectedColor);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure procChangeState(info: string; pFinal: TPoint);
    procedure procInitScreen(const grilla: TtsGrid; fIni, fFin: integer);
    procedure procAddLine(HeightScr: integer);
    procedure procLlegoPrompt(prmLine: string; pIni: TPoint; HeightScr: integer);
    procedure procRefreshLine(const grilla: TtsGrid; fIni, HeightScr: integer);
    procedure procRefreshLines(const grilla: TtsGrid; fIni, fFin, HeightScr: integer);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure Timer1Timer(Sender: TObject);
  private
    hlTerm    : TResaltTerm;
    LlegoPrompt: boolean;   //bandera
    parpadPan0: boolean;   //para activar el parpadeo del panel0
    ticComRec : integer;   //contador para comando recurrente
    procedure DistribuirPantalla;
    procedure EnviarTxt(txt: string);
    procedure InicTerminal;
    procedure PosicionarCursor(HeightScr: integer);
  public
    proc   : TConsoleProc; //referencia al proceso actual
    ejecMac: boolean;   //indica que está ejecutando una macro
    ejecCom: boolean;   //indica que está ejecutando un comando (editor remoto, exp. remoto ...)
    procedure InicConect;
    procedure ActualizarInfoPanel0;
    procedure SetLanguage(lang: string);
  end;

var
  frmPrincipal: TfrmPrincipal;

implementation
{$R *.lfm}

{ TfrmPrincipal }

procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin
  ticComRec  := 0;

  ejecMac := false;
  hlTerm := TResaltTerm.Create(Self);  //crea resaltador

  //configura editor de terminal
  InicTerminal;  //configura ventana de terminal

  //configura editor de Panel de comando
  edPCom.OnKeyUp:=@edPComKeyUp;    //evento
  edPCom.OnKeyDown:=@edPComKeyDown; //evento

  //inicia proceso
  proc := TConsoleProc.Create(StatusBar1.Panels[1]);
  StatusBar1.OnDrawPanel:=@StatusBar1DrawPanel;

  //proc.OnRefreshAll:=@procRefreshEdit;
  proc.OnInitScreen :=@procInitScreen;
  proc.OnRefreshLine:=@procRefreshLine;
  proc.OnRefreshLines:=@procRefreshLines;
  proc.OnAddLine:=@procAddLine;

  proc.OnGetPrompt:=@procLlegoPrompt;
  proc.OnChangeState:=@procChangeState;

  AcTerDescon.Enabled:=false;  //Se supone que inicia siempre sin conectar

end;
procedure TfrmPrincipal.FormDestroy(Sender: TObject);
begin
  proc.Free;
  hlTerm.Free;
end;
procedure TfrmPrincipal.FormShow(Sender: TObject);
begin
  TranslateMsgs := true;  //activa la traducción en los mensajes
  SetLanguage('en');
  frmEditMacros.SetLanguage('en');
  //aquí ya sabemos que Config está creado. Lo configuramos
  Config.edTerm := edTerm;  //pasa referencia de editor.
  Config.edPCom := edPCom;  //pasa referencia de Panel de comando
  Config.edMacr := frmEditMacros.ed;
  Config.prTel := proc;     //pasa referencia a proceso

  Config.Iniciar();  //Inicia la configuración
  DistribuirPantalla; //ubica componentes
  //muestra dirección IP actual
  ActualizarInfoPanel0;

end;

procedure TfrmPrincipal.InicTerminal;
var
  SynMarkup: TSynEditMarkupHighlightAllCaret;  //para resaltar palabras iguales
begin
  edTerm.Highlighter := hlTerm;  //asigna resaltador

  //Inicia resaltado de palabras iguales
  SynMarkup := TSynEditMarkupHighlightAllCaret(edTerm.MarkupByClass[TSynEditMarkupHighlightAllCaret]);
  SynMarkup.MarkupInfo.FrameColor := clSilver;
  SynMarkup.MarkupInfo.Background := clBlack;
  SynMarkup.MarkupInfo.StoredName:='ResPalAct';  //para poder identificarlo

  SynMarkup.WaitTime := 250; // millisec
  SynMarkup.Trim := True;     // no spaces, if using selection
  SynMarkup.FullWord := True; // only full words If "Foo" is under caret, do not mark it in "FooBar"
  SynMarkup.IgnoreKeywords := False;

  //  edTerm.Font.Name:='Courier New';
 //  edTerm.Font.Size:=10;
 //resalta
  edTerm.Options:=[eoBracketHighlight];
  //Limita posición X del cursor para que no escape de la línea
  edTerm.Options := edTerm.Options + [eoKeepCaretX];
  //permite indentar con <Tab>
  edTerm.Options := edTerm.Options + [eoTabIndent];
  //trata a las tabulaciones como un caracter
  edTerm.Options2 := edTerm.Options2 + [eoCaretSkipTab];
  edTerm.OnSpecialLineMarkup:=@edTermSpecialLineMarkup;  //solo para corregir falla de resaltado de línea actual
end;
procedure TfrmPrincipal.edTermSpecialLineMarkup(Sender: TObject; Line: integer;
  var Special: boolean; Markup: TSynSelectedColor);
begin
//vacío
end;
procedure TfrmPrincipal.edPComSpecialLineMarkup(Sender: TObject; Line: integer;
  var Special: boolean; Markup: TSynSelectedColor);
begin
//vacío
end;

/////////////// Funciones para manejo de macros///////////////
procedure TfrmPrincipal.PosicionarCursor(HeightScr: integer);
//Coloca el cursor del editor, en la misma posición que tiene el cursor del
//terminal VT100 virtual.
var
  yvt: Integer;
begin
  yvt := edTerm.Lines.Count-HeightScr-1;  //calcula fila equivalente a inicio de VT100
  edTErm.CaretXY := Point(proc.term.curX, yvt+proc.term.CurY+1);
end;

procedure TfrmPrincipal.procLlegoPrompt(prmLine: string; pIni: TPoint; HeightScr: integer);
begin
  LlegoPrompt := true;  //activa bandera
//  yvt := edTerm.Lines.Count-HeightScr-1;  //calcula fila equivalente a inicio de VT100
//debugln('  llegoPrompt en:'+IntToStr(yvt + pIni.y+1));
end;
procedure TfrmPrincipal.procChangeState(info: string; pFinal: TPoint);
//Hubo un cambio de estado
begin
  AcTerConec.Enabled := proc.state = ECO_STOPPED;
  AcTerDescon.Enabled:= not (proc.state = ECO_STOPPED);
end;
procedure TfrmPrincipal.procInitScreen(const grilla: TtsGrid; fIni, fFin: integer);
var
  i: Integer;
begin
//  debugln('procAddLastLins: '+IntToStr(fIni)+','+IntToSTr(fFin));
  for i:=fIni to fFin do
    edTerm.Lines.Add(grilla[i]);
end;
procedure TfrmPrincipal.procRefreshLine(const grilla: TtsGrid; fIni, HeightScr: integer);
var
  yvt: Integer;
begin
//  debugln('procRefreshLine: '+IntToStr(fIni));
  yvt := edTerm.Lines.Count-HeightScr-1;  //calcula fila equivalente a inicio de VT100
  edTerm.Lines[yvt+fIni] := grilla[fIni];
  PosicionarCursor(HeightScr);
end;
procedure TfrmPrincipal.procRefreshLines(const grilla: TtsGrid; fIni, fFin, HeightScr: integer);
var
  yvt: Integer;
  f: Integer;
begin
//  debugln('procRefreshLines: '+IntToStr(fIni)+','+IntToSTr(fFin));
  yvt := edTerm.Lines.Count-HeightScr-1;  //calcula fila equivalente a inicio de VT100
  edTerm.BeginUpdate();
  for f:=fIni to fFin do
    edTerm.Lines[yvt+ f] := grilla[f];
  PosicionarCursor(HeightScr);
  edTerm.EndUpdate;
  edTerm.Refresh;  //para mostrar el cambio
end;
procedure TfrmPrincipal.procAddLine(HeightScr: integer);
var
  i: Integer;
begin
//  debugln('procAddLine: ');
  edTerm.BeginUpdate();
  if edTerm.Lines.Count> Config.fcConnection.maxLinTer then begin
    //hace espacio
    for i:= 1 to 100 do
      edTerm.Lines.Delete(0);   { TODO : Debe verificarse que no se deba eliminar tanto
como para dejar menos líneas que la que tiene el VT100 }
  end;
  edTerm.Lines.Add('');
//  edTerm.ExecuteCommand(ecEditorBottom,'', nil);  //mueve al final
  edTerm.EndUpdate;
  edTerm.ExecuteCommand(ecLineEnd,'', nil);  //mueve al final
end;

procedure TfrmPrincipal.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if edTerm.Focused then begin
     case Key of
     VK_RETURN:
       proc.Sendln('');  //se envía con la configuración de saltos
     VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END : begin
         //teclas direccionales
        //se interceptan, no se envían
       end;
     VK_TAB:
       if Shift = [ssCtrl] then begin  //Ctrl+Tab
         edPCom.SetFocus;  //pasa el enfoque
       end else begin
         proc.SendVT100Key(Key, Shift);  //envía
       end;
     else
       proc.SendVT100Key(Key, Shift);
     end;
//     debugln('KeyDown:');
   end else if edPCom.Focused then begin
     case Key of
     VK_TAB: if Shift = [ssCtrl] then begin  //Ctrl+Tab
         edterm.SetFocus;  //pasa el enfoque
       end;
     end;
   end;
end;
procedure TfrmPrincipal.FormKeyPress(Sender: TObject; var Key: char);
//Aaquí se interceptan el teclado a los controles
begin
  if edTerm.Focused then begin
    proc.Send(Key);
//    debugln('KeyPress:'+Key);
  end;
end;
procedure TfrmPrincipal.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  Config.escribirArchivoIni;  //guarda configuración
end;

procedure TfrmPrincipal.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  rpta: Byte;
begin
  if ejecMac then begin
    if MsgYesNo('En este momento, se está ejecutando una macro. ¿Detenerla?') = 1 then begin
      frmEditMacros.DetenerEjec;
      exit;
    end;
    canClose := false;  //cancela el cierre
  end;
  if proc.state <> ECO_STOPPED then begin
    rpta := MsgYesNoCancel('Hay una conexión abierta. ¿Cerrarla?');
    if rpta in [2,3] then begin  //cancelar
      canClose := false;  //cancela el cierre
      exit;    //sale
    end;
    if rpta = 1 then begin  //detener primero
      AcTerDesconExecute(nil);
      exit;
    end;
  end;
end;

procedure TfrmPrincipal.StatusBar1DrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
  if panel.Index = 0 then begin
    if ejecMac then begin
      if parpadPan0 then begin
//        StatusBar.Canvas.Font.Bold := true;
        StatusBar.Canvas.Font.Color:=clBlue;
        StatusBar.Canvas.Pen.Color := clWhite;
        StatusBar.Canvas.Brush.Color := clWhite;
        StatusBar.Canvas.Rectangle(Rect);
        StatusBar.Canvas.TextRect(Rect, 2 + Rect.Left, 2 + Rect.Top, 'Running macro');
      end else begin
//        StatusBar.Canvas.Font.Bold := true;
        StatusBar.Canvas.Font.Color:=clWhite;
        StatusBar.Canvas.Pen.Color := clBlue;
        StatusBar.Canvas.Brush.Color := clBlue;
        StatusBar.Canvas.Rectangle(Rect);
        StatusBar.Canvas.TextRect(Rect, 2 + Rect.Left, 2 + Rect.Top, 'Running macro');
      end;
    end else begin
      StatusBar.Canvas.Font.Color:=clBlack;
//      StatusBar.Canvas.Font.Bold := true;
      StatusBar.Canvas.TextRect(Rect, 2 + Rect.Left, 2 + Rect.Top, StatusBar1.Panels[0].Text);
    end;
  end;
  if panel.Index = 1 then proc.DrawStatePanel(StatusBar.Canvas, Rect);
end;

procedure TfrmPrincipal.Timer1Timer(Sender: TObject);
//Temporizador cada de 0.5 segundos
begin
  //muestra mensaje de ejecución
  if ejecMac then begin
    //fuerza refresco del panel
    parpadPan0 := not parpadPan0;  //para el parpadeo
    StatusBar1.InvalidatePanel(0,[ppText]);
  end;
end;

procedure TfrmPrincipal.InicConect;  //Inicia la conexión actual
begin
  //se supone que el proceso ya está configurado y listo para abrir
  proc.Open(Config.fcConnection.Command , '');  //lo abre
  if msjError<>'' then begin
    msgerr(msjError);
  end;
  ActualizarInfoPanel0;  //por si ha cambiado la conexión
end;
procedure TfrmPrincipal.ActualizarInfoPanel0;
//Actualiza el panel 0, con información de la conexión o de la ejecución de macros
begin
   StatusBar1.Panels[0].Text:='Proc: '+Config.fcConnection.Command;
   //refresca para asegurarse, porque el panel 0 está en modo gráfico
   StatusBar1.InvalidatePanel(0,[ppText]);
end;
procedure TfrmPrincipal.edPComKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [ssCtrl] then begin  //Ctrl pulsado
    if key = VK_RETURN then begin //envía línea actual
       Key := 0;
    end;
  end;
end;
procedure TfrmPrincipal.edPComKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [ssCtrl] then begin
    if key = VK_RETURN then begin //envía línea actual
      AcPCmEnvLinExecute(self);
      if edPCom.SelAvail then begin  //había selección
        //no se cambia la selección
      end else if edPCom.CaretY = edPCom.Lines.Count then begin
        //estamos en la última línea
        if edPCom.LineText = '' then exit; //no hay nada que enviar ni agregar
        edpCom.Lines.Add('');  //agrega una línea
        edPCom.ExecuteCommand(ecDown, '',nil);  //baja cursor
      end else begin
        //es una línea normal
        edPCom.ExecuteCommand(ecDown, '',nil);  //baja cursor
      end;
      Key := 0;  //para que ya no lo procese
    end;
  end;
end;
procedure TfrmPrincipal.DistribuirPantalla;
//Redistribuye los paneles de la pantalla
begin
  //primero quita alineamiento de componentes móviles
  PAnel2.Align:=alNone;
  Panel1.Align:=alNone;
  Splitter1.Align:=alNone;
  //alinea de acuerdo a TipAlineam
  Panel1.Align:=alLeft;
  Splitter1.Align:=alLeft;
  Panel2.Align:=alClient;
  if Panel1.Width > Trunc(0.9*self.Width) then Panel1.Width := Trunc(0.5*self.Width);
end;
/////////////////////// ACCIONES ////////////////////////
procedure TfrmPrincipal.AcArcSalirExecute(Sender: TObject);
begin
  Close;
end;
procedure TfrmPrincipal.AcVerEdiMacExecute(Sender: TObject);
begin
  frmEditMacros.Show;
end;
procedure TfrmPrincipal.EnviarTxt(txt: string);
//Envía un tetxo al terminal, aplicando el preprocesamiento si es necesario
begin
  proc.SendLn(txt);
end;
procedure TfrmPrincipal.AcPCmEnvLinExecute(Sender: TObject);
var
  lin: String;
begin
  if proc = nil then exit;
  if edPCom.SelAvail then begin  //hay selección
    //envía texto seleccionado
    EnviarTxt(edPCom.SelText);
  end else begin  //no hay selección, envía la línea actual
    lin := edPCom.LineText;  //línea actual
    EnviarTxt(lin);
  end;
end;
procedure TfrmPrincipal.acPCmEnvCtrCExecute(Sender: TObject); //Envía Ctrl+C
begin
  proc.Send(#3);
end;
procedure TfrmPrincipal.AcTerConecExecute(Sender: TObject);
begin
  InicConect;   //inicia conexión
end;
procedure TfrmPrincipal.AcTerDesconExecute(Sender: TObject); //desconectar
begin
   if not proc.Close then
     msgerr('No se puede cerrar el proceso actual.');
end;
procedure TfrmPrincipal.AcTerDetPrmExecute(Sender: TObject); //Detecta prompt
begin
  proc.AutoConfigPrompt;  //auto-detección
  config.fcConnection.DetecPrompt := proc.detecPrompt;
  config.fcConnection.prIni := proc.promptIni;
  config.fcConnection.prFin := proc.promptFin;
  config.fcConnection.TipDetec:=proc.promptMatch;
  config.fcConnection.OnUpdateChanges;  //actualiza resaltador y al mismo proceso
end;
procedure TfrmPrincipal.AcTerEnvCtrlCExecute(Sender: TObject);  //Ctrl+C
begin
  proc.Send(#3);
end;
procedure TfrmPrincipal.AcTerEnvCRExecute(Sender: TObject);
begin
  proc.Send(#13);
end;
procedure TfrmPrincipal.AcTerEnvLFExecute(Sender: TObject);
begin
  proc.Send(#10);
end;
procedure TfrmPrincipal.AcTerLimBufExecute(Sender: TObject);
//limpia la salida
begin
  edterm.ClearAll;
  proc.ClearTerminal;  //generará el evento OnInitLines()
end;
procedure TfrmPrincipal.AcHerCfgExecute(Sender: TObject);
begin
  Config.Configurar;
  ActualizarInfoPanel0;
end;

procedure TfrmPrincipal.SetLanguage(lang: string);
//Rutina de traducción
begin
  case lowerCase(lang) of
  'es': begin
      mnPanCom.Caption:='Panel de &Comandos';
      mnTerminal.Caption:='&Terminal';
      mnHerram.Caption:='&Herramientas';

      mnTerSend.Caption:='&Enviar';
      MenuItem72.Caption:='&Enviar';

      acPCmEnvCtrC.Caption := 'Enviar Ct&rl+C';
      AcTerConec.Caption := '&Conectar';
      AcTerDescon.Caption := '&Desconectar';
      AcTerLimBuf.Caption := '&Limpiar Buffer';
      AcTerDetPrm.Caption := 'Detectar &Prompt';
      AcTerEnvCtrlC.Caption := 'Enviar &Ctrl-C';
      AcHerCfg.Caption := 'Confi&guración...';
      dicClear;  //los mensajes ya están en español
    end;
  'en': begin
      mnPanCom.Caption:='&Command Panel';
      mnTerminal.Caption:='&Terminal';
      mnHerram.Caption:='&Tools';

      mnTerSend.Caption:='&Send';
      MenuItem72.Caption:='&Send';

      acPCmEnvCtrC.Caption := 'Send Ct&rl+C';
      AcTerConec.Caption := '&Connect';
      AcTerDescon.Caption := '&Disconnect';
      AcTerLimBuf.Caption := '&Clean Buffer';
      AcTerDetPrm.Caption := 'Detect &Prompt';
      AcTerEnvCtrlC.Caption := 'Send &Ctrl-C';
      AcHerCfg.Caption := 'Confi&gure...';
      //traducción
      dicSet('Hay una conexión abierta. ¿Cerrarla?','There is an opened connection. Close?');
      dicSet(' - Archivo: ', ' - File: ');
      dicSet('En este momento, se está ejecutando una macro. ¿Detenerla?',
             'There is a Macro runnig. Stop it?');
      dicSet('Error detectando el prompt del comando. ','Error detecting prompt.');
    end;
  end;
end;

end.

