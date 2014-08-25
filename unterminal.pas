{
UnTerminal 0.2
===============
Por Tito Hinostroza

* Se incluye la propiedad "detParcial", para permitir detectar el prompt cuando
se encuentra parcialmente en una línea.
* Se cambia el nombre de algunas propiedades y métodos al ingles.
* Se elimina TPromptDetec. No se usaba.
* Se cambia el nombre de lso métodos Enviar() y EnviarCom() a Send() y SendLn()
respectivamente.

Descripción
===========
Derivada de la unidad ConexOraSQlP 0.5. Esta unidad permite procesar la entrada y
salida de un cliente de telnet, que trabaja como una consola.
Permite además detectar el prompt y el estado de "ocupado" y "listo".

Para conectarse mediante un proceso, se debe crear una instancia de TConexProc, y seguir la
secuencia de conexión:
  p := TConexProc.Create(StatusBar1.Panels[1]);  //Crea conexión
  ...
  p.Free.

Opcionalmente se le puede pasar la referencia a un panel de una barra de estado, para
que se muestren íconos que indiquen el estado de la conexión (poner en NIL si no se
usará). En este caso se debe manejar el evento OnDrawPanel() de la Barra de estado:

 StatusBar1.OnDrawPanel:=@SBDrawPanel;
 ...
 procedure Form1.SBDrawPanel(StatusBar:TStatusBar; Panel:TStatusPanel; const Rect:TRect);
 begin
  if panel.Index = 1 then q.DibPanelEstado(StatusBar.Canvas, Rect);
 end;

 La conexión se maneja por estados. Una secuencia típica de estados al iniciar una
 conexión exitosa es:
 ECO_DETENIDO -> ECO_CONECTANDO -> ECO_OCUPADO -> ECO_LIBRE

 Una conexión con error:
 ECO_DETENIDO -> ECO_CONECTANDO -> ECO_LIBRE -> ECO_ERROR_CON

 Una consulta cualquiera:
 ECO_LIBRE -> ECO_OCUPADO -> ECO_LIBRE


Está unidad está pensada para ser usada en conexiones lentas, y con volúmenes
considerables de datos, por ello se maneja la llegada de datos completamente por eventos.

Para el manejo de la pantalla usa un terminal VT100 sencillo, de la unidad TermVT. Sin
embargo, esta unidad ha sido diseñada para poder recibir el texto del VT100, en un
TStringList, y poder registrar permanentemente el contenido.

 Para configurar un editor para mostrar la salida del proceso, se debe usar los eventos:
 OnInitLines(), OnRefreshLine(), OnRefreshLines() y OnAddLine():


  proc.OnInitLines:=@procInitLines;
  proc.OnRefreshLine:=@procRefreshLine;
  proc.OnRefreshLines:=@procRefreshLines;
  proc.OnAddLine:=@procAddLine;

procedure TfrmPrincipal.procInitLines(const grilla: TtsGrid; fIni, fFin: integer);
begin
  for i:=fIni to fFin do SynEdit1.Lines.Add(grilla[i]);
end;
procedure TfrmPrincipal.procRefreshLine(const grilla: TtsGrid; fIni, HeightScr: integer);
begin
  yvt := SynEdit1.Lines.Count-HeightScr-1;
  SynEdit1.Lines[yvt+fIni] := grilla[fIni];
end;
procedure TfrmPrincipal.procRefreshLines(const grilla: TtsGrid; fIni, fFin, HeightScr: integer);
begin
  yvt := SynEdit1.Lines.Count-HeightScr-1;  //calcula fila equivalente a inicio de VT100
  for f:=fIni to fFin do SynEdit1.Lines[yvt+ f] := grilla[f];
end;
procedure TfrmPrincipal.procAddLine;
begin
  SynEdit1.Lines.Add('');
end;

El evento OnInitLines(), es llamado solo una vez al inicio para dimensionar el StringList,
de modo que pueda contener a todas las líneas del terminal VT100.

En principio, solo debería mandarse un comando por Enviar() porque, enviar más
de un comando podría hacer que el proceso pase repetidamente
por la las fases ECO_OCUPADO -> ECO_LIBRE -> ECO_OCUPADO ... -> ECO_LIBRE.

Los datos de salida que van llegando del proceso, no se guardan completamente en la
clase. Solo se mantienen las líneas de trabajo del terminal VT100 en el objeto "term".
Así se ahorra memoria, porque por lo general, el texto de salida está destinado a ser
almacenado en algún otro control como un editor de texto o una grilla.
Es responsabilidad del programador, limitar el tamaño de los datos almacenados.

                                                    Por Tito Hinostroza  27/06/2014
 }
unit UnTerminal;

{$mode objfpc}{$H+}
interface
uses  Classes, SysUtils, Process, ExtCtrls, Dialogs, Graphics, ComCtrls,
     LCLProc, types, TermVT;
const
  TAM_BLOQUE = 2048;    //Tamaño de bloque de lectura de salida de proceso

type

TEstadoCon = (
   ECO_CONECTANDO, //Iniciado y Conectando a Oracle
   ECO_ERROR_CON,  //Iniciado y Con error de conexión
   ECO_OCUPADO,    //Iniciado y conectado, pero con consulta pendiente.
   ECO_LIBRE,      //Iniciado y conectado, libre para aceptar consultas.
   ECO_DETENIDO    //Proceso no iniciado. Puede que haya datos pendientes en el "buffer"
);

{Evento. Pasa la cantidad de bytes que llegan y la columna y fila final de la matriz Lin[] }
TEvProcState = procedure(nDat: integer; pFinal: TPoint) of object;
TEvLlegoPrompt = procedure(prompt: string; pIni: TPoint; HeightScr: integer) of object;
TEvRecSysComm = procedure(info: string; pIni: TPoint) of object;

TEvRefreshAll  = procedure(const grilla: TtsGrid; linesAdded: integer) of object;
TEvAddLines    = procedure(const grilla: TtsGrid; fIni, fFin: integer) of object;
TEvRefreshLine = procedure(const grilla: TtsGrid; fIni, HeightScr: integer) of object;
TEvRefreshLines= procedure(const grilla: TtsGrid; fIni, fFin, HeightScr: integer) of object;
TEvOnAddLine   = procedure(HeightScr: integer) of object;

//Clase que define un proceso
{ TConexProc }

TConexProc = class
private
  lastState : TEstadoCon;  //Estado anterior
  FAnchoTerminal: integer;
  function ContienePrompt(const linAct, prIni, prFin: string): integer;
  function EsPrompt(const cad: string): boolean;
  function GetAnchoTerminal: integer;
  procedure SetAnchoTerminal(AValue: integer);
public
  progPath  : string;      //ruta del porgrama a lanzar
  progParam : string;      //parámetros del programa
  state    : TEstadoCon;  //Estado de la conexión

  detecPrompt: boolean;    //activa la detección de prompt.
  detParcial :boolean;     //permite que la detección sea parcial
  prIni, prFin: string;    //cadena inicial, internedia y final del prompt
  txtEstado : string;      //cadena con texto sobre el estado de la conexión
  HayPrompt : boolean;     //bandera, indica si se detectó el prompt en la última línea
  HuboError : boolean;     //Indica que hubo error en la última consulta
  cadError  : string;      //guarda el mensaje de error
  pErr      : TPoint;      //posición del error;
  panel     : TStatusPanel; //referencia a panel para nostrar estado
  term      : TTermVT100;   //Terminal

  //eventos de cambio de estado
  OnConectando : TEvProcState;    //indica que se inicia el proceso y trata de conectar
  OnErrorConex : TEvProcState;    //indica que se detectó un mensaje de error al conectar
  OnOcupado    : TEvProcState;    //indica que está esperando prompt
  OnDetenido   : TEvProcState;    //indica que se terminó el proceso
  OnLlegoPrompt: TEvLlegoPrompt;  //indica que llegó el prompt
  OnChangeState: TEvProcState;    //cambia de estado
  OnRecSysComm : TEvRecSysComm;   {indica que llegó información del sistema remoto (usuario,
                                  directorio actual, etc) Solo para conex. Telnet}
  //eventos de control
  OnHuboError  : TEvProcState;    //indica que hubo mensaje de error
  //eventos de llegada de datos
  OnRefreshAll  : TEvRefreshAll;   //Usado para refresvar todo el contenido del terminal
  OnInitLines   : TEvAddLines;     //indica que se debe agregar líneas de texto
  OnRefreshLine : TEvRefreshLine;  //indica que se deben refrescar una línea
  OnRefreshLines: TEvRefreshLines; //indica que se deben refrescar ese grupo de líneas
  OnAddLine     : TEvOnAddLine;    //inidca que se debe agregar una línea a la salida

  procedure Open(progPath0, progParam0: string); //Inicia conexión
  function Close: boolean;    //Termina la conexión
  procedure LimpiarTerminal;
  property AnchoTerminal: integer read GetAnchoTerminal write SetAnchoTerminal;
  procedure Send(const txt: string);
  procedure SendLn(txt: string; saltoUNIX: boolean=false);  //Envía datos por el "stdin"
  //control de barra de estado
  procedure RefPanelEstado;
  procedure DibPanelEstado(c: TCanvas; const Rect: TRect);
  function LeeUltLinea: string;
  //constructor y destructor
  constructor Create(PanControl: TStatusPanel);     //Constructor
  destructor Destroy; override;   //Limpia los buffers
  procedure termAddLine;
  procedure termRefreshLine(fil: integer);
  procedure termRefreshLines(fIni, fFin: integer);
  procedure termRefreshScreen(linesAdded: integer);
  procedure termRecSysComm(info: string);
private
  p       : TProcess;   //el proceso a manejar
  bolsa   : array[0..TAM_BLOQUE] of char;  //buffer para almacenar salidas(tiene un caracter más)
  nLeidos : LongInt;
  lstTmp : TStringList;
  HayError  : boolean;  //Indica que se detectó un error en la última exploración

  clock    : TTimer;
  cAnim    : integer;  //contador para animación de ícono de state
  angA: integer;       //contador para animación de ícono de state
  procedure RefresConexion(Sender: TObject);  //Refresca la conexión
  function LeeSalidaProc:boolean;
  function CambiarEstado(estado0: TEstadoCon): boolean;  //Cambia el state actual
end;

implementation
//uses FormConfig;   //se necesita acceder a las propiedades de prompt
const
   NUM_LIN_ATRAS = 12;  {número de línea a explorar, hacia atrás, para buscar mensajes de
                        error}

function Explode(delimiter:string; str:string):TStringDynArray;
var
   p,cc,dsize:integer;
begin
   cc := 0;
   dsize := length(delimiter);
   while true do begin
     p := pos(delimiter,str);
     if p > 0 then begin
       inc(cc);
       setlength(result,cc);
       result[cc-1] := copy(str,1,p-1);
       delete(str,1,p+dsize-1);
     end else break;
   end;
   inc(cc);
   setlength(result,cc);
   result[cc-1] := str;
end;

function TConexProc.CambiarEstado(estado0: TEstadoCon): boolean;
{Cambia el estado de la conexión  y actualiza un panel con información sobre el estado}
begin
  lastState := state;  //pasa state actual a anterior
  state := estado0;    //fija state actual
  if lastState <> state then begin   //indica si hubo cambio
    //hubo cambio de state
    Result := true;
    case state of
    ECO_CONECTANDO: begin
        txtEstado := 'Conectando';
        RefPanelEstado;  //fuerza a redibujar panel con el nuevo state
        if OnConectando<>nil then OnConectando(0,term.CurXY);
      end;
    ECO_ERROR_CON: begin
        txtEstado := 'Error en conexión';
        RefPanelEstado;  //fuerza a redibujar panel con el nuevo state
        if OnErrorConex <> nil then OnErrorConex(nLeidos, pErr);
      end;
    ECO_OCUPADO: begin
        txtEstado := 'Ocupado';
        RefPanelEstado;  //fuerza a redibujar panel con el nuevo state
        if OnOcupado <> nil then OnOcupado(nLeidos, term.CurXY);
      end;
    ECO_LIBRE: begin
        txtEstado := 'Disponible';
        RefPanelEstado;  //fuerza a redibujar panel con el nuevo state
        if OnLlegoPrompt <> nil then OnLlegoPrompt('', term.CurXY, term.height);
      end;
    ECO_DETENIDO: begin
        txtEstado := 'Detenido';
        RefPanelEstado;  //fuerza a redibujar panel con el nuevo state
        if OnDetenido <> nil then OnDetenido(nLeidos, term.CurXY);
      end;
    end;
    if OnChangeState<>nil then OnChangeState(nLeidos, term.CurXY);
  end;
end;

function TConexProc.LeeUltLinea: string;
//Devuelve la línea donde se encuentra el cursor. Salvo que haya, saltos en el cursor,
//devolverá siempre los últimos caracteres recibidos.
begin
  Result := term.buf[term.CurY];
end;

procedure TConexProc.RefPanelEstado;   //Refresca el estado del panel del StatusBar asociado.
begin
  if panel = nil then exit;  //protección
  //fuerza a llamar al evento OnDrawPanel del StatusBar
  panel.StatusBar.InvalidatePanel(panel.Index,[ppText]);
  //y este a us vez debe llamar a DibPanelEstado()
end;
procedure TConexProc.DibPanelEstado(c: TCanvas; const Rect: TRect);
{Dibuja un ícono y texto, de acuerdo al estado de la conexión. Este código está pensado
 para ser usado en el evento OnDrawPanel() de una barra de estado}
var
  p1,p2: Tpoint;
  procedure Torta(c: Tcanvas; x1,y1,x2,y2: integer; a1,a2: double);  //dibuja una torta
  var x3,y3,x4,y4: integer;
      xc, yc: integer;
  begin
    xc := (x1+x2) div 2; yc := (y1+y2) div 2;
    x3:=xc + round(1000*cos(a1));
    y3:=yc + round(1000*sin(a1));
    x4:=xc + round(1000*cos(a2));
    y4:=yc + round(1000*sin(a2));
    c.pie(x1,y1,x2,y2,x3,y3,x4,y4);
  end;
  procedure Circulo(c: Tcanvas; xc,yc: integer; n: integer);  //dibuja un círculo
  const r = 2;
  begin
    case n of
    5: c.Brush.Color:=$B0FFB0;
    4: c.Brush.Color:=$40FF40;
    3: c.Brush.Color:=$00E000;
    2: c.Brush.Color:=$00CC00;
    1: c.Brush.Color:=$00A000;
    0: c.Brush.Color:=$008000;
    else
     c.Brush.Color:=clWhite;
    end;
    c.Pen.Color:=c.Brush.Color;
    c.Ellipse(xc-r, yc-r+1, xc+r, yc+r+1);
  end;
begin
  if state in [ECO_CONECTANDO, ECO_OCUPADO] then begin  //estados de espera
    c.Pen.Width:=0;  //restaura ancho
    Circulo(c,Rect.Left+5,Rect.Top+5, angA);
    inc(angA);if angA>7 then angA:=0;
    Circulo(c,Rect.Left+9,Rect.Top+3, angA);
    inc(angA);if angA>7 then angA:=0;
    Circulo(c,Rect.Left+13,Rect.Top+5, angA);
    inc(angA);if angA>7 then angA:=0;
    Circulo(c,Rect.Left+15,Rect.Top+9, angA);
    inc(angA);if angA>7 then angA:=0;
    Circulo(c,Rect.Left+13,Rect.Top+13, angA);
    inc(angA);if angA>7 then angA:=0;
    Circulo(c,Rect.Left+9,Rect.Top+15, angA);
    inc(angA);if angA>7 then angA:=0;
    Circulo(c,Rect.Left+5,Rect.Top+13, angA);
    inc(angA);if angA>7 then angA:=0;
    Circulo(c,Rect.Left+3,Rect.Top+9, angA);
    inc(angA);if angA>7 then angA:=0;

  end else if state = ECO_ERROR_CON then begin //error de conexión
    //c´rculo rojo
    c.Brush.Color:=clRed;
    c.Pen.Color:=clRed;
    c.Ellipse(Rect.Left+2, Rect.Top+2, Rect.Left+16, Rect.Top+16);
    //aspa blanca
    c.Pen.Color:=clWhite;
    c.Pen.Width:=2;
    p1.x := Rect.Left+5; p1.y := Rect.Top+5;
    p2.x := Rect.Left+12; p2.y := Rect.Top+12;
    c.Line(p1,p2);
    p1.x := Rect.Left+5; p1.y := Rect.Top+12;
    p2.x := Rect.Left+12; p2.y := Rect.Top+5;
    c.Line(p1,p2);
  end else if state = ECO_LIBRE then begin //disponible
    c.Brush.Color:=clGreen;
    c.Pen.Color:=clGreen;
    c.Ellipse(Rect.Left+2, Rect.Top+2,Rect.Left+16, Rect.Top+16);
    c.Pen.Color:=clWhite;
    c.Pen.Width:=2;
    p1.x := Rect.Left+6; p1.y := Rect.Top+7;
    p2.x := Rect.Left+8; p2.y := Rect.Top+12;
    c.Line(p1,p2);
    p1.x := Rect.Left+12; p1.y := Rect.Top+5;
//    p2.x := Rect.Left+12; p2.y := Rect.Top+5;
    c.Line(p2,p1);
  end else begin            //estados detenido
    //círculo gris
    c.Brush.Color:=clGray;
    c.Pen.Color:=clGray;
    c.Ellipse(Rect.Left+2, Rect.Top+2, Rect.Left+16, Rect.Top+16);
    //aspa blanca
    c.Pen.Color:=clWhite;
    c.Pen.Width:=2;
    p1.x := Rect.Left+5; p1.y := Rect.Top+5;
    p2.x := Rect.Left+12; p2.y := Rect.Top+12;
    c.Line(p1,p2);
    p1.x := Rect.Left+5; p1.y := Rect.Top+12;
    p2.x := Rect.Left+12; p2.y := Rect.Top+5;
    c.Line(p1,p2);
  end;
  c.Font.Color:=clBlack;
  c.TextRect(Rect, 19 + Rect.Left, 2 + Rect.Top, txtEstado);
end;

function TConexProc.GetAnchoTerminal: integer;
//Devuelve el ancho del terminal
begin
  Result := term.width;
end;
procedure TConexProc.SetAnchoTerminal(AValue: integer);
//Fija el ancho del terminal
begin
  if term.width=AValue then Exit;
  term.width := AValue;
end;

procedure TConexProc.Open(progPath0, progParam0: string);
//Inicia el proceso y verifica si hubo error al lanzar el proceso.
begin
  term.Clear;
  //Inicia la salida de texto, refrescando todo el terminal
  if OnInitLines<>nil then OnInitLines(term.buf, 1, term.height);

  progPath := progPath0;
  progParam := progParam0;  //guarda cadena de conexión

  if p.Running then p.Terminate(0);  { TODO : ¿No debería mandar CTRL-C y EXIT si la conexión está buena? }
  // Vamos a lanzar el compilador de FreePascal
  p.CommandLine := progPath + ' ' + progParam;
  // Definimos comportamiento de 'TProccess'. Es importante direccionar los errores.
  p.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
  //ejecutamos
  CambiarEstado(ECO_CONECTANDO);
  try
    p.Execute;
    if not p.Running then begin
       //Falló al iniciar
       CambiarEstado(ECO_DETENIDO);
       Exit;
    end;
    //Se inició, y esperamos a que RefresConexion() procese los datos recibidos
  except
    if p.CommandLine = '' then
      cadError := 'No se especificó aplicativo para conexión.'
    else
      cadError := 'Fallo al iniciar aplicativo: '+ p.CommandLine;
    HuboError:=true;   //marca error.
    CambiarEstado(ECO_ERROR_CON); //geenra evento
  end;
end;
procedure TConexProc.LimpiarTerminal;
{Reinicia el terminal iniciando en (1,1) y limpiando la grilla}
begin
  term.Clear;   //limpia grilla y reinicia cursor
  //genera evento para reiniciar salida
  if OnInitLines<>nil then OnInitLines(term.buf, 1, term.height);
end;

function TConexProc.ContienePrompt(const linAct, prIni, prFin: string): integer;
//Verifica si una cadena contiene al prompt, usando los valroes de cadena inicial (prIni)
//y cadena final (prFin). LA veriifcación se hace siempre desde el inicio de la cadena.
//Si la cadena contiene al prompt, devuelve la longitud del prompt hallado, de otra forma
//devuelve cero.
//Si la salida del proceso va a ir a un editor con resaltador de sintaxis, esta rutina debe
//ser similar a la del resaltador para que haya sincronía en lo que se ve. No se separra esta
//rutina en otra unidad para que esta unidad no tenga dependencias y se pueda usar como
//librería. Además la detección del prompt para el proceso, es diferente de la deteción
//para un resaltador de sintaxis.
var
  l: Integer;
  pd: SizeInt;
begin
   Result := 0;   //valor por defecto
   l := length(prIni);
   if (l>0) and (copy(linAct,1,l) = prIni) then begin
     //puede ser
     if prFin = '' then begin
       //no hace falta validar más
       Result := l;  //el tamaño del prompt
       exit;    //no hace falta explorar más
     end;
     //hay que validar la existencia del fin del prompt
     pd :=pos(prFin,linAct);
     if pd>0 then begin  //encontró
       Result := pd+length(prFin)-1;  //el tamaño del prompt
       exit;
     end;
   end;
end;
function TConexProc.EsPrompt(const cad: string): boolean;
//Indica si la línea dada, es el prompt, de acuerdo a los parámetros dados. Esta función
//se pone aquí, porque aquí se tiene fácil acceso a las configuraciones del prompt.
var
  n: Integer;
begin
  if detecPrompt then begin  //si hay detección activa
    n := ContienePrompt(cad, prIni, prFin);
    Result := (n>0) and  (n = length(cad));
  end else begin
    Result := false;
  end;
end;
function TConexProc.LeeSalidaProc: boolean;
{Verifica la salida del proceso. Si llegan datos los pasa a "term" y devuelve TRUE.
Lee en un solo bloque si el tamaño de los datos, es menor que TAM_BLOQUE, en caso
contrario lee varios bloques. Actualiza "nLeidos", "HayPrompt" y "HayError". }
var nDis : longint;
    nBytes : LongInt;
begin
//  pIni := LeePosFin;
  Result := false;        //valor por defecto
  nLeidos := 0;
  HayPrompt := false;
  if P.Output = nil then exit;  //no hay cola
  repeat
    //vemos cuantos bytes hay "en este momento"
    nDis := P.Output.NumBytesAvailable;
    if nDis = 0 then break;  //sale del lazo
    if nDis < TAM_BLOQUE then  begin
      //leemos solo los que hay, sino se queda esperando
      nBytes := P.Output.Read(bolsa, nDis);
      bolsa[nBytes] := #0;   //marca fin de cadena
      term.AddData(@bolsa);  //puede generar eventos
      nLeidos += nBytes;
    end else begin
      //leemos bloque de TAM_BLOQUE bytes
      nBytes := P.Output.Read(bolsa, TAM_BLOQUE);
      bolsa[nBytes] := #0;   //marca fin de cadena
      term.AddData(@bolsa);  //puede generar eventos
      nLeidos += nBytes;
    end;
    {aquí también se puede detetar el prompt, con más posibilidad de detectar los
    posibles prompt intermedios}
    Result := true;    //hay datos
  until not P.Running or (nBytes = 0);
  if not Result then exit;
  {Terminó de leer, aquí detectamos el prompt, porque es casi seguro que llegue
   al final de la trama.
  Se Detecta el prompt, viendo que la línea actual, sea realmente el prompt. Se probó
  viendo si la línea actual empezaba con el prompt, pero daba casos (sobretodo en
  conexiones lentas) en que llegaba una trama con pocos cracteres, de modo que se
  generaba el evento de llegada de prompt dos veces (tal vez más) en una misma línea}
  if EsPrompt(term.buf[term.CurY]) then
    HayPrompt:=true;
  {Se pone fuera, la rutina de detcción de prompt, porque también debe servir al
  resaltador de sintaxis}
end;
procedure TConexProc.RefresConexion(Sender: TObject);
//Refresca el estado de la conexión. Verifica si hay datos de salida del proceso.
//Pone "HayError" en true si capturó alguna línea con el mensaje "ORA-*" o "SP2-*".
begin
  if state = ECO_DETENIDO then Exit;  //No está corriendo el proceso.
  HayError := false;
  if p.Running then begin
     //Se está ejecutando
     if LeeSalidaProc then begin //actualiza "HayError" y "HayPrompt"
        if state in [ECO_LIBRE, ECO_OCUPADO] then begin
           //Se tiene conexión a la base de datos
           if HayError then begin   //verifica error
              HuboError:=true; {permite verificar si hubo error al llegar el prompt,
                                porque "HayError", se pierde con cada exploración.}
              if OnHuboError<>nil then OnHuboError(nLeidos, pErr);//
           end;
           if HayPrompt then begin
              CambiarEstado(ECO_LIBRE);
           end else begin
              CambiarEstado(ECO_OCUPADO);
           end;
        end else begin
           //Se está esperando conseguir la conexión (state = ECO_CONECTANDO)
           if HayError then begin
              HuboError:=true; {permite verificar si hubo error al llegar el prompt,
                               porque "HayError", se pierde con cada exploración.}
              CambiarEstado(ECO_ERROR_CON);
           end;
           if HayPrompt then begin
              //se consiguió conectar por primera vez
//              state := ECO_LIBRE;  //para que pase a ECO_OCUPADO
//              SendLn(COMAN_INIC); //envía comandos iniciales (lanza evento Ocupado)
              CambiarEstado(ECO_LIBRE);
           end;
        end;
     end;
  end else begin //terminó
     CambiarEstado(ECO_DETENIDO);
     LeeSalidaProc; //lee por si quedaban datos en el buffer
  end;
  //actualiza animación
  inc(cAnim);
  if (cAnim mod 4) = 0 then begin
    if state in [ECO_CONECTANDO, ECO_OCUPADO] then begin  //estados de espera
      inc(angA);if angA>7 then angA:=0;
      RefPanelEstado;
    end;
    cAnim := 0;
  end;
end;
procedure TConexProc.Send(const txt: string);
{Envía una cadena como como flujo de entrada al proceso.
Es importante agregar el caracter #13#10 al final. De otra forma no se leerá el "stdin"}
begin
  if p = NIL then exit;
  if not p.Running then exit;
  p.Input.Write(txt[1], length(txt));  //pasa el origen de los datos
  //para que se genere un cambio de state aunque el comando sea muy corto
  if state = ECO_LIBRE then CambiarEstado(ECO_OCUPADO);
end;
procedure TConexProc.SendLn(txt: string; saltoUNIX: boolean = false);
{Envía un comando al proceso. Incluye el salto de línea al final de la línea.}
begin
  txt+=#13#10;
  if saltoUNIX then
    txt := StringReplace(txt,#13#10,#10,[rfReplaceAll]);
  Send(txt);
end;

procedure TConexProc.termRefreshScreen(linesAdded: integer);
//EVento que indica que se debe refrescar la pantalla
begin
  if OnRefreshAll<>nil then OnRefreshAll(term.buf, linesAdded);  //evento
end;
procedure TConexProc.termRefreshLine(fil: integer);
//Se pide refrescar una línea.
//"term.height - fil" es la distancia al final de la pantalla VT100
begin
  if OnRefreshLine<> nil then OnRefreshLine(term.buf, fil, term.height);
end;
procedure TConexProc.termRefreshLines(fIni, fFin: integer);
//Se pide refrescar un rango de líneas
begin
  if OnRefreshLines<> nil then OnRefreshLines(term.buf, fIni, fFin, term.height);
end;
procedure TConexProc.termAddLine;
//Se pide agregar líneas a la salida
begin
  if OnAddLine<>nil then OnAddLine(term.height);
end;
procedure TConexProc.termRecSysComm(info: string);
//Se ha recibido comando con información del sistema.
begin
  //se indica que se recibe información del sistema
  if OnRecSysComm<>nil then OnRecSysComm(info, term.CurXY);
  //Se puede asumir que llega el prompt pero no siempre funciona
//  HayPrompt := true;    //marca bandera
//  CambiarEstado(ECO_LIBRE);  //cambia el state
end;

function TConexProc.Close: boolean;
//Cierra la conexión actual. Si hay error devuelve False.
var c: integer;
begin
  Result := true;
  //verifica el proceso
  if p.Running then p.Terminate(0);  { TODO : ¿No debería mandar CTRL-C y EXIT si la conexión está buena? }
  //espera hasta 100 mseg
  c := 0;
  while p.Running and (c<20) do begin
    sleep(5);
    inc(c);
  end;
  if c>= 20 then exit(false);  //sale con error
  //Pasa de Runnig a Not Running
  CambiarEstado(ECO_DETENIDO);
  //Puede que quede datos en el "stdout"
  LeeSalidaProc; //lee lo que queda y actualiza "HayError"
end;
constructor TConexProc.Create(PanControl: TStatusPanel);
//Constructor
begin
  progPath := '';  //ruta por defecto
  lstTmp := TStringList.Create;   //crea lista temporal
  p := TProcess.Create(nil); //Crea proceso
  CambiarEstado(ECO_DETENIDO);  //state inicial. Genera el primer evento
  HayError := false;         //Inicia bandera de error
  //configura temporizador
  clock := TTimer.Create(nil);
  clock.interval:=50;  {100 es un buen valor, pero para mayor velocidad de recepción, se
                        puede usar 50 milisegundos}
  clock.OnTimer := @RefresConexion;
  panel := PanControl;  //inicia referencia a panel
  if panel<> nil then
    panel.Style:=psOwnerDraw;  //configura panel para dibujarse por evento
  detecPrompt := true;    //activa detección de prompt por defecto
  term := TTermVT100.Create; //terminal
  term.OnRefreshAll:=@termRefreshScreen;
  term.OnRefreshLine:=@termRefreshLine;
  term.OnRefreshLines:=@termRefreshLines;
  term.OnAddLine:=@termAddLine;
  term.OnRecSysComm:=@termRecSysComm; {usaremos este evento para detectar la llegada
                                      del prompt}
end;
destructor TConexProc.Destroy;
//Destructor
begin
  term.Free;
  clock.Free;  //destruye temporizador
  //verifica el proceso
  if p.Running then p.Terminate(0);
  //libera objetos
  FreeAndNIL(p);
  lstTmp.Free;    //limpia
end;

end.
