{
XpresBas
========
Por Tito Hinostroza.

Rutinas básicas del framework
Aquí están definidas las rutinas de manejo de error y los contextos de entrada
de Xpres.
Para ver los cambios en esta versión, revisar el archivo cambios.txt.
 }

unit XpresBas;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fgl,
  Forms, LCLType,  //Para mostrar mensajes con Application.MessageBox()
  SynEditHighlighter, SynFacilHighlighter, SynFacilBasic;


type
  //Tipos de contextos
  tTypCon = (
    TC_ARC,      //contexto de tipo archivo
    TC_TXT);     //contexto de tipo texto

  TContext = class;

  {Posición dentro de un contexto. A diferencia de "Tcontexto", es un registro y siempre
   guardará una copia permanente. No guarda el contenido del contexto}
  TPosCont = record
//    arc   : String  ;      //Nombre de archivo
//    nlin  : LongInt ;      //Número de líneas
    fCon  : TContext;      //Referencia al Contexto
    fPos  : TFaLexerState;  //Posición (estado) en el contexto
  End;

  { TPError }
{Define al objeto TPError, el que se usa para tratar los errores del compilador. Solo se
 espera que haya uno de estos objetos, por eso se ha declarado como OBJECT}
  TPError = object
  private
    numER : Integer;   //codigo de error
    cadER :  String;   //cadena de error
    arcER :  String;   //nombre de archivo que origino el error
    fil : Longint;     //número de línea del error
    col : Longint;     //número de columna del error
  public
    NombPrograma: string;  //Usado para poner en el encabezado del mensaje
    procedure IniError;
    procedure Clear;
    procedure GenError(msje: String; archivo: String; nlin: LongInt);
    procedure Generror(msje: String; ctx: TContext);
    function TxtError: string;
    function TxtErrorRC: string;
    procedure Show;
    function ArcError: string;
    function nLinError: longint;
    Function nColError: longint;
    function HayError: boolean;
  end;

  { TContext }
  {Estructura que define a un objeto contexto. Un contexto es un objeto que sirve como
  entrada de datos, en donde se puede cargar un texto, y luego leerlo token por token
  de forma simple.}
  TContext = class
  private
    fLexerState: TFaLexerState;  //almacenamiento temporal
    function getRow: integer;
    function getCol: integer;
  public
    tip      : tTypCon;
    arc      : String;      //Nombre de archivo. En caso de que el contexto corresponda a uno.
    nlin     : LongInt;     //Número de líneas del Contexto
    intLines : TStringList; {Líneas de texto. Se usa como almacenamiento interno, cuando
                             no se especifica algún TStringList externo. Se crea siempre}
    curLines : TStrings;    //Referencia al StringList actual, el que se explora.
    lex      : TSynFacilSyn;  //analizador léxico
    //posición del cursor actual
    property row: integer read getRow;
    property col: integer read getCol;
    function Token: string;  inline;  //Token actual
    function TokenType: TSynHighlighterAttributes; inline;  //Tipo de token actual
    function Block: TFaSynBlock; inline;
    //Métodos de lectura
    Function IniCont:Boolean;
    Function Eof:Boolean;
    procedure SkipWhites;
    procedure SkipWhitesNoEOL;

    function Next: boolean;     //Pasa al siguiente token
    function CurLine: string;   //Retorna la línea actual
    function ReadSource: string;    //Lee el contenido del contexto en un string
    //Control de la posición actual
    procedure SetStartPos;       //Posiciona al inicio del contexto
    procedure SaveLexerState;    //Guarda el estado actual del lexer
    procedure RestoreLexerState; //Restaura el estado actual del lexer
  public    //Métodos de inicialización
    procedure DefSyn(lex0: TSynFacilSyn);  //Fija la sintaxis del lexer
    procedure SetSource(txt : string);   //Fija el contenido del contexto con cadena
    procedure SetSource(lins: Tstrings; MakeCopy: boolean = false); //Fija contenido a partir de una lista
    procedure SetSourceF(file0: string);  //Fija el contenido del contexto con archivo
    constructor Create;
    destructor Destroy; override;
  end;

  //Define una lista de Contextos
  TListaCont = specialize TFPGObjectList<TContext>;


  { TContexts }
  //Extructura para manejar diversas fuentes de datos de contexto
  TContexts = class
  private
    lex    : TSynFacilSyn;   //resaltador - lexer
    cEnt   : TContext;       //referencia al contexto de entrada actual
    ctxList : TListaCont;    //Lista de contextos de entrada
    function LeePosContAct: TPosCont;
    procedure FijPosContAct(pc: TPosCont);
  public
    MsjError : string;
    tok      : string;       //token actual
    tokType  : TSynHighlighterAttributes;  //tipo de token actual
    OnNewLine: procedure(lin: string) of object;
    function tokL: string;   //token actual en minúscula
    property PosAct: TPosCont read LeePosContAct write FijPosContAct;
    property curCon: TContext read cEnt;
    procedure NewContextFromFile(arc0: String);
    procedure NewContextFromFile(arc0: String; lins: Tstrings);
    procedure NewContextFromTxt(txt: string; arc0: String);
    procedure QuitaContexEnt;
    procedure ClearAll;      //elimian todos los contextos

    function Eof: Boolean;
    procedure SkipWhites;
    procedure SkipWhitesNoEOL;
    procedure Next;          //Pasa al siguiente token
  public
    constructor Create(Lex0: TSynFacilSyn);
    destructor Destroy; override;
  end;

implementation

{ TContext }
constructor TContext.Create;
begin
inherited;   //solo se pone por seguridad, ya que no es necesario.
  intLines := TStringList.Create;    //crea lista de cadenas para almacenar el texto
  nlin := 0;
  SetSource('');  //para iniciar con algo en donde leer
end;
destructor TContext.Destroy;
begin
//  lex.Free;     //libera lexer
  intLines.Free;     //libera lista
  inherited Destroy;
end;
function TContext.IniCont: Boolean;
//Devuelve verdadero si se está al inicio del Contexto (fila 1, columna 1)
var
  p: TPoint;
begin
  p :=lex.GetXY;
  Result := (p.x = 1) and (p.y = 1);
end;
function TContext.Eof: Boolean;
//Devuelve verdadero si se ha llegado al final del Contexto.
begin
  //Protección a Contexto vacío
  If nlin = 0 Then begin
      Result := True;
      Exit;
  End;
  //Verifica
  Result := (lex.GetY = nlin) and lex.GetEol;
end;
procedure TContext.SkipWhitesNoEOL;
//Coge los blancos iniciales del contexto de entrada, sin considerar saltos de línea.
//Si no encuentra algun blanco al inicio, devuelve falso
begin
  while not Eof and ((lex.GetTokenAttribute = lex.tkSpace) or
//los saltos son delimitadores  (lex.GetTokenAttribute = lex.tkEol)  or
                     (lex.GetTokenAttribute = lex.tkComment)
                     ) do
    Next;
  //actualiza estado
//  tok := lex.GetToken;    //lee el token
//  tokType := lex.GetTokenAttribute;  //lee atributo
end;
procedure TContext.SkipWhites;
//Coge los blancos iniciales, saltos de línea y comentarios del contexto de entrada.
//Si no encuentra algun blanco al inicio, devuelve falso
begin
//  tok := lex.GetToken;    //lee el token
  while not Eof and ((lex.GetTokenAttribute = lex.tkSpace) or
                     (lex.GetTokenAttribute = lex.tkEol)  or
                     (lex.GetTokenAttribute = lex.tkComment)
                     ) do
  begin
    Next;
//tok := lex.GetToken;    //lee el token
  end;
  //actualiza estado
//  tok := lex.GetToken;    //lee el token
//  tokType := lex.GetTokenAttribute;  //lee atributo
end;
function TContext.getRow: integer;
begin
  Result:=lex.GetY;  //deberías ser equivalente a leer "fFil"
end;
function TContext.getCol: integer;
begin
  Result:=lex.GetX;
end;
function TContext.Token: string;
{Devuelve el token actual}
begin
  Result := lex.GetToken;
end;
function TContext.TokenType: TSynHighlighterAttributes;
{Devuelve el tipo de token actual}
begin
  Result := TSynHighlighterAttributes(UIntPtr(lex.GetTokenKind));
end;
function TContext.Block: TFaSynBlock;
begin
  Result := lex.TopCodeFoldBlock;
end;

function TContext.Next: boolean;
//Pasa al siguiente token. Si hay cambio de líne edvuelve TRUE
var fFil: integer;
begin
  if nlin = 0 then exit;  //protección
  if lex.GetEol then begin  //llegó al fin de línea
    fFil := lex.GetY;  //Pasa a siguiente fila.
    if fFil <= nlin then begin //se puede leer
      lex.SetLine(curLines[fFil],fFil);  //prepara exploración
      //actualiza estado
//      tok := lex.GetToken;    //lee el token
//      tokType := lex.GetTokenAttribute;  //lee atributo
    end;
    exit(true);   //hubo cambio de línea
  end else begin //está en medio de la línea
    lex.Next;        //pasa al siguiente token
    //actualiza estado
//    tok := lex.GetToken;    //lee el token
//    tokType := lex.GetTokenAttribute;  //lee atributo
    exit(false);
  end;
end;

function TContext.CurLine: string;
{Devuelve la línea actual en que se encuentra el lexer}
var
  fFil: Integer;
begin
  fFil := lex.GetY;
  if fFil <= nlin then  //se puede leer
    Result := curLines[fFil-1]
  else
    Result := '';
end;
procedure TContext.SetStartPos;
//Mueve la posición al inicio del contexto.
begin
  if curLines.Count = 0 then begin
    //No hay líneas
    lex.ResetRange;   //fRange_= nil y también inicia información de bloques
  end else begin //hay al menos una línea
    if lex = nil then begin  //No hay lexer. Es posible
//      tok := '';
//      tokType := nil;
    end else begin
      lex.ResetRange;  //fRange_= nil y también inicia información de bloques
      lex.SetLine(curLines[0],0);  //empieza con la primera línea
      //actualiza estado
//      tok := lex.GetToken;     //lee el token
//      tokType := lex.GetTokenAttribute;  //lee atributo
    end;
  end;
end;
procedure TContext.SaveLexerState;
//Guarda el estado actual del lexer en la variable interna "fLexerState".
//Este estado incluye las coordenadas actuales de lectura en el Lexer.
begin
  fLexerState := lex.State;
end;
procedure TContext.RestoreLexerState;
//Copia el estado del lexer grabado en "fLexerState". Se debe ejecutar siempre
//después de SaveLexerState().
begin
  lex.State := fLexerState;
end;
function TContext.ReadSource: string;
//Devuelve el contenido del contexto en una cadena.
begin
  Result := curLines.text;
end;
//Métodos de inicialización
procedure TContext.DefSyn(lex0: TSynFacilSyn);
//Define el lexer a usar en el contexto
begin
  lex := lex0;
end;
procedure TContext.SetSource(txt: string);
//Fija el contenido del contexto con una cadena. Puede ser de varias líneas.
begin
  tip := TC_TXT;          //indica que contenido es Texto
  //guarda en lista interna.
  if txt='' then begin
    //cadena vacía, crea una línea vacía
    intLines.Clear;
    intLines.Add('');
  end else begin
    intLines.Text := txt;
  end;
  curLines := intLines;   //apunta a almacenamiento interno
  nlin := curLines.Count; //actualiza número de líneas
  SetStartPos;             //actualiza posición de cursor
  arc := '';             //No se incluye información de archivo
end;
procedure TContext.SetSource(lins: Tstrings; MakeCopy: boolean = false);
//Fija el contenido del contexto con una lista TStringList. Usa la referencia, no copia.
begin
  tip := TC_TXT;         //indica que contenido es Texto
  if MakeCopy then begin  //crea copia
    intLines.Clear;
    intLines.AddStrings(lins); //carga líneas, de la lista
    curLines := intLines; //apunta a almacenamiento interno
  end else begin
    curLines := lins;    //apunta a la fuente externa. No la copia.
  end;
  nlin := curLines.Count; //actualiza número de líneas
  SetStartPos;             //actualiza posición de cursor
  arc := '';             //No se incluye información de archivo
end;
procedure TContext.SetSourceF(file0: string);
//Fija el contenido del contexto con un archivo
begin
  tip := TC_ARC;         //indica que contenido es Texto
  intLines.LoadFromFile(file0);
  curLines := intLines;  //apunta a almacenamiento interno
  nlin := curLines.Count; //actualiza número de líneas
  SetStartPos;             //actualiza posición de cursor
  arc := file0;          //Toma nombe de archivo
end;

{ TContexts }
function TContexts.LeePosContAct: TPosCont;
//Devuelve Contexto actual y su posición
begin
  Result.fCon := cEnt;
  if cEnt = nil then begin
    //Aún no hay Contexto definido
  end else begin
    Result.fPos := cEnt.lex.State;
//      Result.fil  := cEnt.row;
//      Result.col  := cEnt.col;
//    Result.arc  := cEnt.arc;
//      Result.nlin := cEnt.nlin;
  end;
end;
procedure TContexts.FijPosContAct(pc: TPosCont);
//Fija Contexto actual y su posición
begin
  cEnt := pc.fCon;
  if cEnt = nil then begin
    //No tiene un Contexto actual
  end else begin
    cEnt.lex.State := pc.fPos;
//    cEnt.row := pc.fil;
//    cEnt.col := pc.col;
//    cEnt.arc := pc.arc;
//    cEnt.nlin := pc.nlin;
  end;
  //actualiza token actual
  tok := lex.GetToken;    //lee el token
  tokType := lex.GetTokenAttribute;  //lee atributo
end;
procedure TContexts.NewContextFromTxt(txt: string; arc0: String);
//Crea un Contexto a partir de una cadena.
//Fija el Contexto Actual "cEnt" como el Contexto creado.
begin
  cEnt := TContext.Create; //crea Contexto
  cEnt.DefSyn(Lex);      //asigna lexer
  ctxList.Add(cEnt);        //Registra Contexto
  cEnt.SetSource(txt);    //inicia con texto
  cEnt.arc := arc0;     {Se guarda el nombre del archivo actual, solo para poder procesar
                         las funciones $NOM_ACTUAL y $DIR_ACTUAL}
  //actualiza token actual
  tok := lex.GetToken;    //lee el token
  tokType := lex.GetTokenAttribute;  //lee atributo
end;
procedure TContexts.NewContextFromFile(arc0: String);
//Crea un Contexto a partir de un archivo.
//Fija el Contexto Actual "cEnt" como el Contexto creado.
begin
  If not FileExists(arc0)  Then  begin  //ve si existe
    MsjError := 'No se encuentra archivo: ' + arc0;
    Exit;
  end;
  cEnt := TContext.Create; //crea Contexto
  cEnt.DefSyn(Lex);     //asigna lexer
  ctxList.Add(cEnt);   //Registra Contexto
  cEnt.SetSourceF(arc0);     //inicia con archivo
  //actualiza token actual
  tok := lex.GetToken;    //lee el token
  tokType := lex.GetTokenAttribute;  //lee atributo
end;
procedure TContexts.NewContextFromFile(arc0: String; lins: Tstrings);
//Crea un Contexto a partir de un Tstring, como si fuera un archivo.
//Fija el Contexto Actual "cEnt" como el Contexto creado.
begin
  cEnt := TContext.Create; //crea Contexto
  cEnt.DefSyn(Lex);     //asigna lexer
  ctxList.Add(cEnt);   //Registra Contexto
  cEnt.SetSource(lins);   //inicia con archivo contenido en TStrings
  cEnt.arc :=  arc0;      //archivo
  //actualiza token actual
  tok := lex.GetToken;    //lee el token
  tokType := lex.GetTokenAttribute;  //lee atributo
end;
procedure TContexts.QuitaContexEnt;
//Elimina el contexto de entrada actual. Deja apuntando al anterior en la misma posición.
begin
  if ctxList.Count = 0 then begin
    cEnt := nil;   //por si acaso
    exit;  //no se puede quitar más
  end;
  ctxList.Delete(ctxList.Count-1);
  if ctxList.Count = 0 then
    cEnt := nil
  else  //apunta al último
    CEnt := ctxList[ctxList.Count-1];
end;
procedure TContexts.ClearAll;  //Limpia todos los contextos
begin
  ctxList.Clear;     //elimina todos los Contextos de entrada
end;
function TContexts.Eof: Boolean;
begin
  Result := cEnt.Eof;
end;

procedure TContexts.SkipWhites;
{Salta los blancos incluidos los saltos de línea}
begin
  while not cEnt.Eof and ((lex.GetTokenAttribute = lex.tkSpace) or
                     (lex.GetTokenAttribute = lex.tkEol)  or
                     (lex.GetTokenAttribute = lex.tkComment) ) do
  begin
      if cEnt.Next then begin   //hubo cambio de línea
        if OnNewLine<>nil then OnNewLine(cEnt.CurLine);
      end;
  end;
  //actualiza token actual
  tok := lex.GetToken;    //lee el token
  tokType := lex.GetTokenAttribute;  //lee atributo
end;
procedure TContexts.SkipWhitesNoEOL;
{Salta los blancos sin incluir los saltos de línea}
begin
  while not cEnt.Eof and ((lex.GetTokenAttribute = lex.tkSpace) or
                     (lex.GetTokenAttribute = lex.tkComment) ) do
  begin
      if cEnt.Next then begin   //hubo cambio de línea
        if OnNewLine<>nil then OnNewLine(cEnt.CurLine);
      end;
  end;
  //actualiza token actual
  tok := lex.GetToken;    //lee el token
  tokType := lex.GetTokenAttribute;  //lee atributo
end;
procedure TContexts.Next;
begin
  if cEnt.Next then begin   //hubo cambio de línea
    if OnNewLine<>nil then OnNewLine(cEnt.CurLine);
  end;
  //actualiza token actual
  tok := lex.GetToken;    //lee el token
  tokType := lex.GetTokenAttribute;  //lee atributo
end;
function TContexts.tokL: string; inline;
//Devuelve el token actual, ignorando la caja.
begin
  Result:=lowercase(tok);
end;
constructor TContexts.Create(Lex0: TSynFacilSyn);
begin
  Lex := Lex0;   //guarda referencia
  ctxList := TListaCont.Create(true);  //crea contenedor de Contextos, con control de objetos.
  cEnt := nil;
end;
destructor TContexts.Destroy;
begin
  ctxList.Free;
  inherited Destroy;
end;

{ TPError }
procedure TPError.IniError;
begin
  numER := 0;
  cadER := '';
  arcER := '';
  fil := 0;
end;
procedure TPError.Clear;
//Limpia rápidamente el error actual
begin
  numEr := 0;
end;
procedure TPError.GenError(msje: String; archivo: String; nlin: LongInt);
//Genera un error
begin
  numER := 1;
  cadER := msje;
  arcER := archivo;
  fil := nlin;
end;
procedure TPError.Generror(msje: String; ctx: TContext);
//Genera un error en la posición actual del contexto indicado.
begin
  numER := 1;
  cadER := msje;
  arcER := ctx.arc;  //toma nombre de archivo del contexto
  fil := ctx.row;
  col := ctx.col;
end;
function TPError.TxtError: string;
//Devuelve el mensaje de error
begin
  Result := cadER;
end;
function TPError.TxtErrorRC: string;
//Devuelve el mensaje de error con información de fila y columna
begin
//  If arcER <> '' Then begin
    //Hay nombre de archivo de error
    If fil <> -1 Then       //Hay número de línea
      //Se usa este formato porque incluye información sobre fila-columna.
      Result := '['+ IntToStr(fil) + ',' + IntToStr(col) + '] ' + cadER
    Else          //No hay número de línea, sólo archivo
      Result := cadER;
//  end else
//    Result :=cadER;
end;
procedure TPError.Show;
//Muestra un mensaje de error
begin
  Application.MessageBox(PChar(TxtError), PChar(NombPrograma), MB_ICONEXCLAMATION);
end;
function TPError.ArcError: string;
//Devuelve el nombre del archivo de error
begin
  ArcError := arcER;
end;
function TPError.nLinError: longint;
//Devuelve el número de línea del error
begin
  nLinError := fil;
end;
function TPError.nColError: longint;
//Devuelve el número de línea del error
begin
  nColError := col;
end;
function TPError.HayError: boolean;
begin
  Result := numER <> 0;
end;

end.

