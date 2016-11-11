{
XpresElements
=============
Definiciones para el manejo de los elementos del compilador: funciones, constantes, variables.
Por Tito Hinostroza.
 }
unit XpresElements;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, XpresTypes;

type
//tipo de identificador
TIdentifType = (idtNone, idtVar, idtFunc, idtCons);

TFindFuncResult = (TFF_NONE, TFF_PARTIAL, TFF_FULL);

TxpCon = class
  name : string;   //nombre de la variable
  typ  : Ttype;    //tipo de la variable
  //valores de la constante
  val : TConsValue;
end;
TxpCons = specialize TFPGObjectList<TxpCon>; //lista de variables

TVarAddr = word;
TVarBank = byte;
//registro para almacenar información de las variables
TxpVar = class
  nom : string;   //nombre de la variable
  typ : Ttype;    //tipo de la variable
  amb : string;   //ámbito o alcance de la variable
  //direción física. Usado para implementar un compilador
  addr: TVarAddr;
  bank: TVarBank;   //banco o segmento. Usado solo en algunas arquitecturas
  //Campos usados para implementar el intérprete sin máquina virtual
  //valores de la variable.
  valInt  : Int64;    //valor en caso de que sea un entero
  valUInt : Int64;    //valor en caso de que sea un entero sin signo
  valFloat: extended; //Valor en caso de que sea un flotante
  valBool  : Boolean;  //valor  en caso de que sea un booleano
  valStr  : string;    //valor  en caso de que sea una cadena
end;
TxpVars = specialize TFPGObjectList<TxpVar>; //lista de variables

{ TxpFun }
TxpFun = class;
TProcExecFunction = procedure(fun :TxpFun);  //con índice de función
//registro para almacenar información de las funciones
TxpFun = class
private
public
  name: string;   //nombre de la función
  typ : Ttype;    //tipo que devuelve
  amb : string;   //ámbito o alcance de la función
  pars: array of Ttype;  //parámetros de entrada
  //direción física. Usado para implementar un compilador
  adrr: integer;  //dirección física
  //Campos usados para implementar el intérprete sin máquina virtual
  proc: TProcExecFunction;  //referencia a la función que implementa
  posF: TPoint;    //posición donde empieza la función en el código
  procedure ClearParams;
  procedure CreateParam(parName: string; typ0: ttype);
  function SameParams(Fun2: TxpFun): boolean;
end;
TxpFuns = specialize TFPGObjectList<TxpFun>;

implementation

procedure TxpFun.ClearParams;
//Elimina los parámetros de una función
begin
  setlength(pars,0);
end;
function TxpFun.SameParams(Fun2: TxpFun): boolean;
{Compara los parámetros de la función con las de otra. Si tienen el mismo número
de parámetros y el mismo tipo, devuelve TRUE.}
var
  i: Integer;
begin
  Result:=true;  //se asume que son iguales
  if High(pars) <> High(Fun2.pars) then
    exit(false);   //distinto número de parámetros
  //hay igual número de parámetros, verifica
  for i := 0 to High(pars) do begin
    if pars[i] <> Fun2.pars[i] then begin
      exit(false);
    end;
  end;
  //si llegó hasta aquí, hay coincidencia, sale con TRUE
end;
procedure TxpFun.CreateParam(parName: string; typ0: ttype);
//Crea un parámetro para la función
var
  n: Integer;
begin
  //agrega
  n := high(pars)+1;
  setlength(pars, n+1);
  pars[n] := typ0;  //agrega referencia
end;

end.

