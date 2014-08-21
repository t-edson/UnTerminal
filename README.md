UnTerminal
==========

Unidad en Lazarus, para el control de procesos tipo consola, con detección de "prompt".

Esta unidad permite procesar la entrada y salida de un cliente de telnet, que trabaja como una consola.
Permite además detectar el prompt y el estado de "ocupado" y "listo".

Para conectarse mediante un proceso, se debe crear una instancia de TConexProc, y seguir la
secuencia de conexión:

```
  p := TConexProc.Create(StatusBar1.Panels[1]);  //Crea conexión
  ...
  p.Free.
```

Opcionalmente se le puede pasar la referencia a un panel de una barra de estado, para
que se muestren íconos que indiquen el estado de la conexión (poner en NIL si no se
usará). En este caso se debe manejar el evento OnDrawPanel() de la Barra de estado:

```
 StatusBar1.OnDrawPanel:=@SBDrawPanel;
 ...
 procedure Form1.SBDrawPanel(StatusBar:TStatusBar; Panel:TStatusPanel; const Rect:TRect);
 begin
  if panel.Index = 1 then q.DibPanelEstado(StatusBar.Canvas, Rect);
 end;
```

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

```
  proc.OnInitLines:=@procInitLines;
  proc.OnRefreshLine:=@procRefreshLine;
  proc.OnRefreshLines:=@procRefreshLines;
  proc.OnAddLine:=@procAddLine;
...

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
```

El evento OnInitLines(), es llamado solo una vez al inicio para dimensionar el StringList, de modo que pueda contener a todas las líneas del terminal VT100.

En principio, solo debería mandarse un comando por Enviar() porque, enviar más
de un comando podría hacer que el proceso pase repetidamente
por la las fases ECO_OCUPADO -> ECO_LIBRE -> ECO_OCUPADO ... -> ECO_LIBRE.

Los datos de salida que van llegando del proceso, no se guardan completamente en la
clase. Solo se mantienen las líneas de trabajo del terminal VT100 en el objeto "term".
Así se ahorra memoria, porque por lo general, el texto de salida está destinado a ser
almacenado en algún otro control como un editor de texto o una grilla.
Es responsabilidad del programador, limitar el tamaño de los datos almacenados.
