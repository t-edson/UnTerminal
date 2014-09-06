UnTerminal 0.4b
===============

Unidad en Lazarus, para el control de procesos tipo consola, con detección de "prompt".

Esta unidad permite procesar la entrada y salida de procesos que manejen entrada y salida estándar de texto. Permite además detectar el prompt y el estado de "ocupado" y "listo".

Los procesos a controlar con esta unidad deben cumplir las siguientes condiciones:

1. Que se puedan lanzar como procesos de tipo consola.
2. Que su entrada y salida estándar sea de tipo texto.
3. Que tengan prompt.
4. Que acepten comandos.

Los procesos que se pueden controlar con esta unidad son diversos, como clientes de telnet, ftp, o el mismo shell del sistema operativo, como se muestar en el ejemplo incluido.

En esta versión no permite leer el flujo de salida de errores.

Para conectarse mediante un proceso, se debe crear una instancia de TConexProc, y seguir la secuencia de conexión:

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
 
 ECO_STOPPED -> ECO_CONNECTING -> ECO_READY

El estado de ECO_READY indica que el proceso está listo para recibir comandos y, se detecta cuando se recibe el prompt. Por lo tanto para que se pueda considerar a un proceso como ECO_READY, se debe cumplir que tenga un prompt y que el terminal esté configurado para detectarlo.

La secuencia de una conexión con error sería:
 
 ECO_STOPPED -> ECO_CONNECTING -> ...

Es decir, que no se detecta el prompt, sino que probablemente, el proceso se detiene con un mensaje de error. La detección de los mensajes de error, no se hace en esta unidad, porque pueden ser muy diversos. Se recomienda implementarlos en otra unidad o derivando una clase adicional.
 
Una vez el proceso se encuentre disponible. Un comando cualquiera, tendría el siguiente flujo:

ECO_READY -> ECO_BUSY -> ECO_READY

Está unidad está pensada para ser usada en conexiones lentas, y con volúmenes
considerables de datos, por ello se maneja la llegada de datos completamente por eventos.

Para el manejo de la pantalla usa un terminal VT100 sencillo, de la unidad TermVT. Sin
embargo, esta unidad ha sido diseñada para poder recibir el texto del VT100, en un
TStringList, y poder registrar permanentemente el contenido.

El proceso a lanzar, se inicia con el método Open() y se termina con el método Close().

Por defecto, los saltos de línea se enviarán al proceso como el caracter LF (que es usual en conexiones de tipo Telnet). Para cambiar a CR+LF (que es usual en el entorno Windows), se debe fijar la propiedad 'sendCRLF' en TRUE. 

Esta unidad solo ha sido probada en Windows, pero podría funcionar también en Linux.

# Configurando un Editor para mostrar la salida

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

Los comandos, se envían al proceso con el método Send(). En principio, solo debería mandarse un solo comando por Send() porque, enviar más de un comando podría hacer que el proceso pase repetidamente por  las fases:

ECO_BUSY -> ECO_READY -> ECO_BUSY ... -> ECO_READY.

Haciendo que se pase de un estado de libre a ocupado de forma automática, y pudiendo complicar a alguna rutina que esté programada para enviar más comandos cuando detecte que el proceso se encuentre libre.

Los datos de salida que van llegando del proceso, no se guardan completamente en la
clase. Solo se mantienen las líneas de trabajo del terminal VT100 en el objeto "term".
Así se ahorra memoria, porque por lo general, el texto de salida está destinado a ser
almacenado en algún otro control como un editor de texto o una grilla.
Es responsabilidad del programador, limitar el tamaño de los datos almacenados.

# Detección del Prompt

La detección del prompt se hace explorando las cadenas de texto que van llegando del proceso. No se tiene otra forma de detcción más confiable.

Para configurar la detección del prompt se deben poner la propiedad 'detecPrompt' en TRUE y fijar valores para 'prIni' y 'prFin'.

Si el prompt es fijo y no cambia, se puede poner su valor directamente en 'prIni' y se deja 'prFin' vacío. Pero si el prompt es variable, se puede poner la parte inicial en 'prIni' y la parte final en 'prFin', pero eligiendo valores que no den lugar a confusión con líneas de datos.

Así, por ejemplo para detectar un prompt de tipo:

[usuario@localhost ~]$ 

Se puede configurar: prIni='[' y prFin = '$ '. Los espacios son también parte del texto que se debe configurar.

Tambíen se puede usar la función de configuración automática del prompt, que se ejecuta llamando al método AutoConfigPrompt. Pero este método debe ser llamado cuando se tenga el prompt visible en la última línea del terminal.

'AutoConfigPrompt' lee la última línea asumiendo que es el pprompt y fija valores automáticamente para 'prIni' y 'prFin'.

Se puede usar también una rutina personalizada para la detección deñ prompt. Esta se debe enganchar al evento OnChkForPrompt() que tiene la forma:

function(lin: string): boolean;

El evento recibe la línea a explorar y debe devolver TRUE, si es que se determina que la línea actual contiene al prompt. Al activar este evento, se desactiva la detección interna de la unidad.