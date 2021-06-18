UnTerminal 1.0
==============

By Tito Hinostroza

Lazarus unit for control external programs or console processes, with prompt detection. Tested in Linux and DOS consoles.

This unit allows to launch a process and interact with it trough the standard input and output.

Features:

* Works as a wrapper for TProcess.
* Includes detection of prompt.
* Assigns states for a process, like "Ready" or "Busy".
* Is event driven.
* Recognize some VT100 sequences.

Programs to be controled with this unit should comply:

1. That can be launched as a console command.
2. That input and output are text format.

Optionally they can:

3. Have a prompt.
4. Be controled by commands (Si se quiere enviar información al proceso).

The processes that can be controlled with this unit are diverse, such as telnet clients, ftp, or the operating system shell itself, as shown in the included example.

In the current version, the error output stream cannot be read independently. Only two flows are controlled:

1. Input
2. Output (stdout and stderr).

![Tito's Terminal](https://github.com/t-edson/UnTerminal/blob/1.0/screen1.png "Título de la imagen")

## Hello world

The simplest code to launch a command/process/program and capture the output can be:

```
var outputText: string;

  proc:= TConsoleProc.Create(nil);
  proc.RunInLoop('bash -c "ls" ','', -1, outputText);
```

This code executes the command 'bash -c "ls" ' and captures the output (and the errors) in the variable "outputText".

This code works for linux but can be used in windows too, usign DOS commands.


## Executing commands

To execute a command, first you need to create an instance of the class TConsoleProc:

```
  p := TConsoleProc.Create(nil);  //Create conecction
  ...

```

Then you need to execute the command, choosing the appropriate way.

There are several ways to execute commands using UnTerminal. They can be clasified in two types:

  A) Running in a finite loop (or until the process finishes).
  B) Running in a infinite loop (or until the program finishes).

Both methods can use events to work.

The option A, is implemented using the method RunInLoop(). There are several versions of this method:
 
```
  function RunInLoop(progPath, progParam: string; TimeoutSegs: integer = -1): boolean;
  function RunInLoop(progPath, progParam: string; TimeoutSegs: integer;
    var progOut: TStringList): boolean;
  function RunInLoop(progPath, progParam: string; TimeoutSegs: integer; out
    outText: String): boolean;
```

The idea of this way is to send a command and get the output/error text in a variable, generally without interacting with the process.

The parameter "progPath" is the name of the program or commnand to be executed. Consider that popular commands of DOS or Linux, are not programs, so we cannot launch "dir" or "ls" in "progPath". Instead we need to call the corresponding command processor and pass the commands, like "cmd /c dir" or 'bash -c "ls"'.

The parameter "progParam" is a string containing the paremeters of the program or commnand to be executed. They can include in "progaPath" too, but it's advisable to include them separately.

As an example the next commands are similar:

```
  proc.RunInLoop('cmd','/c dir', -1, outText);
```

```
  proc.RunInLoop('cmd /c dir','', -1, outText);
```

If the program or command doesn't exist, the instruction RunInLoop() will raise an exception in runtime.

The parameters "TimeoutSegs" is the numbers of seconds to wait the command finishes, before continue executing the next instructions. The value -1 indicates, the instruction will wait until the command finishes.

The option B is described in the next sections of this document.

## States of the process

The connection is managed by states. The possible states of the process are:

* ECO_STOPPED .- The Process has not started. This condition always occurs after creating the connection, or after closing an open connection. There may be data pending in the "buffer"

* ECO_CONNECTING .- This condition occurs after opening the connection with the indicated process. Terminates when the prompt is detected. If the prompt is not detected (whether it does not arrive, or it has not been configured correctly), the process will always remain in this state.

* ECO_ERROR_CON .- Occurs when after trying to open the process, an error occurs, for example, when referring to a file that does not exist.

* ECO_READY .- This process starts when the prompt is detected (if prompt detection is used). Indicates that the process is ready to accept commands. It remains in this state until a command is received to send to the process.

* ECO_BUSY .- This state starts after sending a command, when the process was in ECO_READY state. It remains in this state, until the prompt is detected and the ECO_READY state is fired.

The process can only be in one of these states at a time. The transition between states has a logical sequence, but can have changes.

A typical sequence of states when initiating a successful connection is:
 
 ECO_STOPPED -> ECO_CONNECTING -> ECO_READY

The ECO_READY status indicates that the process is ready to receive commands and is detected when the prompt is received. Therefore, for a process to be considered as ECO_READY, it must have a prompt and that the terminal is configured to detect it.

The sequence of a failed connection would be:
 
 ECO_STOPPED -> ECO_CONNECTING -> ...

That is, the prompt is not detected, but the process probably stops with an error message. Error messages detection is not done on this unit, because they can be very diverse. It is recommended to implement them in another unit or by deriving an additional class.
 
Once the process is available. Any command would have the following flow:

ECO_READY -> ECO_BUSY -> ECO_READY

This unit is thought to be used in slow connections, and with long data responses, therefore the arrival of data is handled completely by events.

To operate the display, use a simple VT100 terminal from the TermVT unit. However, this unit has been designed to be able to receive the text from the VT100, in a TStringList, and to be able to permanently record the content.

The process to be launched starts with the Open() method and ends with the Close() method.

## Setting an Editor to show the output

The objective of using UnTerminal is to be able to display the output in a comfortable way for the user. 

"UnTerminal", has beed designed to handle slow processes with massive data output. Therefore the data output is handled entirely by events. In addition, it is assumed that it is required to keep a record on the screen information while appearing. For this reason, the events are oriented to handle a "TStrings" object, which allows adding new lines and saving the previous lines (output register).

There are several groups of events to handle the data output. 

Four methods are defined to control the data output::

1. Output line by line.
2. Output line by line with partial line.
3. Output line by line with partial line in prompt.
4. Output as a VT100 terminal.

Each of these methods has its advantages and disadvantages. And you should use the one that best suits the needs of the application. Thus, to implement a Telnet client, method 4 (the most complete) should be used, but for other processes it will suffice to use simpler methods.

An additional method could be implemented which consists of directly intercepting the terminal events (property term: TTermVT100) of TConsoleProc, to display the information on the screen. However, this working method is of a lower level, but it can be used to capture the complete content of the VT100 terminal, every time there is a change (which would not be very efficient, of course).

Implementing data output does not affect prompt detection. The detection routines work directly on the data blocks that are arriving from the process, without the need for this data to be displayed.

### Output line by line

With respect to the other methods, this method is the simplest, and although it has certain limitations, it may be sufficient for some cases, like when our process will not handle ANSI sequences, but will simply send plain text without delete commands or cursor handling.

To do a line-by-line capture, we need to use a single event:

OnLineCompleted:TEvLinCompleted;

That is generated every time a line break is detected in the arrival of data. So we could use this method to add lines to our output editor, without worrying about starting the screen, or about the previous information it may contain.

Our code would be something like this:

```
  proc := TConsoleProc.Create(nil);
  proc.OnLineCompleted:=@procOnLineCompleted;
  proc.Open('cmd /c dir','');
  
...

procedure TForm1.procOnLineCompleted(const lin: string);
begin
  Memo.Lines.Add(lin);
end;
```

This minimalistic form will work fine, displaying the arrival data, but with an apparent delay, because for a line to be displayed, it must be complete. So when the prompt (or an incomplete line) arrives, this line won't show up in the editor (even though the process might go into ECO_READY state).

The fact that the last line is not displayed does not imply that it will no longer be displayed. This will be visible when the corresponding line break is received, unless the connection is closed before.

This method of capturing output is useful when it is required to capture massive data output without the correct display of the prompt or intermediate lines being very important.

To determine when the data is finished receiving, prompt detection can be used, if the process allows it.

### Output line by line with partial line

The line-by-line method is simple and secure. But it doesn't always update the last line. For the last complete line to be displayed, the corresponding line break must be received. Thus we can say that in this working mode, the last line received will never be shown (unless of course, that the last character received is the line break and we consider that the last line is the one before the jump).

To overcome this inconvenience, you can use the combination of events:

  OnLineCompleted: TEvLinCompleted; 
  OnReadData     : TEvReadData;   
 	
The OnReadData() event is generated when a block of data is finished reading and usually ends with a final line without a jump (it can be the prompt). So what we should do is complete replace this line, if we then receive the OnLineCompleted () event.

The working code will look like this:

```
var LinPartial: boolean = false;

  proc.OnLineCompleted:=@procLineCompleted;
  proc.OnReadData:=@procReadData;
...
  
procedure TForm1.procLineCompleted(const lin: string);
begin
  if LinPartial then begin
    //We are in the prompt line
    Memo1.Lines[Memo1.Lines.Count-1] := lin;  //Replace last line
    LinPartial := false;
  end else begin  //Common case
    Memo1.Lines.Add(lin);
  end;
end;

procedure TForm1.procReadData(nDat: integer; const lastLin: string);
begin
  LinPartial := true;   	//Set flag
  Memo1.Lines.Add(lastLin); //Add line containing the prompt
end;
```

The variable 'LinPartial' helps us to know when a partial line has been received.

This working method will work quite well and is simple, but it could be the case (rare) where the OnReadData() event is received twice in a row, causing an additional line to be generated in the editor. This case could occur when a very long line is generated that arrives in more than one block, or in very slow connections.

To avoid this effect, an additional protection routine could be included to verify that the OnReadData() event is occurring twice in a row and, if so, avoid adding one more line.

The use of OnLineCompleted()-OnReadData() events is a simple way to receive data "line by line", but we must not forget that it should only be applied to processes with simple text output without cursor control or text deletion. Most processes fall into this category (like FTP clients or even the DOS shell itself), but processes like a Telnet or SSH client use escape sequences that may require positioning the cursor to overwrite blocks.

In these cases you should not implement this output capture method; but even if it were implemented here, we could work fine, as long as cursor jumps or text deletion is not generated. Escape sequences that change text attributes do not affect the output of the text, because they are processed (or better said ignored) by TConsoleProc, so that the text output events do not contain ANSI sequences of any kind, in their parameters.

### Output Line-by-line  with partial line at Prompt

As a variation of the previous method, this method is very similar, but with the difference that the detection of partial lines is done only for when the promtp is detected.
 
In this way, it is avoided to generate many overwriting of lines, since it will only be done in the prompt line. This method is more efficient when you have multiple data packets between prompt and prompt.

A disadvantage of this method is that the prompt must be configured, and it always muts appears at the end of a data block, so that the last line is refreshed, otherwise it will not be refreshed in any way.

Therefore it is only recommended for processes that always show the prompt. It is not recommended, for example, in a process that at some point will show a message such as "Please enter a value:", waiting for user input. Therefore you have to be cautious when using this output capture method.

The implementation of the capture is the same as the previous case, only another pair of events is used:

```
  OnLineCompleted: TEvLinCompleted; 
  OnLinePrompt: TEvLinCompleted; 
```

The OnLinePrompt() event is generated when the line containing the prompt is received, even if it is not complete. So what we should do is complete replace this line, if we then receive the OnLineCompleted() event.

The working code will look like this:

```
var LinPrompt: boolean = false;

  proc.OnLineCompleted := @procLineCompleted;
  proc.OnLinePrompt    := @procLinePrompt;
...
  
procedure TForm1.procLineCompleted(const lin: string);
begin
  if LinPrompt then begin
    //We are in the prompt line
    SynEdit1.Lines[SynEdit1.Lines.Count-1] := lin;  //Replace last line
    LinPrompt := false;
  end else begin  //Common case
    SynEdit1.Lines.Add(lin);
  end;
end;

procedure TForm1.procLinePrompt(const lin: string);
begin
  LinPrompt := true;   //Set flag
  SynEdit1.Lines.Add(lin);  //Add line containing the prompt
end;
```

The variable 'LinPrompt', helps us to detect when the prompt has been received on a line.

This working method can also suffer from the problem of the previous method if, it is that a long line is received (where the prompt is detected) that comes in more than one block. The solution to apply is the same.

The effect of this extreme case may not be very critical, as it will only generate an additional line with the prompt in the output editor. This additional line will not remove the readability of the code but it is a lack of fidelity in the output of the process anyway.

### Output as VT100 Terminal

This is the most complete method of handling the terminal output. It allows to recognize the escape sequences that control the cursor, or manipulate the text of the terminal.

It can be said that this is the formal method for handling any process, even if it generates ANSI escape sequences. However, managing processes using this way of working can be difficult when managing the cursor position or when you want to switch from one output editor to another.

To set an editor in this way, you must use the events: OnInitScreen(), OnRefreshLine(), OnRefreshLines() and OnAddLine().

In the following code, a SynEdit editor is used as the output for any process.

```
  proc.OnInitScreen  := @procInitScreen;
  proc.OnRefreshLine := @procRefreshLine;
  proc.OnRefreshLines:= @procRefreshLines;
  proc.OnAddLine     := @procAddLine;
...

procedure TfrmPrincipal.procInitScreen(const grid: TtsGrid; fIni, fFin: integer);
begin
  for i:=fIni to fFin do SynEdit1.Lines.Add(grid[i]);
end;

procedure TfrmPrincipal.procRefreshLine(const grid: TtsGrid; fIni, HeightScr: integer);
begin
  yvt := SynEdit1.Lines.Count-HeightScr-1;
  SynEdit1.Lines[yvt+fIni] := grid[fIni];
end;

procedure TfrmPrincipal.procRefreshLines(const grid: TtsGrid; fIni, fFin, HeightScr: integer);
begin
  yvt := SynEdit1.Lines.Count-HeightScr-1;  //Calculate row equivalent to start of VT100
  for f:=fIni to fFin do SynEdit1.Lines[yvt+ f] := grid[f];
end;

procedure TfrmPrincipal.procAddLine;
begin
  SynEdit1.Lines.Add('');
end;
```

The OnInitScreen() event is called only once at startup to size the StringList, so that it can contain all the lines of the VT100 terminal. The output editor must have at least the number of lines that the VT100 terminal has (25 by default), otherwise an error will be generated at runtime. This condition is compressible, if we consider that it is about "fitting" a virtual screen of a termninal (which can be 80x25 lines) in a TString container.

To position the virtual cursor of the terminal on any line of the TString, the calculation [1] must first be done:

  yvt := SynEdit1.Lines.Count-HeightScr-1;

where 'HeightScr' is the current height of the VT100 terminal, which is provided by the TConsoleProc events.

[1] Of course, if we did not want to save the record of the previous lines, it would not be necessary to complicate ourselves with the calculations and we would only need to have a TString with the same number of lines from the VT100, and we would avoid calculating the vertical position because it would be the same in the terminal and on the TString.

To clean the content of the terminal, you must call the ClearTerminal() method, which also resets the cursor position, setting it to (1,1).

Calling ClearTerminal() will have no effect on the status of the connection or the process in progress, but only on the content of the terminal.

  
## Terminal use

### Sending commands

The commands (or any string in general), are sent to the process with the Send(), SendLn() or SendFile() methods.

SendLn() is similar to Send(), but additionally sends a newline character(s) at the end. The newline character(s) is necessary, in most processes, so that the sent string is recognized, as a command.

The SendFile() method sends the complete content of a file to the program.

The newline character(s) to send depends on the value of "LineDelimSend".

To send control characters, the SendVT100Key() method must be used, because control characters must first be converted into escape sequences, at least for applications that recognize escape sequences. For example, if the right directional key is sent, with SendVT100Key(), it will first be transformed into the sequence: ESC + \[C.

To send simple commands (which are printable character sequences), just use SenLn().

In principle, only a single command should be sent by SendLn() because, sending more than one command could make the process go through the phases repeatedly:

ECO_BUSY -> ECO_READY -> ECO_BUSY ... -> ECO_READY.

Making it go from a free to busy state automatically, and being able to complicate a routine that is programmed to send more commands when it detects that the process is ready.

The output data that comes from the process are not completely saved in the class. Only the working lines of a VT100 terminal are kept in the "term" object. This saves memory, because generally the output text is intended to be stored in some other control such as a text editor or grid.

It is the responsibility of the programmer to limit the size of the stored data.

The process output data, arriving through the terminal, is retrieved by polling the output stream. By default, the output is scanned in intervals of 50 milliseconds (20 times per second), and using an internal 2048-byte "buffer".

To change the polling period of the process output, you must change the value of the 'clock.interval' property (in milliseconds). You can work well with periods of 100 msec or even more, if the amount of information is low. But it is not recommended to go below 50 msec, because an additional CPU load is generated (even when no data arrives). This period should only be lowered when it is required to process the arrival data, immediately.

### Line-ending in sending

Inside UnTerminal, you can configure the line break characters that will be used, both for sending and receiving.

To set the line-ending characters in sending, the "LineDelimSend" property must be used, which can take the following values:

* LDS_CRLF   //Sends CR and LF characters
* LDS_CR     //Send only CR (character #13)
* LDS_LF     //Send only LF (character #10) 

Setting the line-ending for sending implies that the configured characters will be added, each time the TConsoleProc.SendLn() or TConsoleProc.SendFile() methods are used, regardless of the type of delimiter that has been included in the string. The following examples will illustrate the behavior of SendLn(), when "LineDelimSend" is set.

If LineDelimSend has been defined in LDS_LF, then:

SendLn('aaa');             //Will actually send 'aaa'#10
SendLn('aaa'#10);          //Will actually send 'aaa'#10
SendLn('aaa'#13);          //Will actually send 'aaa'#10
SendLn('aaa'#13#10);       //Will actually send 'aaa'#10
SendLn('aaa'#13#10'bbb');  //Will actually send 'aaa'#10'bbb'#10

As you can see, when a newline character is set, all the newlines are changed so that only the configured newline is used.

In general, "LineDelimSend" should be set to LDS_LF, for Linux / UNIX processes, and should be left at LDS_CRLF, for Windows processes.

The "LineDelimSend" property has no effect on the TConsoleProc.Send() method, which will always send the indicated characters.

### Line-ending in receiving

To set the line-ending characters, at the reception, you must use the "LineDelimRecv" property, which can take the following values:

* LDR_CRLF  //The line break is CR-LF (or LF-CR).
* LDR_CR    //The line break is this character. LF is ignored.
* LDR_LF    //The line break is this character. CR is ignored.
* LDR_CR_LF //The line break is CR or LF
   
When "LineDelimRecv" is set, the reception routines are being told how they should interpret the CR and LF characters. If for example "LineDelimRecv" is configured in LDR_LF, then every time the LF character is received, it will be interpreted as a line break, ignoring the CR character.

In general, "LineDelimRecv" should be set to LDR_LF, for Linux / UNIX processes, and it should be left at LDR_CRLF, for Windows processes.

The TTermVT100.OnLineCompleted() event is generated when the arrival of the line-ending character (or characters) is detected. This event will contain the current line as parameter.

## Prompt Detection

Prompt detection is done by exploring the text strings that are arriving from the process, to see if they match the detection parameters. This method may not be very reliable if the Prompt of the process changes too much.

The output data generated by the process is received in blocks that can have a variable size, but that do not exceed the constant UBLOCK_SIZE. The search for the prompt is always done on the last line of each block of data that is received from the process (in the ReadData() method). If the process must send too much information, it usually arrives in several blocks, but the prompt (at least the final part) is always expected to arrive in the last block received. Not every line received is scanned, to reduce the processing load.

To configure the detection of the Prompt, set the 'detecPrompt' property to TRUE and set values ​​for 'promptIni' and 'promptFin'. These strings determine the beginning and ending part of the prompt.

If the prompt does not change, you can put its value directly in 'promptIni' and leave 'promptFin' empty. But if the prompt is variable, you can put the initial part in 'promptIni' and the final part in 'promptFin', but choosing values ​​that do not lead to confusion with data lines.

So, for example to detect a prompt of type:

```[usuario@localhost ~]$ ```

It can be set: promptIni = '\[' and promptFin = '$ '. Spaces are also part of the text that must be configured.

By default, the prompt found is expected to be exactly the same as the last line of the text that arrives from the terminal. But there are additional options. The type of match can be configured in the variable 'promptMatch'. It can have the following values:

*   prmExactly,   //prompt is the whole line.
*   prmAtBegin,   //prompt appears at the beginning of the line
*   prmAtEnd,     //prompt appears at the end of the line
*   prmAtAnyPos   //prompt appears anywhere on the line

By default, 'promptMatch' is set to 'prmExactly'.

You can also use the automatic prompt configuration function, which is executed by calling the AutoConfigPrompt() method. But this method must be called when the Pprompt is visible in the last line of the terminal.

'AutoConfigPrompt' reads the last line assuming it is the prompt and automatically sets values for 'promptIni' and 'promptFin'.

You can also use a custom routine for prompt detection. This must be hooked to the OnChkForPrompt() event that has the form:

function (lin: string): boolean;

The event receives the line to be scanned and must return TRUE, if it is determined that the current line contains the prompt. Activating this event disables the internal detection of the unit.

## Using a Status bar

Optionally, when creating the process, you can pass the reference to a panel of a Statusbar, so animated icons are displayed to indicates the states of the connection. In this case, the OnDrawPanel () event of the Statusbar must be handled.

The next code use the Statusbar StatusBar1 to show animated icons representing the states of the connection.

```
  p := TConsoleProc.Create(StatusBar1.Panels[1]);  //Crea conexión
  StatusBar1.OnDrawPanel:=@SBDrawPanel;
  
  ...
  
  procedure Form1.SBDrawPanel(StatusBar:TStatusBar; Panel:TStatusPanel; const Rect:TRect);
  begin
   if panel.Index = 1 then p.DrawStatePanel(StatusBar.Canvas, Rect);
  end;

```
