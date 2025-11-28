INCLUDE Irvine32.inc

.data

;---------------- For Filling (Login) -----------------------

BUFFER_SIZE = 250
buffer BYTE BUFFER_SIZE DUP(?) 
filename BYTE "LoginData.txt",0   
fileHandle HANDLE ?

;---------------- New Variables for Logic 1 & 2 ----------------

currentID BYTE BUFFER_SIZE DUP(?) ; To store ID for later use
regNum    BYTE BUFFER_SIZE DUP(?) ; To store Reg Number
logFile   BYTE "ParkingLog.txt",0 ; File for vehicle records
logHandle HANDLE ?

askID     BYTE " --> Enter National ID : ",0
askReg    BYTE " --> Enter Vehicle Reg No : ",0

txtType   BYTE "Type: ",0
txtReg    BYTE " | Reg: ",0
txtUser   BYTE " | UserID: ",0
newLine   BYTE 0Dh, 0Ah, 0       

strRik    BYTE "Rikshaw",0
strCar    BYTE "Car",0
strBus    BYTE "Bus",0

;---------------- Bill Generation Variables ----------------
fileBuffer BYTE 5000 DUP(?)     ; Buffer to read file log
bytesRead  DWORD ?              ; Count of bytes read
billTotal  DWORD 0              ; Sum of fare
billMsg    BYTE "------- YOUR PARKING HISTORY -------",0Ah,0
billTotalMsg BYTE " >> TOTAL FARE DUE: ",0
strRikMatch BYTE "Rikshaw",0    ; For comparison
strCarMatch BYTE "Car",0        ; For comparison
strBusMatch BYTE "Bus",0        ; For comparison

;---------------- For Color -----------------------

fore DWORD 9   ; 11 = LightCyan (Foreground)
back DWORD 0   ;  0 = Black (Background)

;---------------- Printing Statements -----------------------

menu BYTE "***************** MENU *********************",0ah,0ah

menu0 BYTE ' Press ( 0 / Enter ) to Clear Screen ',0ah,0ah
menu1 BYTE ' Press 1 for Rikshaw ',0ah
menu2 BYTE ' Press 2 for Cars ',0ah
menu3 BYTE ' Press 3 for Bus ',0ah
menu4 BYTE ' Press 4 to Show the Record ',0ah
menu5 BYTE ' Press 5 to Clear the Records ',0ah
m3    BYTE " Press 6 to Remove Vehicle ",0ah
menu7 BYTE " Press 7 to Generate Bill ",0ah  ; NEW OPTION
menu6 BYTE ' Press 8 to Exit ',0             ; MOVED TO 8

my0 BYTE " (1) Remove a Rikshaw",0ah
my1 BYTE " (2) Remove a Car ",0ah
my2 BYTE " (3) Remove a Bus ",0ah
ml  BYTE " (4) Return to Menu ",0

str1 BYTE " ERROR in File ",0
str2 BYTE "	 --- Welcome to KMC Parking Plaza --- ",0ah,0ah,0 
str3 BYTE " --> Enter National ID : ",0 
str4 BYTE " Welcome User ID : ",0

msg0 BYTE ' --> Parking Is Full',0
msg1 BYTE ' --> Wrong input',0

m1 BYTE "*** Clear Screen ***",0
m2 BYTE "Press OK to Clear Screen ",0
my3 BYTE "--> Vehicle Removed Successfully ",0
my4 BYTE "--> Vehicle isn't Parked ",0

msg2 BYTE ' Rikshaw	',0
msg3 BYTE ' Car	',0
msg4 BYTE ' Bus	',0
msg5 BYTE '	--- Record --- ',0

p1 BYTE 'Parking Fee : ',0
p2 BYTE 'Vehicle : ',0
p3 BYTE 'COUNT : ',0
p4 BYTE ' ---> Total Capacity : 10 Vehicles ',0ah,0

msg6 BYTE ' The Total Amount is : ',0
msg7 BYTE ' The Total Numbers of Vehicles Parked : ',0
msg8 BYTE ' The Total Number of Rikshws Parked : ',0
msg9 BYTE ' The Total Number of Cars Parked : ',0
msg10 BYTE ' The Total Number of Buses Parked : ',0

Choose BYTE "Enter Choice : ",0
msg12 BYTE ' *** Records Deleted Successfully *** ',0
final BYTE " --> Thanks For Using My Application ",0

;---------------- Variables -----------------------

amount WORD 0
count WORD 0
strL DWORD ?
TOTAL DWORD ?

am1 WORD ?
am2 WORD ?
am3 WORD ?

x WORD 0 ; rikshaw
y WORD 0 ; car
z WORD 0 ; bus

;-------------------- MAIN Function ------------------------

.code

main PROC
	
	call setcolor
	call filling    ; Performs Login and Opens Log File
	call ShowMenu

	call crlf
	mov  edx,OFFSET final
	call WriteString

call crlf
exit
main ENDP

;------------------------------------------------------------
; Custom Procedure to find substring (Since Str_find is missing)
; Input:  EBX = Address of Source String
;         EDX = Address of Pattern String
; Output: EAX = 1 if Found, 0 if Not Found
;------------------------------------------------------------
IsSubstring PROC USES ebx ecx edx esi edi
    mov esi, ebx    ; ESI points to Source
    
MainLoop:
    mov al, [esi]
    cmp al, 0
    je NotFound     ; End of source, pattern not found
    
    ; Compare at current position
    push esi        ; Save current source position
    mov edi, edx    ; EDI points to Pattern

CheckMatch:
    mov ah, [edi]
    cmp ah, 0       ; End of pattern?
    je MatchFound   ; If we reached end of pattern, we found a match!
    
    mov al, [esi]
    cmp al, ah
    jne Mismatch    ; Char mismatch
    
    inc esi
    inc edi
    jmp CheckMatch

Mismatch:
    pop esi         ; Restore source position
    inc esi         ; Move to next char in source
    jmp MainLoop

MatchFound:
    pop esi         ; Clean up stack
    mov eax, 1
    ret

NotFound:
    mov eax, 0
    ret
IsSubstring ENDP

;------------------------------------------------------------

filling proc

; CREATE FILE FOR LOGIN CHECK
mov edx,OFFSET filename
call CreateOutputFile
mov fileHandle,eax

cmp eax, INVALID_HANDLE_VALUE
jne file_ok
mov edx,OFFSET str1
call WriteString
jmp quit

file_ok:

mov edx,offset str2
call WriteString

; --- LOGIC 1: Ask for National ID ---
mov edx,offset askID
call WriteString
mov edx,0
clc
mov edx,OFFSET buffer
mov ecx,BUFFER_SIZE
call ReadString
mov strL,eax

; Save the ID to currentID
invoke Str_copy, addr buffer, addr currentID

; WRITE LOGIN DATA TO FILE
mov eax,fileHandle
mov edx,OFFSET buffer
mov ecx,strL
call WriteToFile

; CLOSE LOGIN FILE
call CloseFile	

; --- CREATE/OPEN MAIN LOG FILE FOR VEHICLES ---
mov edx, OFFSET logFile
call CreateOutputFile
mov logHandle, eax

; Print Text on Console
call crlf
mov edx,OFFSET str4
call WriteString
mov edx,OFFSET buffer
call WriteString
call crlf
call crlf
call WaitMsg
call clrscr

quit:
ret 8
filling endp

;------------------------------------------------------------
; Helper: WriteLog
;------------------------------------------------------------

WriteLog PROC USES eax ecx edx
    
    ; 1. Write Type
    push edx
    invoke Str_length, edx
    mov ecx, eax
    pop edx
    mov eax, logHandle
    call WriteToFile

    ; 2. Write " | Reg: "
    mov edx, OFFSET txtReg
    mov ecx, LENGTHOF txtReg
    dec ecx 
    mov eax, logHandle
    call WriteToFile

    ; 3. Write Registration Number
    mov edx, OFFSET regNum
    invoke Str_length, edx
    mov ecx, eax
    mov eax, logHandle
    call WriteToFile

    ; 4. Write " | UserID: "
    mov edx, OFFSET txtUser
    mov ecx, LENGTHOF txtUser
    dec ecx
    mov eax, logHandle
    call WriteToFile

    ; 5. Write User ID
    mov edx, OFFSET currentID
    invoke Str_length, edx
    mov ecx, eax
    mov eax, logHandle
    call WriteToFile

    ; 6. Write New Line
    mov edx, OFFSET newLine
    mov ecx, LENGTHOF newLine
    dec ecx
    mov eax, logHandle
    call WriteToFile

    ret
WriteLog ENDP

;------------------------------------------------------------

setcolor proc

mov eax,0
mov ebx,0
clc
mov edx,0

mov eax,back
mov ebx,16

mul ebx

mov ebx,fore
add eax,ebx

call SetTextColor

ret 
setcolor endp

;------------------------------------------------------------

ShowMenu PROC
	
	call crlf
	mov edx,offset p4
	call writestring
	
	call crlf
	mov  edx,OFFSET menu ; Display Menu
	call WriteString
	
	call crlf
	call crlf
	mov edx,offset Choose
	call writestring	; CHOICE :
	call ReadInt

	call select
	ret 8

ShowMenu endp

;-----------------------------------------------------------------------

select proc

	cmp eax,0
	je clear
	jmp start

clear:
	mov ebx,offset m1
	mov edx,offset m2
	
	call MsgBox
	call crlf
	call clrscr
	call ShowMenu

start:
	cmp  eax,1
	jne  L1
	call crlf
	call rikshw
L1:
	cmp  eax,2	
	jne  L2
	call crlf
	call car
L2:
	cmp  eax,3	
	jne   L3
	call crlf
	call bus	
L3:
	cmp  eax,4	
	jne   L4
	call crlf
	call rec
L4:
	cmp  eax,5	
	jne   L5
	call crlf
	call del
L5:	
	cmp eax,6
	jne L6
	call crlf
	call rem
L6:
    ; --- NEW: Check for Option 7 (Bill) ---
    cmp eax, 7
    jne L7
    call crlf
    call genBill
    call crlf
    call WaitMsg
    call clrscr
    call ShowMenu
    
L7:
    ; --- MOVED: Exit is now 8 ---
	cmp eax,8
	jne quit

	call crlf
	jmp end_

quit:
    ; Close file before exit
    mov eax, logHandle
    call CloseFile

	call crlf
	mov edx,offset msg1
	call writestring
	call crlf
	call crlf
	call WaitMsg
	call clrscr
	call ShowMenu
	
call ShowMenu

end_:
ret
select endp

;-----------------------------------------------------------------------
; NEW PROCEDURE: Generate Bill
;-----------------------------------------------------------------------
genBill PROC USES eax ebx ecx edx esi edi
    
    ; 1. Close the current Write Handle
    mov eax, logHandle
    call CloseFile

    ; 2. Open File for Reading
    mov edx, OFFSET logFile
    call OpenInputFile
    cmp eax, INVALID_HANDLE_VALUE
    je FileError

    ; --- FIX STARTS HERE ---
    mov ebx, eax            ; 1. SAVE the valid File Handle into EBX

    ; Read file into buffer
    mov edx, OFFSET fileBuffer
    mov ecx, SIZEOF fileBuffer
    
    ; ReadFromFile needs the handle in EAX (it is already there),
    ; but it will overwrite EAX with the byte count.
    call ReadFromFile
    mov bytesRead, eax      ; Save the count of bytes read

    mov eax, ebx            ; 2. RESTORE the File Handle from EBX to EAX
    call CloseFile          ; 3. Now CloseFile has the correct handle!
    ; --- FIX ENDS HERE ---

    ; 3. Process the Buffer
    call clrscr
    mov edx, OFFSET billMsg
    call WriteString
    call crlf

    mov billTotal, 0
    
    mov esi, OFFSET fileBuffer ; ESI = Current character pointer
    mov edi, esi               ; EDI = Start of current line
    add bytesRead, esi         ; Calculate end address

ScanLoop:
    cmp esi, bytesRead
    jae DoneScanning

    ; Check for New Line (0Dh = CR)
    mov al, [esi]
    cmp al, 0Dh
    jne NextChar

    ; Temporarily null-terminate this line
    mov byte ptr [esi], 0

    ; --- Logic: Does this line contain the currentID? ---
    ; Using Custom IsSubstring Procedure
    mov ebx, edi            ; Source string
    mov edx, OFFSET currentID ; Pattern
    call IsSubstring
    cmp eax, 1
    jne LineDone ; Not found, skip line

UserFound:
    ; 1. Print the line
    mov edx, edi
    call WriteString
    call crlf

    ; 2. Determine price based on Type
    ; Check "Car"
    mov ebx, edi
    mov edx, OFFSET strCarMatch
    call IsSubstring
    cmp eax, 1
    je IsCar

    ; Check "Bus"
    mov ebx, edi
    mov edx, OFFSET strBusMatch
    call IsSubstring
    cmp eax, 1
    je IsBus

    ; Check "Rikshaw"
    mov ebx, edi
    mov edx, OFFSET strRikMatch
    call IsSubstring
    cmp eax, 1
    je IsRik

    jmp LineDone

IsCar:
    add billTotal, 400
    jmp LineDone
IsBus:
    add billTotal, 600
    jmp LineDone
IsRik:
    add billTotal, 200
    jmp LineDone

LineDone:
    ; Restore the CR
    mov byte ptr [esi], 0Dh
    
    ; Move past CR and LF 
    inc esi ; Skip CR
    inc esi ; Skip LF
    mov edi, esi ; Update start of new line
    jmp ScanLoop

NextChar:
    inc esi
    jmp ScanLoop

DoneScanning:
    call crlf
    mov edx, OFFSET billTotalMsg
    call WriteString
    mov eax, billTotal
    call WriteDec
    call crlf
    jmp RestoreHandle

FileError:
    mov edx, OFFSET str1
    call WriteString
    jmp RestoreHandle

RestoreHandle:
    ; 4. Re-open the file for APPENDING
    
    push 0
    push 128            ; FILE_ATTRIBUTE_NORMAL
    push 3              ; OPEN_EXISTING
    push 0
    push 0
    push 40000000h      ; GENERIC_WRITE
    push OFFSET logFile
    call CreateFile
    mov logHandle, eax

    ; Move File Pointer to the End
    push 2              ; FILE_END
    push 0
    push 0
    push logHandle
    call SetFilePointer

    ret
genBill ENDP

;-----------------------------------------------------------------------

rikshw PROC		
	mov edx,offset p2
	call writeString
	mov edx,OFFSET msg2
	call WriteString

	cmp count,9
	ja L1
    
    call crlf
    mov edx, OFFSET askReg
    call WriteString
    mov edx, OFFSET regNum
    mov ecx, BUFFER_SIZE
    call ReadString
    
    mov edx, OFFSET strRik 
    call WriteLog

	mov ax,200

	call crlf
	mov edx,offset p1
	call writeString
	call writeDec

	add amount,ax
	mov dx,0
	mov bx,10
	mov cx,0
L2:
	div bx
	inc cx
	cmp ax,0
	jne l2

inc count
inc x

call crlf
mov edx,offset p3
call writeString
mov ax,count
call writeDec

call crlf
mov ax,amount
mov am1,ax

call crlf
call waitmsg
call crlf
call clrscr
call ShowMenu
	
jmp end_

L1:
call crlf
call crlf
mov edx,offset msg0
call writeString
call crlf
call crlf
call WaitMsg
call clrscr
call ShowMenu

end_:
ret 8
rikshw endp

;-----------------------------------------------------------------------

car proc
	mov edx,offset p2
	call writeString
	mov edx,OFFSET msg3
	call WriteString

	cmp count,9
	ja L1
    
    call crlf
    mov edx, OFFSET askReg
    call WriteString
    mov edx, OFFSET regNum
    mov ecx, BUFFER_SIZE
    call ReadString

    mov edx, OFFSET strCar 
    call WriteLog

	mov ax,400

	call crlf
	mov edx,offset p1
	call writeString
	call writeDec

	add amount,ax
	mov dx,0
	mov bx,10
	mov cx,0
L2:
	div bx
	inc cx
	cmp ax,0
	jne l2

inc count
inc y

call crlf
mov edx,offset p3
call writeString
mov ax,count
call writeDec

call crlf
mov ax,amount
mov am2,ax

call crlf
call waitmsg
call crlf
call clrscr
call ShowMenu

jmp end_

L1:
call crlf
call crlf
mov edx,offset msg0
call writeString
call crlf
call crlf
call WaitMsg
call clrscr
call ShowMenu

end_:
ret 8
car endp

;-----------------------------------------------------------------------

bus proc
	mov edx,offset p2
	call writeString
	mov edx,OFFSET msg4
	call WriteString

	cmp count,9
	ja L1

    call crlf
    mov edx, OFFSET askReg
    call WriteString
    mov edx, OFFSET regNum
    mov ecx, BUFFER_SIZE
    call ReadString

    mov edx, OFFSET strBus 
    call WriteLog

	mov ax,600

	call crlf
	mov edx,offset p1
	call writeString
	call writeDec

	add amount,ax
	mov dx,0
	mov bx,10
	mov cx,0
L2:
	div bx
	inc cx
	cmp ax,0
	jne l2

inc count
inc z

call crlf
mov edx,offset p3
call writeString
mov ax,count
call writeDec

call crlf
mov ax,amount
mov am3,ax

call crlf
call waitmsg
call crlf
call clrscr
call ShowMenu

jmp end_

L1:
call crlf
call crlf
mov edx,offset msg0
call writeString
call crlf
call crlf
call WaitMsg
call clrscr
call ShowMenu

end_:
ret 8
bus endp

;-----------------------------------------------------------------------

rec proc
	
	mov edx,OFFSET msg5
	call WriteString

	call crlf
	call crlf

	mov edx,offset msg6
	call writeString
	mov ax,amount
	call writeDec

	call crlf
	mov edx,offset msg7
	call writeString
	mov ax,x
	mov bx,y
	add ax,bx
	mov bx,z
	add ax,bx
	mov TOTAL,eax
	call writeDec

	call crlf
	mov edx,offset msg8
	call writeString
	mov ax,x
	call writeDec

	call crlf
	mov edx,offset msg9
	call writeString
	mov ax,y
	call writeDec

	call crlf
	mov edx,offset msg10
	call writeString
	mov ax,z
	call writeDec

	call crlf
	call crlf
	call WaitMsg
	call clrscr
	call ShowMenu

ret 8
rec endp

;-----------------------------------------------------------------------

del proc

	mov eax,0
	mov ebx,0
	clc
	mov edx,0

	mov count,0
	mov amount,0

	mov am1,0
	mov am2,0
	mov am3,0

	mov x,0
	mov y,0
	mov z,0

	call crlf

	mov edx,offset msg12
	call writeString
	call crlf
	call crlf
	call waitmsg
	call clrscr
	call crlf
	call ShowMenu
	ret 8
del endp

;-----------------------------------------------------------------------

rem PROC

call clrscr
call crlf

mov edx,offset my0
call WriteString
mov edx,offset Choose
call crlf
call crlf
call WriteString
call readint

cmp eax,1
jne L1

cmp x,0
je err

dec count
dec TOTAL
dec x
jmp quit

L1:
cmp eax,2
jne L2

cmp y,0
je err

dec count
dec TOTAL
dec y
jmp quit

L2:
cmp eax,3
jne L3

cmp z,0
je err

dec count
dec TOTAL
dec z
jmp quit

L3:
cmp eax,4
je last
jmp uff

uff:
call crlf
mov edx,offset msg1
call writeString
jmp now

err:
call crlf
mov edx,offset my4 ; Vehicle isn't Parked
call writeString
call crlf
call crlf
call waitmsg
call rem

quit:
call crlf
call crlf
mov edx,offset my3 ; REMOVED Vehicle
call WriteString

now:
call crlf
call crlf
call waitmsg
call rem

last:
call crlf
call crlf
call waitmsg
call crlf
call crlf
call clrscr
call ShowMenu
ret
rem ENDP

;-----------------------------------------------------------------------
END main