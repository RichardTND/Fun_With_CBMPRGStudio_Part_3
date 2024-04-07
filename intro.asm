;---------------------------------------
;Fun With CBMPRGStudio - Part 3
;Scene World issue #34
;
;Simple intro with custom character set
;graphics.
;
;Code, graphics and music by RICHARD
;---------------------------------------

;Variables

;Declare screen RAM as a variable
;to point at default screen RAM
;$0400

screen = $0400
colour = $d800

;Declare matrix as a variable 
;to point at $2800. Where the 
;matrix file is imported to. 

matrix = $2800 

;Declare raster split position as
;five variables

split1 = $42
split2 = $82
split3 = $aa
split4 = $ba
split5 = $fa


;Declare music init and play addresses

musicinit = $1000
musicplay = $1003

;Declare start address of linked prg

startaddress = $fce2 ;soft reset

;Set target in CBMPRGSTUDIO 
;to Commodore 64

TGT_C64

;Insert BASIC SYS run address for 
;program at 2064 ($0810)

; 10 SYS2064

        *=$0801

        BYTE $0B,$08,$0A,$00,$9E
        BYTE $32,$30,$36,$34,$00
        BYTE $00,$00

;Code starts here

        *=$0810

        sei

;Set border and background colour to
;black.

        lda #$00
        sta $d020
        sta $d021

        ;Char multicolour 1 = dark blue
        ldx #$06
        ;Char multicolour 2 = light blue
        ldy #$0e
        stx $d022 
        sty $d023
        
;Fill screen RAM with the space char
;inside the screen RAM.

        ldx #$00
clear   lda #$20
        sta screen,x
        sta screen+$100,x
        sta screen+$200,x
        sta screen+$2e8,x

        ;Multicolour cyan
        lda #$0b
        sta colour,x
        sta colour+$100,x
        sta colour+$200,x
        sta colour+$2e8,x
        inx
        bne clear

;Draw the logo on to the screen 
;two rows after the starting row and
;40 columns across, 8 rows down.

        ldx #$00
drawlogo
        lda matrix,x
        sta screen+80,x
        lda matrix+40,x
        sta screen+120,x
        lda matrix+80,x
        sta screen+160,x
        lda matrix+120,x
        sta screen+200,x
        lda matrix+160,x
        sta screen+240,x
        lda matrix+200,x
        sta screen+280,x
        lda matrix+240,x
        sta screen+320,x
        lda matrix+280,x
        sta screen+360,x
        inx
        cpx #40
        bne drawlogo

;Place presentation lines on to the
;screen RAM, two rows after the logo
;matrix.

        ldx #$00
drawlines
        lda line1,x
        sta screen+440,x
        lda line2,x
        sta screen+520,x

        ;Black out lines and scroll
        ;line. 

        lda #0
        sta colour+440,x
        sta colour+520,x
        sta colour+640,x

        inx
        cpx #40
        bne drawlines

        ;Initialize the flash pointers

        lda #0
        sta fadedelay
        sta fadepointer
        sta fadestore

;Initialize the scrolling message

        lda #<scrolltext
        sta messread+1
        lda #>scrolltext
        sta messread+2
        
;Initialize and setup IRQ raster 
;interrupt.

        ldx #<irq1
        ldy #>irq1
        lda #$7f
        stx $0314
        sty $0315
        sta $dc0d
        sta $dd0d
        lda #$2a
        sta $d012
        lda #$1b
        sta $d011
        lda #$01
        sta $d01a
        
        ;Initialize music
        lda #$00
        jsr musicinit

        cli

;Main intro loop 
        
introloop
        ;Subr. synchronize timer
        jsr synctimer
        ;Subr. scroller 
        jsr scroller
        ;Subr. colourrtn (colour flash)
        jsr colourrtn
        ;Wait for spacebar
        lda $dc01
        cmp #$ef
        beq exitintro
        jmp introloop

;Intro has finished. Exit the intro,
;switch off all IRQs and call relocator

exitintro
        sei

        ;Kill IRQ interrupts

        lda #$00
        sta $d019
        sta $d01a
        lda #$81
        sta $dc0d
        sta $dd0d
        lda #$1b
        sta $d011
        ldx #$31
        ldy #$ea
        stx $0314
        sty $0315
        cli

        ;Init with C64 blue screen
        jsr $ff81
        lda #0
        sta $d020
        sta $d021

        ;Clear SID chip

        ldx #$00
silence
        lda #$00
        sta $d400,x
        inx
        cpx #$18
        bne silence

        ;Copy transfer data to low mem

        ldx #$00
copytrs
        lda relocator,x
        sta $0400,x
        lda #0
        sta $d800,x
        inx
        bne copytrs

        lda #$00
        sta $0800
        cli
        jmp $0400

;Main relocate subroutine

relocator
        sei
        lda #$34
        sta $01
        
reloc1  ldx #$00 
reloc2  lda $4000,x ;linked prg pos
        sta $0801,x ;basic ram pos
        inx
        bne reloc2
        inc $0409
        inc $040c
        lda $0409
        bne reloc1
        lda #$37
        sta $01
        cli
        jmp startaddress

;Synchronize raster timer (rt) with
;main intro loop and IRQ raster 
;interrupt

synctimer
        lda #0
        sta rt
        cmp rt
        beq *-3
        rts

;1x1 Scroller routine

scroller
        lda xpos
        sec
        sbc #2 ;Scroll speed
        and #7 ;BITS 0-8
        sta xpos
        bcs exitscroll

        ;Shift screen mem for text

        ldx #$00
shift   lda screen+641,x
        sta screen+640,x
        inx
        cpx #$27 ;39 chars
        bne shift

        ;Record scroll read position

messread
        lda scrolltext
        cmp #$00
        bne store
        
        ;@ detected, reset scroll text

        lda #<scrolltext
        sta messread+1
        lda #>scrolltext
        sta messread+2
        jmp messread

store   sta screen+679
        
        ;Update position of scroll 
        ;read low byte to read next
        ;byte of text.

        inc messread+1
        lda messread+1
        bne exitscroll

        ;256 bytes of low byte read
        ;read the next hi byte of
        ;scroll text

        inc messread+2
        
exitscroll
        rts

;Colour flash and washing routine

colourrtn
        lda fadedelay
        cmp #1 ;Duration of fade
        beq fademain
        inc fadedelay
        rts

        ;Main colour fade routine

fademain
        lda #0 ;reset fade delay 
        sta fadedelay ;pointer
        
        ldx fadepointer
        lda colourtable,x
        sta fadestore
        inx
        cpx #$28 ;40 bytes read?
        beq looptable
        inc fadepointer ;next byte
        jmp storecols

        ;Reset table read 
looptable
        ldx #0
        stx fadepointer

        ;Main colour washing/flashing
        ;routines.

storecols

    ;1. wash colour from right of
    ;screen to left for top text line

        lda fadestore
        sta colour+479
        
        ;Move colour backwards

        ldx #$00
shiftback
        lda colour+441,x
        sta colour+440,x
        inx
        cpx #$28 ;40 bytes read
        bne shiftback

     ;2. wash colour from left of 
     ;screen to right for bottom
     ;text line

        lda fadestore
        sta colour+520

        ;Move colour forwards

        ldx #$27 ;reverse loop
shiftfwd
        lda colour+519,x
        sta colour+520,x
        dex ;reached 0 bytes?
        bpl shiftfwd

     ;3. flash the main scroll text

        ldx #$00
flashtext
        lda fadestore
        sta colour+640,x
        inx
        cpx #$28 ;40 chars read
        bne flashtext
        rts

        

        

        
        

;Setup IRQ raster interrupt - Still
;screen, and synchronize raster timer
;(rt). 

irq1    inc $d019
        lda $dc0d
        sta $dd0d
        lda #split1
        sta $d012
        lda #1
        sta rt 
 
        ;Test colour BLACK

      ; lda #0
      ; sta $d020


        ;Play music
        jsr musicplay

        ;Point to next IRQ raster 
        ;interrupt

        
        ldx #<irq2
        ldy #>irq2
        stx $0314
        sty $0315
        jmp $ea7e

;Setup IRQ raster interrupt 2 - 
;Logo matrix position

irq2    inc $d019
        lda #split2
        sta $d012

        

        ;Setup screen multicolour mode
        ;for logo.

        lda #$18
        sta $d016

        ;Set charset mode at $2000 
        ;(where logo charset is placed)

        lda #$18
        sta $d018
        
        ;Test colour WHITE

      ;  lda #1
      ;  sta $d020

        ;Point to next IRQ raster 
        ;interrupt 

        ldx #<irq3
        ldy #>irq3
        stx $0314
        sty $0315
        jmp $ea7e

;Setup IRQ raster interrupt 3 -
;Still text 

irq3    inc $d019
        lda #split3
        sta $d012

        ;Set char multicolour mode 
        ;to off

        lda #$08
        sta $d016

        ;Set charset mode for 1x1
        ;character set which is at
        ;$3000

        lda #$1c
        sta $d018

      ; lda #2
      ; sta $d020

        ldx #<irq4
        ldy #>irq4
        stx $0314
        sty $0315
        jmp $ea7e

;Setup IRQ raster interrupt 4 -
;Smooth scroll text 

irq4    inc $d019
        lda #split4
        sta $d012

        ;Set char scroll byte to 
        ;screen horizontal position 
        ;for smooth scroll hires mode

        lda xpos
        sta $d016

      ;  lda #3
      ;  sta $d020

        ldx #<irq5
        ldy #>irq5
        stx $0314
        sty $0315
        jmp $ea7e

;Setup IRQ raster interrupt 5 -
;Still screen again.

irq5    inc $d019
        lda #split5
        sta $d012

        ;Set char multicolour off

        lda #$08
        sta $d016

        ;Test border
        ;lda #$04
        ;sta $d020

        ;Jump back to IRQ 1

        ldx #<irq1
        ldy #>irq1
        stx $0314
        sty $0315
        jmp $ea7e 

xpos    byte 0

;Sync raster time control byte 

rt      byte 0

;Colour fade pointers

fadedelay 
        byte 0
fadepointer
        byte 0
fadestore
        byte 0

;Colour table

colourtable
 byte $06,$04,$0e,$05,$03,$0d,$01,$01
 byte $01,$01,$01,$01,$01,$01,$01,$01
 byte $01,$01,$01,$01,$01,$01,$01,$01
 byte $01,$01,$01,$01,$01,$01,$01,$01
 byte $01,$0d,$03,$05,$0E,$04,$06,$06


;2 line presentation text

line1 text '            proudly '
      text 'presents            '
line2 text '     fun with cbmprg'
      text 'studio - part 3     '

;---------------------------------------

;Include music data
        *=$1000
        incbin "c64/music.prg",2,0

;---------------------------------------

;Include logo charset data
        *=$2000
        incbin "c64/logo_charset.bin"

;---------------------------------------

;Include logo matrix data
        *=$2800
        incbin "c64/logo_matrix.bin"

;---------------------------------------

;Include 1x1 charset data
        *=$3000
        incbin "c64\charset.bin"

;---------------------------------------

;Scroll text message
        *=$3800
scrolltext
 text ' ... scene world is proud to pr'
 text 'esent another edition of - fun '
 text 'with cbmprgstudio - ...   this '
 text 'is a fun example of what you ca'
 text 'n code in cbmprgstudio by follo'
 text 'wing the features in scene worl'
 text 'd ...   last issue i showed you'
 text ' how to make a simple intro usi'
 text 'ng petcsii graphics ...   this '
 text 'edition shows you how to make a'
 text 'n intro with custom character s'
 text 'et graphics and make use of fiv'
 text 'e raster irqs in place ....   p'
 text 'lease read scene world issue 34'
 text ' to learn the basic steps in co'
 text 'ding a simple and pretty cool i'
 text 'ntro like this ...   intro code'
 text ', graphics and music by richard'
 text ' ...                           '
 byte 0

;--------------------------------------