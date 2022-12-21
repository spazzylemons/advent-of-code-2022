PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
OAMADDR   = $2003
PPUSCROLL = $2005
PPUADDR   = $2006
PPUDATA   = $2007

DMCCTRL   = $4010
OAMDMA    = $4014
APUSTATUS = $4015

.segment "HEADER"

; NROM, 16K PRG, 8K CHR, NES 2.0
.byt "NES", $1a
.byt $01
.byt $01

.zeropage

frame_counter: .res 1
sprite_pos: .res 1
row: .res 1
col: .res 1
ppu_pos: .res 2
tempvar: .res 1
local: .res 20

.code

input:
.incbin "10_input.bin"

.proc nmi
    inc frame_counter
    rti
.endproc

; busy wait for PPU to be ready - used during startup
.proc wait_vblank
    bit PPUSTATUS
    bne wait_vblank
    rts
.endproc

; reset vector
.proc reset
    ; turn off irq
    sei
    ; turn off decimal mode for clones
    cld
    ; set stack pointer
    ldx #$ff
    txs
    inx
    ; turn off nmi
    stx PPUCTRL
    stx PPUMASK
    stx DMCCTRL
    stx APUSTATUS
    txa
    ; warm up the ppu status register
    bit PPUSTATUS
    ; wait for vblank
    jsr wait_vblank
    ; initialize variables to zero
clear_ram:
    sta $00,x
    sta $100,x
    sta $200,x
    sta $300,x
    sta $400,x
    sta $500,x
    sta $600,x
    sta $700,x
    inx
    bne clear_ram
    ; wait for another vblank
    jsr wait_vblank
    ; clear nametable RAM
    lda #$20
    sta PPUADDR
    txa
    sta PPUADDR
    ldx #4
    tay
clear_nametable:
    sta PPUDATA
    dey
    bne clear_nametable
    dex
    bne clear_nametable
    lda #$ff
hide_sprites:
    sta $200,x
    inx
    bne hide_sprites
    stx OAMADDR
    lda #2
    sta OAMDMA
    ; set basic palette
    lda #$3f
    sta PPUADDR
    stx PPUADDR
    lda #$0f
    sta PPUDATA
    lda #$20
    sta PPUDATA
    lda #$3f
    sta PPUADDR
    lda #$10
    sta PPUADDR
    lda #$0f
    sta PPUDATA
    stx PPUDATA
    lda #$27
    sta PPUDATA
    ; fix scroll
    stx PPUSCROLL
    stx PPUSCROLL
    ; turn on screen
    lda #%00011110
    sta PPUMASK
    ; enable NMI
    lda #%10000000
    sta PPUCTRL
    ; solve part 2
    ldy #0
    inc sprite_pos
loop:
    jsr run_cycle
    lda input,y
    beq noop
    pha
    jsr run_cycle
    pla
    clc
    adc sprite_pos
    sta sprite_pos
noop:
    iny
    lda row
    cmp #6
    bne loop
end:
    jmp end
.endproc

.proc draw_sprite_sprite
    lda sprite_pos
    asl
    asl
    clc
    adc #44
    sta $207
    clc
    adc #8
    sta $20b
    lda #95
    sta $204
    sta $208
    lda #4
    sta $205
    lda #5
    sta $209
    lda #$20
    sta $206
    sta $20a
    ldx #$04
fill_downwards:
    lda $200,x
    clc
    adc #8
    sta $208,x
    lda $201,x
    sta $209,x
    lda $202,x
    sta $20a,x
    lda $203,x
    sta $20b,x
    inx
    inx
    inx
    inx
    cpx #$08+36
    bne fill_downwards
    rts
.endproc

.proc draw_beam_sprite
    lda col
    asl
    asl
    clc
    adc #48
    sta $203
    lda row
    asl
    asl
    asl
    clc
    adc #95
    sta $200
    lda #6
    sta $201
    lda #0
    sta $202
    rts
.endproc

.proc wait_for_frames
    ldx #5
loop:
    lda frame_counter
wait:
    cmp frame_counter
    beq wait
    dex
    bpl loop
    rts
.endproc

.proc run_cycle
    jsr wait_for_frames
    ldx col
    cpx sprite_pos
    beq draw_tile
    dex
    cpx sprite_pos
    beq draw_tile
    inx
    inx
    cpx sprite_pos
    bne no_drawing
draw_tile:
    lda row
    sta ppu_pos+0
    lda #0
    sta ppu_pos+1
    asl ppu_pos+0
    rol ppu_pos+1
    asl ppu_pos+0
    rol ppu_pos+1
    asl ppu_pos+0
    rol ppu_pos+1
    asl ppu_pos+0
    rol ppu_pos+1
    asl ppu_pos+0
    rol ppu_pos+1
    lda ppu_pos+0
    lda col
    lsr
    tax
    ora ppu_pos+0
    clc
    adc #$86
    sta ppu_pos+0
    lda #$21
    adc ppu_pos+1
    sta ppu_pos+1
    lda local,x
    sta tempvar
    lda col
    and #1
    clc
    adc #1
    ora tempvar
    sta tempvar
    sta local,x
    lda ppu_pos+1
    sta PPUADDR
    lda ppu_pos+0
    sta PPUADDR
    lda tempvar
    sta PPUDATA
    lda #0
    sta PPUSCROLL
    sta PPUSCROLL
no_drawing:
    jsr draw_beam_sprite
    jsr draw_sprite_sprite
    lda #0
    sta OAMADDR
    lda #2
    sta OAMDMA
    inc col
    lda col
    cmp #40
    bne no_carry
    lda #0
    sta col
    inc row
    ldx #19
    lda #0
clear_local:
    sta local,x
    dex
    bpl clear_local
no_carry:
    rts
.endproc

.segment "VECTORS"

.addr nmi
.addr reset
.addr 0

.segment "CHARS"

.incbin "10.chr"
