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

result_digits: .res 4

frame_counter: .res 1
address: .res 2

counts: .res 26
window_index: .res 1
num_ones: .res 1

input_ptr: .res 2

window: .res 14
current: .res 1
window_size: .res 1
initial_counter: .res 1

.code

input:
.incbin "input"

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

.proc print
    ; if new line, just move to next line and quit
    cmp #10
    bne not_new_line
    lda address+0
    and #$e0
    clc
    adc #$20
    sta address+0
    lda address+1
    adc #0
    sta address+1
    rts
not_new_line:
    ; save character
    pha
    ; wait for vblank
    lda frame_counter
wait:
    cmp frame_counter
    beq wait
    ; set vram address
    lda address+1
    sta PPUADDR
    lda address+0
    sta PPUADDR
    ; write character
    pla
    sta PPUDATA
    ; increment address
    inc address+0
    bne done
    inc address+1
done:
    ; fix scroll
    lda #0
    sta PPUSCROLL
    sta PPUSCROLL
    lda #$80
    sta PPUCTRL
    rts
.endproc

.proc print_string
    lda string,y
    beq end
    jsr print
    iny
    jmp print_string
end:
    rts
.endproc

.proc print_result
    lda result_digits+0
    jsr print
    lda result_digits+1
    jsr print
    lda result_digits+2
    jsr print
    lda result_digits+3
    jmp print
.endproc

string:

string_part_1:
.byte "part 1: ", 0
string_part_2:
.byte "part 2: ", 0

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
    ; set basic palette
    lda #$3f
    sta PPUADDR
    stx PPUADDR
    lda #$0f
    sta PPUDATA
    lda #$20
    sta PPUDATA
    ; fix scroll
    stx PPUSCROLL
    stx PPUSCROLL
    ; set the initial print address, accounting for vertical overscan
    lda #$20
    sta address+0
    lda #$60
    sta address+1
    ; turn on screen
    lda #%00001010
    sta PPUMASK
    ; enable NMI
    lda #%10000000
    sta PPUCTRL
    ; part 1
    lda #4
    sta window_size
    jsr solve
    ldy #string_part_1-string
    jsr print_string
    jsr print_result
    lda #10
    jsr print
    ; part 2
    lda #14
    sta window_size
    jsr solve
    ldy #string_part_2-string
    jsr print_string
    jsr print_result
    lda #10
    jsr print
end:
    jmp end
.endproc

.proc read_input
    ldy #0
    lda (input_ptr),y
    sta current
    inc input_ptr+0
    bne no_carry
    inc input_ptr+1
no_carry:
    rts
.endproc

.proc get_count_index_x
    ldx window_index
    lda window,x
    sec
    sbc #$61
    tax
    rts
.endproc

.proc increment_count
    jsr get_count_index_x
    lda counts,x
    cmp #1
    bne not_one
    dec num_ones
    jmp merge
not_one:
    cmp #0
    bne merge
    inc num_ones
merge:
    inc counts,x
    rts
.endproc

.proc decrement_count
    jsr get_count_index_x
    lda counts,x
    cmp #1
    bne not_one
    dec num_ones
    jmp merge
not_one:
    cmp #2
    bne merge
    inc num_ones
merge:
    dec counts,x
    rts
.endproc

.proc increment_result_digits
    ldx #3
loop:
    inc result_digits,x
    lda result_digits,x
    cmp #$3a
    bne no_carry
    lda #'0'
    sta result_digits,x
    dex
    jmp loop
no_carry:
    rts
.endproc

.proc solve
    ; initialize variables
    lda #0
    sta window_index
    sta num_ones
    sta initial_counter
    tay
clear_counts:
    sta counts,y
    iny
    cpy #26
    bne clear_counts
    lda #'0'
    sta result_digits+0
    sta result_digits+1
    sta result_digits+2
    sta result_digits+3
    lda #<input
    sta input_ptr+0
    lda #>input
    sta input_ptr+1
loop:
    ; read a byte
    jsr read_input
    jsr increment_result_digits
    ; don't decrement counts if window not full
    lda initial_counter
    cmp window_size
    beq window_is_full
    inc initial_counter
    jmp window_not_full
window_is_full:
    jsr decrement_count
window_not_full:
    lda current
    ldx window_index
    sta window,x
    jsr increment_count
    inc window_index
    lda window_index
    cmp window_size
    bne no_wrap
    lda #0
    sta window_index
no_wrap:
    lda num_ones
    cmp window_size
    bne loop
    rts
.endproc

.segment "VECTORS"

.addr nmi
.addr reset
.addr 0

.segment "CHARS"

.incbin "font.chr"
