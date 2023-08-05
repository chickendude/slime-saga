include "inc/hardware.inc"

def WIDTH_T equ 10
def map_w equ _HRAM

section "header", rom0[$0100]
entrypoint:
	di
	jr start
	ds ($0150 - @), 0


section "main", rom0[$0150]
start:
	ld a, [rLY]
	cp SCRN_Y
	 jr nz, start
	xor a
	ld [rLCDC], a

	ld a, %11100100 ; light to dark
	ld [rBGP], a	; update palette

	ld de, tile_data
	ld hl, _VRAM8000
	ld b, 16
.loop:
	ld a, [de]
	inc de
	ld [hl+], a
	ld a, high(tile_end)
	dec b
	 ;jr z, main
	cp d
	 jr nz, .loop
	ld a, low(tile_end)
	cp e
	 jr nz, .loop

	call load_tilemap

	ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG9800 | LCDCF_BG8000
	ld [rLCDC], a

.vblank
	ld a, [rLY]
	cp SCRN_Y
	 jr nz, .vblank

main:
	jr main


; Loads tile map into VRAM
load_tilemap:
	ld de, tilemap
	ld hl, _SCRN0
	ld c, 9
	ld a, [de]
	sub a, WIDTH_T
	ldh [map_w], a
.row:
	ld b, WIDTH_T	; 10 16x16 tiles wide (160 pixels)
.draw_sprite:
	inc de			; check next entry in tilemap
	ld a, [de]		; tile id
	; cp $FF			; check if end of map
	 ; ret z			; quit if end of map
	add a, a		; x2
	push hl			; hl = SCRN
	push bc			; bc = loop counters
		ld [hl+], a	; first tile into SCRN0 (16x16 tiles have 4 8x8 tiles)
		inc a		; next tile id
		ld [hl], a	; byte 2
		ld bc, $1F	; move down to next row in tilemap (32 tiles wide)
		add hl, bc
		add a, $F	; move down to next row in tileset
		ld [hl+], a	; byte 3
		inc a
		ld [hl], a	; byte 4
	pop bc
	pop hl
	inc hl			; shift to next 16x16 tile in tilemap
	inc hl
	dec b			; check if full row has been drawn
	 jr nz, .draw_sprite
	dec c			; check if all rows have been drawn
	 ret z			; if so, exit
	push bc
		ld c, 12 + 32
		add hl, bc
		push hl
			push de
			pop hl
			ldh a, [map_w]
			ld c, a
			add hl, bc
			ld e, l
			ld d, h
		pop hl
	pop bc
	jr .row

; ### DATA ###

tile_data:
incbin "gfx/tiles.bin"
tile_end:

tilemap:
db 12
db 0, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 2
db 2, 0, 0, 2, 2, 0, 0, 0, 1, 2, 1, 2
db 0, 0, 0, 2, 2, 0, 0, 0, 2, 2, 1, 2
db 0, 0, 0, 2, 0, 0, 0, 2, 2, 2, 1, 2
db 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 2
db 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 2
db 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 2
db 0, 0, 0, 0, 1, 0, 0, 0, 0, 2, 1, 2
db 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 2
db $FF
