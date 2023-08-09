include "inc/hardware.inc"

def HEIGHT_T equ 9
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

main:
.vblank:
	ld a, [rLY]
	cp SCRN_Y
	 jr nz, .vblank

	ld a, P1F_GET_DPAD	; prepare to read DPAD from key port
	ld [rP1], a			; send request to read DPAD status
	ldh a, [rP1]		; wait state
	ldh a, [rP1]		; DPAD status in a

	swap a
	push af
		and PADF_UP			;
		 call z, move_up
	pop af
	push af
		and PADF_DOWN		;
		 call z, move_down
	pop af
	push af
		and PADF_LEFT		;
		 call z, move_left
	pop af
	push af
		and PADF_RIGHT		;
		 call z, move_right
	pop af

	ld a, P1F_GET_NONE	; disable key polling
	ld [rP1], a			;
	jr main

; ### CODE ###

move_up:
	ld hl, rSCY
	dec [hl]
	ret
move_right:
	ld hl, rSCX
	inc [hl]
	ret
move_down:
	ld hl, rSCY
	inc [hl]
	ret
move_left:
	ld hl, rSCX
	dec [hl]
	ret

include "tilemap.asm"

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
db 2, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 2
db $FF
