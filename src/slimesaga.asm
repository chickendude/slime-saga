include "inc/hardware.inc"

def HEIGHT_T	equ 9
def WIDTH_T		equ 10

; high RAM: ~126 bytes from $FF80 - $FFFE
def map_w_extra	equ _HRAM ; how much to add to move down a row in the map


; RAM: ~8000 bytes from $C000 - $DFFF
section "variables", wram0[_RAM]
START_OF_VARIABLES:
map_w: DS 1
map_h: DS 1
map_x: DS 2
map_y: DS 2
player_x: DS 2
player_y: DS 2
END_OF_VARIABLES:


section "header", rom0[$0100]
entrypoint:
	di
	jr start
	ds ($0150 - @), 0


section "main", rom0[$0150]
start:
	ld a, [rLY]			; wait for VBLANK to turn off LCD
	cp SCRN_Y
	 jr nz, start
	xor a
	ld [rLCDC], a		; disable LCD

	ld a, %11100100 	; light to dark
	ld [rBGP], a		; update palette

; set up RAM variables
	ld hl, map_w
	ld b, END_OF_VARIABLES - START_OF_VARIABLES
	xor a
.reset:
	ld [hl+], a
	dec b
	 jr nz, .reset

; load tile data into VRAM
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
	
	call update_camera	; [camera.asm]

	ld a, P1F_GET_DPAD	; prepare to read DPAD from key port
	ld [rP1], a			; send request to read DPAD status
	ldh a, [rP1]		; wait state
	ldh a, [rP1]		; DPAD status in a

	swap a
	push af
		and PADF_UP			;
		 call z, move_up	; [tilemap.asm]
	pop af
	push af
		and PADF_DOWN		;
		 call z, move_down	; [tilemap.asm]
	pop af
	push af
		and PADF_LEFT		;
		 call z, move_left	; [tilemap.asm]
	pop af
	push af
		and PADF_RIGHT		;
		 call z, move_right	; [tilemap.asm]
	pop af

	ld a, P1F_GET_NONE	; disable key polling
	ld [rP1], a			;
	jr main

; ### CODE ###

include "camera.asm"
include "tilemap.asm"

; ### DATA ###

tile_data:
incbin "gfx/tiles.bin"
tile_end:

tilemap:
db 13
db 0, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2
db 2, 0, 0, 2, 2, 0, 0, 0, 1, 2, 0, 2, 2
db 0, 0, 0, 2, 2, 0, 0, 0, 2, 2, 0, 0, 0
db 0, 0, 0, 2, 0, 0, 0, 2, 2, 2, 0, 0, 2
db 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 0, 2
db 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 0, 2
db 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 0, 2
db 0, 0, 0, 0, 1, 0, 0, 0, 0, 2, 2, 0, 2
db 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 1, 2
db 2, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2
db $FF
