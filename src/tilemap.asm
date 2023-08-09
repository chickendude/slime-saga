; Loads tile map into VRAM
load_tilemap:
	ld de, tilemap
	ld hl, _SCRN0
	ld c, HEIGHT_T + 1
	ld a, [de]			; first byte in tilemap holds map width
	sub a, WIDTH_T + 1
	ldh [map_w], a
.row:
	ld b, WIDTH_T + 1	; 10 16x16 tiles wide (160 pixels)
.draw_tile:
	inc de				; check next entry in tilemap
	ld a, [de]			; tile id
	add a, a			; x2 - our tiles are 16x16
	push hl				; hl = SCRN
	push bc				; bc = loop counters
		ld [hl+], a		; first tile into SCRN0 (16x16 tiles have 4 8x8 tiles)
		inc a			; next tile id
		ld [hl], a		; byte 2
		ld bc, $1F		; move down to next row in tilemap (32 tiles wide)
		add hl, bc		;
		add a, $F		; move down to next row in tileset
		ld [hl+], a		; byte 3
		inc a			;
		ld [hl], a		; byte 4
	pop bc				; bc = loop counters
	pop hl				; hl = SCRN
	inc hl				; shift to next 16x16 tile in tilemap
	inc hl				;
	dec b				; check if full row has been drawn
	 jr nz, .draw_tile	; .. if not, draw next tile
	dec c				; check if all rows have been drawn
	 ret z				; .. if so, exit
	push bc				; bc = row + column counters
		ld c, 10 + 32	; we drew 22 colums (11 16x16 sprites), tilemaps are 32
		add hl, bc		; .. tiles wide, so add 10 to get to next row
		push hl			; hl = SCRN
			push de		; ex de, hl
			pop hl		; hl = tilemap position, de = SCRN
			ldh a, [map_w]	; load map width into bc
			ld c, a		; b should still be zero
			add hl, bc	; add map width to tilemap (move down one row in tilemap)
			ld e, l		; de = new tilemap position
			ld d, h		;
		pop hl			; hl = SCRN
	pop bc				; bc = row + column counters
	jr .row

