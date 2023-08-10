; Loads tile map into VRAM
load_tilemap:
	ld de, tilemap
	ld hl, _SCRN0
	ld c, HEIGHT_T + 1
	ld a, [de]			; first byte in tilemap holds map width
	ld [map_w], a
	sub a, WIDTH_T + 1
	ldh [map_w_extra], a
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
	inc hl				; hl + 2: shift to next 16x16 tile in tilemap
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
			ldh a, [map_w_extra]	; load map width into bc
			ld c, a		; b should still be zero
			add hl, bc	; add map width to tilemap (move down one row in tilemap)
			ld e, l		; de = new tilemap position
			ld d, h		;
		pop hl			; hl = SCRN
	pop bc				; bc = row + column counters
	jr .row

; a = player_x
draw_column:
	ld bc, HEIGHT_T + 1	; c = number of rows to draw
	rra					; /2
	rra					; /4
	rra					; /8
	rra					; /16
	and %00011111		; clear out shifted bits
	ld e, a				; de = x position
	ld d, b				; (b set to 0 above)
	ld hl, tilemap + WIDTH_T + 1	; right side of tilemap
	add hl, de			; first tile in column
	; TODO: add y position
	push hl
		ld hl, _SCRN0 + 20
		add hl, de
		add hl, de
	pop de				; hl = VRAM tilemap, de = local tilemap
.loop:
	push bc				; save row counter (c)
		ld a, [de]		; a = tilemap tile
		add a, a		; x2
		ld [hl+], a		; save BYTE 1
		inc a			; 
		ld [hl], a		; BYTE 2
		add a, $F		; currently, tiles are stored in 2D, so we need to shift down a row
		ld c, 31		; bc = 31 (shift down a row in SCRN0)
		add hl, bc		; go to next row in SCRN0
		ld [hl+], a		; BYTE 3
		inc a			;
		ld [hl], a		; BYTE 4
		add hl,bc
		push hl
			ld l, e		; ld hl, de = position in tilemap
			ld h, d		;
			ld a, [map_w]	;
			ld c, a		; bc = map width
			add hl, bc	; go to next row in tilemap
			ld e, l		; ld de, hl
			ld d, h		; .. save result back to de (new position in tilemap)
		pop hl
	pop bc
	dec c
	 jr nz, .loop
	ret
	

move_up:
	;ld hl, rSCY
	ld hl, player_y
	ld a, [hl]
	or a
	 ret z
	dec [hl]
	ret
move_left:
	;ld hl, rSCX
	ld hl, player_x
	ld a, [hl]
	or a
	 ret z
	dec [hl]
	ret
move_right:
	;ld hl, rSCX
	ld hl, player_x
	ld a, [hl]			; a = player's x position
	inc a				; check x position to the right one pixel
	rra					; /2 tiles are 16 pixels wide
	rra					; /4
	rra					; /8
	rra					; /16
	and %00001111		; clear out possible carries
	add a, WIDTH_T		; check right edge of the screen
	ld b, a				;
	ld a, [map_w]		; check if x position has reached right edge of screen
	cp b				; if so, don't move player any further
	 ret z				; 
	inc [hl]			; move player right one pixel
	ld a, [hl]			; check if we've passed a tile boundary
	and $0F				;
	 ret nz				; if not, we're done
	ld a, [hl]			; a = new player x
	jr draw_column		; draw a new column
move_down:
	;ld hl, rSCY
	ld hl, player_y
	inc [hl]
	ret

