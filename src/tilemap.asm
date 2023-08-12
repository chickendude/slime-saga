; Loads tile map into VRAM
load_tilemap:
	ld de, tilemap
	ld hl, _SCRN0
	ld c, HEIGHT_T + 1
	ld a, [de]			; first byte in tilemap holds map width
	ld [map_w], a
	sub a, WIDTH_T + 1
	ldh [map_w_extra], a
	inc de
	ld a, [de]			; first byte in tilemap holds map width
	ld [map_h], a
.row:
	ld b, WIDTH_T + 1	; 10 16x16 tiles wide (160 pixels)
.draw_tile:
	inc de				; check next entry in tilemap
	ld a, [de]			; tile id
	add a, a			; x2
	add a, a			; x4 - our tiles are 16x16
	push hl				; hl = SCRN
	push bc				; bc = loop counters
		ld [hl+], a		; first tile into SCRN0 (16x16 tiles have 4 8x8 tiles)
		inc a			; next tile id
		ld [hl], a		; byte 2
		ld bc, $1F		; move down to next row in tilemap (32 tiles wide)
		add hl, bc		;
		inc a			; move down to next row in tileset
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

; ######################
; Multiply h and e together, store result in hl.
; 
; destroys:
;	- d, b
; output:
;	- hl = h * e
; ######################
mult_he:
	ld d, 0				; set d and l to 0
	ld l, d				; 
	ld b, 8				; 8 bits
.loop:
	add hl, hl          ; if there was a carry, we need to add e
     jr nc, .skip
		add	hl, de
.skip: 
	dec b				; repeat 8 times
     jr nz, .loop
	ret

; a = map X tile position
draw_column:
	ld c, a				; save a/map X tile
	ld a, [map_w]
	ld h, a
	ld a, [map_y + 1]	; to find the starting y position of the tilemap, we need to
	ld e, a				; .. multiply (y tile position * map width)
	call mult_he
	ld b, a				; save for later
	ld a, c
; set de to start of tilemap data
	ld e, low(tilemap + 2)	; de = right edge of tilemap + player X tile
	add a, e			; add map X to de (start of tilemap data)
	ld e, a				;
	adc a, high(tilemap + 2)
	sub e				;
	ld d, a				; de = a + tilemap + WIDTH_T + 1
	add hl, de
	ld e, l
	ld d, h
; set hl to start of SCRN0 data
	ld a, c				; a = map X tile
	and $F				; SCRN0 can only fit 16 (16x16) tiles, after that it wraps
	add a, a			; x2
	ld h, high(_SCRN0)	; hl = SCRN0 + X offset
	ld l, a				; SCRN0 = $9800, so no need to do any calculations here
; add y offset to SCRN0
	ld a, b
	rla
	rla
	swap a
	ld c, a
	and %1110_0000
	add a, l
	ld l, a
	adc a, h
	sub l
	ld h, a
	ld a, $3
	and c
	add a, h
	ld h, a
	ld c, HEIGHT_T + 1	; c = number of rows to draw
; hl = VRAM tilemap, de = local tilemap, c = number of rows left to draw
.loop:
	ld a, [de]			; a = tilemap tile
	add a, a			; x2
	add a, a			; x4
	ld [hl+], a			; set BYTE 1
	inc a				; 
	ld [hl-], a			; BYTE 2
	set 5, l			; + 32, even to odd row
	inc a				; 
	ld [hl+], a			; BYTE 3
	inc a				;
	ld [hl], a			; BYTE 4
; shift SCRN0 down a row
	ld a, 31			; add hl, 31: shift down another row
	add a, l			;
	ld l, a				; l = l + 31
	adc a, h			; add carry over from l to h
	sub l				; subtract l (because we only wanted to add the carry)
	ld h, a				; hl = hl + 31, SCRN0 shifted down a row
; shift de (tilemap) down a row
	ld a, [map_w]		; add hl, 31: shift down another row
	add a, e			;
	ld e, a				; l = l + 31
	adc a, d			; add carry over from l to h
	sub e				; subtract l (because we only wanted to add the carry)
	ld d, a				; hl = hl + 31, SCRN0 shifted down a row
	dec c
	 jr nz, .loop
	ret

move_left:
	ld hl, map_x		; first check if we're at the edge of the map
	ld a, [hl+]			; pixel offset + subpixel offset
	and $F0				; clear out subpixel offset (12.4 fixed point)
	cp [hl]				; check if both the tile and pixel offset are zero
	 ret z				; .. if so, can't move left any further
; update x position
	dec hl				; LSB of map_w
	ld a, [hl]			; subtract walking speed from map_x
	sub a, SPEED		; 
	ld [hl+], a
	 jr nc, .skip		; if there was no carry, no need to update the MSB of map_x
		dec [hl]
.skip:
	ld a, [hl]
	jr draw_column		; draw a new column

move_right:
	ld hl, map_x + 1	; first check if we're at the edge of the map
	ld a, [map_w]		; MSB of map_x holds the tile position
	sub WIDTH_T			;
	cp [hl]				; check if it equals map width
	 ret z
; update x position
	dec hl				; LSB of map_w
	ld a, [hl]			; increase map_x by one, making sure to carry over
	add a, SPEED		; 
	ld c, a
	ld [hl+], a
	adc a, [hl]
	sub c
	ld [hl], a
	add a, WIDTH_T + 1
	jr draw_column		; draw a new column

move_up:
	ld hl, map_y		; check if we're already at the top of the map
	ld a, [hl+]			; a = LSB
	and $F0				; clear out the subpixel offset
	cp [hl]				; if LSB and MSB both = 0, we're at the top
	 ret z				; quit if so
; update y position
	dec hl
	ld a, [hl]			; subtract walking speed from map_y
	sub a, SPEED		; 
	ld [hl+], a
	 jr nc, .skip		; if there was no carry, no need to update the MSB of map_y
		dec [hl]
.skip:
	ret

move_down:
	ld hl, map_y + 1
	ld a, [map_h]
	sub HEIGHT_T
	cp [hl]
	 ret z
; updaty y position
	dec hl

	ld a, [hl]
	add a, SPEED
	ld c, a
	ld [hl+], a
	adc a, [hl]
	sub c
	ld [hl], a
	ret

