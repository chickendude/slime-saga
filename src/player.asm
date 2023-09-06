; Our sprite data isn't optimal currently, so we have to do some magic
; to draw the sprites piecemeal. We draw the top left half of the sprite
load_player_sprites:
	ld de, sprite_data
	ld hl, _VRAM8000
	ld bc, 2 * 4
.sprite_loop:
	push bc
		call draw_8		; draw top half of sprite
		ld a, 16
		add a, e
		ld e, a
		adc a, d
		sub e
		ld d, a
		call draw_8		; draw bottom half of sprite
		ld a, -32
		add a, e
		ld e, a
		 jr c, .skip
			dec d
.skip:
		call draw_8		; draw top half of sprite
		ld a, 16
		add a, e
		ld e, a
		adc a, d
		sub e
		ld d, a
		call draw_8		; draw bottom half of sprite
	pop bc
	dec bc
	ld a, c
	cp b
     jr nz, .sprite_loop
	ret

draw_8:
	ld b, 8
.loop:
; byte 1
	ld a, [de]			; a = sprite 1 byte 1
	ld [hl+], a			; store sprite 1 byte 1
	inc de				; de = sprite 1 byte 2
; byte 2
	ld a, [de]			; a = sprite 1 byte 2
	ld [hl+], a			; store sprite 1 byte 2
	inc de				; de = sprite 2, byte 1
	dec b
	 jr nz, .loop
	ret

; Determine player's X/Y position and which sprite ID (which frame) to draw
draw_player:
; update x
	ld hl, map_x			; subtract map x from player x to determine where
	ld de, player_x			;
	ld a, [de]				;
	sub [hl]				;
	ld c, a					; save result into c
	inc hl					; MSB of map_x
	inc de					; MSB of player_x
	ld a, [de]				;
	sbc [hl]				; subtract MSBs of map_x/player_x
	xor c					; put LSB into top 4 bits then swap to essentially
	and $0f					; .. shift right 4 pixels (or divide by 16)
	xor c					;
	swap a					;
	add a, 8				; sprites start at (8,16)
	ld [_OAMRAM + OAMA_X], a	; x of left half of sprite
	add a, 8
	ld [_OAMRAM + 4 + OAMA_X], a	; x of right half
; update y
	inc hl					; map Y
	inc de					; player Y
	ld a, [de]				; a = lower byte of player y
	sub [hl]				; player y - map y
	ld c, a					; save result into c
	inc hl					; MSB of map_y
	inc de					; MSB of player_y
	ld a, [de]				;
	sbc [hl]				; subtract MSBs of map_y/player_y
	xor c					; put LSB into top 4 bits then swap to essentially
	and $0f					; .. shift right 4 pixels (or divide by 16)
	xor c					;
	swap a					;
	add a, 16				; sprites start at (8,16)
	ld [_OAMRAM + OAMA_Y], a	; y of left half of sprite
	ld [_OAMRAM + 4 + OAMA_Y], a; y of right half
; sprite ids
	ld hl, player_dir		; determine which direction the player is facing
	ld a, [hl+]				; hl = player_frame (used to calculate which animation frame we're on)
	add a, a				; x2 - each frame has 2 frames, taking up 4 sprites each
	add a, a				; x4
	add a, a				; x8
	bit 4, [hl]				; used to alternate frames, toggles every 16 frames
	 jr z, .update_id
		add a, 4			; skip to second frame: 4 sprites per animation frame
.update_id:
	ld [_OAMRAM + OAMA_TILEID], a	; left tile id - player sprite is 2 8x16 sprites side by side
	add a, 2						; right side of sprite begins two 8x8 sprites after left side
	ld [_OAMRAM + 4 + OAMA_TILEID], a; right tile id
	ret


move_left:
	ld a, LEFT
	ld [player_dir], a
	ld hl, player_x		; first check if we're at the edge of the map
	ld a, [hl+]			; pixel offset + subpixel offset
	and $F0				; clear out subpixel offset (12.4 fixed point)
	or [hl]				; check if both the tile and pixel offset are zero
	 ret z				; .. if so, can't move left any further
; check if tile is passable
	ld a, [hl-]			; determine player's target x tile
	ld l, [hl]
	ld h, a
	ld bc, 16 * 2 - SPEED
	add hl, bc
	ld a, h
	call check_collision_horiz	; [tilemap.asm] a = x
	 ret nz
; update x position
	ld hl, player_x
	ld a, [hl]			; subtract walking speed from player_x
	sub a, SPEED		; 
	ld [hl+], a
	 ret nc				; if there was no carry, no need to update the MSB of player_x
	dec [hl]
	ret

move_right:
	ld a, RIGHT
	ld [player_dir], a
	ld hl, player_x + 1	; first check if we're at the edge of the map
	ld a, [map_w]		; MSB of player_x holds the tile position
	dec a				; We don't want player to go past the end of the map (or do we?)
	cp [hl]				; check if it equals map width
	 ret z
; check if tile is passable
	ld a, [hl-]			; determine player's target x tile
	ld l, [hl]
	ld h, a
	ld bc, 16 * 14 + SPEED
	add hl, bc
	ld a, h
	call check_collision_horiz	; [tilemap.asm] a = x
	 ret nz
; update x position
	ld hl, player_x
	ld a, [hl]			; increase player_x by one, making sure to carry over
	add a, SPEED		; 
	ld [hl+], a
	 ret nc
	inc [hl]
	ret

move_up:
	xor a				; UP = 0
	ld [player_dir], a
	ld hl, player_y		; check if we're already at the top of the map
	ld a, [hl+]			; a = LSB
	and $F0				; clear out the subpixel offset
	or [hl]				; if LSB and MSB both = 0, we're at the top
	 ret z				; quit if so
; update y position
	dec hl				; hl = LSB of player_y
	ld a, [hl]			; subtract walking speed from player_y
	sub a, SPEED		; 
	ld [hl+], a			; save new position
	 ret nc				; if there was no carry, no need to update the MSB of player_y
	dec [hl]		; otherwise, update MSB of player_y
	ret

move_down:
	ld a, DOWN
	ld [player_dir], a
	ld hl, player_y + 1	; MSB of player_y (aligned tile position)
	ld a, [map_h]		; check if at the bottom of the map
	dec a
	cp [hl]				; player_y == player_h - num_rows
	 ret z				; quit if we're at the bottom
; update y position
	dec hl				; LSB of player_y
	ld a, [hl]			; 
	add a, SPEED		; add speed to player_y
	ld [hl+], a
	 ret nc
	inc [hl]
	ret

