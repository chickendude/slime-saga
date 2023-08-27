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


draw_player:
	ld hl, player_x
	inc [hl]
	ld a, [hl+]
	ld [_OAMRAM], a		; x
	ld [_OAMRAM + 4], a		; x

	inc hl
	inc [hl]
	ld a, [hl]
	rra
	ld [_OAMRAM + 1], a	; y
	add a, 8
	ld [_OAMRAM + 5], a	; y
; tile ids
	rra
	rra
	rra
	and %1
	add a, a
	add a, a
	ld [_OAMRAM + 2], a	; tile id
	inc a
	inc a
	ld [_OAMRAM + 6], a	; tile id
	ret
