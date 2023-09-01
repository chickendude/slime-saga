; There is some tricky math in here.
; 
; We leave the upper 4 bits of map_x intact in a and use some xor magic to put
; the bottom 4 bits of map_x + 1 into the bottom 4 bits of a. Then we swap, so
; that the MSB occupies the upper 4 bits and the LSB occupies the lower 4 bits,
; essentially dividing by 16, and clearing out the .4 in our 12.4 position.
update_camera:
	ld hl, map_x	; 
	ld a, [hl+]		; a = pixel + .4 subpixel offset
; this shifts the 16-bit number right 4 bits
	xor [hl]		; xor'ing twice will undo the change
	and $F0			; clearing out bottom 4 bits means xor leaves upper 4 bits
	xor [hl]		; .. intact and bottom 4 bits = map_x + 1
	swap a			; swap nibbles so that we have TTTTPPPP (Tile/Pixel)
	ld [rSCX], a	; update scroll register
	inc hl			; hl = map_y
; repeat with map_y
	ld a, [hl+]		; a = LSB, hl = MSB
	xor [hl]		; >> 4
	and $F0			; ..
	xor [hl]		; ..
	swap a			; ..
	ld [rSCY], a	; save to y scroll register
; check if screen needs to scroll horizontally to keep up with player
	ld hl, map_x	; 
	ld a, [hl+]		; a = pixel offset + .4 subpixel offset
	ld e, a
	ld a, 4			; middle of screen (start x + 4 tiles)
	add a, [hl]		; a = pixel offset + .4 subpixel offset
	ld d, a
	ld a, [player_x + 1]
	cp d
	push af			; a = player x tile position - need to save these as they're destroyed
	push de			; d = map x tile position
		call c, map_left
	pop de
	pop af
	dec a
	cp d
	 call nc, map_right
; check if screen needs to scroll vertically
	ld hl, map_y	; 
	ld a, [hl+]		; a = pixel offset + .4 subpixel offset
	ld e, a
	ld d, [hl]		; a = pixel offset + .4 subpixel offset
	ld a, 4
	add a, d
	ld d, a
	ld a, [player_y + 1]
	cp d
	 jp c, map_up
	dec a
	cp d
	 jp nc, map_down
	ret
