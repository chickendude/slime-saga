; There is some tricky math in here.
; 
; We leave the upper 4 bits of map_x intact in a and use some xor magic to put
; the bottom 4 bits of map_x + 1 into the bottom 4 bits of a. Then we swap, so
; that the MSB occupies the upper 4 bits and the LSB occupies the lower 4 bits,
; essentially dividing by 16, and clearing out the .4 in our 12.4 position.
;
; We then check if we need to scroll the map by comparing the player's position
; to the map position, offset to be in the center of the screen. 
; - If player's position is greater than the map position, then we need to shift
;   down/right.
; - If the player is in the center, we don't need to do anything.
; - If the player's position is less than the centered map position, then we need
;   to shift up/left.
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
	ld hl, player_x	; we're going to subtract player_x from map_x to see if we need to scroll
	ld a, [hl+]		; a = LSB of player x
	and $F0			; clear out subpixel positions
	ld b, [hl]		; bc = player x
	ld c, a			;
	ld hl, map_x	; hl = map x + 72 px (72px to the left, 16px for sprite, 72px to the right)
	ld a, [hl+]		; a = LSB of map x
	and $F0			; clear out subpixel positions
	ld h, [hl]		; hl = map x
	ld l, a			;
	ld de, $0480	; 4.5 tiles, 10 tiles fit on screen, so 4.5 scrolls when the player is right
	add hl, de		; .. in the middle
	ld a, l			; hl - bc: (center of) map x - player x
	sub c			;
	ld l, a			; save LSB, we need to check if it's zero later
	ld a, h			;
	sbc b			; subtract MSB
	rlca			; check if number is negative
	 jr c, .player_in_right_side
	cp l			; if a (which holds MSB) and l are both 0, the player's in the exact center of
	 jr z, .check_y	; .. screen and map doesn't need to be shifted
	call map_left	; otherwise, player is on the left side of the screen and map should be shifted
	jr .check_y		; .. left
.player_in_right_side:
	call map_right
.check_y:
	ld hl, player_y	; same as above, check if player y is >, <, or = to center of map y
	ld a, [hl+]		; a = pixel offset + .4 subpixel offset
	and $F0			; remove the subpixel offset to make calculation easier
	ld b, [hl]		; bc = player y
	ld c, a			;
	ld hl, map_y	; hl = map y + 64 px (centered: 64px above, 16px for sprite, 64 px below)
	ld a, [hl+]		; MSB
	and $F0			; clear out subpixel offset
	ld h, [hl]		; hl = map y
	ld l, a			;
	inc h			; shift down 4 tiles (screen is 9 tiles high, so the fifth tile is the middle of
	inc h			; .. the screen)
	inc h			;
	inc h			;
	ld a, l			; hl - bc
	sub c			;
	ld l, a			;
	ld a, h			;
	sbc b			;
	rlca			; check if bit 7 is set
	 jp c, map_down	; if so, player y (bc) > map y (hl), so map needs to be shifted down
	cp l			; if MSB (in a) and LSB (in l) are both zero, then player is in center
	 jp nz, map_up	; otherwise, they are above the center and we need to shift the screen up
	ret
