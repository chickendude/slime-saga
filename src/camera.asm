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
	ret
