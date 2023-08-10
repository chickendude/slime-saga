update_camera:
	ld hl, map_x
	;ld de, map_x
	ld a, [hl+]
	;ld [de], a
	ld [rSCX], a
	inc hl
	;inc de
	;inc de
	ld a, [hl]
	;ld [de], a
	ld [rSCY], a
	ret
