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
		add hl, de
.skip: 
	dec b				; repeat 8 times
     jr nz, .loop
	ret

