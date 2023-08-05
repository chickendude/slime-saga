include "inc/hardware.inc"

section "header", rom0[$0100]
entrypoint:
	di
	jr main
	ds ($0150 - @), 0


section "main", rom0[$0150]
main:
	jr main
