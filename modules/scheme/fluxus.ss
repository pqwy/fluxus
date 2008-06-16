;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

#lang scheme/base

(require "fluxus-engine.ss"
         "fluxus-audio.ss"
         "fluxus-osc.ss"
			; todo: get rid of burnt in versions
		 fluxus-015/input
		 fluxus-015/help
		 fluxus-015/camera
		 fluxus-015/building-blocks
		 fluxus-015/collada-import
		 fluxus-015/randomness
;		 fluxus-015/fluxa ; removed until big pause in liblo startup is fixed
		 fluxus-015/scratchpad
		 (only-in srfi/13 string-pad))
		 
(provide 
 (all-from-out "fluxus-engine.ss")
 (all-from-out "fluxus-audio.ss")
 (all-from-out "fluxus-osc.ss")
 (all-from-out fluxus-015/input)
 (all-from-out fluxus-015/help)
 (all-from-out fluxus-015/camera)
 (all-from-out fluxus-015/building-blocks)
 (all-from-out fluxus-015/collada-import)
 (all-from-out fluxus-015/randomness)  
 ;(all-from-out fluxus-015/fluxa)
 (all-from-out fluxus-015/scratchpad))