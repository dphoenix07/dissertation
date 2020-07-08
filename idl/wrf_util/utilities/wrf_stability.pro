FUNCTION WRF_STABILITY, theta, p

;+
;NAME:
;     WRF_STABILITY
;PURPOSE:
;     This computes the static stability d(theta)/dp form the WRF output files
;     using a generally uncentered finite-difference formula.
;CATEGORY:
;     WRF data utility.
;CALLING SEQUENCE:
;     dthetadp = WRF_STABILITY(theta)
;INPUT:
;     theta    : structure containing potential temperature (K)
;OUTPUT:
;     dthetadp : structure containing d(theta)/dp
;KEYWORDS:
;     None.
;MODIFICATION HISTORY:
;     Cameron R. Homeyer. 2012-11-28
;-

COMPILE_OPT IDL2

dthetadp = theta
dim      = SIZE(theta, /DIMENSIONS)
nz       = dim[2]

FOR k = 1, nz-2 DO BEGIN
    dp1 = p[*,*,k  ] - p[*,*,k-1]
    dp2 = p[*,*,k+1] - p[*,*,k  ]
    dthetadp[*,*,k]	= ((theta[*,*,k+1] - theta[*,*,k])*dp1^2 - $			;Uncentered differences on irregular grid
										(theta[*,*,k-1] - theta[*,*,k])*dp2^2)  $
										/(dp1*dp2*(dp1+dp2))             
ENDFOR

dp1 = p[*,*,1] - p[*,*,0]
dp2 = p[*,*,2] - p[*,*,0]
dthetadp[*,*,   0]	= ((theta[*,*,   1] - theta[*,*,   0])*dp2^2 - $		;Uncentered difference at bottom
										(theta[*,*,   2] - theta[*,*,   0])*dp1^2)  $
                     			/(dp1*dp2*(dp2-dp1))

dp1 = p[*,*,nz-2] - p[*,*,nz-1]
dp2 = p[*,*,nz-3] - p[*,*,nz-1]
dthetadp[*,*,nz-1]	= ((theta[*,*,nz-2] - theta[*,*,nz-1])*dp2^2 - $		;Uncentered difference at top
										(theta[*,*,nz-3] - theta[*,*,nz-1])*dp1^2)  $
                       		 /(dp1*dp2*(dp2-dp1))

RETURN, dthetadp
END
