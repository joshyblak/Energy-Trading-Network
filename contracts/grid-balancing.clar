;; Grid Balancing Contract
;; Manages overall stability of the energy network

(define-map grid-status
  { region: (string-ascii 20) }
  {
    total-production: uint,
    total-consumption: uint,
    last-updated: uint,
    balanced: bool
  }
)

;; Initialize a new grid region
(define-public (initialize-region (region (string-ascii 20)))
  (let
    (
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    (asserts! (is-none (map-get? grid-status { region: region })) (err u1))

    (map-set grid-status
      { region: region }
      {
        total-production: u0,
        total-consumption: u0,
        last-updated: current-time,
        balanced: true
      }
    )
    (ok true)
  )
)

;; Update production for a region
(define-public (update-production (region (string-ascii 20)) (production uint))
  (let
    (
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (status (unwrap! (map-get? grid-status { region: region }) (err u404)))
      (consumption (get total-consumption status))
      (balanced (is-balanced production consumption))
    )
    (map-set grid-status
      { region: region }
      {
        total-production: production,
        total-consumption: consumption,
        last-updated: current-time,
        balanced: balanced
      }
    )
    (ok balanced)
  )
)

;; Update consumption for a region
(define-public (update-consumption (region (string-ascii 20)) (consumption uint))
  (let
    (
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (status (unwrap! (map-get? grid-status { region: region }) (err u404)))
      (production (get total-production status))
      (balanced (is-balanced production consumption))
    )
    (map-set grid-status
      { region: region }
      {
        total-production: production,
        total-consumption: consumption,
        last-updated: current-time,
        balanced: balanced
      }
    )
    (ok balanced)
  )
)

;; Helper function to determine if grid is balanced
(define-private (is-balanced (production uint) (consumption uint))
  (let
    (
      (difference (if (> production consumption)
                     (- production consumption)
                     (- consumption production)))
      (threshold (* consumption u5)) ;; 5% threshold
    )
    (<= (* difference u100) threshold)
  )
)

;; Get grid status for a region
(define-read-only (get-grid-status (region (string-ascii 20)))
  (map-get? grid-status { region: region })
)

;; Check if a region's grid is balanced
(define-read-only (is-grid-balanced (region (string-ascii 20)))
  (default-to
    false
    (get balanced (map-get? grid-status { region: region }))
  )
)

