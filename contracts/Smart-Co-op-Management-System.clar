(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-member (err u101))
(define-constant err-already-member (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-proposal-not-found (err u104))
(define-constant err-already-voted (err u105))
(define-constant err-proposal-expired (err u106))
(define-constant err-proposal-not-passed (err u107))
(define-constant err-invalid-amount (err u108))
(define-constant err-invalid-duration (err u109))

(define-data-var next-proposal-id uint u1)
(define-data-var total-pool-funds uint u0)
(define-data-var member-count uint u0)

(define-map members principal 
  {
    contribution: uint,
    join-block: uint,
    active: bool
  })

(define-map proposals uint
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    amount: uint,
    proposal-type: (string-ascii 20),
    votes-for: uint,
    votes-against: uint,
    executed: bool,
    created-at: uint,
    expires-at: uint
  })

(define-map votes {proposal-id: uint, voter: principal} bool)

(define-map profit-distributions uint
  {
    total-amount: uint,
    per-member-share: uint,
    distribution-block: uint,
    claimed-count: uint
  })

(define-map profit-claims {distribution-id: uint, member: principal} bool)

(define-public (join-coop (contribution uint))
  (let ((sender tx-sender))
    (asserts! (> contribution u0) err-invalid-amount)
    (asserts! (is-none (map-get? members sender)) err-already-member)
    (try! (stx-transfer? contribution sender (as-contract tx-sender)))
    (map-set members sender
      {
        contribution: contribution,
        join-block: stacks-block-height,
        active: true
      })
    (var-set total-pool-funds (+ (var-get total-pool-funds) contribution))
    (var-set member-count (+ (var-get member-count) u1))
    (ok true)))

(define-public (add-funds (amount uint))
  (let ((sender tx-sender)
        (member-data (unwrap! (map-get? members sender) err-not-member)))
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (get active member-data) err-not-member)
    (try! (stx-transfer? amount sender (as-contract tx-sender)))
    (map-set members sender
      (merge member-data {contribution: (+ (get contribution member-data) amount)}))
    (var-set total-pool-funds (+ (var-get total-pool-funds) amount))
    (ok true)))

(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 500)) (amount uint) (proposal-type (string-ascii 20)) (duration uint))
  (let ((sender tx-sender)
        (proposal-id (var-get next-proposal-id))
        (member-data (unwrap! (map-get? members sender) err-not-member)))
    (asserts! (get active member-data) err-not-member)
    (asserts! (> duration u0) err-invalid-duration)
    (asserts! (>= (var-get total-pool-funds) amount) err-insufficient-funds)
    (map-set proposals proposal-id
      {
        proposer: sender,
        title: title,
        description: description,
        amount: amount,
        proposal-type: proposal-type,
        votes-for: u0,
        votes-against: u0,
        executed: false,
        created-at: stacks-block-height,
        expires-at: (+ stacks-block-height duration)
      })
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)))

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let ((sender tx-sender)
        (member-data (unwrap! (map-get? members sender) err-not-member))
        (proposal-data (unwrap! (map-get? proposals proposal-id) err-proposal-not-found)))
    (asserts! (get active member-data) err-not-member)
    (asserts! (<= stacks-block-height (get expires-at proposal-data)) err-proposal-expired)
    (asserts! (is-none (map-get? votes {proposal-id: proposal-id, voter: sender})) err-already-voted)
    (map-set votes {proposal-id: proposal-id, voter: sender} vote-for)
    (if vote-for
      (map-set proposals proposal-id
        (merge proposal-data {votes-for: (+ (get votes-for proposal-data) u1)}))
      (map-set proposals proposal-id
        (merge proposal-data {votes-against: (+ (get votes-against proposal-data) u1)})))
    (ok true)))

(define-public (execute-proposal (proposal-id uint))
  (let ((proposal-data (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
        (total-votes (+ (get votes-for proposal-data) (get votes-against proposal-data)))
        (required-votes (/ (var-get member-count) u2)))
    (asserts! (not (get executed proposal-data)) err-proposal-not-passed)
    (asserts! (> stacks-block-height (get expires-at proposal-data)) err-proposal-expired)
    (asserts! (> (get votes-for proposal-data) (get votes-against proposal-data)) err-proposal-not-passed)
    (asserts! (>= total-votes required-votes) err-proposal-not-passed)
    (if (is-eq (get proposal-type proposal-data) "purchase")
      (begin
        (try! (as-contract (stx-transfer? (get amount proposal-data) tx-sender (get proposer proposal-data))))
        (var-set total-pool-funds (- (var-get total-pool-funds) (get amount proposal-data))))
      (if (is-eq (get proposal-type proposal-data) "investment")
        (begin
          (try! (as-contract (stx-transfer? (get amount proposal-data) tx-sender (get proposer proposal-data))))
          (var-set total-pool-funds (- (var-get total-pool-funds) (get amount proposal-data))))
        true))
    (map-set proposals proposal-id
      (merge proposal-data {executed: true}))
    (ok true)))

(define-public (distribute-profits (total-profit uint))
  (let ((sender tx-sender)
        (distribution-id (var-get next-proposal-id))
        (member-count-val (var-get member-count))
        (per-member-share (/ total-profit member-count-val)))
    (asserts! (is-eq sender contract-owner) err-owner-only)
    (asserts! (> total-profit u0) err-invalid-amount)
    (asserts! (> member-count-val u0) err-invalid-amount)
    (try! (stx-transfer? total-profit sender (as-contract tx-sender)))
    (map-set profit-distributions distribution-id
      {
        total-amount: total-profit,
        per-member-share: per-member-share,
        distribution-block: stacks-block-height,
        claimed-count: u0
      })
    (var-set next-proposal-id (+ distribution-id u1))
    (ok distribution-id)))

(define-public (claim-profit (distribution-id uint))
  (let ((sender tx-sender)
        (member-data (unwrap! (map-get? members sender) err-not-member))
        (distribution-data (unwrap! (map-get? profit-distributions distribution-id) err-proposal-not-found)))
    (asserts! (get active member-data) err-not-member)
    (asserts! (is-none (map-get? profit-claims {distribution-id: distribution-id, member: sender})) err-already-voted)
    (try! (as-contract (stx-transfer? (get per-member-share distribution-data) tx-sender sender)))
    (map-set profit-claims {distribution-id: distribution-id, member: sender} true)
    (map-set profit-distributions distribution-id
      (merge distribution-data {claimed-count: (+ (get claimed-count distribution-data) u1)}))
    (ok true)))

(define-public (leave-coop)
  (let ((sender tx-sender)
        (member-data (unwrap! (map-get? members sender) err-not-member)))
    (asserts! (get active member-data) err-not-member)
    (map-set members sender
      (merge member-data {active: false}))
    (var-set member-count (- (var-get member-count) u1))
    (ok true)))

(define-read-only (get-member-info (member principal))
  (map-get? members member))

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id))

(define-read-only (get-pool-funds)
  (var-get total-pool-funds))

(define-read-only (get-member-count)
  (var-get member-count))

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes {proposal-id: proposal-id, voter: voter}))

(define-read-only (get-profit-distribution (distribution-id uint))
  (map-get? profit-distributions distribution-id))

(define-read-only (has-claimed-profit (distribution-id uint) (member principal))
  (map-get? profit-claims {distribution-id: distribution-id, member: member}))

(define-read-only (get-next-proposal-id)
  (var-get next-proposal-id))
