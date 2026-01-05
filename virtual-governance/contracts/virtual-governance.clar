;; VirtualGovernance - Advanced DAO Governance System
;; A reputation-based governance framework with proposal management

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-voted (err u102))
(define-constant err-proposal-closed (err u103))
(define-constant err-insufficient-reputation (err u104))
(define-constant err-unauthorized (err u105))

;; Minimum reputation required to create proposals
(define-constant min-proposal-reputation u100)

;; Data Variables
(define-data-var proposal-count uint u0)
(define-data-var voting-period uint u1440) ;; blocks (~10 days)

;; Data Maps

;; Member reputation tracking
(define-map members
    principal
    {
        reputation: uint,
        tokens: uint,
        proposals-created: uint,
        votes-cast: uint,
        delegation: (optional principal)
    }
)

;; Proposal structure
(define-map proposals
    uint
    {
        proposer: principal,
        title: (string-ascii 256),
        description: (string-ascii 1024),
        votes-for: uint,
        votes-against: uint,
        start-block: uint,
        end-block: uint,
        executed: bool,
        status: (string-ascii 20)
    }
)

;; Track individual votes
(define-map votes
    {proposal-id: uint, voter: principal}
    {vote: bool, weight: uint, block-height: uint}
)

;; Delegation records
(define-map delegations
    {delegator: principal, delegate: principal}
    {active: bool, domains: (list 5 (string-ascii 50))}
)

;; Read-only functions

(define-read-only (get-member (member principal))
    (map-get? members member)
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (get-voting-power (member principal))
    (let
        (
            (member-data (unwrap! (map-get? members member) u0))
            (reputation (get reputation member-data))
            (tokens (get tokens member-data))
        )
        ;; Voting power = tokens + (reputation / 10)
        (+ tokens (/ reputation u10))
    )
)

(define-read-only (get-proposal-count)
    (var-get proposal-count)
)

(define-read-only (is-proposal-active (proposal-id uint))
    (match (map-get? proposals proposal-id)
        proposal-data
        (let
            (
                (current-block block-height)
                (end-block (get end-block proposal-data))
                (executed (get executed proposal-data))
            )
            (and (< current-block end-block) (not executed))
        )
        false
    )
)

;; Public functions

;; Register as a member
(define-public (register-member)
    (let
        (
            (existing-member (map-get? members tx-sender))
        )
        (if (is-none existing-member)
            (begin
                (map-set members tx-sender
                    {
                        reputation: u50,
                        tokens: u0,
                        proposals-created: u0,
                        votes-cast: u0,
                        delegation: none
                    }
                )
                (ok true)
            )
            (ok false)
        )
    )
)

;; Add tokens (simulated for demonstration)
(define-public (add-tokens (amount uint))
    (let
        (
            (member-data (unwrap! (map-get? members tx-sender) err-not-found))
            (current-tokens (get tokens member-data))
        )
        (map-set members tx-sender
            (merge member-data {tokens: (+ current-tokens amount)})
        )
        (ok true)
    )
)

;; Create a new proposal
(define-public (create-proposal (title (string-ascii 256)) (description (string-ascii 1024)))
    (let
        (
            (member-data (unwrap! (map-get? members tx-sender) err-not-found))
            (reputation (get reputation member-data))
            (new-proposal-id (+ (var-get proposal-count) u1))
            (start-block block-height)
            (end-block (+ block-height (var-get voting-period)))
        )
        ;; Check minimum reputation
        (asserts! (>= reputation min-proposal-reputation) err-insufficient-reputation)
        
        ;; Create proposal
        (map-set proposals new-proposal-id
            {
                proposer: tx-sender,
                title: title,
                description: description,
                votes-for: u0,
                votes-against: u0,
                start-block: start-block,
                end-block: end-block,
                executed: false,
                status: "active"
            }
        )
        
        ;; Update member stats
        (map-set members tx-sender
            (merge member-data 
                {
                    proposals-created: (+ (get proposals-created member-data) u1),
                    reputation: (+ reputation u10)
                }
            )
        )
        
        ;; Increment proposal count
        (var-set proposal-count new-proposal-id)
        (ok new-proposal-id)
    )
)

;; Cast a vote on a proposal
(define-public (cast-vote (proposal-id uint) (vote-for bool))
    (let
        (
            (proposal-data (unwrap! (map-get? proposals proposal-id) err-not-found))
            (member-data (unwrap! (map-get? members tx-sender) err-not-found))
            (existing-vote (map-get? votes {proposal-id: proposal-id, voter: tx-sender}))
            (voting-power (get-voting-power tx-sender))
        )
        ;; Check proposal is active
        (asserts! (is-proposal-active proposal-id) err-proposal-closed)
        
        ;; Check hasn't voted already
        (asserts! (is-none existing-vote) err-already-voted)
        
        ;; Record vote
        (map-set votes 
            {proposal-id: proposal-id, voter: tx-sender}
            {vote: vote-for, weight: voting-power, block-height: block-height}
        )
        
        ;; Update proposal vote counts
        (if vote-for
            (map-set proposals proposal-id
                (merge proposal-data 
                    {votes-for: (+ (get votes-for proposal-data) voting-power)}
                )
            )
            (map-set proposals proposal-id
                (merge proposal-data 
                    {votes-against: (+ (get votes-against proposal-data) voting-power)}
                )
            )
        )
        
        ;; Update member reputation and stats
        (map-set members tx-sender
            (merge member-data 
                {
                    votes-cast: (+ (get votes-cast member-data) u1),
                    reputation: (+ (get reputation member-data) u5)
                }
            )
        )
        
        (ok true)
    )
)

;; Delegate voting power to another member
(define-public (delegate-to (delegate principal))
    (let
        (
            (member-data (unwrap! (map-get? members tx-sender) err-not-found))
            (delegate-data (unwrap! (map-get? members delegate) err-not-found))
        )
        ;; Update delegation
        (map-set members tx-sender
            (merge member-data {delegation: (some delegate)})
        )
        
        (map-set delegations
            {delegator: tx-sender, delegate: delegate}
            {active: true, domains: (list)}
        )
        
        (ok true)
    )
)

;; Remove delegation
(define-public (remove-delegation)
    (let
        (
            (member-data (unwrap! (map-get? members tx-sender) err-not-found))
        )
        (map-set members tx-sender
            (merge member-data {delegation: none})
        )
        (ok true)
    )
)

;; Execute a proposal (simplified)
(define-public (execute-proposal (proposal-id uint))
    (let
        (
            (proposal-data (unwrap! (map-get? proposals proposal-id) err-not-found))
            (votes-for (get votes-for proposal-data))
            (votes-against (get votes-against proposal-data))
            (executed (get executed proposal-data))
        )
        ;; Check proposal hasn't been executed
        (asserts! (not executed) err-proposal-closed)
        
        ;; Check voting period has ended
        (asserts! (>= block-height (get end-block proposal-data)) err-proposal-closed)
        
        ;; Check proposal passed (simple majority)
        (if (> votes-for votes-against)
            (begin
                (map-set proposals proposal-id
                    (merge proposal-data {executed: true, status: "executed"})
                )
                (ok true)
            )
            (begin
                (map-set proposals proposal-id
                    (merge proposal-data {executed: true, status: "rejected"})
                )
                (ok false)
            )
        )
    )
)

;; Admin function to update voting period
(define-public (set-voting-period (new-period uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set voting-period new-period)
        (ok true)
    )
)

;; Initialize contract (can be called once by anyone to set up initial state)
(define-public (initialize)
    (let
        (
            (registration-result (register-member))
        )
        (ok true)
    )
)