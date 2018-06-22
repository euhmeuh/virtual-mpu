#lang racket/base

(provide
  (struct-out reg-info)
  (struct-out status-info)
  (struct-out interrupts-info))

(struct reg-info (name size) #:transparent)
(struct status-info (register bits) #:transparent)
(struct interrupts-info (bit interrupts) #:transparent)
