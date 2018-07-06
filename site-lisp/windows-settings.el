(eval-when-compile (require 'req-package))

(req-package cygwin-mount
  :if (system-type-is-win)
  :functions system-type-is-win)

;; (req-package setup-cygwin
;;   :if (system-type-is-win)
;;   :functions system-type-is-win)

(req-package w32-browser
  :if (system-type-is-win)
  :functions system-type-is-win)

(req-package open-in-msvs
  :if (system-type-is-win)
  :functions system-type-is-win
  :commands
  open-in-msvs)

(provide 'windows-settings)
