# Madelaine mode for Emacs

## Install Madelaine

A `madelaine-mode.el` is provided.


## Install Tilleul (LSP)

With `use-package`:

```
(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(madeleine-mode . "madeleine"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "tilleul")
                    :activation-fn (lsp-activate-on "madeleine")
                    :server-id 'tilleul)))
```
